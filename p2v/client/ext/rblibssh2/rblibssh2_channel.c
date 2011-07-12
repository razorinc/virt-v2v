/* Copyright (C) 2011 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <ruby.h>
#include <rubyio.h>

#include <aio.h>
#include <unistd.h>
#include <pthread.h>
#include <libssh2.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/mman.h>

#include "rblibssh2.h"

/* Defined in rblibssh2.h */
VALUE cChannel;

static VALUE eApplicationError;
static VALUE eShortReadError;

static VALUE session_exec(VALUE self_rv, VALUE cmd_rv);

static VALUE channel_read(VALUE self_rv, VALUE bytes_rv);
static VALUE channel_write(VALUE self_rv, VALUE data_rv);
static VALUE channel_send_data(VALUE self_rv, VALUE io_rv);
static VALUE channel_close(VALUE self_rv);

void rblibssh2_channel_init()
{
    rb_define_method(cSession, "exec", session_exec, 1);

    cChannel = rb_define_class_under(cLibssh2, "Channel", rb_cObject);
    rb_define_method(cChannel, "read", channel_read, 1);
    rb_define_method(cChannel, "write", channel_write, 1);
    rb_define_method(cChannel, "send_data", channel_send_data, 1);
    rb_define_method(cChannel, "close", channel_close, 0);

    eApplicationError = rb_define_class_under(cChannel, "ApplicationError",
                                              rb_eStandardError);
    eShortReadError = rb_define_class_under(cChannel, "ShortReadError",
                                            rb_eStandardError);
}

struct channel {
    struct session *s;
    LIBSSH2_CHANNEL *channel;

    VALUE rv; /* Only to be used when removing from the session */
};

static void channel_free(void *arg) {
    struct channel *c = (struct channel *)arg;

    if (c->channel)
        channel_close(c->rv);

    xfree(c);

    /* session is just a handle. Don't free it here. */
}

struct session_exec {
    struct channel *c;
    const char *cmd;
};

static void *session_exec_w(void *params)
{
    struct session_exec *se = (struct session_exec *)params;
    struct channel *c = se->c;

    LIBSSH2_SESSION *session = rblibssh2_session_get(c->s);

    for (;;) {
        c->channel = libssh2_channel_open_session(session);
        if (c->channel == NULL) {
            char *err;
            if (libssh2_session_last_error(session, &err, NULL, 0) ==
                LIBSSH2_ERROR_EAGAIN)
            {
                rblibssh2_session_wait(c->s);
                continue;
            } else {
                rblibssh2_session_set_error(rb_eIOError,
                        "Failed to open channel: %s", err);
                return NULL;
            }
        }
        break;
    }

    int rc;
    while ((rc = libssh2_channel_exec(c->channel, se->cmd)) ==
        LIBSSH2_ERROR_EAGAIN)
    {
        rblibssh2_session_wait(c->s);
    }

    if (rc != 0) {
        char *err;
        libssh2_session_last_error(session, &err, NULL, 0);
        rblibssh2_session_set_error(rb_eIOError,
                "Failed to exec %s: %s", se->cmd, err);
        return NULL;
    }

    return c;
}

static VALUE session_exec(VALUE self_rv, VALUE cmd_rv)
{
    struct session *s;
    Data_Get_Struct(self_rv, struct session, s);

    if (rblibssh2_session_get(s) == NULL)
        rb_raise(eInternalError, "Session is closed");

    struct channel *c;
    VALUE channel_rv = Data_Make_Struct(cChannel, struct channel,
                                        NULL, channel_free, c);
    memset(c, 0, sizeof(*c));
    c->s = s;
    c->rv = channel_rv;

    rblibssh2_session_channel_add(s, channel_rv);

    struct session_exec p = {
        .c = c,
        .cmd = StringValueCStr(cmd_rv)
    };

    rblibssh2_session_runthread(s, session_exec_w, &p, xfree, NULL, NULL, NULL);

    return channel_rv;
}

struct channel_data {
    struct channel *c;
    char *data;
    size_t len;
};

static void channel_data_free(void *arg)
{
    struct channel_data *cd = (struct channel_data *)arg;
    xfree(cd->data);
    xfree(cd);
}

static void *channel_read_w(void *params)
{
    struct channel_data *cd = (struct channel_data *) params;
    struct channel *c = cd->c;

    for (;;) {
        char errbuf[1024];
        ssize_t l;

        l = libssh2_channel_read_stderr(c->channel, errbuf, sizeof(errbuf));
        if (l == LIBSSH2_ERROR_EAGAIN || l == 0) {
            /* Nothing, fall through */
        } else if (l < 0) {
            goto error;
        } else {
            if (l < sizeof(errbuf)) {
                errbuf[l] = '\0';
            } else {
                errbuf[sizeof(errbuf)-1] = '\0';
            }
            rblibssh2_session_set_error(eApplicationError, errbuf);
            return NULL;
        }

        l = libssh2_channel_read(c->channel, cd->data, cd->len);
        if (l == LIBSSH2_ERROR_EAGAIN) {
            rblibssh2_session_wait(c->s);
        } else if (l < 0) {
            goto error;
        } else if (l == 0) {
            if (libssh2_channel_eof(c->channel)) {
                rblibssh2_session_set_error(rb_eIOError, "Unexpected EOF");
                return NULL;
            }
            /* Can't think of any other reason we'd get a zero-length return,
             * but go round again anyway. */
        } else {
            cd->len = l;
            break;
        }
    }

    return cd;

    char *err;
error:
    libssh2_session_last_error(rblibssh2_session_get(c->s), &err, NULL, 0);
    rblibssh2_session_set_error(rb_eIOError,
            "Error reading from channel: %s", err);
    return NULL;
}

static VALUE channel_read(VALUE self_rv, VALUE bytes_rv)
{
    struct channel *c;
    Data_Get_Struct(self_rv, typeof(*c), c);

    if (c->channel == NULL)
        rb_raise(eInternalError, "Channel is closed");

    size_t len = NUM2INT(bytes_rv);

    char *data = xmalloc(len);
    struct channel_data *cd = xmalloc(sizeof *cd);
    cd->c = c;
    cd->data = data;
    cd->len = len;

    rblibssh2_session_runthread(c->s, channel_read_w,
                                cd, channel_data_free,
                                NULL, NULL, NULL);

    VALUE str = rb_str_new(cd->data, cd->len);
    channel_data_free(cd);

    return str;
}

static void *channel_write_w(void *params)
{
    struct channel_data *cd = (struct channel_data *)params;
    struct channel *c = cd->c;

    size_t written = 0;
    while (written < cd->len) {
        char errbuf[1024];
        ssize_t l;

        l = libssh2_channel_read_stderr(c->channel, errbuf, sizeof(errbuf));
        if (l == LIBSSH2_ERROR_EAGAIN || l == 0) {
            /* Nothing, fall through */
        } else if (l < 0) {
            goto error;
        } else {
            if (l < sizeof(errbuf)) {
                errbuf[l] = '\0';
            } else {
                errbuf[sizeof(errbuf)-1] = '\0';
            }
            rblibssh2_session_set_error(eApplicationError, errbuf);
            return NULL;
        }

        l = libssh2_channel_write(c->channel, cd->data, cd->len);
        if (l == LIBSSH2_ERROR_EAGAIN) {
            rblibssh2_session_wait(c->s);
        } else if (l < 0) {
            goto error;
        } else {
            written += l;
        }
    }
    libssh2_channel_flush(c->channel);

    return cd;

    char *err;
error:
    libssh2_session_last_error(rblibssh2_session_get(c->s), &err, NULL, 0);
    rblibssh2_session_set_error(rb_eIOError,
            "Error writing to channel: %s", err);
    return NULL;
}

static VALUE channel_write(VALUE self_rv, VALUE data_rv)
{
    struct channel *c;
    Data_Get_Struct(self_rv, typeof(*c), c);

    if (c->channel == NULL)
        rb_raise(eInternalError, "Channel is closed");

    VALUE string_rv = StringValue(data_rv);

    struct channel_data *cd = xmalloc(sizeof(*cd));
    cd->c = c;
    cd->data = RSTRING_PTR(string_rv);
    cd->len = RSTRING_LEN(string_rv);

    rblibssh2_session_runthread(c->s, channel_write_w,
                                cd, xfree,
                                NULL, NULL, NULL);
    xfree(cd);

    return Qnil;
}

struct channel_send_data {
    struct channel *c;
    int fd;
    uint64_t sent;
    pthread_mutex_t mutex;
};

static pthread_mutex_t cb_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cb_cond = PTHREAD_COND_INITIALIZER;
static unsigned short int cb_reading = 0;

static void cb_completion(union sigval dummy)
{
    pthread_mutex_lock(&cb_mutex);
    cb_reading = !cb_reading;
    pthread_cond_signal(&cb_cond);
    pthread_mutex_unlock(&cb_mutex);
}

static void *channel_send_data_w(void *params)
{
    struct channel_send_data *csd = (struct channel_send_data *) params;
    struct channel *c = csd->c;

    char buf[2][2*1024*1024];
    struct aiocb64 cbs[2] = {
        {
            .aio_fildes = csd->fd,
            .aio_buf = buf[0],
            .aio_nbytes = sizeof(buf[0]),
            .aio_sigevent = {
                .sigev_notify = SIGEV_THREAD,
                .sigev_notify_function = cb_completion,
                .sigev_value = {
                    .sival_int = 0
                }
            }
        },
        {
            .aio_fildes = csd->fd,
            .aio_buf = buf[1],
            .aio_nbytes = sizeof(buf[0]),
            .aio_sigevent = {
                .sigev_notify = SIGEV_THREAD,
                .sigev_notify_function = cb_completion,
                .sigev_value = {
                    .sival_int = 0
                }
            }
        }
    };

    unsigned short int cb_sending = 0;
    __off64_t read_pos = 0;

    /* Read the first block */
    pthread_mutex_lock(&cb_mutex);
    if (aio_read64(&cbs[cb_reading]) < 0) {
        pthread_mutex_unlock(&cb_mutex);
        rblibssh2_session_set_error(rb_eIOError,
                "Error scheduling read request: %m");
        return NULL;
    }
    pthread_mutex_unlock(&cb_mutex);

    for(;;) {
        /* Wait if we're still reading the next block */
        pthread_mutex_lock(&cb_mutex);
        while (cb_reading == cb_sending)
            pthread_cond_wait(&cb_cond, &cb_mutex);

        ssize_t in = aio_return64(&cbs[cb_sending]);
        if (in == 0) {
            pthread_mutex_unlock(&cb_mutex);
            break;
        } else if (in < 0) {
            pthread_mutex_unlock(&cb_mutex);
            rblibssh2_session_set_error(rb_eIOError,
                    "Error reading data: %s", strerror(-in));
            return NULL;
        }
        read_pos += in;

        /* Start reading the next block */
        cbs[cb_reading].aio_offset = read_pos;
        if (aio_read64(&cbs[cb_reading]) < 0) {
            pthread_mutex_unlock(&cb_mutex);
            rblibssh2_session_set_error(rb_eIOError,
                    "Error scheduling read request: %m");
            return NULL;
        }

        pthread_mutex_unlock(&cb_mutex);

        ssize_t sent = 0;
        while (sent < in) {
            ssize_t out = libssh2_channel_write(c->channel,
                                                buf[cb_sending] + sent,
                                                in - sent);
            if (out == LIBSSH2_ERROR_EAGAIN) {
                rblibssh2_session_wait(c->s);
            } else if (out < 0) {
                char *err;
                LIBSSH2_SESSION *session = rblibssh2_session_get(c->s);
                libssh2_session_last_error(session, &err, NULL, 0);
                rblibssh2_session_set_error(rb_eIOError,
                        "Error writing to channel: %s", err);
                return NULL;
            } else {
                sent += out;
            }
        }

        pthread_mutex_lock(&csd->mutex);
        csd->sent += sent;
        pthread_mutex_unlock(&csd->mutex);
        cb_sending = !cb_sending;
    }
    libssh2_channel_flush(c->channel);

    return csd;
}

static void channel_send_data_cb(void *arg) {
    struct channel_send_data *csd = (struct channel_send_data *) arg;

    /* We mustn't optimize this away. We really need to copy it. */
    volatile uint64_t sent;
    pthread_mutex_lock(&csd->mutex);
    sent = csd->sent;
    pthread_mutex_unlock(&csd->mutex);

    VALUE sent_rv = INT2NUM(sent);
    rb_yield(sent_rv);
}

static VALUE channel_send_data(VALUE self_rv, VALUE io_rv)
{
    struct channel *c;
    Data_Get_Struct(self_rv, typeof(*c), c);

    if (c->channel == NULL)
        rb_raise(eInternalError, "Channel is closed");

    rb_io_t *io;
    GetOpenFile(io_rv, io);
    rb_io_check_readable(io);

    struct channel_send_data *csd = xmalloc(sizeof(*csd));
    csd->c = c;
    csd->fd = fileno(io->f);
    csd->sent = 0;

    int rc;
    if ((rc = pthread_mutex_init(&csd->mutex, NULL)) != 0)
        rb_raise(eInternalError, "Initializing mutex: %s", strerror(rc));

    if (rb_block_given_p()) {
        struct timeval tv = {
            .tv_sec = 1,
            .tv_usec = 0,
        };

        rblibssh2_session_runthread(c->s, channel_send_data_w,
                                    csd, xfree,
                                    &tv, channel_send_data_cb, csd);
    } else {
        rblibssh2_session_runthread(c->s, channel_send_data_w,
                                    csd, xfree,
                                    NULL, NULL, NULL);
    }

    return Qnil;
}

static void *channel_close_w(void *arg)
{
    struct channel *c = (struct channel *) arg;

    int rc;

    while ((rc = libssh2_channel_send_eof(c->channel)) == LIBSSH2_ERROR_EAGAIN)
        rblibssh2_session_wait(c->s);
    if (rc != 0) {
        char *err;
        libssh2_session_last_error(rblibssh2_session_get(c->s), &err, NULL, 0);
        rblibssh2_session_set_error(rb_eIOError,
                "Error sending EOF: %s", err);
    }

    while ((rc = libssh2_channel_wait_eof(c->channel)) == LIBSSH2_ERROR_EAGAIN)
        rblibssh2_session_wait(c->s);
    if (rc != 0) {
        char *err;
        libssh2_session_last_error(rblibssh2_session_get(c->s), &err, NULL, 0);
        rblibssh2_session_set_error(rb_eIOError,
                "Error waiting for EOF ack: %s", err);
    }

    while ((rc = libssh2_channel_close(c->channel)) == LIBSSH2_ERROR_EAGAIN)
        rblibssh2_session_wait(c->s);
    if (rc != 0) {
        char *err;
        libssh2_session_last_error(rblibssh2_session_get(c->s), &err, NULL, 0);
        rblibssh2_session_set_error(rb_eIOError,
                "Error closing channel client side: %s", err);
    }

    while ((rc = libssh2_channel_wait_closed(c->channel)) ==
           LIBSSH2_ERROR_EAGAIN)
    {
        rblibssh2_session_wait(c->s);
    }
    if (rc != 0) {
        char *err;
        libssh2_session_last_error(rblibssh2_session_get(c->s), &err, NULL, 0);
        rblibssh2_session_set_error(rb_eIOError,
                 "Error closing channel server side: %s", err);
    }

    rblibssh2_session_channel_remove(c->s, c->rv);
    libssh2_channel_free(c->channel);
    c->channel = NULL;

    return c;
}

static VALUE channel_close(VALUE self_rv)
{
    struct channel *c;
    Data_Get_Struct(self_rv, typeof(*c), c);

    if (c->channel == NULL)
        rb_raise(eInternalError, "Channel is closed");

    rblibssh2_session_runthread(c->s, channel_close_w, c, NULL,
                                NULL, NULL, NULL);

    return Qnil;
}
