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

#include <unistd.h>
#include <netdb.h>
#include <errno.h>
#include <stdarg.h>
#include <pthread.h>

#include <libssh2.h>

#include "rblibssh2.h"

/* Defined in rblibssh2.h */
VALUE cSession;

static VALUE eConnectError;
static VALUE eInvalidHostnameError;
static VALUE eInvalidCredentialsError;

static VALUE libssh2_connect(VALUE module_rv,
                            VALUE hostname_rv,
                            VALUE username_rv, VALUE password_rv);
static VALUE session_reconnect(VALUE self_rv);
static VALUE session_close(VALUE self_rv);

struct channel_list {
    struct channel_list *next;
    VALUE channel;
};

struct session {
    int sock;
    LIBSSH2_SESSION *session;

    char *hostname;
    char *username;
    char *password;

    struct channel_list *channels;

    pthread_t thread;
    unsigned short int running;
};

void rblibssh2_session_init()
{
    cSession = rb_define_class_under(cLibssh2, "Session", rb_cObject);

    eConnectError =
        rb_define_class_under(cSession, "ConnectError", rb_eStandardError);
    eInvalidHostnameError =
        rb_define_class_under(cSession,
                              "InvalidHostnameError", rb_eStandardError);
    eInvalidCredentialsError =
        rb_define_class_under(cSession,
                              "InvalidCredentialsError", rb_eStandardError);

    rb_define_singleton_method(cLibssh2, "connect", libssh2_connect, 3);
    rb_define_method(cSession, "reconnect", session_reconnect, 0);
    rb_define_method(cSession, "close", session_close, 0);
}

/* Worker thread implementation */
static VALUE err_class;
static char err_msg[1024];

struct worker_args {
    worker_t *worker;
    void *arg;

    int completion_fd;
};

static void *worker_wrapper(void *arg)
{
    const struct worker_args *args = (struct worker_args *)arg;

    void *result = (args->worker)(args->arg);

    /* We need a memory barrier here to ensure that the parent sees all changes
     * made by the worker. We don't get this automatically because we're not
     * using pthread call to signal between the worker and the parent. */
    pthread_mutex_t barrier = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_lock(&barrier);
    pthread_mutex_unlock(&barrier);

    /* Notify the parent that we finished by writing a byte to the signal pipe
     */
    for(;;) {
        int out = write(args->completion_fd, "", 1);
        if (out == 1)
            break;

        if (out < 0 && errno != EINTR) {
            perror("write to thread pipe");
        }
    }

    return result;
}

void *rblibssh2_session_runthread(struct session *s,
                                  worker_t worker, void *w_arg,
                                  worker_free_t free,
                                  struct timeval *tv,
                                  worker_cb_t cb, void *cb_arg)
{
    if (s->running) {
        rb_raise(eInternalError, "session already has an active thread");
    }

    int signal[2];
    if (pipe(signal) < 0)
        rb_sys_fail("pipe");

    struct worker_args args = {
        .worker = worker,
        .arg = w_arg,
        .completion_fd = signal[1],
    };

    if (pthread_create(&s->thread, NULL, worker_wrapper, &args) < 0)
        rb_sys_fail("Failed to create worker thread");
    s->running = 1;

    /* Wait for a byte from the signal pipe, ensuring we allow other ruby
     * threads to run while we wait */
    for (;;) {
#ifdef HAVE_RB_THREAD_FD_SELECT
        rb_fdset_t fds;
        rb_fd_init(&fds);
        rb_fd_set(signal[0], &fds);
#else
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(signal[0], &fds);
#endif /* HAVE_RB_THREAD_FD_SELECT */

        int rc;
        if (cb) {
            struct timeval tv_c = *tv;
#ifdef HAVE_RB_THREAD_FD_SELECT
            rc = rb_thread_fd_select(signal[0] + 1, &fds, NULL, NULL, &tv_c);
#else
            rc = rb_thread_select(signal[0] + 1, &fds, NULL, NULL, &tv_c);
#endif
        } else {
#ifdef HAVE_RB_THREAD_FD_SELECT
            rc = rb_thread_fd_select(signal[0] + 1, &fds, NULL, NULL, NULL);
#else
            rc = rb_thread_select(signal[0] + 1, &fds, NULL, NULL, NULL);
#endif
        }

        /* Timeout, call the registered callback */
        if (rc == 0 && cb) {
            (*cb)(cb_arg);
        }
        /* Unlikely error */
        else if (rc < 0 && errno != EINTR) {
            rb_raise(eInternalError, "select failed: %m");
        }
        /* select returned waiting descriptors */
        else {
            break;
        }
    }

    char buf;
    for(;;) {
        int in = read(signal[0], &buf, 1);
        if (in == 1)
            break;
        if (in < 0 && errno != EINTR)
            rb_sys_fail("read from thread pipe");
        if (in == 0)
            rb_raise(eInternalError, "Unexpected EOF on thread pipe");
    }

    close(signal[0]);
    close(signal[1]);

    void *result = NULL;
    errno = pthread_join(s->thread, &result);
    if (errno < 0) {
        s->running = 0;
        rb_sys_fail("Failed to join worker thread");
    }
    s->running = 0;

    if (result == NULL) {
        if (free) (*free)(w_arg);
        rb_raise(err_class, err_msg);
    }

    return result;
}

void rblibssh2_session_set_error(VALUE class, char *msg, ...)
{
    va_list ap;

    err_class = class;

    va_start(ap, msg);
    vsnprintf(err_msg, sizeof(err_msg), msg, ap);
    va_end(ap);

    err_msg[sizeof(err_msg) - 1] = '\0';
}

LIBSSH2_SESSION *rblibssh2_session_get(struct session *s)
{
    return s->session;
}

void rblibssh2_session_channel_add(struct session *s, VALUE channel)
{
    struct channel_list **i = &s->channels;

    while (*i)
        *i = (*i)->next;

    *i = xmalloc(sizeof(**i));
    (*i)->next = NULL;
    (*i)->channel = channel;
}

void rblibssh2_session_channel_remove(struct session *s, VALUE channel)
{
    struct channel_list **n = &s->channels;

    while (*n && (*n)->channel != channel)
        n = &(*n)->next;

    if (*n == NULL) return;

    xfree(*n);
    *n = NULL;
}

static void session_free(void *arg) {
    struct session *s = (struct session *)arg;

    if (s->running) {
        pthread_cancel(s->thread);

        void *result;
        pthread_join(s->thread, &result);
    }

    if (s->session) {
        libssh2_session_disconnect(s->session, "Client exited normally");
        libssh2_session_free(s->session);
    }

    if (s->sock != -1)
        close(s->sock);

    xfree(s->hostname);
    xfree(s->username);
    xfree(s->password);

    xfree(s);
}

static void session_mark(void *arg) {
    struct session *s = (struct session *)arg;

    struct channel_list *i = s->channels;
    while (i) {
        rb_gc_mark(i->channel);
        i = i->next;
    }
}

static void *libssh2_connect_w(void *arg)
{
    struct session *s = (struct session *) arg;

    struct addrinfo *result, *rp;
    const struct addrinfo hints = {
        .ai_family = AF_UNSPEC,
        .ai_socktype = SOCK_STREAM,
    };

    int err = getaddrinfo(s->hostname, "ssh", &hints, &result);
    if (err != 0) {
        rblibssh2_session_set_error(eInvalidHostnameError,
                                    "Name lookup failed: %s",
                                    gai_strerror(err));
        return NULL;
    }

    int firsterr = 0;
    for (rp = result; rp != NULL; rp = rp->ai_next) {
        s->sock = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (s->sock < 0)
            continue;

        if (connect(s->sock, rp->ai_addr, rp->ai_addrlen) == 0) {
            break;
        }

        if (firsterr == 0)
            firsterr = errno;

        close(s->sock);
    }

    freeaddrinfo(result);

    if (rp == NULL) {
        rblibssh2_session_set_error(eConnectError, "Connect failed: %s",
                                    strerror(firsterr));
        return NULL;
    }

    s->session = libssh2_session_init();
    if (s->session == NULL) {
        char *err;
        libssh2_session_last_error(s->session, &err, NULL, 0);
        rblibssh2_session_set_error(eConnectError,
                                    "Failed to create ssh session: %s", err);
        return NULL;
    }

    if (libssh2_session_startup(s->session, s->sock) < 0) {
        char *err;
        libssh2_session_last_error(s->session, &err, NULL, 0);
        rblibssh2_session_set_error(eConnectError,
                                    "Failed to establish ssh session: %s", err);
        return NULL;
    }

    /* We don't do hostkey checking here, as we have nothing to verify it
     * against */

    const char *userauthlist =
        libssh2_userauth_list(s->session, s->username, strlen(s->username));
    if (userauthlist == NULL) {
        char *err;
        libssh2_session_last_error(s->session, &err, NULL, 0);
        rblibssh2_session_set_error(eConnectError,
                "Failed to get list of supported authentication methods: %s",
                err);
        return NULL;
    }

    if (strstr(userauthlist, "password") == NULL) {
        rblibssh2_session_set_error(eConnectError,
                "Remote host does not support password authentication");
        return NULL;
    }

    if (libssh2_userauth_password(s->session, s->username, s->password) < 0) {
        char *err;
        int err_no = libssh2_session_last_error(s->session, &err, NULL, 0);
        if (err_no == LIBSSH2_ERROR_PASSWORD_EXPIRED ||
            err_no == LIBSSH2_ERROR_AUTHENTICATION_FAILED)
        {
            rblibssh2_session_set_error(eInvalidCredentialsError,
                    "Invalid username/password");
        } else {
            rblibssh2_session_set_error(eConnectError,
                     "Authentication failed: %s", err);
        }
        return NULL;
    }

    return s;
}

static VALUE libssh2_connect(VALUE module_rv,
                            VALUE hostname_rv,
                            VALUE username_rv, VALUE password_rv)
{
    /* Initialize this up here so we can leave cleanup on error to Ruby's
     * garbage collector */
    struct session *s;
    VALUE session_rv = Data_Make_Struct(cSession, struct session,
                                        session_mark, session_free, s);
    memset(s, 0, sizeof(struct session));
    s->sock = -1;
    s->hostname = strdup(StringValueCStr(hostname_rv));
    s->username = strdup(StringValueCStr(username_rv));
    s->password = strdup(StringValueCStr(password_rv));

    rblibssh2_session_runthread(s, libssh2_connect_w, s, NULL,
                                NULL, NULL, NULL);

    return session_rv;
}

static VALUE session_reconnect(VALUE self_rv)
{
    struct session *s;
    Data_Get_Struct(self_rv, struct session, s);

    rblibssh2_session_runthread(s, libssh2_connect_w, s, NULL,
                                NULL, NULL, NULL);

    return Qnil;
}

void *session_close_w(void *arg)
{
    struct session *s = (struct session *)arg;

    libssh2_session_disconnect(s->session, "Session closed");
    libssh2_session_free(s->session);
    s->session = NULL;

    return (void*)1;
}

static VALUE session_close(VALUE self_rv)
{
    struct session *s;
    Data_Get_Struct(self_rv, struct session, s);

    ID close = rb_intern("close");

    struct channel_list *i = s->channels;
    while(i) {
        rb_funcall(i->channel, close, 0);
        i = i->next;
    }

    rblibssh2_session_runthread(s, session_close_w, s, NULL, NULL, NULL, NULL);

    return Qnil;
}
