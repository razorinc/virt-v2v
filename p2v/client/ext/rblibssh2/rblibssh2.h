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
#include <libssh2.h>
#include <sys/time.h>

extern VALUE cChannel;
extern VALUE cLibssh2;
extern VALUE cSession;
extern VALUE eInternalError;

struct session;

/* Worker threads */
typedef void *worker_t(void *arg);
typedef void worker_cb_t(void *arg);
typedef void worker_free_t(void *arg);
void *rblibssh2_session_runthread(struct session *s,
                                  worker_t worker, void *w_params,
                                  worker_free_t free,
                                  struct timeval *tv,
                                  worker_cb_t cb, void *cb_arg);
void rblibssh2_session_set_error(VALUE class, char *msg, ...);

void rblibssh2_session_init(void);
void rblibssh2_channel_init(void);

LIBSSH2_SESSION *rblibssh2_session_get(struct session *s);
void rblibssh2_session_channel_add(struct session *s, VALUE channel);
void rblibssh2_session_channel_remove(struct session *s, VALUE channel);
