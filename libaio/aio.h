/*
 * Copyright (c) 2025 PCC Project
 * Async I/O Library
 * Cross-platform asynchronous I/O with event loop
 */

#ifndef AIO_H
#define AIO_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
typedef struct aio_loop aio_loop_t;
typedef struct aio_handle aio_handle_t;
typedef struct aio_req aio_req_t;
typedef struct aio_timer aio_timer_t;

/* Handle types */
typedef enum {
	AIO_HANDLE_FILE,
	AIO_HANDLE_SOCKET,
	AIO_HANDLE_PIPE,
	AIO_HANDLE_TTY,
	AIO_HANDLE_TIMER,
	AIO_HANDLE_SIGNAL,
	AIO_HANDLE_POLL
} aio_handle_type_t;

/* Event types */
typedef enum {
	AIO_EVENT_READ = (1 << 0),
	AIO_EVENT_WRITE = (1 << 1),
	AIO_EVENT_ERROR = (1 << 2),
	AIO_EVENT_CLOSE = (1 << 3)
} aio_event_t;

/* Error codes */
typedef enum {
	AIO_OK = 0,
	AIO_ERROR = -1,
	AIO_EAGAIN = -2,
	AIO_ENOMEM = -3,
	AIO_EINVAL = -4,
	AIO_ENOENT = -5,
	AIO_EOF = -6,
	AIO_ECANCELED = -7
} aio_error_t;

/* Callbacks */
typedef void (*aio_read_cb_t)(aio_handle_t *handle, ssize_t nread, const void *buf);
typedef void (*aio_write_cb_t)(aio_handle_t *handle, ssize_t nwritten);
typedef void (*aio_connect_cb_t)(aio_handle_t *handle, int status);
typedef void (*aio_accept_cb_t)(aio_handle_t *server, aio_handle_t *client);
typedef void (*aio_close_cb_t)(aio_handle_t *handle);
typedef void (*aio_timer_cb_t)(aio_timer_t *timer);
typedef void (*aio_signal_cb_t)(int signum);

/* Loop configuration */
typedef struct {
	int max_handles;
	int use_poll;      /* Use poll() instead of select() */
	int use_epoll;     /* Use epoll on Linux */
	int use_kqueue;    /* Use kqueue on BSD/macOS */
	int use_iocp;      /* Use IOCP on Windows */
} aio_loop_config_t;

/*
 * Event Loop
 */

/* Create/destroy event loop */
aio_loop_t *aio_loop_create(const aio_loop_config_t *config);
void aio_loop_destroy(aio_loop_t *loop);

/* Get default loop */
aio_loop_t *aio_default_loop(void);

/* Run event loop */
int aio_loop_run(aio_loop_t *loop);
int aio_loop_run_once(aio_loop_t *loop);
void aio_loop_stop(aio_loop_t *loop);

/* Loop state */
int aio_loop_alive(aio_loop_t *loop);
uint64_t aio_loop_now(aio_loop_t *loop);
void aio_loop_update_time(aio_loop_t *loop);

/*
 * File I/O
 */

/* Open file for async I/O */
aio_handle_t *aio_file_open(aio_loop_t *loop, const char *path, int flags, int mode);
void aio_file_close(aio_handle_t *file, aio_close_cb_t cb);

/* Read/write */
int aio_read(aio_handle_t *handle, void *buf, size_t len, aio_read_cb_t cb);
int aio_write(aio_handle_t *handle, const void *buf, size_t len, aio_write_cb_t cb);

/* Seek */
int aio_seek(aio_handle_t *file, int64_t offset, int whence);

/* File stats */
typedef struct {
	uint64_t size;
	uint64_t mtime;
	uint64_t ctime;
	int is_dir;
	int is_file;
	int is_link;
} aio_stat_t;

int aio_fstat(aio_handle_t *file, aio_stat_t *stat);

/*
 * Network I/O
 */

/* Socket types */
typedef enum {
	AIO_SOCKET_TCP,
	AIO_SOCKET_UDP,
	AIO_SOCKET_UNIX
} aio_socket_type_t;

/* Address */
typedef struct {
	char host[256];
	int port;
	int family;  /* AF_INET, AF_INET6, AF_UNIX */
} aio_addr_t;

/* Create socket */
aio_handle_t *aio_socket_create(aio_loop_t *loop, aio_socket_type_t type);

/* Connect/bind/listen */
int aio_connect(aio_handle_t *socket, const aio_addr_t *addr, aio_connect_cb_t cb);
int aio_bind(aio_handle_t *socket, const aio_addr_t *addr);
int aio_listen(aio_handle_t *socket, int backlog, aio_accept_cb_t cb);

/* Accept connection */
aio_handle_t *aio_accept(aio_handle_t *server);

/* Send/receive */
int aio_send(aio_handle_t *socket, const void *buf, size_t len, aio_write_cb_t cb);
int aio_recv(aio_handle_t *socket, void *buf, size_t len, aio_read_cb_t cb);

/* Sendto/recvfrom (UDP) */
int aio_sendto(aio_handle_t *socket, const void *buf, size_t len,
               const aio_addr_t *addr, aio_write_cb_t cb);
int aio_recvfrom(aio_handle_t *socket, void *buf, size_t len,
                 aio_addr_t *addr, aio_read_cb_t cb);

/*
 * Timers
 */

/* Create timer */
aio_timer_t *aio_timer_create(aio_loop_t *loop, aio_timer_cb_t cb);
void aio_timer_destroy(aio_timer_t *timer);

/* Start/stop timer */
int aio_timer_start(aio_timer_t *timer, uint64_t timeout_ms, uint64_t repeat_ms);
void aio_timer_stop(aio_timer_t *timer);

/* Timer info */
uint64_t aio_timer_get_repeat(aio_timer_t *timer);
void aio_timer_set_repeat(aio_timer_t *timer, uint64_t repeat_ms);

/*
 * Pipes
 */

/* Create pipe */
int aio_pipe_create(aio_loop_t *loop, aio_handle_t **read_handle,
                    aio_handle_t **write_handle);

/* Named pipes */
aio_handle_t *aio_pipe_open(aio_loop_t *loop, const char *name, int flags);

/*
 * Polling
 */

/* Poll handle */
typedef struct {
	int fd;
	aio_event_t events;
} aio_poll_t;

aio_handle_t *aio_poll_create(aio_loop_t *loop, int fd);
int aio_poll_start(aio_handle_t *poll, aio_event_t events, void (*cb)(aio_handle_t*, aio_event_t));
void aio_poll_stop(aio_handle_t *poll);

/*
 * Handle Management
 */

/* Close handle */
void aio_handle_close(aio_handle_t *handle, aio_close_cb_t cb);

/* Handle state */
int aio_is_active(aio_handle_t *handle);
int aio_is_closing(aio_handle_t *handle);

/* Get handle type */
aio_handle_type_t aio_handle_get_type(aio_handle_t *handle);

/* Get loop from handle */
aio_loop_t *aio_handle_get_loop(aio_handle_t *handle);

/* User data */
void aio_handle_set_data(aio_handle_t *handle, void *data);
void *aio_handle_get_data(aio_handle_t *handle);

/*
 * Utilities
 */

/* Error handling */
const char *aio_strerror(int err);
int aio_get_last_error(void);

/* Address parsing */
int aio_addr_parse(const char *str, aio_addr_t *addr);
int aio_addr_format(const aio_addr_t *addr, char *buf, size_t len);

/* DNS resolution */
typedef void (*aio_resolve_cb_t)(int status, aio_addr_t *addrs, int naddrs);
int aio_resolve(aio_loop_t *loop, const char *host, aio_resolve_cb_t cb);

/*
 * Platform-specific
 */

/* Get native handle (file descriptor or HANDLE) */
int aio_get_native_handle(aio_handle_t *handle);

/* Set non-blocking mode */
int aio_set_nonblocking(int fd);

#ifdef __cplusplus
}
#endif

#endif /* AIO_H */
