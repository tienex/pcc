/*
 * Copyright (c) 2025 PCC Project
 *
 * Networking and I/O Library
 *
 * Cross-platform networking and advanced I/O operations
 */

#ifndef _PCC_NET_H_
#define _PCC_NET_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Socket Types
 */
typedef enum {
	NET_SOCKET_STREAM,    /* TCP */
	NET_SOCKET_DGRAM,     /* UDP */
	NET_SOCKET_RAW        /* Raw socket */
} net_socket_type_t;

/*
 * Address Families
 */
typedef enum {
	NET_AF_INET,          /* IPv4 */
	NET_AF_INET6,         /* IPv6 */
	NET_AF_UNIX           /* Unix domain sockets */
} net_af_t;

/*
 * Protocol Types
 */
typedef enum {
	NET_PROTO_TCP,
	NET_PROTO_UDP,
	NET_PROTO_ICMP,
	NET_PROTO_RAW
} net_proto_t;

/*
 * Socket Options
 */
typedef enum {
	NET_OPT_REUSEADDR,
	NET_OPT_KEEPALIVE,
	NET_OPT_NODELAY,
	NET_OPT_BROADCAST,
	NET_OPT_RCVBUF,
	NET_OPT_SNDBUF,
	NET_OPT_RCVTIMEO,
	NET_OPT_SNDTIMEO,
	NET_OPT_LINGER
} net_sockopt_t;

/*
 * I/O Multiplexing Events
 */
typedef enum {
	NET_EVENT_READ = 0x01,
	NET_EVENT_WRITE = 0x02,
	NET_EVENT_ERROR = 0x04,
	NET_EVENT_HUP = 0x08
} net_event_t;

/*
 * Shutdown Modes
 */
typedef enum {
	NET_SHUT_READ,
	NET_SHUT_WRITE,
	NET_SHUT_RDWR
} net_shutdown_t;

/*
 * Socket Handle
 */
typedef struct net_socket net_socket_t;

/*
 * Address Structure
 */
typedef struct {
	net_af_t family;
	union {
		struct {
			uint8_t addr[4];
			uint16_t port;
		} ipv4;
		struct {
			uint8_t addr[16];
			uint16_t port;
			uint32_t flowinfo;
			uint32_t scope_id;
		} ipv6;
		struct {
			char path[108];
		} unix_addr;
	} u;
} net_addr_t;

/*
 * I/O Event Structure
 */
typedef struct {
	net_socket_t *socket;
	uint32_t events;
	void *user_data;
} net_poll_event_t;

/*
 * Network Statistics
 */
typedef struct {
	uint64_t bytes_sent;
	uint64_t bytes_received;
	uint64_t packets_sent;
	uint64_t packets_received;
	uint64_t errors;
	uint64_t timeouts;
} net_stats_t;

/*
 * Error Codes
 */
typedef enum {
	NET_OK = 0,
	NET_ERROR_INVALID_ARG,
	NET_ERROR_SOCKET_FAILED,
	NET_ERROR_BIND_FAILED,
	NET_ERROR_LISTEN_FAILED,
	NET_ERROR_CONNECT_FAILED,
	NET_ERROR_ACCEPT_FAILED,
	NET_ERROR_SEND_FAILED,
	NET_ERROR_RECV_FAILED,
	NET_ERROR_TIMEOUT,
	NET_ERROR_CLOSED,
	NET_ERROR_WOULD_BLOCK,
	NET_ERROR_ADDRESS_IN_USE,
	NET_ERROR_NO_MEMORY,
	NET_ERROR_UNSUPPORTED
} net_error_t;

/*
 * Library Initialization
 */

/* Initialize networking library */
int net_init(void);

/* Cleanup networking library */
void net_cleanup(void);

/* Get last error */
net_error_t net_get_last_error(void);

/* Get error message */
const char *net_error_string(net_error_t error);

/*
 * Address Operations
 */

/* Create IPv4 address */
net_addr_t net_addr_ipv4(const char *ip, uint16_t port);

/* Create IPv6 address */
net_addr_t net_addr_ipv6(const char *ip, uint16_t port);

/* Create Unix domain socket address */
net_addr_t net_addr_unix(const char *path);

/* Parse address from string */
int net_addr_parse(const char *str, net_addr_t *addr);

/* Convert address to string */
char *net_addr_to_string(const net_addr_t *addr);

/* Get hostname */
char *net_get_hostname(void);

/* Resolve hostname to address */
int net_resolve(const char *hostname, net_af_t family, net_addr_t *addr);

/* Reverse lookup */
char *net_reverse_lookup(const net_addr_t *addr);

/*
 * Socket Operations
 */

/* Create socket */
net_socket_t *net_socket_create(net_af_t family, net_socket_type_t type);

/* Close socket */
void net_socket_close(net_socket_t *sock);

/* Bind socket to address */
int net_socket_bind(net_socket_t *sock, const net_addr_t *addr);

/* Listen for connections */
int net_socket_listen(net_socket_t *sock, int backlog);

/* Accept incoming connection */
net_socket_t *net_socket_accept(net_socket_t *sock, net_addr_t *peer_addr);

/* Connect to address */
int net_socket_connect(net_socket_t *sock, const net_addr_t *addr);

/* Shutdown socket */
int net_socket_shutdown(net_socket_t *sock, net_shutdown_t how);

/* Set socket option */
int net_socket_setopt(net_socket_t *sock, net_sockopt_t opt, const void *value, size_t len);

/* Get socket option */
int net_socket_getopt(net_socket_t *sock, net_sockopt_t opt, void *value, size_t *len);

/* Set blocking/non-blocking mode */
int net_socket_set_blocking(net_socket_t *sock, int blocking);

/* Get local address */
int net_socket_get_local_addr(net_socket_t *sock, net_addr_t *addr);

/* Get peer address */
int net_socket_get_peer_addr(net_socket_t *sock, net_addr_t *addr);

/*
 * Data Transfer
 */

/* Send data */
ssize_t net_socket_send(net_socket_t *sock, const void *data, size_t len, int flags);

/* Receive data */
ssize_t net_socket_recv(net_socket_t *sock, void *buffer, size_t len, int flags);

/* Send to specific address (UDP) */
ssize_t net_socket_sendto(net_socket_t *sock, const void *data, size_t len,
                          const net_addr_t *dest, int flags);

/* Receive from address (UDP) */
ssize_t net_socket_recvfrom(net_socket_t *sock, void *buffer, size_t len,
                            net_addr_t *src, int flags);

/* Send all data (blocking until complete) */
ssize_t net_socket_send_all(net_socket_t *sock, const void *data, size_t len);

/* Receive exact amount (blocking until complete) */
ssize_t net_socket_recv_all(net_socket_t *sock, void *buffer, size_t len);

/* Send file descriptor (Unix only) */
int net_socket_send_fd(net_socket_t *sock, int fd);

/* Receive file descriptor (Unix only) */
int net_socket_recv_fd(net_socket_t *sock);

/*
 * I/O Multiplexing
 */

/* Poll handle */
typedef struct net_poll net_poll_t;

/* Create poll object */
net_poll_t *net_poll_create(void);

/* Destroy poll object */
void net_poll_destroy(net_poll_t *poll);

/* Add socket to poll */
int net_poll_add(net_poll_t *poll, net_socket_t *sock, uint32_t events, void *user_data);

/* Remove socket from poll */
int net_poll_remove(net_poll_t *poll, net_socket_t *sock);

/* Modify socket events */
int net_poll_modify(net_poll_t *poll, net_socket_t *sock, uint32_t events);

/* Wait for events */
int net_poll_wait(net_poll_t *poll, net_poll_event_t *events, int max_events, int timeout_ms);

/*
 * High-Level Helpers
 */

/* Create TCP server */
net_socket_t *net_tcp_server(const char *bind_addr, uint16_t port, int backlog);

/* Create TCP client */
net_socket_t *net_tcp_client(const char *host, uint16_t port);

/* Create UDP socket */
net_socket_t *net_udp_socket(const char *bind_addr, uint16_t port);

/* Send line (adds newline) */
int net_send_line(net_socket_t *sock, const char *line);

/* Receive line (until newline) */
char *net_recv_line(net_socket_t *sock, size_t max_len);

/* Send formatted data */
int net_send_printf(net_socket_t *sock, const char *format, ...);

/*
 * File I/O Extensions
 */

/* Read entire file into memory */
void *net_file_read_all(const char *filename, size_t *out_size);

/* Write buffer to file */
int net_file_write_all(const char *filename, const void *data, size_t size);

/* Copy file */
int net_file_copy(const char *src, const char *dest);

/* Get file size */
int64_t net_file_size(const char *filename);

/* Check if file exists */
int net_file_exists(const char *filename);

/* Create directory recursively */
int net_mkdir_recursive(const char *path);

/* List directory contents */
char **net_list_directory(const char *path, int *count);

/*
 * Asynchronous I/O (Optional)
 */

typedef struct net_async net_async_t;
typedef void (*net_async_callback_t)(net_socket_t *sock, void *user_data, int status);

/* Create async I/O context */
net_async_t *net_async_create(void);

/* Destroy async context */
void net_async_destroy(net_async_t *async);

/* Async connect */
int net_async_connect(net_async_t *async, net_socket_t *sock,
                     const net_addr_t *addr, net_async_callback_t callback, void *user_data);

/* Async send */
int net_async_send(net_async_t *async, net_socket_t *sock,
                  const void *data, size_t len, net_async_callback_t callback, void *user_data);

/* Async receive */
int net_async_recv(net_async_t *async, net_socket_t *sock,
                  void *buffer, size_t len, net_async_callback_t callback, void *user_data);

/* Process async events */
int net_async_process(net_async_t *async, int timeout_ms);

/*
 * Statistics
 */

/* Get socket statistics */
void net_socket_get_stats(net_socket_t *sock, net_stats_t *stats);

/* Reset socket statistics */
void net_socket_reset_stats(net_socket_t *sock);

/* Get global statistics */
void net_get_global_stats(net_stats_t *stats);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_NET_H_ */
