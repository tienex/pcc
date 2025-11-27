/*
 * Copyright (c) 2025 PCC Project
 *
 * Networking and I/O Library - Implementation
 */

#include "net.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#pragma comment(lib, "ws2_32.lib")
typedef int socklen_t;
#define NET_CLOSE closesocket
#define NET_INVALID_SOCKET INVALID_SOCKET
#define NET_WOULD_BLOCK WSAEWOULDBLOCK
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <dirent.h>
#include <poll.h>
#define NET_CLOSE close
#define NET_INVALID_SOCKET (-1)
#define NET_WOULD_BLOCK EWOULDBLOCK
typedef int SOCKET;
#endif

/*
 * Socket Structure
 */
struct net_socket {
	SOCKET fd;
	net_af_t family;
	net_socket_type_t type;
	int blocking;
	net_stats_t stats;
};

/*
 * Poll Structure
 */
struct net_poll {
#ifdef _WIN32
	fd_set read_fds;
	fd_set write_fds;
	fd_set error_fds;
	net_socket_t **sockets;
	uint32_t *events;
	void **user_data;
	int count;
	int capacity;
#else
	struct pollfd *fds;
	net_socket_t **sockets;
	void **user_data;
	int count;
	int capacity;
#endif
};

/*
 * Global State
 */
static int net_initialized = 0;
static net_error_t net_last_error = NET_OK;
static net_stats_t net_global_stats = {0};

/*
 * Error Handling
 */

static void net_set_error(net_error_t error) {
	net_last_error = error;
}

net_error_t net_get_last_error(void) {
	return net_last_error;
}

const char *net_error_string(net_error_t error) {
	switch (error) {
	case NET_OK: return "Success";
	case NET_ERROR_INVALID_ARG: return "Invalid argument";
	case NET_ERROR_SOCKET_FAILED: return "Socket creation failed";
	case NET_ERROR_BIND_FAILED: return "Bind failed";
	case NET_ERROR_LISTEN_FAILED: return "Listen failed";
	case NET_ERROR_CONNECT_FAILED: return "Connect failed";
	case NET_ERROR_ACCEPT_FAILED: return "Accept failed";
	case NET_ERROR_SEND_FAILED: return "Send failed";
	case NET_ERROR_RECV_FAILED: return "Receive failed";
	case NET_ERROR_TIMEOUT: return "Operation timed out";
	case NET_ERROR_CLOSED: return "Connection closed";
	case NET_ERROR_WOULD_BLOCK: return "Would block";
	case NET_ERROR_ADDRESS_IN_USE: return "Address already in use";
	case NET_ERROR_NO_MEMORY: return "Out of memory";
	case NET_ERROR_UNSUPPORTED: return "Operation not supported";
	default: return "Unknown error";
	}
}

/*
 * Library Initialization
 */

int net_init(void) {
	if (net_initialized) return 0;

#ifdef _WIN32
	WSADATA wsa_data;
	int result = WSAStartup(MAKEWORD(2, 2), &wsa_data);
	if (result != 0) {
		net_set_error(NET_ERROR_SOCKET_FAILED);
		return -1;
	}
#endif

	net_initialized = 1;
	memset(&net_global_stats, 0, sizeof(net_stats_t));
	return 0;
}

void net_cleanup(void) {
	if (!net_initialized) return;

#ifdef _WIN32
	WSACleanup();
#endif

	net_initialized = 0;
}

/*
 * Address Operations
 */

net_addr_t net_addr_ipv4(const char *ip, uint16_t port) {
	net_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.family = NET_AF_INET;
	addr.u.ipv4.port = port;

	if (ip) {
		inet_pton(AF_INET, ip, addr.u.ipv4.addr);
	} else {
		/* INADDR_ANY */
		memset(addr.u.ipv4.addr, 0, 4);
	}

	return addr;
}

net_addr_t net_addr_ipv6(const char *ip, uint16_t port) {
	net_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.family = NET_AF_INET6;
	addr.u.ipv6.port = port;

	if (ip) {
		inet_pton(AF_INET6, ip, addr.u.ipv6.addr);
	} else {
		/* in6addr_any */
		memset(addr.u.ipv6.addr, 0, 16);
	}

	return addr;
}

net_addr_t net_addr_unix(const char *path) {
	net_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.family = NET_AF_UNIX;
	if (path) {
		strncpy(addr.u.unix_addr.path, path, sizeof(addr.u.unix_addr.path) - 1);
	}
	return addr;
}

int net_addr_parse(const char *str, net_addr_t *addr) {
	if (!str || !addr) return -1;

	/* Try IPv4 with port */
	char ip[256];
	int port;
	if (sscanf(str, "%255[^:]:%d", ip, &port) == 2) {
		*addr = net_addr_ipv4(ip, port);
		return 0;
	}

	/* Try IPv6 with port [addr]:port */
	if (sscanf(str, "[%255[^]]]:%d", ip, &port) == 2) {
		*addr = net_addr_ipv6(ip, port);
		return 0;
	}

	/* Try Unix socket */
	if (str[0] == '/') {
		*addr = net_addr_unix(str);
		return 0;
	}

	return -1;
}

char *net_addr_to_string(const net_addr_t *addr) {
	if (!addr) return NULL;

	char *result = malloc(256);
	if (!result) return NULL;

	switch (addr->family) {
	case NET_AF_INET: {
		char ip[INET_ADDRSTRLEN];
		inet_ntop(AF_INET, addr->u.ipv4.addr, ip, sizeof(ip));
		snprintf(result, 256, "%s:%u", ip, addr->u.ipv4.port);
		break;
	}
	case NET_AF_INET6: {
		char ip[INET6_ADDRSTRLEN];
		inet_ntop(AF_INET6, addr->u.ipv6.addr, ip, sizeof(ip));
		snprintf(result, 256, "[%s]:%u", ip, addr->u.ipv6.port);
		break;
	}
	case NET_AF_UNIX:
		snprintf(result, 256, "%s", addr->u.unix_addr.path);
		break;
	default:
		free(result);
		return NULL;
	}

	return result;
}

char *net_get_hostname(void) {
	char hostname[256];
	if (gethostname(hostname, sizeof(hostname)) != 0) {
		return NULL;
	}
	return strdup(hostname);
}

int net_resolve(const char *hostname, net_af_t family, net_addr_t *addr) {
	if (!hostname || !addr) return -1;

	struct addrinfo hints, *result;
	memset(&hints, 0, sizeof(hints));
	hints.ai_family = (family == NET_AF_INET) ? AF_INET : AF_INET6;
	hints.ai_socktype = SOCK_STREAM;

	if (getaddrinfo(hostname, NULL, &hints, &result) != 0) {
		return -1;
	}

	if (result->ai_family == AF_INET) {
		struct sockaddr_in *sin = (struct sockaddr_in *)result->ai_addr;
		*addr = net_addr_ipv4(NULL, 0);
		memcpy(addr->u.ipv4.addr, &sin->sin_addr, 4);
	} else if (result->ai_family == AF_INET6) {
		struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)result->ai_addr;
		*addr = net_addr_ipv6(NULL, 0);
		memcpy(addr->u.ipv6.addr, &sin6->sin6_addr, 16);
	}

	freeaddrinfo(result);
	return 0;
}

char *net_reverse_lookup(const net_addr_t *addr) {
	if (!addr) return NULL;

	char hostname[NI_MAXHOST];
	struct sockaddr_storage ss;
	socklen_t len;

	memset(&ss, 0, sizeof(ss));

	if (addr->family == NET_AF_INET) {
		struct sockaddr_in *sin = (struct sockaddr_in *)&ss;
		sin->sin_family = AF_INET;
		memcpy(&sin->sin_addr, addr->u.ipv4.addr, 4);
		sin->sin_port = htons(addr->u.ipv4.port);
		len = sizeof(struct sockaddr_in);
	} else if (addr->family == NET_AF_INET6) {
		struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)&ss;
		sin6->sin6_family = AF_INET6;
		memcpy(&sin6->sin6_addr, addr->u.ipv6.addr, 16);
		sin6->sin6_port = htons(addr->u.ipv6.port);
		len = sizeof(struct sockaddr_in6);
	} else {
		return NULL;
	}

	if (getnameinfo((struct sockaddr *)&ss, len, hostname, sizeof(hostname),
	                NULL, 0, NI_NAMEREQD) != 0) {
		return NULL;
	}

	return strdup(hostname);
}

/*
 * Socket Operations
 */

static void sockaddr_from_net_addr(const net_addr_t *addr, struct sockaddr_storage *ss, socklen_t *len) {
	memset(ss, 0, sizeof(*ss));

	if (addr->family == NET_AF_INET) {
		struct sockaddr_in *sin = (struct sockaddr_in *)ss;
		sin->sin_family = AF_INET;
		memcpy(&sin->sin_addr, addr->u.ipv4.addr, 4);
		sin->sin_port = htons(addr->u.ipv4.port);
		*len = sizeof(struct sockaddr_in);
	} else if (addr->family == NET_AF_INET6) {
		struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)ss;
		sin6->sin6_family = AF_INET6;
		memcpy(&sin6->sin6_addr, addr->u.ipv6.addr, 16);
		sin6->sin6_port = htons(addr->u.ipv6.port);
		sin6->sin6_flowinfo = addr->u.ipv6.flowinfo;
		sin6->sin6_scope_id = addr->u.ipv6.scope_id;
		*len = sizeof(struct sockaddr_in6);
	}
#ifndef _WIN32
	else if (addr->family == NET_AF_UNIX) {
		struct sockaddr_un *sun = (struct sockaddr_un *)ss;
		sun->sun_family = AF_UNIX;
		strncpy(sun->sun_path, addr->u.unix_addr.path, sizeof(sun->sun_path) - 1);
		*len = sizeof(struct sockaddr_un);
	}
#endif
}

static void net_addr_from_sockaddr(net_addr_t *addr, const struct sockaddr *sa) {
	memset(addr, 0, sizeof(*addr));

	if (sa->sa_family == AF_INET) {
		struct sockaddr_in *sin = (struct sockaddr_in *)sa;
		addr->family = NET_AF_INET;
		memcpy(addr->u.ipv4.addr, &sin->sin_addr, 4);
		addr->u.ipv4.port = ntohs(sin->sin_port);
	} else if (sa->sa_family == AF_INET6) {
		struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)sa;
		addr->family = NET_AF_INET6;
		memcpy(addr->u.ipv6.addr, &sin6->sin6_addr, 16);
		addr->u.ipv6.port = ntohs(sin6->sin6_port);
		addr->u.ipv6.flowinfo = sin6->sin6_flowinfo;
		addr->u.ipv6.scope_id = sin6->sin6_scope_id;
	}
#ifndef _WIN32
	else if (sa->sa_family == AF_UNIX) {
		struct sockaddr_un *sun = (struct sockaddr_un *)sa;
		addr->family = NET_AF_UNIX;
		strncpy(addr->u.unix_addr.path, sun->sun_path, sizeof(addr->u.unix_addr.path) - 1);
	}
#endif
}

net_socket_t *net_socket_create(net_af_t family, net_socket_type_t type) {
	if (!net_initialized) {
		if (net_init() < 0) return NULL;
	}

	net_socket_t *sock = calloc(1, sizeof(net_socket_t));
	if (!sock) {
		net_set_error(NET_ERROR_NO_MEMORY);
		return NULL;
	}

	int af = (family == NET_AF_INET) ? AF_INET :
	         (family == NET_AF_INET6) ? AF_INET6 : AF_UNIX;
	int sock_type = (type == NET_SOCKET_STREAM) ? SOCK_STREAM :
	                (type == NET_SOCKET_DGRAM) ? SOCK_DGRAM : SOCK_RAW;

	sock->fd = socket(af, sock_type, 0);
	if (sock->fd == NET_INVALID_SOCKET) {
		net_set_error(NET_ERROR_SOCKET_FAILED);
		free(sock);
		return NULL;
	}

	sock->family = family;
	sock->type = type;
	sock->blocking = 1;
	memset(&sock->stats, 0, sizeof(sock->stats));

	return sock;
}

void net_socket_close(net_socket_t *sock) {
	if (!sock) return;
	if (sock->fd != NET_INVALID_SOCKET) {
		NET_CLOSE(sock->fd);
	}
	free(sock);
}

int net_socket_bind(net_socket_t *sock, const net_addr_t *addr) {
	if (!sock || !addr) {
		net_set_error(NET_ERROR_INVALID_ARG);
		return -1;
	}

	struct sockaddr_storage ss;
	socklen_t len;
	sockaddr_from_net_addr(addr, &ss, &len);

	if (bind(sock->fd, (struct sockaddr *)&ss, len) < 0) {
		net_set_error(NET_ERROR_BIND_FAILED);
		return -1;
	}

	return 0;
}

int net_socket_listen(net_socket_t *sock, int backlog) {
	if (!sock) {
		net_set_error(NET_ERROR_INVALID_ARG);
		return -1;
	}

	if (listen(sock->fd, backlog) < 0) {
		net_set_error(NET_ERROR_LISTEN_FAILED);
		return -1;
	}

	return 0;
}

net_socket_t *net_socket_accept(net_socket_t *sock, net_addr_t *peer_addr) {
	if (!sock) {
		net_set_error(NET_ERROR_INVALID_ARG);
		return NULL;
	}

	struct sockaddr_storage ss;
	socklen_t len = sizeof(ss);

	SOCKET client_fd = accept(sock->fd, (struct sockaddr *)&ss, &len);
	if (client_fd == NET_INVALID_SOCKET) {
		net_set_error(NET_ERROR_ACCEPT_FAILED);
		return NULL;
	}

	net_socket_t *client = calloc(1, sizeof(net_socket_t));
	if (!client) {
		NET_CLOSE(client_fd);
		net_set_error(NET_ERROR_NO_MEMORY);
		return NULL;
	}

	client->fd = client_fd;
	client->family = sock->family;
	client->type = sock->type;
	client->blocking = 1;

	if (peer_addr) {
		net_addr_from_sockaddr(peer_addr, (struct sockaddr *)&ss);
	}

	return client;
}

int net_socket_connect(net_socket_t *sock, const net_addr_t *addr) {
	if (!sock || !addr) {
		net_set_error(NET_ERROR_INVALID_ARG);
		return -1;
	}

	struct sockaddr_storage ss;
	socklen_t len;
	sockaddr_from_net_addr(addr, &ss, &len);

	if (connect(sock->fd, (struct sockaddr *)&ss, len) < 0) {
#ifdef _WIN32
		if (WSAGetLastError() != WSAEWOULDBLOCK || sock->blocking)
#else
		if (errno != EINPROGRESS || sock->blocking)
#endif
		{
			net_set_error(NET_ERROR_CONNECT_FAILED);
			return -1;
		}
	}

	return 0;
}

int net_socket_shutdown(net_socket_t *sock, net_shutdown_t how) {
	if (!sock) return -1;

#ifdef _WIN32
	int mode = (how == NET_SHUT_READ) ? SD_RECEIVE :
	           (how == NET_SHUT_WRITE) ? SD_SEND : SD_BOTH;
#else
	int mode = (how == NET_SHUT_READ) ? SHUT_RD :
	           (how == NET_SHUT_WRITE) ? SHUT_WR : SHUT_RDWR;
#endif

	return shutdown(sock->fd, mode);
}

int net_socket_set_blocking(net_socket_t *sock, int blocking) {
	if (!sock) return -1;

#ifdef _WIN32
	u_long mode = blocking ? 0 : 1;
	if (ioctlsocket(sock->fd, FIONBIO, &mode) != 0) return -1;
#else
	int flags = fcntl(sock->fd, F_GETFL, 0);
	if (flags < 0) return -1;
	flags = blocking ? (flags & ~O_NONBLOCK) : (flags | O_NONBLOCK);
	if (fcntl(sock->fd, F_SETFL, flags) < 0) return -1;
#endif

	sock->blocking = blocking;
	return 0;
}

int net_socket_setopt(net_socket_t *sock, net_sockopt_t opt, const void *value, size_t len) {
	if (!sock || !value) return -1;

	int level = SOL_SOCKET;
	int optname;

	switch (opt) {
	case NET_OPT_REUSEADDR: optname = SO_REUSEADDR; break;
	case NET_OPT_KEEPALIVE: optname = SO_KEEPALIVE; break;
	case NET_OPT_BROADCAST: optname = SO_BROADCAST; break;
	case NET_OPT_RCVBUF: optname = SO_RCVBUF; break;
	case NET_OPT_SNDBUF: optname = SO_SNDBUF; break;
	case NET_OPT_RCVTIMEO: optname = SO_RCVTIMEO; break;
	case NET_OPT_SNDTIMEO: optname = SO_SNDTIMEO; break;
	case NET_OPT_LINGER: optname = SO_LINGER; break;
	case NET_OPT_NODELAY:
		level = IPPROTO_TCP;
		optname = TCP_NODELAY;
		break;
	default:
		return -1;
	}

	return setsockopt(sock->fd, level, optname, (const char *)value, len);
}

int net_socket_getopt(net_socket_t *sock, net_sockopt_t opt, void *value, size_t *len) {
	if (!sock || !value || !len) return -1;

	int level = SOL_SOCKET;
	int optname;

	switch (opt) {
	case NET_OPT_REUSEADDR: optname = SO_REUSEADDR; break;
	case NET_OPT_KEEPALIVE: optname = SO_KEEPALIVE; break;
	case NET_OPT_BROADCAST: optname = SO_BROADCAST; break;
	case NET_OPT_RCVBUF: optname = SO_RCVBUF; break;
	case NET_OPT_SNDBUF: optname = SO_SNDBUF; break;
	case NET_OPT_RCVTIMEO: optname = SO_RCVTIMEO; break;
	case NET_OPT_SNDTIMEO: optname = SO_SNDTIMEO; break;
	case NET_OPT_LINGER: optname = SO_LINGER; break;
	case NET_OPT_NODELAY:
		level = IPPROTO_TCP;
		optname = TCP_NODELAY;
		break;
	default:
		return -1;
	}

	socklen_t socklen = *len;
	int result = getsockopt(sock->fd, level, optname, (char *)value, &socklen);
	*len = socklen;
	return result;
}

int net_socket_get_local_addr(net_socket_t *sock, net_addr_t *addr) {
	if (!sock || !addr) return -1;

	struct sockaddr_storage ss;
	socklen_t len = sizeof(ss);

	if (getsockname(sock->fd, (struct sockaddr *)&ss, &len) < 0) {
		return -1;
	}

	net_addr_from_sockaddr(addr, (struct sockaddr *)&ss);
	return 0;
}

int net_socket_get_peer_addr(net_socket_t *sock, net_addr_t *addr) {
	if (!sock || !addr) return -1;

	struct sockaddr_storage ss;
	socklen_t len = sizeof(ss);

	if (getpeername(sock->fd, (struct sockaddr *)&ss, &len) < 0) {
		return -1;
	}

	net_addr_from_sockaddr(addr, (struct sockaddr *)&ss);
	return 0;
}

/*
 * Data Transfer
 */

ssize_t net_socket_send(net_socket_t *sock, const void *data, size_t len, int flags) {
	if (!sock || !data) {
		net_set_error(NET_ERROR_INVALID_ARG);
		return -1;
	}

	ssize_t sent = send(sock->fd, (const char *)data, len, flags);
	if (sent < 0) {
#ifdef _WIN32
		if (WSAGetLastError() == WSAEWOULDBLOCK)
#else
		if (errno == EWOULDBLOCK || errno == EAGAIN)
#endif
		{
			net_set_error(NET_ERROR_WOULD_BLOCK);
		} else {
			net_set_error(NET_ERROR_SEND_FAILED);
		}
		return -1;
	}

	sock->stats.bytes_sent += sent;
	sock->stats.packets_sent++;
	net_global_stats.bytes_sent += sent;
	net_global_stats.packets_sent++;

	return sent;
}

ssize_t net_socket_recv(net_socket_t *sock, void *buffer, size_t len, int flags) {
	if (!sock || !buffer) {
		net_set_error(NET_ERROR_INVALID_ARG);
		return -1;
	}

	ssize_t received = recv(sock->fd, (char *)buffer, len, flags);
	if (received < 0) {
#ifdef _WIN32
		if (WSAGetLastError() == WSAEWOULDBLOCK)
#else
		if (errno == EWOULDBLOCK || errno == EAGAIN)
#endif
		{
			net_set_error(NET_ERROR_WOULD_BLOCK);
		} else {
			net_set_error(NET_ERROR_RECV_FAILED);
		}
		return -1;
	}

	if (received == 0) {
		net_set_error(NET_ERROR_CLOSED);
		return 0;
	}

	sock->stats.bytes_received += received;
	sock->stats.packets_received++;
	net_global_stats.bytes_received += received;
	net_global_stats.packets_received++;

	return received;
}

ssize_t net_socket_sendto(net_socket_t *sock, const void *data, size_t len,
                          const net_addr_t *dest, int flags) {
	if (!sock || !data || !dest) {
		net_set_error(NET_ERROR_INVALID_ARG);
		return -1;
	}

	struct sockaddr_storage ss;
	socklen_t socklen;
	sockaddr_from_net_addr(dest, &ss, &socklen);

	ssize_t sent = sendto(sock->fd, (const char *)data, len, flags,
	                      (struct sockaddr *)&ss, socklen);
	if (sent < 0) {
		net_set_error(NET_ERROR_SEND_FAILED);
		return -1;
	}

	sock->stats.bytes_sent += sent;
	sock->stats.packets_sent++;

	return sent;
}

ssize_t net_socket_recvfrom(net_socket_t *sock, void *buffer, size_t len,
                            net_addr_t *src, int flags) {
	if (!sock || !buffer) {
		net_set_error(NET_ERROR_INVALID_ARG);
		return -1;
	}

	struct sockaddr_storage ss;
	socklen_t socklen = sizeof(ss);

	ssize_t received = recvfrom(sock->fd, (char *)buffer, len, flags,
	                            (struct sockaddr *)&ss, &socklen);
	if (received < 0) {
		net_set_error(NET_ERROR_RECV_FAILED);
		return -1;
	}

	if (src) {
		net_addr_from_sockaddr(src, (struct sockaddr *)&ss);
	}

	sock->stats.bytes_received += received;
	sock->stats.packets_received++;

	return received;
}

ssize_t net_socket_send_all(net_socket_t *sock, const void *data, size_t len) {
	if (!sock || !data) return -1;

	size_t total = 0;
	const uint8_t *ptr = (const uint8_t *)data;

	while (total < len) {
		ssize_t sent = net_socket_send(sock, ptr + total, len - total, 0);
		if (sent < 0) {
			if (net_last_error == NET_ERROR_WOULD_BLOCK) continue;
			return -1;
		}
		total += sent;
	}

	return total;
}

ssize_t net_socket_recv_all(net_socket_t *sock, void *buffer, size_t len) {
	if (!sock || !buffer) return -1;

	size_t total = 0;
	uint8_t *ptr = (uint8_t *)buffer;

	while (total < len) {
		ssize_t received = net_socket_recv(sock, ptr + total, len - total, 0);
		if (received < 0) {
			if (net_last_error == NET_ERROR_WOULD_BLOCK) continue;
			return -1;
		}
		if (received == 0) break;  /* Connection closed */
		total += received;
	}

	return total;
}

/*
 * High-Level Helpers
 */

net_socket_t *net_tcp_server(const char *bind_addr, uint16_t port, int backlog) {
	net_socket_t *sock = net_socket_create(NET_AF_INET, NET_SOCKET_STREAM);
	if (!sock) return NULL;

	/* Enable address reuse */
	int reuse = 1;
	net_socket_setopt(sock, NET_OPT_REUSEADDR, &reuse, sizeof(reuse));

	net_addr_t addr = net_addr_ipv4(bind_addr, port);
	if (net_socket_bind(sock, &addr) < 0) {
		net_socket_close(sock);
		return NULL;
	}

	if (net_socket_listen(sock, backlog) < 0) {
		net_socket_close(sock);
		return NULL;
	}

	return sock;
}

net_socket_t *net_tcp_client(const char *host, uint16_t port) {
	net_addr_t addr;

	/* Try to parse as IP first */
	if (net_addr_parse(host, &addr) < 0) {
		/* Try DNS resolution */
		if (net_resolve(host, NET_AF_INET, &addr) < 0) {
			return NULL;
		}
	}

	addr.u.ipv4.port = port;

	net_socket_t *sock = net_socket_create(NET_AF_INET, NET_SOCKET_STREAM);
	if (!sock) return NULL;

	if (net_socket_connect(sock, &addr) < 0) {
		net_socket_close(sock);
		return NULL;
	}

	return sock;
}

net_socket_t *net_udp_socket(const char *bind_addr, uint16_t port) {
	net_socket_t *sock = net_socket_create(NET_AF_INET, NET_SOCKET_DGRAM);
	if (!sock) return NULL;

	if (bind_addr || port) {
		net_addr_t addr = net_addr_ipv4(bind_addr, port);
		if (net_socket_bind(sock, &addr) < 0) {
			net_socket_close(sock);
			return NULL;
		}
	}

	return sock;
}

int net_send_line(net_socket_t *sock, const char *line) {
	if (!sock || !line) return -1;

	size_t len = strlen(line);
	if (net_socket_send_all(sock, line, len) < 0) return -1;
	if (net_socket_send_all(sock, "\n", 1) < 0) return -1;

	return 0;
}

char *net_recv_line(net_socket_t *sock, size_t max_len) {
	if (!sock) return NULL;

	char *buffer = malloc(max_len + 1);
	if (!buffer) return NULL;

	size_t pos = 0;
	while (pos < max_len) {
		char c;
		ssize_t received = net_socket_recv(sock, &c, 1, 0);
		if (received <= 0) {
			free(buffer);
			return NULL;
		}

		if (c == '\n') break;
		if (c != '\r') {
			buffer[pos++] = c;
		}
	}

	buffer[pos] = '\0';
	return buffer;
}

int net_send_printf(net_socket_t *sock, const char *format, ...) {
	if (!sock || !format) return -1;

	char buffer[4096];
	va_list args;
	va_start(args, format);
	int len = vsnprintf(buffer, sizeof(buffer), format, args);
	va_end(args);

	if (len < 0) return -1;
	if (len >= (int)sizeof(buffer)) len = sizeof(buffer) - 1;

	return net_socket_send_all(sock, buffer, len);
}

/*
 * File I/O Extensions (continued in next part due to length...)
 */

/*
 * File I/O Extensions
 */

void *net_file_read_all(const char *filename, size_t *out_size) {
	if (!filename) return NULL;

	FILE *f = fopen(filename, "rb");
	if (!f) return NULL;

	fseek(f, 0, SEEK_END);
	long size = ftell(f);
	fseek(f, 0, SEEK_SET);

	if (size < 0) {
		fclose(f);
		return NULL;
	}

	void *buffer = malloc(size + 1);
	if (!buffer) {
		fclose(f);
		return NULL;
	}

	size_t read_size = fread(buffer, 1, size, f);
	fclose(f);

	((char *)buffer)[size] = '\0';  /* Null-terminate for convenience */

	if (out_size) *out_size = read_size;
	return buffer;
}

int net_file_write_all(const char *filename, const void *data, size_t size) {
	if (!filename || !data) return -1;

	FILE *f = fopen(filename, "wb");
	if (!f) return -1;

	size_t written = fwrite(data, 1, size, f);
	fclose(f);

	return (written == size) ? 0 : -1;
}

int net_file_copy(const char *src, const char *dest) {
	if (!src || !dest) return -1;

	size_t size;
	void *data = net_file_read_all(src, &size);
	if (!data) return -1;

	int result = net_file_write_all(dest, data, size);
	free(data);
	return result;
}

int64_t net_file_size(const char *filename) {
	if (!filename) return -1;

	FILE *f = fopen(filename, "rb");
	if (!f) return -1;

	fseek(f, 0, SEEK_END);
	long size = ftell(f);
	fclose(f);

	return size;
}

int net_file_exists(const char *filename) {
	if (!filename) return 0;

	FILE *f = fopen(filename, "rb");
	if (!f) return 0;

	fclose(f);
	return 1;
}

int net_mkdir_recursive(const char *path) {
	if (!path) return -1;

	char tmp[512];
	char *p = NULL;
	size_t len;

	snprintf(tmp, sizeof(tmp), "%s", path);
	len = strlen(tmp);
	if (tmp[len - 1] == '/' || tmp[len - 1] == '\\')
		tmp[len - 1] = 0;

	for (p = tmp + 1; *p; p++) {
		if (*p == '/' || *p == '\\') {
			*p = 0;
#ifdef _WIN32
			CreateDirectoryA(tmp, NULL);
#else
			mkdir(tmp, 0755);
#endif
			*p = '/';
		}
	}

#ifdef _WIN32
	return CreateDirectoryA(tmp, NULL) ? 0 : -1;
#else
	return mkdir(tmp, 0755);
#endif
}

char **net_list_directory(const char *path, int *count) {
	if (!path || !count) return NULL;

	*count = 0;

#ifdef _WIN32
	WIN32_FIND_DATAA find_data;
	char search_path[512];
	snprintf(search_path, sizeof(search_path), "%s\\*", path);

	HANDLE hFind = FindFirstFileA(search_path, &find_data);
	if (hFind == INVALID_HANDLE_VALUE) return NULL;

	/* Count entries */
	int n = 0;
	do {
		if (strcmp(find_data.cFileName, ".") != 0 &&
		    strcmp(find_data.cFileName, "..") != 0) {
			n++;
		}
	} while (FindNextFileA(hFind, &find_data));

	/* Allocate array */
	char **entries = calloc(n + 1, sizeof(char *));
	if (!entries) {
		FindClose(hFind);
		return NULL;
	}

	/* Read entries */
	FindClose(hFind);
	hFind = FindFirstFileA(search_path, &find_data);
	int i = 0;
	do {
		if (strcmp(find_data.cFileName, ".") != 0 &&
		    strcmp(find_data.cFileName, "..") != 0) {
			entries[i++] = strdup(find_data.cFileName);
		}
	} while (FindNextFileA(hFind, &find_data));

	FindClose(hFind);
	*count = n;
	return entries;
#else
	DIR *dir = opendir(path);
	if (!dir) return NULL;

	/* Count entries */
	int n = 0;
	struct dirent *entry;
	while ((entry = readdir(dir)) != NULL) {
		if (strcmp(entry->d_name, ".") != 0 &&
		    strcmp(entry->d_name, "..") != 0) {
			n++;
		}
	}

	/* Allocate array */
	char **entries = calloc(n + 1, sizeof(char *));
	if (!entries) {
		closedir(dir);
		return NULL;
	}

	/* Read entries */
	rewinddir(dir);
	int i = 0;
	while ((entry = readdir(dir)) != NULL) {
		if (strcmp(entry->d_name, ".") != 0 &&
		    strcmp(entry->d_name, "..") != 0) {
			entries[i++] = strdup(entry->d_name);
		}
	}

	closedir(dir);
	*count = n;
	return entries;
#endif
}

/*
 * I/O Multiplexing
 */

net_poll_t *net_poll_create(void) {
	net_poll_t *poll = calloc(1, sizeof(net_poll_t));
	if (!poll) return NULL;

	poll->capacity = 16;
#ifdef _WIN32
	FD_ZERO(&poll->read_fds);
	FD_ZERO(&poll->write_fds);
	FD_ZERO(&poll->error_fds);
	poll->sockets = calloc(poll->capacity, sizeof(net_socket_t *));
	poll->events = calloc(poll->capacity, sizeof(uint32_t));
	poll->user_data = calloc(poll->capacity, sizeof(void *));
#else
	poll->fds = calloc(poll->capacity, sizeof(struct pollfd));
	poll->sockets = calloc(poll->capacity, sizeof(net_socket_t *));
	poll->user_data = calloc(poll->capacity, sizeof(void *));
#endif

	return poll;
}

void net_poll_destroy(net_poll_t *poll) {
	if (!poll) return;

#ifdef _WIN32
	free(poll->sockets);
	free(poll->events);
	free(poll->user_data);
#else
	free(poll->fds);
	free(poll->sockets);
	free(poll->user_data);
#endif

	free(poll);
}

int net_poll_add(net_poll_t *poll, net_socket_t *sock, uint32_t events, void *user_data) {
	if (!poll || !sock) return -1;

	/* Expand if needed */
	if (poll->count >= poll->capacity) {
		int new_capacity = poll->capacity * 2;
#ifdef _WIN32
		poll->sockets = realloc(poll->sockets, new_capacity * sizeof(net_socket_t *));
		poll->events = realloc(poll->events, new_capacity * sizeof(uint32_t));
		poll->user_data = realloc(poll->user_data, new_capacity * sizeof(void *));
#else
		poll->fds = realloc(poll->fds, new_capacity * sizeof(struct pollfd));
		poll->sockets = realloc(poll->sockets, new_capacity * sizeof(net_socket_t *));
		poll->user_data = realloc(poll->user_data, new_capacity * sizeof(void *));
#endif
		poll->capacity = new_capacity;
	}

	int idx = poll->count;
	poll->sockets[idx] = sock;
	poll->user_data[idx] = user_data;

#ifdef _WIN32
	poll->events[idx] = events;
#else
	poll->fds[idx].fd = sock->fd;
	poll->fds[idx].events = 0;
	if (events & NET_EVENT_READ) poll->fds[idx].events |= POLLIN;
	if (events & NET_EVENT_WRITE) poll->fds[idx].events |= POLLOUT;
	poll->fds[idx].revents = 0;
#endif

	poll->count++;
	return 0;
}

int net_poll_remove(net_poll_t *poll, net_socket_t *sock) {
	if (!poll || !sock) return -1;

	/* Find socket */
	int idx = -1;
	for (int i = 0; i < poll->count; i++) {
		if (poll->sockets[i] == sock) {
			idx = i;
			break;
		}
	}

	if (idx < 0) return -1;

	/* Remove by shifting */
	for (int i = idx; i < poll->count - 1; i++) {
		poll->sockets[i] = poll->sockets[i + 1];
		poll->user_data[i] = poll->user_data[i + 1];
#ifdef _WIN32
		poll->events[i] = poll->events[i + 1];
#else
		poll->fds[i] = poll->fds[i + 1];
#endif
	}

	poll->count--;
	return 0;
}

int net_poll_modify(net_poll_t *poll, net_socket_t *sock, uint32_t events) {
	if (!poll || !sock) return -1;

	for (int i = 0; i < poll->count; i++) {
		if (poll->sockets[i] == sock) {
#ifdef _WIN32
			poll->events[i] = events;
#else
			poll->fds[i].events = 0;
			if (events & NET_EVENT_READ) poll->fds[i].events |= POLLIN;
			if (events & NET_EVENT_WRITE) poll->fds[i].events |= POLLOUT;
#endif
			return 0;
		}
	}

	return -1;
}

int net_poll_wait(net_poll_t *poll, net_poll_event_t *events, int max_events, int timeout_ms) {
	if (!poll || !events) return -1;

#ifdef _WIN32
	/* Windows select() implementation */
	FD_ZERO(&poll->read_fds);
	FD_ZERO(&poll->write_fds);
	FD_ZERO(&poll->error_fds);

	for (int i = 0; i < poll->count; i++) {
		if (poll->events[i] & NET_EVENT_READ)
			FD_SET(poll->sockets[i]->fd, &poll->read_fds);
		if (poll->events[i] & NET_EVENT_WRITE)
			FD_SET(poll->sockets[i]->fd, &poll->write_fds);
		FD_SET(poll->sockets[i]->fd, &poll->error_fds);
	}

	struct timeval tv;
	struct timeval *tvp = NULL;
	if (timeout_ms >= 0) {
		tv.tv_sec = timeout_ms / 1000;
		tv.tv_usec = (timeout_ms % 1000) * 1000;
		tvp = &tv;
	}

	int ready = select(0, &poll->read_fds, &poll->write_fds, &poll->error_fds, tvp);
	if (ready <= 0) return ready;

	int n = 0;
	for (int i = 0; i < poll->count && n < max_events; i++) {
		uint32_t revents = 0;

		if (FD_ISSET(poll->sockets[i]->fd, &poll->read_fds))
			revents |= NET_EVENT_READ;
		if (FD_ISSET(poll->sockets[i]->fd, &poll->write_fds))
			revents |= NET_EVENT_WRITE;
		if (FD_ISSET(poll->sockets[i]->fd, &poll->error_fds))
			revents |= NET_EVENT_ERROR;

		if (revents) {
			events[n].socket = poll->sockets[i];
			events[n].events = revents;
			events[n].user_data = poll->user_data[i];
			n++;
		}
	}

	return n;
#else
	/* Unix poll() implementation */
	int ready = poll(poll->fds, poll->count, timeout_ms);
	if (ready <= 0) return ready;

	int n = 0;
	for (int i = 0; i < poll->count && n < max_events; i++) {
		if (poll->fds[i].revents == 0) continue;

		uint32_t revents = 0;
		if (poll->fds[i].revents & POLLIN) revents |= NET_EVENT_READ;
		if (poll->fds[i].revents & POLLOUT) revents |= NET_EVENT_WRITE;
		if (poll->fds[i].revents & (POLLERR | POLLNVAL)) revents |= NET_EVENT_ERROR;
		if (poll->fds[i].revents & POLLHUP) revents |= NET_EVENT_HUP;

		events[n].socket = poll->sockets[i];
		events[n].events = revents;
		events[n].user_data = poll->user_data[i];
		n++;
	}

	return n;
#endif
}

/*
 * Statistics
 */

void net_socket_get_stats(net_socket_t *sock, net_stats_t *stats) {
	if (!sock || !stats) return;
	*stats = sock->stats;
}

void net_socket_reset_stats(net_socket_t *sock) {
	if (!sock) return;
	memset(&sock->stats, 0, sizeof(sock->stats));
}

void net_get_global_stats(net_stats_t *stats) {
	if (!stats) return;
	*stats = net_global_stats;
}
