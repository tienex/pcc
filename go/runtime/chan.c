/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Channel operations
 * Simplified channel implementation without full goroutine support
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include "runtime.h"

/* Channel buffer entry */
typedef struct {
	void *data;
} chan_buffer_entry;

/* Channel structure */
struct go_channel {
	size_t elem_size;       /* Size of each element */
	go_int buffer_size;     /* Buffer capacity */
	go_int count;           /* Number of elements in buffer */
	go_int read_idx;        /* Read position */
	go_int write_idx;       /* Write position */
	chan_buffer_entry *buffer;
	pthread_mutex_t lock;
	pthread_cond_t not_empty;
	pthread_cond_t not_full;
	bool closed;
};

/*
 * Create new channel
 */
go_channel *
go_chan_new(size_t elem_size, go_int buffer_size)
{
	go_channel *ch;

	ch = (go_channel *)go_malloc(sizeof(go_channel));
	ch->elem_size = elem_size;
	ch->buffer_size = buffer_size;
	ch->count = 0;
	ch->read_idx = 0;
	ch->write_idx = 0;
	ch->closed = false;

	if (buffer_size > 0) {
		ch->buffer = (chan_buffer_entry *)go_calloc(buffer_size,
		    sizeof(chan_buffer_entry));
		/* Pre-allocate element storage */
		for (go_int i = 0; i < buffer_size; i++) {
			ch->buffer[i].data = go_malloc(elem_size);
		}
	} else {
		ch->buffer = NULL;
	}

	pthread_mutex_init(&ch->lock, NULL);
	pthread_cond_init(&ch->not_empty, NULL);
	pthread_cond_init(&ch->not_full, NULL);

	return ch;
}

/*
 * Send to channel (blocking)
 */
void
go_chan_send(go_channel *ch, void *elem)
{
	if (ch == NULL) {
		fprintf(stderr, "runtime error: send on nil channel\n");
		go_panic((go_interface){ NULL, NULL });
	}

	pthread_mutex_lock(&ch->lock);

	/* Check if closed */
	if (ch->closed) {
		pthread_mutex_unlock(&ch->lock);
		fprintf(stderr, "runtime error: send on closed channel\n");
		go_panic((go_interface){ NULL, NULL });
	}

	/* Wait for space in buffer */
	while (ch->count >= ch->buffer_size && !ch->closed) {
		pthread_cond_wait(&ch->not_full, &ch->lock);
	}

	if (ch->closed) {
		pthread_mutex_unlock(&ch->lock);
		fprintf(stderr, "runtime error: send on closed channel\n");
		go_panic((go_interface){ NULL, NULL });
	}

	/* Copy element to buffer */
	memcpy(ch->buffer[ch->write_idx].data, elem, ch->elem_size);
	ch->write_idx = (ch->write_idx + 1) % ch->buffer_size;
	ch->count++;

	/* Signal receivers */
	pthread_cond_signal(&ch->not_empty);
	pthread_mutex_unlock(&ch->lock);
}

/*
 * Receive from channel (blocking)
 */
bool
go_chan_recv(go_channel *ch, void *elem)
{
	bool ok;

	if (ch == NULL) {
		/* Receive from nil channel blocks forever */
		pthread_mutex_t dummy;
		pthread_mutex_init(&dummy, NULL);
		pthread_mutex_lock(&dummy);
		pthread_cond_t cond;
		pthread_cond_init(&cond, NULL);
		pthread_cond_wait(&cond, &dummy);
		return false;  /* Never reached */
	}

	pthread_mutex_lock(&ch->lock);

	/* Wait for data or close */
	while (ch->count == 0 && !ch->closed) {
		pthread_cond_wait(&ch->not_empty, &ch->lock);
	}

	if (ch->count == 0 && ch->closed) {
		/* Channel closed and empty */
		pthread_mutex_unlock(&ch->lock);
		if (elem != NULL)
			memset(elem, 0, ch->elem_size);
		return false;
	}

	/* Copy element from buffer */
	if (elem != NULL)
		memcpy(elem, ch->buffer[ch->read_idx].data, ch->elem_size);
	ch->read_idx = (ch->read_idx + 1) % ch->buffer_size;
	ch->count--;
	ok = true;

	/* Signal senders */
	pthread_cond_signal(&ch->not_full);
	pthread_mutex_unlock(&ch->lock);

	return ok;
}

/*
 * Try to send (non-blocking)
 */
bool
go_chan_try_send(go_channel *ch, void *elem)
{
	bool sent = false;

	if (ch == NULL)
		return false;

	pthread_mutex_lock(&ch->lock);

	if (!ch->closed && ch->count < ch->buffer_size) {
		/* Space available */
		memcpy(ch->buffer[ch->write_idx].data, elem, ch->elem_size);
		ch->write_idx = (ch->write_idx + 1) % ch->buffer_size;
		ch->count++;
		sent = true;
		pthread_cond_signal(&ch->not_empty);
	}

	pthread_mutex_unlock(&ch->lock);
	return sent;
}

/*
 * Try to receive (non-blocking)
 */
bool
go_chan_try_recv(go_channel *ch, void *elem)
{
	bool received = false;

	if (ch == NULL)
		return false;

	pthread_mutex_lock(&ch->lock);

	if (ch->count > 0) {
		/* Data available */
		if (elem != NULL)
			memcpy(elem, ch->buffer[ch->read_idx].data, ch->elem_size);
		ch->read_idx = (ch->read_idx + 1) % ch->buffer_size;
		ch->count--;
		received = true;
		pthread_cond_signal(&ch->not_full);
	} else if (ch->closed) {
		/* Closed and empty */
		if (elem != NULL)
			memset(elem, 0, ch->elem_size);
		received = true;
	}

	pthread_mutex_unlock(&ch->lock);
	return received;
}

/*
 * Close channel
 */
void
go_chan_close(go_channel *ch)
{
	if (ch == NULL) {
		fprintf(stderr, "runtime error: close of nil channel\n");
		go_panic((go_interface){ NULL, NULL });
	}

	pthread_mutex_lock(&ch->lock);

	if (ch->closed) {
		pthread_mutex_unlock(&ch->lock);
		fprintf(stderr, "runtime error: close of closed channel\n");
		go_panic((go_interface){ NULL, NULL });
	}

	ch->closed = true;

	/* Wake up all waiting goroutines */
	pthread_cond_broadcast(&ch->not_empty);
	pthread_cond_broadcast(&ch->not_full);

	pthread_mutex_unlock(&ch->lock);
}

/*
 * Check if channel is closed
 */
bool
go_chan_is_closed(go_channel *ch)
{
	bool closed;

	if (ch == NULL)
		return false;

	pthread_mutex_lock(&ch->lock);
	closed = ch->closed;
	pthread_mutex_unlock(&ch->lock);

	return closed;
}

/*
 * Free channel
 */
void
go_chan_free(go_channel *ch)
{
	if (ch == NULL)
		return;

	/* Free buffer */
	if (ch->buffer != NULL) {
		for (go_int i = 0; i < ch->buffer_size; i++) {
			if (ch->buffer[i].data != NULL)
				go_free(ch->buffer[i].data);
		}
		go_free(ch->buffer);
	}

	pthread_mutex_destroy(&ch->lock);
	pthread_cond_destroy(&ch->not_empty);
	pthread_cond_destroy(&ch->not_full);

	go_free(ch);
}

/*
 * Select implementation (simplified)
 */
int
go_select(go_select_case *cases, int ncase)
{
	int i;

	/* Simple non-blocking select */
	for (i = 0; i < ncase; i++) {
		switch (cases[i].op) {
		case GO_SELECT_SEND:
			if (go_chan_try_send(cases[i].ch, cases[i].data))
				return i;
			break;

		case GO_SELECT_RECV:
			if (go_chan_try_recv(cases[i].ch, cases[i].data))
				return i;
			break;

		case GO_SELECT_DEFAULT:
			return i;
		}
	}

	/* No case ready - block on first channel */
	if (ncase > 0 && cases[0].op != GO_SELECT_DEFAULT) {
		if (cases[0].op == GO_SELECT_SEND)
			go_chan_send(cases[0].ch, cases[0].data);
		else if (cases[0].op == GO_SELECT_RECV)
			go_chan_recv(cases[0].ch, cases[0].data);
		return 0;
	}

	return -1;
}
