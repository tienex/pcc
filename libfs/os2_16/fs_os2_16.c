/* os2_16 File System Implementation - Stub */
#include <stdio.h>
#include <stdlib.h>

struct fs_file { FILE *fp; };
struct fs_dir { void *handle; };

fs_file_t *fs_open(const char *path, int mode) {
	fs_file_t *f = calloc(1, sizeof(fs_file_t));
	f->fp = fopen(path, "rb");
	return f->fp ? f : (free(f), NULL);
}

void fs_close(fs_file_t *f) { if (f) { if (f->fp) fclose(f->fp); free(f); } }
ssize_t fs_read(fs_file_t *f, void *b, size_t s) { return f ? fread(b, 1, s, f->fp) : -1; }
ssize_t fs_write(fs_file_t *f, const void *b, size_t s) { return f ? fwrite(b, 1, s, f->fp) : -1; }
