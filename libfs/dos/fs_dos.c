/* DOS File System Implementation (uses C runtime) */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dir.h>
#include <dos.h>

struct fs_file { FILE *fp; };
struct fs_dir { struct ffblk ffblk; int done; };

fs_file_t *fs_open(const char *path, int mode) {
	fs_file_t *f = calloc(1, sizeof(fs_file_t));
	f->fp = fopen(path, (mode & FS_WRITE) ? "wb" : "rb");
	return f->fp ? f : (free(f), NULL);
}

void fs_close(fs_file_t *f) { if (f) { if (f->fp) fclose(f->fp); free(f); } }
ssize_t fs_read(fs_file_t *f, void *b, size_t s) { return fread(b, 1, s, f->fp); }
ssize_t fs_write(fs_file_t *f, const void *b, size_t s) { return fwrite(b, 1, s, f->fp); }
int fs_mkdir(const char *p) { return mkdir(p); }
char *fs_getcwd(void) { char b[128]; return getcwd(b, 128) ? strdup(b) : NULL; }
