/* Win32 File System Implementation */
#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#include <io.h>

struct fs_file { FILE *fp; HANDLE hFile; };
struct fs_dir { HANDLE hFind; WIN32_FIND_DATAA findData; int first; };

/* Simplified Win32 implementation - delegates to C runtime where possible */
fs_file_t *fs_open(const char *path, int mode) {
	fs_file_t *file = calloc(1, sizeof(fs_file_t));
	const char *fmode = (mode & FS_WRITE) ? "wb" : "rb";
	file->fp = fopen(path, fmode);
	return file->fp ? file : (free(file), NULL);
}

void fs_close(fs_file_t *file) {
	if (file) { if (file->fp) fclose(file->fp); free(file); }
}

ssize_t fs_read(fs_file_t *file, void *buffer, size_t size) {
	return file && file->fp ? fread(buffer, 1, size, file->fp) : -1;
}

ssize_t fs_write(fs_file_t *file, const void *buffer, size_t size) {
	return file && file->fp ? fwrite(buffer, 1, size, file->fp) : -1;
}

int fs_remove(const char *path) { return DeleteFileA(path) ? 0 : -1; }
int fs_rename(const char *old, const char *new) { return MoveFileA(old, new) ? 0 : -1; }
int fs_mkdir(const char *path) { return _mkdir(path); }
int fs_rmdir(const char *path) { return _rmdir(path); }
int fs_chdir(const char *path) { return _chdir(path); }
char *fs_getcwd(void) { char buf[MAX_PATH]; return _getcwd(buf, MAX_PATH) ? strdup(buf) : NULL; }

fs_dir_t *fs_opendir(const char *path) {
	fs_dir_t *dir = calloc(1, sizeof(fs_dir_t));
	char search[MAX_PATH]; snprintf(search, MAX_PATH, "%s\\*", path);
	dir->hFind = FindFirstFileA(search, &dir->findData);
	if (dir->hFind == INVALID_HANDLE_VALUE) { free(dir); return NULL; }
	dir->first = 1;
	return dir;
}

int fs_readdir(fs_dir_t *dir, fs_info_t *info) {
	if (!dir) return -1;
	if (dir->first) { dir->first = 0; }
	else if (!FindNextFileA(dir->hFind, &dir->findData)) return -1;
	strncpy(info->name, dir->findData.cFileName, 260);
	info->is_directory = (dir->findData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
	return 0;
}

void fs_closedir(fs_dir_t *dir) {
	if (dir) { if (dir->hFind != INVALID_HANDLE_VALUE) FindClose(dir->hFind); free(dir); }
}

/* Delegate other functions to stubs or Unix-like implementations */
char *fs_path_join(const char *a, const char *b) {
	char *r = malloc(strlen(a) + strlen(b) + 2);
	sprintf(r, "%s\\%s", a, b); return r;
}
int fs_path_is_absolute(const char *p) { return p && (p[1] == ':' || p[0] == '\\'); }
#endif
