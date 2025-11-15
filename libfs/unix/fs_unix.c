/*
 * Unix/POSIX File System Implementation
 * Covers: Linux, BSD, macOS, Solaris, AIX, HPUX, Tru64, IRIX, etc.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <libgen.h>
#include <glob.h>

struct fs_file {
	FILE *fp;
	int fd;
	int error;
};

struct fs_dir {
	DIR *dp;
};

/* Path manipulation */
char *fs_path_join(const char *path1, const char *path2) {
	if (!path1 || !path2) return NULL;
	size_t len1 = strlen(path1);
	size_t len2 = strlen(path2);
	char *result = malloc(len1 + len2 + 2);
	if (!result) return NULL;
	
	strcpy(result, path1);
	if (len1 > 0 && path1[len1-1] != '/') strcat(result, "/");
	strcat(result, path2);
	return result;
}

char *fs_path_dirname(const char *path) {
	if (!path) return NULL;
	char *copy = strdup(path);
	char *dir = dirname(copy);
	char *result = strdup(dir);
	free(copy);
	return result;
}

char *fs_path_basename(const char *path) {
	if (!path) return NULL;
	char *copy = strdup(path);
	char *base = basename(copy);
	char *result = strdup(base);
	free(copy);
	return result;
}

char *fs_path_extension(const char *path) {
	if (!path) return NULL;
	const char *dot = strrchr(path, '.');
	const char *slash = strrchr(path, '/');
	if (dot && (!slash || dot > slash)) return strdup(dot + 1);
	return strdup("");
}

char *fs_path_absolute(const char *path) {
	if (!path) return NULL;
	return realpath(path, NULL);
}

int fs_path_is_absolute(const char *path) {
	return path && path[0] == '/';
}

int fs_path_exists(const char *path) {
	struct stat st;
	return stat(path, &st) == 0;
}

/* File operations */
fs_file_t *fs_open(const char *path, int mode) {
	if (!path) return NULL;
	
	fs_file_t *file = calloc(1, sizeof(fs_file_t));
	if (!file) return NULL;
	
	const char *fmode = "r";
	if ((mode & (FS_READ|FS_WRITE)) == (FS_READ|FS_WRITE)) fmode = "r+";
	else if (mode & FS_WRITE) {
		if (mode & FS_APPEND) fmode = "a";
		else if (mode & FS_CREATE) fmode = "w";
		else fmode = "w";
	}
	if (mode & FS_BINARY) {
		/* Binary mode (no-op on Unix) */
	}
	
	file->fp = fopen(path, fmode);
	if (!file->fp) {
		free(file);
		return NULL;
	}
	file->fd = fileno(file->fp);
	return file;
}

void fs_close(fs_file_t *file) {
	if (!file) return;
	if (file->fp) fclose(file->fp);
	free(file);
}

ssize_t fs_read(fs_file_t *file, void *buffer, size_t size) {
	if (!file || !file->fp) return -1;
	return fread(buffer, 1, size, file->fp);
}

ssize_t fs_write(fs_file_t *file, const void *buffer, size_t size) {
	if (!file || !file->fp) return -1;
	return fwrite(buffer, 1, size, file->fp);
}

int64_t fs_seek(fs_file_t *file, int64_t offset, fs_seek_t origin) {
	if (!file || !file->fp) return -1;
	int whence = (origin == FS_SEEK_SET) ? SEEK_SET :
	             (origin == FS_SEEK_CUR) ? SEEK_CUR : SEEK_END;
	return fseeko(file->fp, offset, whence);
}

int64_t fs_tell(fs_file_t *file) {
	if (!file || !file->fp) return -1;
	return ftello(file->fp);
}

int fs_flush(fs_file_t *file) {
	if (!file || !file->fp) return -1;
	return fflush(file->fp);
}

/* File management */
int fs_remove(const char *path) {
	return unlink(path);
}

int fs_rename(const char *oldpath, const char *newpath) {
	return rename(oldpath, newpath);
}

int fs_link(const char *target, const char *linkpath) {
	return link(target, linkpath);
}

int fs_symlink(const char *target, const char *linkpath) {
	return symlink(target, linkpath);
}

/* Directory operations */
int fs_mkdir(const char *path) {
	return mkdir(path, 0755);
}

int fs_rmdir(const char *path) {
	return rmdir(path);
}

int fs_chdir(const char *path) {
	return chdir(path);
}

char *fs_getcwd(void) {
	return getcwd(NULL, 0);
}

fs_dir_t *fs_opendir(const char *path) {
	fs_dir_t *dir = calloc(1, sizeof(fs_dir_t));
	if (!dir) return NULL;
	
	dir->dp = opendir(path);
	if (!dir->dp) {
		free(dir);
		return NULL;
	}
	return dir;
}

int fs_readdir(fs_dir_t *dir, fs_info_t *info) {
	if (!dir || !dir->dp || !info) return -1;
	
	struct dirent *ent = readdir(dir->dp);
	if (!ent) return -1;
	
	strncpy(info->name, ent->d_name, sizeof(info->name) - 1);
	info->is_directory = (ent->d_type == DT_DIR);
	info->is_symlink = (ent->d_type == DT_LNK);
	
	return 0;
}

void fs_closedir(fs_dir_t *dir) {
	if (!dir) return;
	if (dir->dp) closedir(dir->dp);
	free(dir);
}

/* File attributes */
int fs_stat(const char *path, fs_info_t *info) {
	if (!path || !info) return -1;
	
	struct stat st;
	if (stat(path, &st) < 0) return -1;
	
	info->size = st.st_size;
	info->create_time = st.st_ctime;
	info->modify_time = st.st_mtime;
	info->access_time = st.st_atime;
	info->is_directory = S_ISDIR(st.st_mode);
	info->is_symlink = S_ISLNK(st.st_mode);
	
	return 0;
}

int fs_chmod(const char *path, int mode) {
	return chmod(path, mode);
}

char **fs_glob(const char *pattern, int *count) {
	if (!pattern || !count) return NULL;
	
	glob_t globbuf;
	if (glob(pattern, 0, NULL, &globbuf) != 0) {
		*count = 0;
		return NULL;
	}
	
	char **results = malloc(globbuf.gl_pathc * sizeof(char *));
	for (size_t i = 0; i < globbuf.gl_pathc; i++) {
		results[i] = strdup(globbuf.gl_pathv[i]);
	}
	
	*count = globbuf.gl_pathc;
	globfree(&globbuf);
	return results;
}
