/*
 * Copyright (c) 2025 PCC Project
 * Universal File System Abstraction Library
 * 
 * Provides uniform API across all supported platforms:
 * DOS, Win16, OS/2 16-bit, DOS32, Win32, OS/2 32-bit,
 * OpenVMS, Haiku/BeOS, Unix/BSD/Linux/macOS/etc.
 */

#ifndef _PCC_FS_H_
#define _PCC_FS_H_

#include <stddef.h>
#include <stdint.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* File handle */
typedef struct fs_file fs_file_t;
typedef struct fs_dir fs_dir_t;

/* File attributes */
typedef enum {
	FS_ATTR_READONLY  = 0x01,
	FS_ATTR_HIDDEN    = 0x02,
	FS_ATTR_SYSTEM    = 0x04,
	FS_ATTR_DIRECTORY = 0x10,
	FS_ATTR_ARCHIVE   = 0x20
} fs_attr_t;

/* File open modes */
typedef enum {
	FS_READ     = 0x01,
	FS_WRITE    = 0x02,
	FS_APPEND   = 0x04,
	FS_CREATE   = 0x08,
	FS_TRUNCATE = 0x10,
	FS_EXCL     = 0x20,
	FS_BINARY   = 0x40,
	FS_TEXT     = 0x80
} fs_mode_t;

/* Seek origins */
typedef enum {
	FS_SEEK_SET,
	FS_SEEK_CUR,
	FS_SEEK_END
} fs_seek_t;

/* File information */
typedef struct {
	char name[260];
	uint64_t size;
	time_t create_time;
	time_t modify_time;
	time_t access_time;
	uint32_t attributes;
	int is_directory;
	int is_symlink;
} fs_info_t;

/* File system information */
typedef struct {
	char volume_name[64];
	uint64_t total_bytes;
	uint64_t free_bytes;
	uint64_t available_bytes;
	uint32_t bytes_per_sector;
	uint32_t sectors_per_cluster;
} fs_volume_info_t;

/* Path manipulation */
char *fs_path_join(const char *path1, const char *path2);
char *fs_path_dirname(const char *path);
char *fs_path_basename(const char *path);
char *fs_path_extension(const char *path);
char *fs_path_absolute(const char *path);
char *fs_path_normalize(const char *path);
int fs_path_is_absolute(const char *path);
int fs_path_exists(const char *path);

/* File operations */
fs_file_t *fs_open(const char *path, int mode);
void fs_close(fs_file_t *file);
ssize_t fs_read(fs_file_t *file, void *buffer, size_t size);
ssize_t fs_write(fs_file_t *file, const void *buffer, size_t size);
int64_t fs_seek(fs_file_t *file, int64_t offset, fs_seek_t origin);
int64_t fs_tell(fs_file_t *file);
int fs_flush(fs_file_t *file);
int64_t fs_size(fs_file_t *file);
int fs_eof(fs_file_t *file);
int fs_error(fs_file_t *file);

/* File management */
int fs_remove(const char *path);
int fs_rename(const char *oldpath, const char *newpath);
int fs_copy(const char *src, const char *dest);
int fs_move(const char *src, const char *dest);
int fs_link(const char *target, const char *linkpath);
int fs_symlink(const char *target, const char *linkpath);

/* Directory operations */
int fs_mkdir(const char *path);
int fs_rmdir(const char *path);
int fs_chdir(const char *path);
char *fs_getcwd(void);
fs_dir_t *fs_opendir(const char *path);
int fs_readdir(fs_dir_t *dir, fs_info_t *info);
void fs_closedir(fs_dir_t *dir);

/* File attributes */
int fs_stat(const char *path, fs_info_t *info);
int fs_fstat(fs_file_t *file, fs_info_t *info);
int fs_chmod(const char *path, int mode);
int fs_chown(const char *path, int uid, int gid);
int fs_utime(const char *path, time_t atime, time_t mtime);

/* Volume operations */
int fs_volume_info(const char *path, fs_volume_info_t *info);
char **fs_list_volumes(int *count);

/* Temporary files */
fs_file_t *fs_tmpfile(void);
char *fs_tmpnam(void);
char *fs_mktemp(const char *template);

/* Path search */
char *fs_which(const char *name);
char **fs_glob(const char *pattern, int *count);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_FS_H_ */
