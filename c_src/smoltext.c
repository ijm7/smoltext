#ifdef _WIN32
#include <windows.h>
#define ARG_MAX 8192
#define PATH_LEN MAX_PATH
#define NULL_DIR ""

#elif __linux__
#include <unistd.h>
#include <stdlib.h>
#include <linux/limits.h>
#define PATH_LEN PATH_MAX
#define NULL_DIR " > /dev/null"

#endif
#include <string.h>
#include <stdio.h>

static int exePath(char* path) {
	int ret_val;
	char* last_slash;

#ifdef _WIN32
	ret_val = GetModuleFileName(NULL, path, PATH_LEN);
	if (ret == 0 || ret > PATH_LEN) {
		return -2;
	}
	last_slash = strrchr(path, '\\');
#elif __linux__
	ret_val = readlink("/proc/self/exe", path, PATH_LEN);
	if (ret_val == -1) {
		return -2;
	}
	last_slash = strrchr(path, '/');
#endif
	if (last_slash != NULL) {
		*last_slash = '\0';
		return 0;
	}
	return -1;
}

int main() {
#if !defined(_WIN32) && !defined(__linux__)
	// Incompatible OS
	return -3;
#endif
	char path[PATH_LEN];
	const int ret_val = exePath(path);
	if (ret_val < 0) {
		return ret_val;
	}

	const char * const commands[] = {
		"cd ",
		path,
		"/ebin",
		" && ",
		"erl -run smoltext startFromBin",
		NULL_DIR
	};
	char command[ARG_MAX];

	int i;
	for (i = 0; i < sizeof(commands) / sizeof(commands[0]); i++) {
		if ((strlen(command) + 1 + strlen(commands[i]) + 1) > ARG_MAX) {
			return -2;
		}
		strcat(command, commands[i]);
	}
	#ifdef _WIN32
	ShowWindow(GetConsoleWindow(), SW_HIDE);
	#endif
	system(command);
	#ifdef _WIN32
	FreeConsole();
	#endif
	return 0;
}
