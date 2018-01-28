#ifdef _WIN32
#include <windows.h>
#elif __unix__
#include <unistd.h>
#include <stdlib.h>
#include <limits.h>
#endif
#include <string>
#include <iostream>

std::string exePath() {
	#ifdef _WIN32
    char buffer[MAX_PATH];
    GetModuleFileName(NULL, buffer, MAX_PATH);
    #elif __unix__
    char buffer[PATH_MAX];
    readlink("/proc/self/exe", buffer, PATH_MAX);
    #endif
    std::string::size_type pos = std::string(buffer).find_last_of("\\/");
    return std::string(buffer).substr(0, pos);
}

int main() {
	//#ifdef _WIN32
	//std::string nullDir = " > NUL";
	std::string nullDir = "";
	#if __unix__
	nullDir = " > /dev/null";
	#endif
    std::string cd = "cd " + exePath() + "/ebin" + nullDir;
    std::string run = "erl -run smoltext start" + nullDir;
    std::string command = cd + " && " + run;
	#ifdef _WIN32
	ShowWindow(GetConsoleWindow(), SW_HIDE);
	#endif
    system(command.c_str());
	#ifdef _WIN32
    FreeConsole();
	#endif
    return 0;
}
