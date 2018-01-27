#include <windows.h>
#include <string>
#include <iostream>

std::string exePath() {
    char buffer[MAX_PATH];
    GetModuleFileName(NULL, buffer, MAX_PATH);
    std::string::size_type pos = std::string(buffer).find_last_of("\\/");
    return std::string(buffer).substr(0, pos);
}

int main() {
    std::string cd = "cd " + exePath();
    std::string run = "erl -run smoltext start";
    ShowWindow(GetConsoleWindow(), SW_HIDE);
    system(cd.c_str());
    system(run.c_str());
    return 0;
}
