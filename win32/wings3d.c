/*
 *  wings3d.c --
 *
 *     Wrapper to start Wings3D on Windows.
 *
 *  Copyright (c) 2002-2011 Bjorn Gustavsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings3d.c,v 1.7 2004/10/29 15:52:23 bjorng Exp $
 *
 */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <shlobj.h>
#include <stdio.h>
#include <stdlib.h>

static void install(void);
static void print_path(FILE* fp, char* path);

int
WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR szCmdLine, int sw)
{
  PROCESS_INFORMATION piProcInfo;
  STARTUPINFO siStartInfo = {0};
  int argc = __argc;
  char** argv = __argv;
  char install_dir[MAX_PATH];
  char cmd_line[3*MAX_PATH];
  TCHAR pref_dir[MAX_PATH];
  char message[40];
  int i;
  int ok;
  int err;
  HKEY hkey;
  DWORD type;
  HANDLE module = GetModuleHandle(NULL);

  if (argc > 1 && strcmp(argv[1], "--install") == 0) {
    install();
  }

  if (module == NULL) {
    MessageBox(NULL, "Fatal: Failed to get module handle", NULL, MB_OK);
    exit(1);
  }
  if (GetModuleFileName(module, install_dir, MAX_PATH) == 0) {
    MessageBox(NULL, "Fatal: Failed to get module file name", NULL, MB_OK);
    exit(1);
  }
  i = strlen(install_dir) - 1;
  while (i >= 0 && install_dir[i] != '\\') {
    --i;
  }
  install_dir[i] = '\0';

  pref_dir[0] = '\0';
  SHGetFolderPath(NULL,	CSIDL_APPDATA|CSIDL_FLAG_CREATE, NULL, 0, pref_dir);
  sprintf(cmd_line, "\"%s\\bin\\werl.exe\" -smp enable -detached -run wings_start start_halt",
          install_dir);
  if (argc > 1) {
    sprintf(cmd_line+strlen(cmd_line), " \"%s\"", argv[1]);
  }
  sprintf(cmd_line+strlen(cmd_line),  " -extra \"%s\"", pref_dir);

  siStartInfo.cb = sizeof(STARTUPINFO); 
  siStartInfo.wShowWindow = SW_MINIMIZE;
  siStartInfo.dwFlags = STARTF_USESHOWWINDOW;

  ok = CreateProcess(NULL, 
                     cmd_line, 
                     NULL, 
                     NULL, 
                     FALSE,
                     0,
                     NULL,
                     NULL,
                     &siStartInfo,
                     &piProcInfo);
  if (!ok) {
    sprintf(message, "Failed to start Wings 3D: %u", GetLastError());
    MessageBox(NULL, message, NULL, MB_OK);
  }
  exit(0);
}

static void
install(void)
{
  FILE* fp = fopen("bin/erl.ini", "w");
  char dir[MAX_PATH];

  getcwd(dir, MAX_PATH);
  if (fp == NULL) {
    MessageBox(NULL, "Failed to install Erlang/OTP components", NULL, MB_OK);
    exit(1);
  }
  fprintf(fp, "[erlang]\n");
  fprintf(fp, "Bindir=");
  print_path(fp, dir);
  fprintf(fp, "\\\\bin\n");
  fprintf(fp, "Progname=erl\n");
  fprintf(fp, "Rootdir=");
  print_path(fp, dir);
  putc('\n', fp);
  fclose(fp);
  exit(0);
}

static void
print_path(FILE* fp, char* path)
{
  int c;

  while ((c = *path) != 0) {
    if (c != '\\') {
      putc(c, fp);
    } else {
      putc('\\', fp);
      putc('\\', fp);
    }
    path++;
  }
}
