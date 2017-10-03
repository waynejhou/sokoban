@echo off
REM make
REM Assembles and links the 32-bit ASM program into .exe which can be used by WinDBG
REM Uses MicroSoft Macro Assembler version 6.11 and 32-bit Incremental Linker version 5.10.7303
REM Created by Huang 

REM delete related files
del P_Assembly.lst
del P_Assembly.obj
del P_Assembly.ilk
del P_Assembly.pdb
del P_Assembly.exe
del P_Assembly.RES


REM /c          assemble without linking
REM /coff       generate object code to be linked into flat memory model 
REM /Zi         generate symbolic debugging information for WinDBG
REM /Fl		Generate a listing file
 
REM P_Assembly.asm      The name of the source file 


ML /c /coff /Zi /Fl P_Assembly.asm
if errorlevel 1 goto terminate
rc P_Assembly.rc
REM /debug              generate symbolic debugging information
REM /subsystem:console  generate console application code
REM /entry:start        entry point from WinDBG to the program 

REM                           the entry point of the program must be _start

REM /out:P_Assembly.exe         output P_Assembly.exe code
REM P_Assembly.obj              input P_Assembly.obj
REM Kernel32.lib        library procedures to be invoked from the program
REM irvine32.lib
REM user32.lib

LINK /debug /subsystem:console  /entry:main  /out:P_Assembly.exe P_Assembly.obj P_Assembly.RES Kernel32.lib irvine32.lib user32.lib 
if errorlevel 1 goto terminate

REM Display all files related to this program:
DIR P_Assembly.*

:terminate
pause
