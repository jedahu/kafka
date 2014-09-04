@echo off
mkdir bootstrap
copy src\build bootstrap\
fsi test.fsx %*
