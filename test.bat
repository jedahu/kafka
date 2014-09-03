@echo off
mkdir bootstrap\Steel.Finabs\lib
copy src\build\Steel.Finabs.dll bootstrap\Steel.Finabs\lib\Steel.Finabs.dll
fsi test.fsx %*
