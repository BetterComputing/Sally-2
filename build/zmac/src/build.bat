@setlocal
@echo off
@rem Generate doc.inl and ..\zmac_doc.html
gcc -Wall -o doc.exe -DMK_DOC doc.c
doc > ..\zmac_doc.html
@rem convert grammar into C code
bison --output=zmac.c zmac.y
@rem compile the rest
gcc -c -o zmac.o zmac.c
gcc -c -o mio.o mio.c
gcc -c -o doc.o doc.c
g++ -c -o zi80dis.o zi80dis.cpp
g++ -Wall  -o ..\zmac.exe zmac.o mio.o doc.o zi80dis.o
@endlocal
REM To clean, use these commands:
REM del ../zmac.exe ../zmac_doc.html zmac.c doc.exe doc.inl zmac.o mio.o doc.o zi80dis.o

