.PHONY: clean
NASM := C:\Tools\nasm\nasm
GCC := 'C:\mingw-w64\mingw32\bin\i686-w64-mingw32-gcc'
SYS := $(shell uname)

all: lab1 timing
	./lab1
	./timing

lab1:
ifeq ($(SYS), MINGW64_NT-6.1)
	$(NASM) -f win32 lab1.asm -o lab1.o
	$(GCC) -o lab1 lab1.o
else
	sed -e 's/_printf/printf/' -e 's/_WinMain@16/main/' lab1.asm >lab1L.asm
	nasm -f elf32 lab1L.asm -o lab1.o
	gcc -m32 -o lab1 lab1.o
	rm lab1L.asm
endif

timing:
ifeq ($(SYS), MINGW64_NT-6.1)
	$(NASM) -f win32 lab1.asm -o lab1.o
	$(GCC) -m32 -o timing timing.c lab1.o
else
	sed -e 's/_printf/printf/' lab1.asm >lab1L.asm
	nasm -f elf32 lab1L.asm -o lab1.o
	gcc -m32 -o timing timing.c lab1.o
	rm lab1L.asm
endif

clean:
ifeq ($(SYS), MINGW64_NT-6.1)
	rm *.o lab1 lab1.exe timing timing.exe
else
	rm *.o lab1 lab1.exe timing timing.exe
endif
