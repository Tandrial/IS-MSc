#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

BYTE memory[] = { 0x53, 0x09, 0x5A, 0x7C, 0x63, 0x08, 0x26,
                  0x43, 0x1A, 0x33, 0x23, 0x1D, 0x3C, 0x39 };

LPVOID __cdecl MyAlloc (SIZE_T dwBytes) {
  return calloc(dwBytes + 1, 1);
}

BYTE* __cdecl func(BYTE* arg0, signed int arg1) {
  BYTE* result = (BYTE*) MyAlloc(arg1 + 1);
  if (arg1 > 0) {
    for (signed short i = 0; i < arg1; i++) {
      result[i] = memory[i % 0xe] ^ arg0[i];
    }
  }
  return result;
}

int main(void) {
  BYTE arg0[] = { 0x02, 0x38, 0x30, 0x35, 0x0A, 0x6A, 0x63,
                  0x02, 0x74, 0x7C, 0x53, 0x51, 0x6C, 0x7F,
                  0x1F, 0x3E, 0x3F, 0x45, 0x04, 0x44, 0x4B };
  signed int arg1 = 21;

  BYTE* result = func(arg0, arg1);
  for(int i = 0; i < arg1; i++) {
    printf("%02x",result[i]);
  }
  printf(" => %s", result);
  free(result);
  return 0;
}
