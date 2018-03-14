; Lab 1 - Michael Krane
; Dazu soll ein Programm geschrieben werden, dass die ersten 100 ungeraden
; Ganzzahlen aus der Menge der natürlichen Zahlen aufsummiert.
; Das Ergebnis der Berechnung soll am Ende im Register ebx liegen.

;    | AMD X4 965 | Intel i7-5600U
; v1 |  3040 ms   |     1306 ms
; v2 |  1256 ms   |     1960 ms
; v3 |    30 ms   |       27 ms
; v4 |    29 ms   |       22 ms


extern _printf

global _WinMain@16
global _v1
global _v2
global _v3
global _v4

global v1
global v2
global v3
global v4

SECTION .text

; Version 1:
;   int cnt = 100;
;   int sum = 0;
;   int cur = 0;
;   while (cnt > 0) {
;     if ( cur & 0x1 != 0) {
;       sum += cur;
;       cnt--;
;     }
;     cur++;
;   }
_v1:
v1:
  push ebx

  mov ecx, 100
  xor ebx, ebx
  xor eax, eax

  start_loop_v1:
  test eax, 0x1
  jz even
  add ebx, eax
  dec ecx

  even:
  inc eax
  cmp ecx, 0x0
  jne start_loop_v1

  mov eax, ebx
  pop ebx
  ret


; Version 2:
; Version 1 mit start@1 und STEP_SIZE 2
;   int cnt = 100;
;   int sum = 0;
;   int cur = 1;
;   while (cnt-- > 0) {
;     sum += cur;
;     cur += 2;
;   }
_v2:
v2:
  push ebx

  mov ecx, 100
  xor ebx, ebx
  mov eax, 1

  start_loop_v2:
  add ebx, eax
  add eax, 2
  loop start_loop_v2

  mov eax, ebx
  pop ebx
  ret


; Version 3:
; Sum(2k - 1, 1, n) = n^2
; Annahme S(n) = n^2
; S(1)     = 1 = 1^2
; S(n + 1) = 1 + 3 + ... + (2n - 1) + (2(n + 1) - 1) = S(n) + 2n + 1
;          = n^2 + 2n + 1 = (n + 1)^2
_v3:
v3:
  push ebx

  mov ebx, 100
  imul ebx, ebx

  mov eax, ebx
  pop ebx
  ret


; Version 4:
; Version 3 mit festem n
_v4:
v4:
  push ebx

  mov ebx, 10000

  mov eax, ebx
  pop ebx
  ret


_WinMain@16:
  push ebp
  mov ebp, esp

  call v1
  push eax
  push fmtv1
  call _printf

  call v2
  push eax
  push fmtv2
  call _printf

  call v3
  push eax
  push fmtv3
  call _printf

  call v4
  push eax
  push fmtv4
  call _printf

  mov eax, 0
  leave
  ret

SECTION .data

fmtv1: db "v1=%d", 10, 0
fmtv2: db "v2=%d", 10, 0
fmtv3: db "v3=%d", 10, 0
fmtv4: db "v4=%d", 10, 0
