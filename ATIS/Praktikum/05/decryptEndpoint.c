/*
main()
.text:08049F1B                          loc_08049F1B: // encryptedEndpointStr laden
.text:08049F1B 8B 0D FD A9 04 08        mov     ecx, ds:dword_804A9FD // encryptedEndPoint
.text:08049F21 89 8D 95 DF FF FF        mov     [ebp-206Bh], ecx // encryptedEndPoint

.text:0804A179                          loc_0804A179: // vor decryptEndPoint
.text:0804A179 8D 8D 95 DF FF FF        lea     ecx, [ebp-206Bh] // encryptedEndPoint
.text:0804A17F 8D 95 88 CB FF FF        lea     edx, [ebp-3478h]
.text:0804A185 31 F6                    xor     esi, esi
.text:0804A187 BF 00 04 00 00           mov     edi, 400h
.text:0804A18C 89 14 24                 mov     [esp], edx
.text:0804A18F C7 44 24 04 00 00 00 00  mov     dword ptr [esp+4], 0
.text:0804A197 C7 44 24 08 00 04 00 00  mov     dword ptr [esp+8], 400h
.text:0804A19F 89 85 68 CA FF FF        mov     [ebp-3598h], eax
.text:0804A1A5 89 8D 64 CA FF FF        mov     [ebp-359Ch], ecx // encryptedEndPoint
.text:0804A1AB 89 95 60 CA FF FF        mov     [ebp-35A0h], edx
.text:0804A1B1 89 B5 5C CA FF FF        mov     [ebp-35A4h], esi
.text:0804A1B7 89 BD 58 CA FF FF        mov     [ebp-35A8h], edi
.text:0804A1BD E8 3E E7 FF FF           call    _memset
.text:0804A1C2 8B 85 64 CA FF FF        mov     eax, [ebp-359Ch] // encryptedEndPoint
.text:0804A1C8 89 04 24                 mov     [esp], eax  // 1. Arg == Str
.text:0804A1CB E8 60 EA FF FF           call    sub_8048C30 // decryptEndPoint()


sub_8048C30() - decryptEndPoint
.text:08048C30                          loc_08048C30: // function Start
.text:08048C30 55                       push    ebp
.text:08048C31 89 E5                    mov     ebp, esp
.text:08048C33 53                       push    ebx
.text:08048C34 57                       push    edi
.text:08048C35 56                       push    esi
.text:08048C36 83 EC 6C                 sub     esp, 6Ch
.text:08048C39 8B 45 08                 mov     eax, [ebp+arg_0] // endPointStr
.text:08048C3C 89 45 EC                 mov     [ebp+var_14], eax // endPointStr
.text:08048C3F C7 45 E8 00 00 00 00     mov     [ebp+var_18], 0 // counter = 0
.text:08048C46 C7 45 E4 7B 33 74 FB     mov     [ebp+var_1C], 0FB74337Bh // state

.text:08048FA9                          loc_8048FA9: // counter++
.text:08048FA9 8B 45 E8                 mov     eax, [ebp+var_18] // counter
.text:08048FAC 2D 0A 1B B7 F6           sub     eax, 0F6B71B0Ah
.text:08048FB1 83 C0 01                 add     eax, 1
.text:08048FB4 05 0A 1B B7 F6           add     eax, 0F6B71B0Ah
.text:08048FB9 89 45 E8                 mov     [ebp+var_18], eax
.text:08048FBC C7 45 E4 A6 A9 A0 EC     mov     [ebp+var_1C], 0ECA0A9A6h // state

.text:08048E45                          loc_8048E45: // decrypt byte
.text:08048E45 B8 FF FF FF FF           mov     eax, 0FFFFFFFFh
.text:08048E4A 8B 4D EC                 mov     ecx, [ebp+var_14] // endPointStr
.text:08048E4D 8B 55 E8                 mov     edx, [ebp+var_18] // counter
.text:08048E50 0F BE 34 11              movsx   esi, byte ptr [ecx+edx]
.text:08048E54 89 F7                    mov     edi, esi
.text:08048E56 83 F7 FF                 xor     edi, 0FFFFFFFFh
.text:08048E59 81 E7 8E 00 00 00        and     edi, 8Eh
.text:08048E5F 35 8E 00 00 00           xor     eax, 8Eh
.text:08048E64 21 C6                    and     esi, eax
.text:08048E66 09 F7                    or      edi, esi
.text:08048E68 89 F8                    mov     eax, edi
.text:08048E6A 88 C3                    mov     bl, al
.text:08048E6C 88 1C 11                 mov     [ecx+edx], bl
.text:08048E6F C7 45 E4 16 6B B3 47     mov     [ebp+var_1C], 47B36B16h // state
.text:08048E76 E9 48 01 00 00           jmp     loc_8048FC3
*/

#include <stdio.h>
#include <stdint.h>

void decrypt(uint8_t* endPoint) {
  for (size_t pos = 0; endPoint[pos] != 0; pos++) {
  /*uint32_t byte = endPoint[pos];
    byte ^= 0xffffffff;
    byte &= 0x8e;
    byte |= endPoint[pos] & 0x71;
    endPoint[pos] = byte & 0xff;

    endPoint[pos] = (~endPoint[pos] & 0x8e) | (endPoint[pos] & 0x71); */

    endPoint[pos] ^= 0x8e;
  }
}

int main(void) {
    uint8_t endPoint[] = { 0xd4, 0xe0, 0xb6, 0xb7, 0xc9, 0xb8, 0xbd, 0xe4, 0x8e, 0x00 };
    decrypt(endPoint);
    printf("%s\n", endPoint);
  return 0;
}
