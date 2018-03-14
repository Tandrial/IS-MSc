from keystone import *

v1 = """  mov ecx, 100
  xor ebx, ebx
  xor eax, eax

  start_loop:
  test eax, 0x1
  jz even
  add ebx, eax
  dec ecx

  even:
  inc eax
  cmp ecx, 0x0
  jne start_loop
"""

v2 = """  mov ecx, 100
  xor ebx, ebx
  mov eax, 1

  start_loop:
  add ebx, eax
  add eax, 2

  loop start_loop
"""

v3 = """  mov ebx, 100
  imul ebx, ebx
"""

v4 = """  mov ebx, 10000
"""

for index, version in enumerate([v1, v2, v3, v4]):
  try:
    encoding = ''
    instrCnt = 0
    ks = Ks(KS_ARCH_X86, KS_MODE_32)
    for inst in version.splitlines():
      if len(inst) == 0:
        continue
      enc, count = ks.asm(inst)
      encoding += ''.join(r'\x{:02x}'.format(byte) for byte in enc)
      instrCnt += count
    print('version %u (%u instructions)\n%s= %s\n' % (index + 1, instrCnt, version, encoding))
  except KsError as e:
    print('version %u contains errors:\n%s\n%s' % (index + 1, e, inst))
    continue
