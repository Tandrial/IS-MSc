#!/usr/bin/python
# Lab 2
from __future__ import print_function
from capstone import *
import sys

fileName = ''
offset = 0x0

if len(sys.argv) == 1:
  print("""Usage: %s file [offset]
  file: A file containing assembled X86 IA32 instructions
  offset: optional Baseaddress, either as hex or dec""" % sys.argv[0])
  exit()

fileName = sys.argv[1]

if len(sys.argv) == 3:
  num = sys.argv[2]
  if num.startswith('0x'):
    offset = int(num, 16)
  else:
    offset = int(num, 10)

with open(fileName, mode='rb') as file:
  fileContent = file.read()

with open(fileName + '.txt', mode='w+') as file:
  md = Cs(CS_ARCH_X86, CS_MODE_32)
  for inst in md.disasm(fileContent, offset):
    asmBytes = ''.join('{:02x}'.format(byte) for byte in inst.bytes)
    instStr = '  {:#010x}: {:15s}\t\t'.format(inst.address, asmBytes)
    instStr += '{:4s}\t{:s}'.format(inst.mnemonic, inst.op_str).rstrip()
    print(instStr, file=file)
    print(instStr)
