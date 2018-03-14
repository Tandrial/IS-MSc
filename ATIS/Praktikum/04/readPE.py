#!/usr/bin/python3
# Lab 4 - Michael Krane
import struct
import datetime

class DisasmFile(object):
  def __init__(self, fileName):
    self.file = open(fileName, mode='rb')
    self.imageBase = 0
    self.adrEntryPoint = 0
    self.sectionAlignment = 0
    self.textSection = 0
    self.textSize = 0

  @staticmethod
  def getFileType(fileName):
    file = open(fileName, mode='rb')
    magic = struct.unpack('@I', file.read(4))[0]
    if magic & 0xFFFF == int('0x5a4d', 16):
      return 'pe-file'
    elif magic & 0xFFFF == int('0x14c', 16):
      return 'coff-file'
    elif magic & 0xFFFFFFFF == int('0x464c457f', 16):
      return 'elf-file'
    else:
      return 'raw'

  def readElfFile(self):
    if self.getInt(0) != int('0x464c457f', 16):
      raise AssertionError('File is not an ELF-File!')

    if self.getWord(0x12) != int('0x3', 16):
      raise AssertionError('File is not x86!')

    self.adrEntryPoint = self.getInt(0x18)

    e_phentsize = self.getWord(0x2a)
    e_phnum = self.getWord(0x2c)
    e_phoff = self.getInt(0x1c) + 2 * e_phentsize

    e_shoff = self.getInt(0x20)
    e_shentsize = self.getWord(0x2e)

    textstart = e_shoff
    print(hex(e_shoff))
    print(hex(810036))

    print("text adr = ", hex(self.getInt(textstart + 0xc)))
    print("text offset = ", hex(self.getInt(textstart + 0x10)))
    print("text size = ", hex(self.getInt(textstart + 0x14)))



    #.text starts at 0x1a150
    #headers end at  0x134
    # diff = 0x1A01C
    print("header end = ", hex(self.getInt(0x1c)+ e_phnum * e_phentsize))
    self.textSection = self.getInt(e_phoff + 0x4) + e_phnum * e_phentsize
    print(hex(self.textSection))
    self.imageBase = self.getInt(e_phoff + 0x8)
    print(hex(self.imageBase + 0x1a150))
    self.textSize = self.getInt(e_phoff + 0x10)

    self.adrEntryPoint -= self.imageBase

  def readCoffFile(self):
    #http://delorie.com/djgpp/doc/coff/
    if hex(self.getWord(0)) != '0x14c':
      raise AssertionError('File is not a COFF-File!')

    f_nscns = self.getWord(0x2)
    f_timdat = self.getLong(0x4)
    f_symptr = self.getLong(0xc)
    f_nsyms = self.getLong(0x14)
    f_opthdr = self.getWord(0x1c)
    f_flags = self.getWord(0x1e)
    #t = datetime.datetime.fromtimestamp(f_timdat).isoformat()
    print("f_nscns = ",f_nscns)
    print(f_timdat)
                                                         1543408324410
    print("f_timdat = ", datetime.datetime.fromtimestamp(1515138746).isoformat())
    print("f_symptr = ",f_symptr)
    print("f_nsyms = ",f_nsyms)
    print("f_opthdr = ",f_opthdr)
    print("f_flags = ",f_flags)


  def readPeFile(self):
    if hex(self.getWord(0)) != '0x5a4d':
      raise AssertionError('File is not a PE-File!')

    e_lfanew = self.getInt(0x3c)

    if hex(self.getInt(e_lfanew)) != '0x4550':
      raise AssertionError('Broken signature!')

    optHeaderStart = e_lfanew + 0x18
    sizeOptHeader = self.getWord(e_lfanew + 0x14)

    if hex(self.getWord(optHeaderStart)) != '0x10b':
      raise AssertionErrornt('Optional Header signature wrong!')

    self.adrEntryPoint = self.getInt(optHeaderStart + 0x10)
    self.imageBase = self.getInt(optHeaderStart + 0x1c)
    self.sectionAlignment = self.getInt(optHeaderStart + 0x20)
    self.textSection = self.getWord(optHeaderStart + 0x3c)
    self.textSize = self.getInt(optHeaderStart + sizeOptHeader + 0x8)

  def getWord(self, offset):
    self.file.seek(offset)
    return struct.unpack('@H', self.file.read(2))[0]

  def getInt(self, offset):
    self.file.seek(offset)
    return struct.unpack('@I', self.file.read(4))[0]

  def getLong(self, offset):
    self.file.seek(offset)
    return struct.unpack('@Q', self.file.read(8))[0]

  def getDisasm(self):
    disasmStart = self.textSection
    disasmEnd = disasmStart + self.textSize
    self.file.seek(disasmStart)
    disasm = self.file.read(self.textSize)
    return disasm

  def getOffset(self):
    return self.imageBase + self.sectionAlignment

  def getEntryPoint(self):
    return self.imageBase + self.adrEntryPoint


def main():
  #fileName = 'lab1.exe'
  fileName = 'lab1.obj'
  print(DisasmFile.getFileType(fileName))

  file = DisasmFile(fileName)

  fileType = DisasmFile.getFileType(fileName)
  if fileType == "pe-file":
    file.readPeFile()
  elif fileType == 'coff-file':
    file.readCoffFile();
  else:
    file.readElfFile()
  #print(file.getDisasm())
  print("disasm Start = ", hex(file.textSection))
  print("disasm size = ", hex(file.textSize))
  print("offset = ", hex(file.getOffset()))
  print("entryPoint = ", hex(file.getEntryPoint()))

if __name__ == '__main__':
  main()