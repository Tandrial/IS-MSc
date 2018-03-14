#!/usr/bin/python3
# Lab 4 - Michael Krane
import sys
import itertools
from capstone import Cs, CS_ARCH_X86, CS_MODE_32

class Logger(object):
  def __init__(self, fileName):
    self.terminal = sys.stdout
    self.log = open('%s.log' % fileName, 'w+')

  def write(self, message):
    self.terminal.write(message)
    self.log.write(message)

  def flush(self):
    self.terminal.flush()
    self.log.flush()


class Block(object):
  def __init__(self, num, address, instructions):
    self.num = num
    self.address = address
    self.instructions = instructions
    self.links = []
    self.parent = self
    self.rank = 0

  @staticmethod
  def instToStr(inst):
    return f'{inst.mnemonic} {inst.op_str}'.rstrip()

  def __str__(self):
    bStr = f'bb{self.num} at 0x{self.address:x}\n'
    bStr += '\n'.join([f'    {Block.instToStr(inst)}' for inst in self.instructions])
    return bStr

  def addLink(self, endBlock, dotAnnotation=';'):
    self.links.append(Link(self.num, endBlock, dotAnnotation))

  def toDot(self, prefix):
    bStr = f'{prefix}bb{self.num}[shape=box, label="bb{self.num} at 0x{self.address:x}\\n\\n'
    bStr += ''.join([f'{Block.instToStr(inst)}\\l' for inst in self.instructions])
    bStr += '"];\n'
    bStr += '\n'.join([link.toDot(prefix) for link in self.links])
    return bStr


class Link(object):
  def __init__(self, start, end, dotAnnotation):
    self.start = start
    self.end = end
    self.dotAnnotation = dotAnnotation

  def __str__(self):
    return f'bb{self.start} -> bb{self.end}'

  def toDot(self, prefix):
    gate = ''
    if self.start + 1 < self.end:
      gate = ':e'
    elif self.start > self.end:
      gate = ':w'
    return f'{prefix}bb{self.start}{gate} -> bb{self.end}{gate}{self.dotAnnotation}'


def createDisassembly(fileContent, offset):
  capStone = Cs(CS_ARCH_X86, CS_MODE_32)
  return list(capStone.disasm(fileContent, offset))

def printDisassembly(disasm):
  for inst in disasm:
    asmBytes = ''.join(f'{byte:02x}' for byte in inst.bytes)
    instStr = f'  {inst.address:#010x}: {asmBytes:15s}\t'
    instStr += f'{inst.mnemonic:4s}\t{inst.op_str}'.rstrip()
    print(instStr)

def findLeaders(disasm, entryPoint):
  leaders = []
  targets = genTargets(disasm)
  for (idx, inst) in enumerate(disasm):
    if inst.address == entryPoint:
      leaders.append(idx)
    elif prevIsJump(disasm, idx):
      leaders.append(idx)
    elif hex(inst.address) in targets:
      leaders.append(idx)
  return leaders

def genTargets(disasm):
  targets = []
  for (idx, inst) in enumerate(disasm):
    if any(inst.mnemonic.startswith(jmpType) for jmpType in ['j', 'loop', 'call']):
      if inst.mnemonic.startswith('call') and inst.op_str == hex(disasm[idx + 1].address):
        continue
      targets.append(inst.op_str)
  return targets

def prevIsJump(disasm, idx):
  for jmpType in ['j', 'loop', 'ret']:
    if disasm[idx - 1].mnemonic.startswith(jmpType):
      return True
  return False

def createBlocks(disasm, leaders):
  blocks = []
  indicies = leaders + [len(disasm)]
  for (start, end) in zip(indicies, indicies[1:]):
    instList = disasm[start:end]
    address = instList[0].address
    blocks.append(Block(len(blocks) + 1, address, instList))
  return blocks

def createLinks(blocks):
  for block in blocks:
    lastInst = block.instructions[-1]
    if lastInst.mnemonic.startswith('j') or lastInst.mnemonic.startswith('loop'):
      try:
        adr = int(lastInst.op_str, 16)
      except ValueError as _:
        continue
      goal = [goalBlock for goalBlock in blocks if goalBlock.address == adr][0]
      if lastInst.mnemonic == 'jmp':
        block.addLink(goal.num)
      else:
        block.addLink(goal.num, '[color=green];')    # jmp taken
        block.addLink(block.num + 1, '[color=red];') # jmp not take
    elif lastInst.mnemonic != 'ret':
      block.addLink(block.num + 1)
  return blocks

def printCFG(blocks):
  for block in blocks:
    print(block)
    print()
  for block in blocks:
    for link in block.links:
      print(link)

def generateDotFile(blocks, fileName):
  with open(fileName + '.dot', mode='w+') as file:
    print(f'digraph CFG {{\n  label="CFG - {fileName}";\n', file=file)
    print('  labelloc=top;\n  splines=splines;\n', file=file)
    linkGroups = genLinkGroups(blocks)
    for linkGroup in linkGroups:
      if len(linkGroup) == 1:
        continue
      linkString = ' -> '.join([f'bb{link}' for link in linkGroup])
      print(f'  {linkString}[style=invis];\n', file=file)
    for block in blocks:
      print(block.toDot('  '), file=file)
    print('}', file=file)

def genLinkGroups(blocks):
  for block in blocks:
    for link in block.links:
      union(blocks[link.start - 1], blocks[link.end - 1])
  result = []
  for _, group in itertools.groupby(blocks, find):
    result.append(list(map(lambda block: block.num, group)))
  return result

def union(blockX, blockY):
  xRoot = find(blockX)
  yRoot = find(blockY)
  if xRoot.rank > yRoot.rank:
    yRoot.parent = xRoot
  elif xRoot.rank < yRoot.rank:
    xRoot.parent = yRoot
  elif xRoot != yRoot:
    yRoot.parent = xRoot
    xRoot.rank = xRoot.rank + 1

def find(block):
  if block.parent == block:
    return block
  block.parent = find(block.parent)
  return block.parent

def main():
  if len(sys.argv) == 1:
    print(f"""Usage: {sys.argv[0]} file [offset] [entryPoint]
    file      : A file containing assembled X86 IA32 instructions
    offset    : optional Baseaddress as hex
    entryPoint: optional Entrypoint  as hex""")
    exit()

  fileName = sys.argv[1]
  sys.stdout = Logger(fileName)

  offset = 0
  if len(sys.argv) > 2:
    offset = int(sys.argv[2], 16)

  entryPoint = offset
  if len(sys.argv) > 3:
    entryPoint = int(sys.argv[3], 16)

  with open(fileName, mode='rb') as file:
    fileContent = file.read()

  print('[+] creating Disassembly')
  print(f' Offset     : {hex(offset)}\n Entrypoint : {hex(entryPoint)}\n')
  disasm = createDisassembly(fileContent, offset)
  printDisassembly(disasm)
  print('done.\n')

  print('[+] creating leaders')
  leaders = findLeaders(disasm, entryPoint)
  print('done.\n')

  print('[+] creating blocks')
  blocks = createBlocks(disasm, leaders)
  for block in blocks:
    print(f'  bb{block.num} at 0x{block.address:x}')
  print('done.\n')

  print('[+] creating links')
  blocks = createLinks(blocks)
  print('done.\n')

  print('[+] dumping CFG')
  printCFG(blocks)
  print('done.\n')

  print(f'[+] Writing CFG to {fileName}.dot file')
  generateDotFile(blocks, fileName)
  print('done. https://dreampuf.github.io/GraphvizOnline/ to render!')

if __name__ == '__main__':
  main()
