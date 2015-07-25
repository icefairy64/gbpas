unit cpu;

{$mode fpc}{$H+}

interface

uses
  virtual16;

type
  PCPU = ^TCPU;
  TCPU = record
    Memory: Pointer;
    A: Byte;
    BC: Word;
    DE: Word;
    HL: Word;
    SP: Word;
    IP: Word;
    CycleCount: QWord;
    Status: Byte;
    Zero: Boolean;
    Carry: Boolean;
    HalfCarry: Boolean;
    Negative: Boolean;
    Interrupts: Boolean;
  end;

const
  CPU_STATUS_HALT = $01;
  CPU_STATUS_STOP = $02;

  CPU_HALFCARRY_MASK     = $10;
  CPU_HIGHHALFCARRY_MASK = $1000;

function CPUInit: PCPU;
procedure CPUTick(CPU: PCPU);

implementation

uses
  instructions;

function CPUInit: PCPU;
begin
  CPUInit := GetMem(SizeOf(TCPU));
end;

procedure CPUTick(CPU: PCPU);
var
  opcode: Byte;
begin
  opcode := GetByte(CPU^.Memory, CPU^.IP);
end;

end.

