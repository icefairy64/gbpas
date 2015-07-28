unit cpu;

{$mode fpc}{$H+}

interface

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
    RAMBankEnabled: Boolean;
    RAMBank: Word;
    ROMBank: Word;
  end;

  TInterruptType = ( VBlank := 0, LCDCStatus, TimerOverflow,
                     SerialTransfer, HiToLow );

const
  CPU_STATUS_HALT = $01;
  CPU_STATUS_STOP = $02;

  CPU_HALFCARRY_MASK     = $10;
  CPU_HIGHHALFCARRY_MASK = $1000;

  CPU_INTERRUPTBASE = $0040;
  CPU_INTERRUPTFLAG = $FF0F;
  CPU_INTERRUPTEN   = $FFFF;
  CPU_INTERRUPTSIZE = $08;

function CPU_Init: PCPU;
procedure CPU_Tick(CPU: PCPU);
procedure CPU_CheckInterrupts(CPU: PCPU);
procedure CPU_RequestInterrupt(CPU: PCPU; IntType: TInterruptType);

implementation

uses
  instructions, virtual16;

function CPU_Init: PCPU;
begin
  CPU_Init := GetMem(SizeOf(TCPU));
end;

procedure CPU_Tick(CPU: PCPU);
var
  opcode: Byte;
  ip: Word;
begin
  opcode := GetByte(CPU, CPU^.IP);
  ip := CPU^.IP;
  InstrSet[opcode].Run(CPU);
  if CPU^.IP = ip then
    CPU^.IP += InstrSet[opcode].ArgLen + 1;
  CPU^.CycleCount += InstrSet[opcode].ClockCount;
end;

procedure CPU_CheckInterrupts(CPU: PCPU);
var
  intFlag: Byte;
  intType: TInterruptType;
begin
  intFlag := GetByte(CPU, CPU_INTERRUPTFLAG) and GetByte(CPU, CPU_INTERRUPTEN);
  if not CPU^.Interrupts then
    Exit;
  for intType in TInterruptType do begin
    if intFlag and (1 shl Ord(intType)) > 0 then begin
      SetByte(CPU, CPU_INTERRUPTFLAG,
              GetByte(CPU, CPU_INTERRUPTFLAG) and not (1 shl Ord(intType)));
      Push(CPU, CPU^.IP);
      CPU^.IP := CPU_INTERRUPTBASE + Ord(intType) * CPU_INTERRUPTSIZE;
      CPU^.Interrupts := False;
      CPU^.CycleCount += 20;
      Break;
    end;
  end;
end;

procedure CPU_RequestInterrupt(CPU: PCPU; IntType: TInterruptType);
begin
  SetByte(CPU, CPU_INTERRUPTFLAG,
          GetByte(CPU, CPU_INTERRUPTFLAG) or (1 shl Ord(IntType)))
end;

end.

