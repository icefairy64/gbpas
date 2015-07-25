unit instructions;

{$mode fpc}{$H+}
{$inline on}

interface

uses
  cpu, virtual16;

type
  PInstructionImpl = procedure(CPU: PCPU);
  PInstruction = ^TInstruction;
  TInstruction = record
    Run: PInstructionImpl;
    ArgLen: Byte;
    ClockCount: Byte;
  end;

var
  InstrSet: array [$00 .. $FF] of TInstruction;

procedure InstrSetInit;

implementation

// Helpers

function GetLow(Value: Word): Byte; inline;
begin
  GetLow := Value and $00FF;
end;

function GetHigh(Value: Word): Byte; inline;
begin
  GetHigh := Value and $FF00;
end;

procedure SetLow(Target: PWord; Value: Byte); inline;
begin
  Target^ := GetHigh(Target^) or Value;
end;

procedure SetHigh(Target: PWord; Value: Byte); inline;
begin
  Target^ := (Value shl 8) or GetLow(Target^);
end;

procedure CheckZero(CPU: PCPU; Data: Word); inline;
begin
  CPU^.Zero := Data = 0;
end;

procedure CheckHalfCarryAdd8(CPU: PCPU; A, B: Byte); inline;
begin
  CPU^.HalfCarry := (A and $0F + B and $0F) and CPU_HALFCARRY_MASK > 0;
end;

procedure CheckHalfCarrySub8(CPU: PCPU; A, B: Byte); inline;
begin
  CPU^.HalfCarry := (A and $0F - B and $0F) and CPU_HALFCARRY_MASK > 0;
end;

procedure CheckHalfCarryAdd16(CPU: PCPU; A, B: Word); inline;
begin
  CPU^.HalfCarry := (A and $0FFF + B and $0FFF) and CPU_HIGHHALFCARRY_MASK > 0;
end;

procedure CheckHalfCarrySub16(CPU: PCPU; A, B: Word); inline;
begin
  CPU^.HalfCarry := (A and $0FFF - B and $0FFF) and CPU_HIGHHALFCARRY_MASK > 0;
end;

// Affects: H=0 Z=0 N=0 C
procedure RLC(Target: PByte; CPU: PCPU); inline;
begin
  CPU^.Carry := Target^ > $7F;
  Target^ := Target^ shl 1;
  if CPU^.Carry then
    Target^ += 1;
  with CPU^ do begin
    HalfCarry := False;
    Zero := False;
    Negative := False;
  end;
end;

// Affects: H=0 Z=0 N=0 C
procedure RL(Target: PByte; CPU: PCPU); inline;
var
  prevCarry: Boolean;
begin
  prevCarry := CPU^.Carry;
  CPU^.Carry := Target^ > $7F;
  Target^ := Target^ shl 1;
  if prevCarry then
    Target^ += 1;
  with CPU^ do begin
    HalfCarry := False;
    Zero := False;
    Negative := False;
  end;
end;

// Affects: H=0 Z=0 N=0 C
procedure RRC(Target: PByte; CPU: PCPU); inline;
begin
  CPU^.Carry := (Target^ and 1) > 0;
  Target^ := Target^ shr 1;
  if CPU^.Carry then
    Target^ += $80;
  with CPU^ do begin
    HalfCarry := False;
    Zero := False;
    Negative := False;
  end;
end;

// Affects: H=0 Z=0 N=0 C
procedure RR(Target: PByte; CPU: PCPU); inline;
var
  prevCarry: Boolean;
begin
  prevCarry := CPU^.Carry;
  CPU^.Carry := (Target^ and 1) > 0;
  Target^ := Target^ shr 1;
  if prevCarry then
    Target^ += $80;
  with CPU^ do begin
    HalfCarry := False;
    Zero := False;
    Negative := False;
  end;
end;

// Affects: H=0 Z N=0 C
procedure SLA(Target: PByte; CPU: PCPU); inline;
begin
  CPU^.Carry := Target^ > $7F;
  Target^ := Target^ shl 1;
  Target^ += (Target^ and $02) shr 1;
  CPU^.Zero := Target^ = 0;
  CPU^.HalfCarry := False;
  CPU^.Negative := False;
end;

// Affects: H=0 Z N=0 C
procedure SRA(Target: PByte; CPU: PCPU); inline;
begin
  CPU^.Carry := Target^ and $01 > 0;
  Target^ := Target^ shr 1;
  Target^ += (Target^ and $40) shl 1;
  CPU^.Zero := Target^ = 0;
  CPU^.HalfCarry := False;
  CPU^.Negative := False;
end;

// Affects: H=0 Z N=0 C
procedure SRL(Target: PByte; CPU: PCPU); inline;
begin
  CPU^.Carry := Target^ and $01 > 0;
  Target^ := Target^ shr 1;
  if CPU^.Carry then
    Target^ += $80;
  CPU^.Zero := Target^ = 0;
  CPU^.HalfCarry := False;
  CPU^.Negative := False;
end;

// Affects: Z H=1 N=0
procedure Bit(Src: Byte; Pos: Byte; CPU: PCPU); inline;
begin
  CPU^.Zero := Src and (1 shl Pos) = 0;
end;

procedure ResBit(Target: PByte; Pos: Byte); inline;
begin
  Target^ := Target^ and not (1 shl Pos);
end;

procedure SetBit(Target: PByte; Pos: Byte); inline;
begin
  Target^ := Target^ or (1 shl Pos);
end;

function GetAF(CPU: PCPU): Word; inline;
begin
  GetAF := CPU^.A shl 8;
  if CPU^.Zero then
    GetAF += $80;
  if CPU^.Negative then
    GetAF += $40;
  if CPU^.HalfCarry then
    GetAF += $20;
  if CPU^.Carry then
    GetAF += $10;
end;

procedure SetAF(CPU: PCPU; Data: Word); inline;
begin
  CPU^.A := Data and $FF00 shr 8;
  CPU^.Zero := Data and $80 > 0;
  CPU^.Negative := Data and $40 > 0;
  CPU^.HalfCarry := Data and $20 > 0;
  CPU^.Carry := Data and $10 > 0;
end;

// Affects: H=0 Z N=0 C=0
procedure Swap(Target: PByte; CPU: PCPU); inline;
begin
  Target^ := (Target^ and $0F) shl 4 + Target^ shr 4;
  CPU^.Zero := Target^ = 0;
  CPU^.HalfCarry := False;
  CPU^.Negative := False;
  CPU^.Carry := False;
end;

// Affects: H N=0
procedure Add8(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  CheckHalfCarryAdd8(CPU, Target^, Data);
  Target^ += Data;
  CPU^.Negative := False;
end;

// Affects: H N=0 C Z
procedure Add8CZ(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  CheckHalfCarryAdd8(CPU, Target^, Data);
  Target^ += Data;
  CPU^.Negative := False;
  CPU^.Carry := Target^ < Data;
  CPU^.Zero := Target^ = 0;
end;

// Affects: H N=0 C Z
procedure AddC8CZ(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  CheckHalfCarryAdd8(CPU, Target^, Data);
  Target^ += Data;
  if CPU^.Carry then
    Target^ += 1;
  CPU^.Negative := False;
  CPU^.Carry := Target^ < Data;
  CPU^.Zero := Target^ = 0;
end;

// Affects: H N=0
procedure Add16(Target: PWord; Data: Word; CPU: PCPU); inline;
begin
  CheckHalfCarryAdd16(CPU, Target^, Data);
  Target^ += Data;
  CPU^.Negative := False;
end;

// Affects: H N=1
procedure Sub8(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  CheckHalfCarryAdd8(CPU, Target^, Data);
  Target^ -= Data;
  CPU^.Negative := True;
end;

// Affects: H N=1 C Z
procedure Sub8CZ(Target: PByte; Data: Byte; CPU: PCPU); inline;
var
  prevTarget: Byte;
begin
  CheckHalfCarryAdd8(CPU, Target^, Data);
  prevTarget := Target^;
  Target^ -= Data;
  CPU^.Negative := True;
  CPU^.Carry := Target^ > prevTarget;
  CPU^.Zero := Target^ = 0;
end;

// Affects: H N=1 C Z
procedure SubC8CZ(Target: PByte; Data: Byte; CPU: PCPU); inline;
var
  prevTarget: Byte;
begin
  CheckHalfCarrySub8(CPU, Target^, Data);
  prevTarget := Target^;
  Target^ -= Data;
  if CPU^.Carry then
    Target^ -= 1;
  CPU^.Negative := True;
  CPU^.Carry := Target^ > prevTarget;
  CPU^.Zero := Target^ = 0;
end;

// Affects: H N=1
procedure Sub16(Target: PWord; Data: Word; CPU: PCPU); inline;
begin
  CheckHalfCarrySub16(CPU, Target^, Data);
  Target^ -= Data;
  CPU^.Negative := True;
end;

// Affects: H N=1 C Z
procedure Cmp8(Target: Byte; Data: Byte; CPU: PCPU); inline;
var
  prevTarget: Byte;
begin
  CheckHalfCarrySub8(CPU, Target, Data);
  prevTarget := Target;
  Target -= Data;
  CPU^.Negative := True;
  CPU^.Carry := Target > prevTarget;
  CPU^.Zero := Target = 0;
end;

// Affects: N=0 Z H
procedure IncX4(CPU: PCPU; Target: PByte); inline;
begin
  CheckHalfCarryAdd8(CPU, Target^, 1);
  Target^ += 1;
  CheckZero(CPU, Target^);
  CPU^.Negative := False;
end;

// Affects: N=1 Z H
procedure DecX5(CPU: PCPU; Target: PByte); inline;
begin
  CheckHalfCarrySub8(CPU, Target^, 1);
  Target^ -= 1;
  CheckZero(CPU, Target^);
  CPU^.Negative := True;
end;

procedure And8(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  Target^ := Target^ and Data;
  CPU^.HalfCarry := True;
  CPU^.Carry := False;
  CPU^.Negative := False;
  CPU^.Zero := Target^ = 0;
end;

procedure Or8(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  Target^ := Target^ or Data;
  CPU^.HalfCarry := False;
  CPU^.Carry := False;
  CPU^.Negative := False;
  CPU^.Zero := Target^ = 0;
end;

procedure Xor8(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  Target^ := Target^ xor Data;
  CPU^.HalfCarry := False;
  CPU^.Carry := False;
  CPU^.Negative := False;
  CPU^.Zero := Target^ = 0;
end;

procedure Push(CPU: PCPU; Data: Word); inline;
begin
  CPU^.SP -= 2;
  SetWord(CPU^.Memory, CPU^.SP, Data);
end;

function Pop(CPU: PCPU): Word; inline;
begin
  Pop := GetWord(CPU^.Memory, CPU^.SP);
  CPU^.SP += 2;
end;

function GetByteArg(CPU: PCPU; Offset: Byte): Byte; inline;
begin
  GetByteArg := GetByte(CPU^.Memory, CPU^.IP + Offset);
end;

function GetWordArg(CPU: PCPU; Offset: Byte): Byte; inline;
begin
  GetWordArg := GetWord(CPU^.Memory, CPU^.IP + Offset);
end;

// Instructions

// nop
procedure Instr00_NOP(CPU: PCPU);
begin

end;

// ld bc, d16
procedure Instr01_LDBCD16(CPU: PCPU);
begin
  CPU^.BC := GetWordArg(CPU^.Memory, 1);
end;

// ld (bc), a
procedure Instr02_LDRBCA(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.BC, CPU^.A);
end;

// inc bc
procedure Instr03_INCBC(CPU: PCPU);
begin
  CPU^.BC += 1;
end;

// inc b
procedure Instr04_INCB(CPU: PCPU);
begin
  IncX4(CPU, PByte(@CPU^.BC) + 1);
end;

// dec b
procedure Instr05_DECB(CPU: PCPU);
begin
  DecX5(CPU, PByte(@CPU^.BC) + 1);
end;

// ld b, d8
procedure Instr06_LDBD8(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, GetByteArg(CPU, 1));
end;

// rlca
procedure Instr07_RLCA(CPU: PCPU);
begin
  RLC(@CPU^.A, CPU);
end;

// ld (a16), sp
procedure Instr08_LDSP(CPU: PCPU);
var
  addr: Word;
begin
  addr := GetWordArg(CPU, 1);
  SetWord(CPU, addr, CPU^.SP);
end;

// add hl, bc
procedure Instr09_ADDHLBC(CPU: PCPU);
begin
  Add16(@CPU^.HL, CPU^.BC, CPU);
end;

// ld a, (bc)
procedure Instr0A_LDARBC(CPU: PCPU);
begin
  CPU^.A := GetByte(CPU^.Memory, CPU^.BC);
end;

// dec bc
procedure Instr0B_DECBC(CPU: PCPU);
begin
  CPU^.BC -= 1;
end;

// inc c
procedure Instr0C_INCC(CPU: PCPU);
begin
  IncX4(CPU, PByte(@CPU^.BC));
end;

// dec c
procedure Instr0D_DECC(CPU: PCPU);
begin
  DecX5(CPU, PByte(@CPU^.BC));
end;

// ld c, d8
procedure Instr0E_LDCD8(CPU: PCPU);
begin
  SetLow(@CPU^.BC, GetByteArg(CPU, 1));
end;

// rrca
procedure Instr0F_RRCA(CPU: PCPU);
begin
  RRC(@CPU^.A, CPU);
end;

{ 10 }

// stop 0
procedure Instr10_STOP0(CPU: PCPU);
begin
  CPU^.Status := CPU_STATUS_STOP;
end;

// ld de, d16
procedure Instr11_LDDED16(CPU: PCPU);
begin
  CPU^.DE := GetWordArg(CPU^.Memory, 1);
end;

// ld (de), a
procedure Instr12_LDRDEA(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.DE, CPU^.A);
end;

// inc d
procedure Instr13_INCDE(CPU: PCPU);
begin
  CPU^.DE += 1;
end;

// inc d
procedure Instr14_INCD(CPU: PCPU);
begin
  IncX4(CPU, PByte(@CPU^.DE) + 1);
end;

// dec d
procedure Instr15_DECD(CPU: PCPU);
begin
  DecX5(CPU, PByte(@CPU^.DE) + 1);
end;

// ld d, d8
procedure Instr16_LDDD8(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, GetByteArg(CPU, 1));
end;

// rla
procedure Instr17_RLA(CPU: PCPU);
begin
  RL(@CPU^.A, CPU);
end;

// jr r8
procedure Instr18_JRR8(CPU: PCPU);
begin
  CPU^.IP += GetByteArg(CPU, 1);
end;

// add hl, de
procedure Instr19_ADDHLDE(CPU: PCPU);
begin
  Add16(@CPU^.HL, CPU^.DE, CPU);
end;

// ld a, (de)
procedure Instr1A_LDARDE(CPU: PCPU);
begin
  CPU^.A := GetByte(CPU^.Memory, CPU^.DE);
end;

// dec de
procedure Instr1B_DECDE(CPU: PCPU);
begin
  CPU^.DE -= 1;
end;

// inc e
procedure Instr1C_INCE(CPU: PCPU);
begin
  IncX4(CPU, PByte(@CPU^.DE));
end;

// dec e
procedure Instr1D_DECE(CPU: PCPU);
begin
  DecX5(CPU, PByte(@CPU^.DE));
end;

// ld e, d8
procedure Instr1E_LDED8(CPU: PCPU);
begin
  SetLow(@CPU^.DE, GetByteArg(CPU, 1));
end;

// rra
procedure Instr1F_RRA(CPU: PCPU);
begin
  RR(@CPU^.A, CPU);
end;

{ 20 }

// jr nz, r8
procedure Instr20_JRNZR8(CPU: PCPU);
begin
  if not CPU^.Zero then begin
    CPU^.IP += GetByteArg(CPU, 1);
    CPU^.CycleCount += 4;
  end;
end;

// ld hl, d16
procedure Instr21_LDHLD16(CPU: PCPU);
begin
  CPU^.HL := GetWordArg(CPU^.Memory, 1);
end;

// ld (hl+), a
procedure Instr22_LDIRHLA(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, CPU^.A);
  CPU^.HL += 1;
end;

// inc hl
procedure Instr23_INCHL(CPU: PCPU);
begin
  CPU^.HL += 1;
end;

// inc h
procedure Instr24_INCH(CPU: PCPU);
begin
  IncX4(CPU, PByte(@CPU^.HL) + 1);
end;

// dec h
procedure Instr25_DECH(CPU: PCPU);
begin
  DecX5(CPU, PByte(@CPU^.HL) + 1);
end;

// ld h, d8
procedure Instr26_LDHD8(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, GetByteArg(CPU, 1));
end;

// daa
procedure Instr27_DAA(CPU: PCPU);
begin
  CPU^.Carry := CPU^.A > 99;
  CPU^.A := (CPU^.A mod 10) or ((CPU^.A div 10) shl 4);
  CheckZero(CPU, CPU^.A);
  CPU^.HalfCarry := False;
end;

// jr z, r8
procedure Instr28_JRZR8(CPU: PCPU);
begin
  if CPU^.Zero then begin
    CPU^.IP += GetByteArg(CPU, 1);
    CPU^.CycleCount += 4;
  end;
end;

// add hl, hl
procedure Instr29_ADDHLHL(CPU: PCPU);
begin
  Add16(@CPU^.HL, CPU^.HL, CPU);
end;

// ld a, (hl+)
procedure Instr2A_LDIARHL(CPU: PCPU);
begin
  CPU^.A := GetByte(CPU^.Memory, CPU^.HL);
  CPU^.HL += 1;
end;

// dec hl
procedure Instr2B_DECHL(CPU: PCPU);
begin
  CPU^.HL -= 1;
end;

// inc l
procedure Instr2C_INCL(CPU: PCPU);
begin
  IncX4(CPU, PByte(@CPU^.HL));
end;

// dec l
procedure Instr2D_DECL(CPU: PCPU);
begin
  DecX5(CPU, PByte(@CPU^.HL));
end;

// ld l, d8
procedure Instr2E_LDLD8(CPU: PCPU);
begin
  SetLow(@CPU^.HL, GetByteArg(CPU, 1));
end;

// cpl
procedure Instr2F_CPL(CPU: PCPU);
begin
  CPU^.A := not CPU^.A;
  CPU^.Negative := True;
  CPU^.HalfCarry := True;
end;

{ 30 }

// jr nc, r8
procedure Instr30_JRNCR8(CPU: PCPU);
begin
  if not CPU^.Carry then begin
    CPU^.IP += GetByteArg(CPU, 1);
    CPU^.CycleCount += 4;
  end;
end;

// ld sp, d16
procedure Instr31_LDSPD16(CPU: PCPU);
begin
  CPU^.SP := GetWordArg(CPU^.Memory, 1);
end;

// ld (hl-), a
procedure Instr32_LDDRHLA(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, CPU^.A);
  CPU^.HL -= 1;
end;

// inc sp
procedure Instr33_INCSP(CPU: PCPU);
begin
  CPU^.SP += 1;
end;

// inc (hl)
procedure Instr34_INCRHL(CPU: PCPU);
begin
  IncX4(CPU, PByte(CPU^.Memory + CPU^.HL));
end;

// dec (hl)
procedure Instr35_DECRHL(CPU: PCPU);
begin
  DecX5(CPU, PByte(CPU^.Memory + CPU^.HL));
end;

// ld (hl), d8
procedure Instr36_LDRHLD8(CPU: PCPU);
begin
  PByte(CPU^.Memory + CPU^.HL)^ := GetByteArg(CPU, 1);
end;

// scf
procedure Instr37_SCF(CPU: PCPU);
begin
  CPU^.Carry := True;
  CPU^.Negative := False;
  CPU^.HalfCarry := False;
end;

// jr с, r8
procedure Instr38_JRCR8(CPU: PCPU);
begin
  if CPU^.Carry then begin
    CPU^.IP += GetByteArg(CPU, 1);
    CPU^.CycleCount += 4;
  end;
end;

// add hl, sp
procedure Instr39_ADDHLSP(CPU: PCPU);
begin
  Add16(@CPU^.HL, CPU^.SP, CPU);
end;

// ld a, (hl-)
procedure Instr3A_LDDARHL(CPU: PCPU);
begin
  CPU^.A := GetByte(CPU^.Memory, CPU^.HL);
  CPU^.HL -= 1;
end;

// dec sp
procedure Instr3B_DECSP(CPU: PCPU);
begin
  CPU^.SP -= 1;
end;

// inc a
procedure Instr3C_INCA(CPU: PCPU);
begin
  IncX4(CPU, @CPU^.A);
end;

// dec a
procedure Instr3D_DECA(CPU: PCPU);
begin
  DecX5(CPU, @CPU^.A);
end;

// ld a, d8
procedure Instr3E_LDAD8(CPU: PCPU);
begin
  CPU^.A := GetByteArg(CPU, 1);
end;

// ccf
procedure Instr3F_CCF(CPU: PCPU);
begin
  CPU^.Carry := not CPU^.Carry;
  CPU^.Negative := False;
  CPU^.HalfCarry := False;
end;

{ 40 }

// ld b, b
procedure Instr40_LDBB(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, GetHigh(CPU^.BC));
end;

// ld b, c
procedure Instr41_LDBC(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, GetLow(CPU^.BC));
end;

// ld b, d
procedure Instr42_LDBD(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, GetHigh(CPU^.DE));
end;

// ld b, e
procedure Instr43_LDBE(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, GetLow(CPU^.DE));
end;

// ld b, h
procedure Instr44_LDBH(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, GetHigh(CPU^.HL));
end;

// ld b, l
procedure Instr45_LDBL(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, GetLow(CPU^.HL));
end;

// ld b, (hl)
procedure Instr46_LDBRHL(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, GetByte(CPU^.Memory, CPU^.HL));
end;

// ld b, a
procedure Instr47_LDBA(CPU: PCPU);
begin
  SetHigh(@CPU^.BC, CPU^.A);
end;

// ld c, b
procedure Instr48_LDCB(CPU: PCPU);
begin
  SetLow(@CPU^.BC, GetHigh(CPU^.BC));
end;

// ld c, c
procedure Instr49_LDCC(CPU: PCPU);
begin
  SetLow(@CPU^.BC, GetLow(CPU^.BC));
end;

// ld c, d
procedure Instr4A_LDCD(CPU: PCPU);
begin
  SetLow(@CPU^.BC, GetHigh(CPU^.DE));
end;

// ld c, e
procedure Instr4B_LDCE(CPU: PCPU);
begin
  SetLow(@CPU^.BC, GetLow(CPU^.DE));
end;

// ld c, h
procedure Instr4C_LDCH(CPU: PCPU);
begin
  SetLow(@CPU^.BC, GetHigh(CPU^.HL));
end;

// ld c, l
procedure Instr4D_LDCL(CPU: PCPU);
begin
  SetLow(@CPU^.BC, GetLow(CPU^.HL));
end;

// ld c, (hl)
procedure Instr4E_LDCRHL(CPU: PCPU);
begin
  SetLow(@CPU^.BC, GetByte(CPU^.Memory, CPU^.HL));
end;

// ld c, a
procedure Instr4F_LDCA(CPU: PCPU);
begin
  SetLow(@CPU^.BC, CPU^.A);
end;

{ 50 }

// ld d, b
procedure Instr50_LDDB(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, GetHigh(CPU^.BC));
end;

// ld d, c
procedure Instr51_LDDC(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, GetLow(CPU^.BC));
end;

// ld d, d
procedure Instr52_LDDD(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, GetHigh(CPU^.DE));
end;

// ld d, e
procedure Instr53_LDDE(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, GetLow(CPU^.DE));
end;

// ld d, h
procedure Instr54_LDDH(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, GetHigh(CPU^.HL));
end;

// ld d, l
procedure Instr55_LDDL(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, GetLow(CPU^.HL));
end;

// ld d, (hl)
procedure Instr56_LDDRHL(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, GetByte(CPU^.Memory, CPU^.HL));
end;

// ld d, a
procedure Instr57_LDDA(CPU: PCPU);
begin
  SetHigh(@CPU^.DE, CPU^.A);
end;

// ld e, b
procedure Instr58_LDEB(CPU: PCPU);
begin
  SetLow(@CPU^.DE, GetHigh(CPU^.BC));
end;

// ld e, c
procedure Instr59_LDEC(CPU: PCPU);
begin
  SetLow(@CPU^.DE, GetLow(CPU^.BC));
end;

// ld e, d
procedure Instr5A_LDED(CPU: PCPU);
begin
  SetLow(@CPU^.DE, GetHigh(CPU^.DE));
end;

// ld e, e
procedure Instr5B_LDEE(CPU: PCPU);
begin
  SetLow(@CPU^.DE, GetLow(CPU^.DE));
end;

// ld e, h
procedure Instr5C_LDEH(CPU: PCPU);
begin
  SetLow(@CPU^.DE, GetHigh(CPU^.HL));
end;

// ld e, l
procedure Instr5D_LDEL(CPU: PCPU);
begin
  SetLow(@CPU^.DE, GetLow(CPU^.HL));
end;

// ld e, (hl)
procedure Instr5E_LDERHL(CPU: PCPU);
begin
  SetLow(@CPU^.DE, GetByte(CPU^.Memory, CPU^.HL));
end;

// ld e, a
procedure Instr5F_LDEA(CPU: PCPU);
begin
  SetLow(@CPU^.DE, CPU^.A);
end;

{ 60 }

// ld h, b
procedure Instr60_LDHB(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, GetHigh(CPU^.BC));
end;

// ld h, c
procedure Instr61_LDHC(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, GetLow(CPU^.BC));
end;

// ld h, d
procedure Instr62_LDHD(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, GetHigh(CPU^.DE));
end;

// ld h, e
procedure Instr63_LDHE(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, GetLow(CPU^.DE));
end;

// ld h, h
procedure Instr64_LDHH(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, GetHigh(CPU^.HL));
end;

// ld h, l
procedure Instr65_LDHL(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, GetLow(CPU^.HL));
end;

// ld h, (hl)
procedure Instr66_LDHRHL(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, GetByte(CPU^.Memory, CPU^.HL));
end;

// ld h, a
procedure Instr67_LDHA(CPU: PCPU);
begin
  SetHigh(@CPU^.HL, CPU^.A);
end;

// ld l, b
procedure Instr68_LDLB(CPU: PCPU);
begin
  SetLow(@CPU^.HL, GetHigh(CPU^.BC));
end;

// ld l, c
procedure Instr69_LDLC(CPU: PCPU);
begin
  SetLow(@CPU^.HL, GetLow(CPU^.BC));
end;

// ld l, d
procedure Instr6A_LDLD(CPU: PCPU);
begin
  SetLow(@CPU^.HL, GetHigh(CPU^.DE));
end;

// ld l, e
procedure Instr6B_LDLE(CPU: PCPU);
begin
  SetLow(@CPU^.HL, GetLow(CPU^.DE));
end;

// ld l, h
procedure Instr6C_LDLH(CPU: PCPU);
begin
  SetLow(@CPU^.HL, GetHigh(CPU^.HL));
end;

// ld l, l
procedure Instr6D_LDLL(CPU: PCPU);
begin
  SetLow(@CPU^.HL, GetLow(CPU^.HL));
end;

// ld l, (hl)
procedure Instr6E_LDLRHL(CPU: PCPU);
begin
  SetLow(@CPU^.HL, GetByte(CPU^.Memory, CPU^.HL));
end;

// ld l, a
procedure Instr6F_LDLA(CPU: PCPU);
begin
  SetLow(@CPU^.HL, CPU^.A);
end;

{ 70 }

// ld (hl), b
procedure Instr70_LDRHLB(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, GetHigh(CPU^.BC));
end;

// ld (hl), c
procedure Instr71_LDRHLC(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, GetLow(CPU^.BC));
end;

// ld (hl), d
procedure Instr72_LDRHLD(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, GetHigh(CPU^.DE));
end;

// ld (hl), e
procedure Instr73_LDRHLE(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, GetLow(CPU^.DE));
end;

// ld (hl), h
procedure Instr74_LDRHLH(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, GetHigh(CPU^.HL));
end;

// ld (hl), l
procedure Instr75_LDRHLL(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, GetLow(CPU^.HL));
end;

// halt
procedure Instr76_HALT(CPU: PCPU);
begin
  CPU^.Status := CPU_STATUS_HALT;
end;

// ld (hl), a
procedure Instr77_LDRHLA(CPU: PCPU);
begin
  SetByte(CPU^.Memory, CPU^.HL, CPU^.A);
end;

// ld a, b
procedure Instr78_LDAB(CPU: PCPU);
begin
  CPU^.A := GetHigh(CPU^.BC);
end;

// ld a, c
procedure Instr79_LDAC(CPU: PCPU);
begin
  CPU^.A := GetLow(CPU^.BC);
end;

// ld a, d
procedure Instr7A_LDAD(CPU: PCPU);
begin
  CPU^.A := GetHigh(CPU^.DE);
end;

// ld a, e
procedure Instr7B_LDAE(CPU: PCPU);
begin
  CPU^.A := GetLow(CPU^.DE);
end;

// ld a, h
procedure Instr7C_LDAH(CPU: PCPU);
begin
  CPU^.A := GetHigh(CPU^.HL);
end;

// ld a, l
procedure Instr7D_LDAL(CPU: PCPU);
begin
  CPU^.A := GetLow(CPU^.HL);
end;

// ld a, (hl)
procedure Instr7E_LDARHL(CPU: PCPU);
begin
  CPU^.A := GetByte(CPU^.Memory, CPU^.HL);
end;

// ld a, a
procedure Instr7F_LDAA(CPU: PCPU);
begin
  CPU^.A := CPU^.A;
end;

{ 80 }

// add a, b
procedure Instr80_ADDAB(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, GetHigh(CPU^.BC), CPU);
end;

// add a, c
procedure Instr81_ADDAC(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, GetLow(CPU^.BC), CPU);
end;

// add a, d
procedure Instr82_ADDAD(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, GetHigh(CPU^.DE), CPU);
end;

// add a, e
procedure Instr83_ADDAE(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, GetLow(CPU^.DE), CPU);
end;

// add a, h
procedure Instr84_ADDAH(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, GetHigh(CPU^.HL), CPU);
end;

// add a, l
procedure Instr85_ADDAL(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, GetLow(CPU^.HL), CPU);
end;

// add a, (hl)
procedure Instr86_ADDARHL(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, GetByte(CPU^.Memory, CPU^.HL), CPU);
end;

// add a, a
procedure Instr87_ADDAA(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, CPU^.A, CPU);
end;

// adc a, b
procedure Instr88_ADCAB(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, GetHigh(CPU^.BC), CPU);
end;

// adc a, c
procedure Instr89_ADCAC(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, GetLow(CPU^.BC), CPU);
end;

// adc a, d
procedure Instr8A_ADCAD(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, GetHigh(CPU^.DE), CPU);
end;

// adc a, e
procedure Instr8B_ADCAE(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, GetLow(CPU^.DE), CPU);
end;

// adc a, h
procedure Instr8C_ADCAH(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, GetHigh(CPU^.HL), CPU);
end;

// adc a, l
procedure Instr8D_ADCAL(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, GetLow(CPU^.HL), CPU);
end;

// adc a, (hl)
procedure Instr8E_ADCARHL(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, GetByte(CPU^.Memory, CPU^.HL), CPU);
end;

// adc a, a
procedure Instr8F_ADCAA(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, CPU^.A, CPU);
end;

{ 90 }

// sub b
procedure Instr90_SUBB(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, GetHigh(CPU^.BC), CPU);
end;

// sub c
procedure Instr91_SUBC(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, GetLow(CPU^.BC), CPU);
end;

// sub d
procedure Instr92_SUBD(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, GetHigh(CPU^.DE), CPU);
end;

// sub e
procedure Instr93_SUBE(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, GetLow(CPU^.DE), CPU);
end;

// sub h
procedure Instr94_SUBH(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, GetHigh(CPU^.HL), CPU);
end;

// sub l
procedure Instr95_SUBL(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, GetLow(CPU^.HL), CPU);
end;

// sub (hl)
procedure Instr96_SUBRHL(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, GetByte(CPU^.Memory, CPU^.HL), CPU);
end;

// sub a
procedure Instr97_SUBA(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, CPU^.A, CPU);
end;

// sbc a, b
procedure Instr98_SBCAB(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, GetHigh(CPU^.BC), CPU);
end;

// sbc a, c
procedure Instr99_SBCAC(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, GetLow(CPU^.BC), CPU);
end;

// sbc a, d
procedure Instr9A_SBCAD(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, GetHigh(CPU^.DE), CPU);
end;

// sbc a, e
procedure Instr9B_SBCAE(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, GetLow(CPU^.DE), CPU);
end;

// sbc a, h
procedure Instr9C_SBCAH(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, GetHigh(CPU^.HL), CPU);
end;

// sbc a, l
procedure Instr9D_SBCAL(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, GetLow(CPU^.HL), CPU);
end;

// sbc a, (hl)
procedure Instr9E_SBCARHL(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, GetByte(CPU^.Memory, CPU^.HL), CPU);
end;

// sbc a, a
procedure Instr9F_SBCAA(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, CPU^.A, CPU);
end;

{ A0 }

// and b
procedure InstrA0_ANDB(CPU: PCPU);
begin
  And8(@CPU^.A, GetHigh(CPU^.BC), CPU);
end;

// and c
procedure InstrA1_ANDC(CPU: PCPU);
begin
  And8(@CPU^.A, GetLow(CPU^.BC), CPU);
end;

// and d
procedure InstrA2_ANDD(CPU: PCPU);
begin
  And8(@CPU^.A, GetHigh(CPU^.DE), CPU);
end;

// and e
procedure InstrA3_ANDE(CPU: PCPU);
begin
  And8(@CPU^.A, GetLow(CPU^.DE), CPU);
end;

// and h
procedure InstrA4_ANDH(CPU: PCPU);
begin
  And8(@CPU^.A, GetHigh(CPU^.HL), CPU);
end;

// and l
procedure InstrA5_ANDL(CPU: PCPU);
begin
  And8(@CPU^.A, GetLow(CPU^.HL), CPU);
end;

// and (hl)
procedure InstrA6_ANDRHL(CPU: PCPU);
begin
  And8(@CPU^.A, GetByte(CPU^.Memory, CPU^.HL), CPU);
end;

// and a
procedure InstrA7_ANDA(CPU: PCPU);
begin
  And8(@CPU^.A, CPU^.A, CPU);
end;

// xor b
procedure InstrA8_XORB(CPU: PCPU);
begin
  Xor8(@CPU^.A, GetHigh(CPU^.BC), CPU);
end;

// xor c
procedure InstrA9_XORC(CPU: PCPU);
begin
  Xor8(@CPU^.A, GetLow(CPU^.BC), CPU);
end;

// xor d
procedure InstrAA_XORD(CPU: PCPU);
begin
  Xor8(@CPU^.A, GetHigh(CPU^.DE), CPU);
end;

// xor e
procedure InstrAB_XORE(CPU: PCPU);
begin
  Xor8(@CPU^.A, GetLow(CPU^.DE), CPU);
end;

// xor h
procedure InstrAC_XORH(CPU: PCPU);
begin
  Xor8(@CPU^.A, GetHigh(CPU^.HL), CPU);
end;

// xor l
procedure InstrAD_XORL(CPU: PCPU);
begin
  Xor8(@CPU^.A, GetLow(CPU^.HL), CPU);
end;

// xor (hl)
procedure InstrAE_XORRHL(CPU: PCPU);
begin
  Xor8(@CPU^.A, GetByte(CPU^.Memory, CPU^.HL), CPU);
end;

// xor a
procedure InstrAF_XORA(CPU: PCPU);
begin
  Xor8(@CPU^.A, CPU^.A, CPU);
end;

{ B0 }

// or b
procedure InstrB0_ORB(CPU: PCPU);
begin
  Or8(@CPU^.A, GetHigh(CPU^.BC), CPU);
end;

// or c
procedure InstrB1_ORC(CPU: PCPU);
begin
  Or8(@CPU^.A, GetLow(CPU^.BC), CPU);
end;

// or d
procedure InstrB2_ORD(CPU: PCPU);
begin
  Or8(@CPU^.A, GetHigh(CPU^.DE), CPU);
end;

// or e
procedure InstrB3_ORE(CPU: PCPU);
begin
  Or8(@CPU^.A, GetLow(CPU^.DE), CPU);
end;

// or h
procedure InstrB4_ORH(CPU: PCPU);
begin
  Or8(@CPU^.A, GetHigh(CPU^.HL), CPU);
end;

// or l
procedure InstrB5_ORL(CPU: PCPU);
begin
  Or8(@CPU^.A, GetLow(CPU^.HL), CPU);
end;

// or (hl)
procedure InstrB6_ORRHL(CPU: PCPU);
begin
  Or8(@CPU^.A, GetByte(CPU^.Memory, CPU^.HL), CPU);
end;

// or a
procedure InstrB7_ORA(CPU: PCPU);
begin
  Or8(@CPU^.A, CPU^.A, CPU);
end;

// cp b
procedure InstrB8_CPB(CPU: PCPU);
begin
  Cmp8(CPU^.A, GetHigh(CPU^.BC), CPU);
end;

// cp c
procedure InstrB9_CPC(CPU: PCPU);
begin
  Cmp8(CPU^.A, GetLow(CPU^.BC), CPU);
end;

// cp d
procedure InstrBA_CPD(CPU: PCPU);
begin
  Cmp8(CPU^.A, GetHigh(CPU^.DE), CPU);
end;

// cp e
procedure InstrBB_CPE(CPU: PCPU);
begin
  Cmp8(CPU^.A, GetLow(CPU^.DE), CPU);
end;

// cp h
procedure InstrBC_CPH(CPU: PCPU);
begin
  Cmp8(CPU^.A, GetHigh(CPU^.HL), CPU);
end;

// cp l
procedure InstrBD_CPL(CPU: PCPU);
begin
  Cmp8(CPU^.A, GetLow(CPU^.HL), CPU);
end;

// cp (hl)
procedure InstrBE_CPRHL(CPU: PCPU);
begin
  Cmp8(CPU^.A, GetByte(CPU^.Memory, CPU^.HL), CPU);
end;

// cp a
procedure InstrBF_CPA(CPU: PCPU);
begin
  Cmp8(CPU^.A, CPU^.A, CPU);
end;

{ C0 }

// ret nz
procedure InstrC0_RETNZ(CPU: PCPU);
begin
  if not CPU^.Zero then begin
    CPU^.IP := Pop(CPU);
    CPU^.CycleCount += 12;
  end;
end;

// pop bc
procedure InstrC1_POPBC(CPU: PCPU);
begin
  CPU^.BC := Pop(CPU);
end;

// jp nz, a16
procedure InstrC2_JPNZA16(CPU: PCPU);
begin
  if not CPU^.Zero then begin
    CPU^.IP := GetWordArg(CPU, 1);
    CPU^.CycleCount += 4;
  end;
end;

// jp a16
procedure InstrC3_JPA16(CPU: PCPU);
begin
  CPU^.IP := GetWordArg(CPU, 1);
end;

// call nz, a16
procedure InstrC4_CALLNZA16(CPU: PCPU);
begin
  if not CPU^.Zero then begin
    Push(CPU, CPU^.IP);
    CPU^.IP := GetWordArg(CPU, 1);
    CPU^.CycleCount += 12;
  end;
end;

// push bc
procedure InstrC5_PUSHBC(CPU: PCPU);
begin
  Push(CPU, CPU^.BC);
end;

// add a, d8
procedure InstrC6_ADDAD8(CPU: PCPU);
begin
  Add8CZ(@CPU^.A, GetByteArg(CPU, 1), CPU);
end;

// rst 00h
procedure InstrC7_RST00(CPU: PCPU);
begin
  CPU^.IP := $00;
end;

// ret z
procedure InstrC8_RETZ(CPU: PCPU);
begin
  if CPU^.Zero then begin
    CPU^.IP := Pop(CPU);
    CPU^.CycleCount += 12;
  end;
end;

// ret
procedure InstrC9_RET(CPU: PCPU);
begin
  CPU^.IP := Pop(CPU);
end;

// jp z, a16
procedure InstrCA_JPZA16(CPU: PCPU);
begin
  if CPU^.Zero then begin
    CPU^.IP := GetWordArg(CPU, 1);
    CPU^.CycleCount += 4;
  end;
end;

// Prefix CB
procedure InstrCB_PREFCB(CPU: PCPU);
var
  target: PByte;
  arg: Byte;
begin
  arg := GetByteArg(CPU, 1);
  if arg and $07 = 6 then
    CPU^.CycleCount += 12
  else
    CPU^.CycleCount += 4;
  case arg and $07 of
    0: target := PByte(@CPU^.BC) + 1;
    1: target := PByte(@CPU^.BC);
    2: target := PByte(@CPU^.DE) + 1;
    3: target := PByte(@CPU^.DE);
    4: target := PByte(@CPU^.HL) + 1;
    5: target := PByte(@CPU^.HL);
    6: target := PByte(CPU^.Memory) + CPU^.HL;
    7: target := @CPU^.A;
  end;
  case arg shr 3 of
    0: begin RLC(target, CPU); CheckZero(CPU, target^) end;
    1: begin RRC(target, CPU); CheckZero(CPU, target^) end;
    2: begin RL (target, CPU); CheckZero(CPU, target^) end;
    3: begin RR (target, CPU); CheckZero(CPU, target^) end;
    4: SLA (target, CPU);
    5: SRA (target, CPU);
    6: Swap(target, CPU);
    7: SRL (target, CPU);
    8 ..15: Bit(target^, arg shr 3 and $07, CPU);
    16..23: ResBit(target, arg shr 3 and $07);
    24..31: SetBit(target, arg shr 3 and $07);
  end;
end;

// call z, a16
procedure InstrCC_CALLZA16(CPU: PCPU);
begin
  if CPU^.Zero then begin
    Push(CPU, CPU^.IP);
    CPU^.IP := GetWordArg(CPU, 1);
    CPU^.CycleCount += 12;
  end;
end;

// call a16
procedure InstrCD_CALLA16(CPU: PCPU);
begin
  Push(CPU, CPU^.IP);
  CPU^.IP := GetWordArg(CPU, 1);
end;

// adc a, d8
procedure InstrCE_ADCAD8(CPU: PCPU);
begin
  AddC8CZ(@CPU^.A, GetByteArg(CPU, 1), CPU);
end;

// rst 08h
procedure InstrCF_RST08(CPU: PCPU);
begin
  CPU^.IP := $08;
end;

{ D0 }

// ret nc
procedure InstrD0_RETNC(CPU: PCPU);
begin
  if not CPU^.Carry then begin
    CPU^.IP := Pop(CPU);
    CPU^.CycleCount += 12;
  end;
end;

// pop de
procedure InstrD1_POPDE(CPU: PCPU);
begin
  CPU^.DE := Pop(CPU);
end;

// jp nc, a16
procedure InstrD2_JPNCA16(CPU: PCPU);
begin
  if not CPU^.Carry then begin
    CPU^.IP := GetWordArg(CPU, 1);
    CPU^.CycleCount += 4;
  end;
end;

// call nC, a16
procedure InstrD4_CALLNCA16(CPU: PCPU);
begin
  if not CPU^.Carry then begin
    Push(CPU, CPU^.IP);
    CPU^.IP := GetWordArg(CPU, 1);
    CPU^.CycleCount += 12;
  end;
end;

// push de
procedure InstrD5_PUSHDE(CPU: PCPU);
begin
  Push(CPU, CPU^.DE);
end;

// sub d8
procedure InstrD6_SUBD8(CPU: PCPU);
begin
  Sub8CZ(@CPU^.A, GetByteArg(CPU, 1), CPU);
end;

// rst 10h
procedure InstrD7_RST10(CPU: PCPU);
begin
  CPU^.IP := $10;
end;

// ret c
procedure InstrD8_RETC(CPU: PCPU);
begin
  if CPU^.Carry then begin
    CPU^.IP := Pop(CPU);
    CPU^.CycleCount += 12;
  end;
end;

// reti
procedure InstrD9_RETI(CPU: PCPU);
begin
  CPU^.IP := Pop(CPU);
  CPU^.Interrupts := True;
end;

// jp c, a16
procedure InstrDA_JPCA16(CPU: PCPU);
begin
  if CPU^.Carry then begin
    CPU^.IP := GetWordArg(CPU, 1);
    CPU^.CycleCount += 4;
  end;
end;

// call c, a16
procedure InstrDC_CALLCA16(CPU: PCPU);
begin
  if CPU^.Carry then begin
    Push(CPU, CPU^.IP);
    CPU^.IP := GetWordArg(CPU, 1);
    CPU^.CycleCount += 12;
  end;
end;

// sbc a, d8
procedure InstrDE_SBCAD8(CPU: PCPU);
begin
  SubC8CZ(@CPU^.A, GetByteArg(CPU, 1), CPU);
end;

// rst 18h
procedure InstrDF_RST18(CPU: PCPU);
begin
  CPU^.IP := $18;
end;

{ E0 }

// ldh (a8), a
procedure InstrE0_LDHR8A(CPU: PCPU);
begin
  SetByte(CPU^.Memory, $FF00 + GetByteArg(CPU, 1), CPU^.A);
end;

// pop hl
procedure InstrE1_POPHL(CPU: PCPU);
begin
  CPU^.HL := Pop(CPU);
end;

// ldh (c), a
procedure InstrE2_LDRCA(CPU: PCPU);
begin
  SetByte(CPU^.Memory, $FF00 + GetLow(CPU^.BC), CPU^.A);
end;

// push hl
procedure InstrE5_PUSHHL(CPU: PCPU);
begin
  Push(CPU, CPU^.HL);
end;

// and d8
procedure InstrE6_ANDD8(CPU: PCPU);
begin
  And8(@CPU^.A, GetByteArg(CPU, 1), CPU);
end;

// rst 20h
procedure InstrE7_RST20(CPU: PCPU);
begin
  CPU^.IP := $20;
end;

// add sp, r8
procedure InstrE8_ADDSPR8(CPU: PCPU);
var
  data: ShortInt;
begin
  data := ShortInt(GetByteArg(CPU, 1));
  CPU^.Carry := CPU^.SP + data <> DWord(CPU^.SP) + data;
  if data >= 0 then
    CheckHalfCarryAdd16(CPU, CPU^.SP, data)
  else
    CheckHalfCarrySub16(CPU, CPU^.SP, Abs(data));
  CPU^.SP += data;
  CPU^.Zero := False;
  CPU^.Negative := False;
end;

// jp (hl)
procedure InstrE9_JPHL(CPU: PCPU);
begin
  CPU^.IP := CPU^.HL;
end;

// ld (a16), a
procedure InstrEA_LDA16A(CPU: PCPU);
begin
  SetByte(CPU^.Memory, GetWordArg(CPU, 1), CPU^.A);
end;

// xor d8
procedure InstrEE_XORD8(CPU: PCPU);
begin
  Xor8(@CPU^.A, GetByteArg(CPU, 1), CPU);
end;

// rst 28h
procedure InstrEF_RST28(CPU: PCPU);
begin
  CPU^.IP := $28;
end;

{ F0 }

// ldh a, (a8)
procedure InstrF0_LDHAA8(CPU: PCPU);
begin
  CPU^.A := GetByte(CPU^.Memory, $FF00 + GetByteArg(CPU, 1));
end;

// pop af
procedure InstrF1_POPAF(CPU: PCPU);
begin
  SetAF(CPU, Pop(CPU));
end;

// ld a, (c)
procedure InstrF2_LDARC(CPU: PCPU);
begin
  CPU^.A := GetByte(CPU^.Memory, $FF00 + GetLow(CPU^.BC));
end;

// di
procedure InstrF3_DI(CPU: PCPU);
begin
  CPU^.Interrupts := False;
end;

// push af
procedure InstrF5_PUSHAF(CPU: PCPU);
begin
  Push(CPU, GetAF(CPU));
end;

// or d8
procedure InstrF6_ORD8(CPU: PCPU);
begin
  Or8(@CPU^.A, GetByteArg(CPU, 1), CPU);
end;

// rst 30h
procedure InstrF7_RST30(CPU: PCPU);
begin
  CPU^.IP := $30;
end;

// ld hl, sp+r8
procedure InstrF8_LDHLSPR8(CPU: PCPU);
var
  data: ShortInt;
begin
  data := ShortInt(GetByteArg(CPU, 1));
  if data >= 0 then
    CheckHalfCarryAdd8(CPU, GetHigh(CPU^.SP), data)
  else
    CheckHalfCarrySub8(CPU, GetHigh(CPU^.SP), Abs(data));
  CPU^.HL := CPU^.SP + data;
  CPU^.Carry := DWord(CPU^.SP) + data <> CPU^.HL;
  CPU^.Negative := False;
  CPU^.Zero := False;
end;

// ld sp, hl
procedure InstrF9_LDSPHL(CPU: PCPU);
begin
  CPU^.SP := CPU^.HL;
end;

// ld a, (a16)
procedure InstrFA_LDAA16(CPU: PCPU);
begin
  CPU^.A := GetByte(CPU^.Memory, GetWordArg(CPU, 1));
end;

// ei
procedure InstrFB_EI(CPU: PCPU);
begin
  CPU^.Interrupts := True;
end;

// cp d8
procedure InstrFE_CPD8(CPU: PCPU);
begin
  Cmp8(CPU^.A, GetByteArg(CPU, 1), CPU);
end;

// rst 38H
procedure InstrFF_RST38(CPU: PCPU);
begin
  CPU^.IP := $38;
end;

// Initializing

procedure InitInstruction(Target: PInstruction; ArgLen: Byte; ClockCount: Byte; Impl: PInstructionImpl);
begin
  Target^.ArgLen := ArgLen;
  Target^.ClockCount := ClockCount;
  Target^.Run := Impl;
end;

procedure InstrSetInit;
begin
  InitInstruction(@InstrSet[$00], 0,  4, @Instr00_NOP);
  InitInstruction(@InstrSet[$01], 2, 12, @Instr01_LDBCD16);
  InitInstruction(@InstrSet[$02], 0,  8, @Instr02_LDRBCA);
  InitInstruction(@InstrSet[$03], 0,  8, @Instr03_INCBC);
  InitInstruction(@InstrSet[$04], 0,  4, @Instr04_INCB);
  InitInstruction(@InstrSet[$05], 0,  4, @Instr05_DECB);
  InitInstruction(@InstrSet[$06], 1,  8, @Instr06_LDBD8);
  InitInstruction(@InstrSet[$07], 0,  4, @Instr07_RLCA);
  InitInstruction(@InstrSet[$08], 2, 20, @Instr08_LDSP);
  InitInstruction(@InstrSet[$09], 0,  8, @Instr09_ADDHLBC);
  InitInstruction(@InstrSet[$0A], 0,  8, @Instr0A_LDARBC);
  InitInstruction(@InstrSet[$0B], 0,  8, @Instr0B_DECBC);
  InitInstruction(@InstrSet[$0C], 0,  4, @Instr0C_INCC);
  InitInstruction(@InstrSet[$0D], 0,  4, @Instr0D_DECC);
  InitInstruction(@InstrSet[$0E], 1,  8, @Instr0E_LDCD8);
  InitInstruction(@InstrSet[$0F], 0,  4, @Instr0F_RRCA);

  InitInstruction(@InstrSet[$10], 1,  4, @Instr10_STOP0);
  InitInstruction(@InstrSet[$11], 2, 12, @Instr11_LDDED16);
  InitInstruction(@InstrSet[$12], 0,  8, @Instr12_LDRDEA);
  InitInstruction(@InstrSet[$13], 0,  8, @Instr13_INCDE);
  InitInstruction(@InstrSet[$14], 0,  4, @Instr14_INCD);
  InitInstruction(@InstrSet[$15], 0,  4, @Instr15_DECD);
  InitInstruction(@InstrSet[$16], 1,  8, @Instr16_LDDD8);
  InitInstruction(@InstrSet[$17], 0,  4, @Instr17_RLA);
  InitInstruction(@InstrSet[$18], 2, 12, @Instr18_JRR8);
  InitInstruction(@InstrSet[$19], 0,  8, @Instr19_ADDHLDE);
  InitInstruction(@InstrSet[$1A], 0,  8, @Instr1A_LDARDE);
  InitInstruction(@InstrSet[$1B], 0,  8, @Instr1B_DECDE);
  InitInstruction(@InstrSet[$1C], 0,  4, @Instr1C_INCE);
  InitInstruction(@InstrSet[$1D], 0,  4, @Instr1D_DECE);
  InitInstruction(@InstrSet[$1E], 1,  8, @Instr1E_LDED8);
  InitInstruction(@InstrSet[$1F], 0,  4, @Instr1F_RRA);

  InitInstruction(@InstrSet[$20], 1,  8, @Instr20_JRNZR8);
  InitInstruction(@InstrSet[$21], 2, 12, @Instr21_LDHLD16);
  InitInstruction(@InstrSet[$22], 0,  8, @Instr22_LDIRHLA);
  InitInstruction(@InstrSet[$23], 0,  8, @Instr23_INCHL);
  InitInstruction(@InstrSet[$24], 0,  4, @Instr24_INCH);
  InitInstruction(@InstrSet[$25], 0,  4, @Instr25_DECH);
  InitInstruction(@InstrSet[$26], 1,  8, @Instr26_LDHD8);
  InitInstruction(@InstrSet[$27], 0,  4, @Instr27_DAA);
  InitInstruction(@InstrSet[$28], 2,  8, @Instr28_JRZR8);
  InitInstruction(@InstrSet[$29], 0,  8, @Instr29_ADDHLHL);
  InitInstruction(@InstrSet[$2A], 0,  8, @Instr2A_LDIARHL);
  InitInstruction(@InstrSet[$2B], 0,  8, @Instr2B_DECHL);
  InitInstruction(@InstrSet[$2C], 0,  4, @Instr2C_INCL);
  InitInstruction(@InstrSet[$2D], 0,  4, @Instr2D_DECL);
  InitInstruction(@InstrSet[$2E], 1,  8, @Instr2E_LDLD8);
  InitInstruction(@InstrSet[$2F], 0,  4, @Instr2F_CPL);

  InitInstruction(@InstrSet[$30], 1,  8, @Instr30_JRNCR8);
  InitInstruction(@InstrSet[$31], 2, 12, @Instr31_LDSPD16);
  InitInstruction(@InstrSet[$32], 0,  8, @Instr32_LDDRHLA);
  InitInstruction(@InstrSet[$33], 0,  8, @Instr33_INCSP);
  InitInstruction(@InstrSet[$34], 0, 12, @Instr34_INCRHL);
  InitInstruction(@InstrSet[$35], 0, 12, @Instr35_DECRHL);
  InitInstruction(@InstrSet[$36], 1, 12, @Instr36_LDRHLD8);
  InitInstruction(@InstrSet[$37], 0,  4, @Instr37_SCF);
  InitInstruction(@InstrSet[$38], 2,  8, @Instr38_JRCR8);
  InitInstruction(@InstrSet[$39], 0,  8, @Instr39_ADDHLSP);
  InitInstruction(@InstrSet[$3A], 0,  8, @Instr3A_LDDARHL);
  InitInstruction(@InstrSet[$3B], 0,  8, @Instr3B_DECSP);
  InitInstruction(@InstrSet[$3C], 0,  4, @Instr3C_INCA);
  InitInstruction(@InstrSet[$3D], 0,  4, @Instr3D_DECA);
  InitInstruction(@InstrSet[$3E], 1,  8, @Instr3E_LDAD8);
  InitInstruction(@InstrSet[$3F], 0,  4, @Instr3F_CCF);

  InitInstruction(@InstrSet[$40], 0,  4, @Instr40_LDBB);
  InitInstruction(@InstrSet[$41], 0,  4, @Instr41_LDBC);
  InitInstruction(@InstrSet[$42], 0,  4, @Instr42_LDBD);
  InitInstruction(@InstrSet[$43], 0,  4, @Instr43_LDBE);
  InitInstruction(@InstrSet[$44], 0,  4, @Instr44_LDBH);
  InitInstruction(@InstrSet[$45], 0,  4, @Instr45_LDBL);
  InitInstruction(@InstrSet[$46], 0,  8, @Instr46_LDBRHL);
  InitInstruction(@InstrSet[$47], 0,  4, @Instr47_LDBA);
  InitInstruction(@InstrSet[$48], 0,  4, @Instr48_LDCB);
  InitInstruction(@InstrSet[$49], 0,  4, @Instr49_LDCC);
  InitInstruction(@InstrSet[$4A], 0,  4, @Instr4A_LDCD);
  InitInstruction(@InstrSet[$4B], 0,  4, @Instr4B_LDCE);
  InitInstruction(@InstrSet[$4C], 0,  4, @Instr4C_LDCH);
  InitInstruction(@InstrSet[$4D], 0,  4, @Instr4D_LDCL);
  InitInstruction(@InstrSet[$4E], 0,  8, @Instr4E_LDCRHL);
  InitInstruction(@InstrSet[$4F], 0,  4, @Instr4F_LDCA);

  InitInstruction(@InstrSet[$50], 0,  4, @Instr50_LDDB);
  InitInstruction(@InstrSet[$51], 0,  4, @Instr51_LDDC);
  InitInstruction(@InstrSet[$52], 0,  4, @Instr52_LDDD);
  InitInstruction(@InstrSet[$53], 0,  4, @Instr53_LDDE);
  InitInstruction(@InstrSet[$54], 0,  4, @Instr54_LDDH);
  InitInstruction(@InstrSet[$55], 0,  4, @Instr55_LDDL);
  InitInstruction(@InstrSet[$56], 0,  8, @Instr56_LDDRHL);
  InitInstruction(@InstrSet[$57], 0,  4, @Instr57_LDDA);
  InitInstruction(@InstrSet[$58], 0,  4, @Instr58_LDEB);
  InitInstruction(@InstrSet[$59], 0,  4, @Instr59_LDEC);
  InitInstruction(@InstrSet[$5A], 0,  4, @Instr5A_LDED);
  InitInstruction(@InstrSet[$5B], 0,  4, @Instr5B_LDEE);
  InitInstruction(@InstrSet[$5C], 0,  4, @Instr5C_LDEH);
  InitInstruction(@InstrSet[$5D], 0,  4, @Instr5D_LDEL);
  InitInstruction(@InstrSet[$5E], 0,  8, @Instr5E_LDERHL);
  InitInstruction(@InstrSet[$5F], 0,  4, @Instr5F_LDEA);

  InitInstruction(@InstrSet[$60], 0,  4, @Instr60_LDHB);
  InitInstruction(@InstrSet[$61], 0,  4, @Instr61_LDHC);
  InitInstruction(@InstrSet[$62], 0,  4, @Instr62_LDHD);
  InitInstruction(@InstrSet[$63], 0,  4, @Instr63_LDHE);
  InitInstruction(@InstrSet[$64], 0,  4, @Instr64_LDHH);
  InitInstruction(@InstrSet[$65], 0,  4, @Instr65_LDHL);
  InitInstruction(@InstrSet[$66], 0,  8, @Instr66_LDHRHL);
  InitInstruction(@InstrSet[$67], 0,  4, @Instr67_LDHA);
  InitInstruction(@InstrSet[$68], 0,  4, @Instr68_LDLB);
  InitInstruction(@InstrSet[$69], 0,  4, @Instr69_LDLC);
  InitInstruction(@InstrSet[$6A], 0,  4, @Instr6A_LDLD);
  InitInstruction(@InstrSet[$6B], 0,  4, @Instr6B_LDLE);
  InitInstruction(@InstrSet[$6C], 0,  4, @Instr6C_LDLH);
  InitInstruction(@InstrSet[$6D], 0,  4, @Instr6D_LDLL);
  InitInstruction(@InstrSet[$6E], 0,  8, @Instr6E_LDLRHL);
  InitInstruction(@InstrSet[$6F], 0,  4, @Instr6F_LDLA);

  InitInstruction(@InstrSet[$70], 0,  8, @Instr70_LDRHLB);
  InitInstruction(@InstrSet[$71], 0,  8, @Instr71_LDRHLC);
  InitInstruction(@InstrSet[$72], 0,  8, @Instr72_LDRHLD);
  InitInstruction(@InstrSet[$73], 0,  8, @Instr73_LDRHLE);
  InitInstruction(@InstrSet[$74], 0,  8, @Instr74_LDRHLH);
  InitInstruction(@InstrSet[$75], 0,  8, @Instr75_LDRHLL);
  InitInstruction(@InstrSet[$76], 0,  4, @Instr76_HALT);
  InitInstruction(@InstrSet[$77], 0,  8, @Instr77_LDRHLA);
  InitInstruction(@InstrSet[$78], 0,  4, @Instr78_LDAB);
  InitInstruction(@InstrSet[$79], 0,  4, @Instr79_LDAC);
  InitInstruction(@InstrSet[$7A], 0,  4, @Instr7A_LDAD);
  InitInstruction(@InstrSet[$7B], 0,  4, @Instr7B_LDAE);
  InitInstruction(@InstrSet[$7C], 0,  4, @Instr7C_LDAH);
  InitInstruction(@InstrSet[$7D], 0,  4, @Instr7D_LDAL);
  InitInstruction(@InstrSet[$7E], 0,  8, @Instr7E_LDARHL);
  InitInstruction(@InstrSet[$7F], 0,  4, @Instr7F_LDAA);

  InitInstruction(@InstrSet[$80], 0,  4, @Instr80_ADDAB);
  InitInstruction(@InstrSet[$81], 0,  4, @Instr81_ADDAC);
  InitInstruction(@InstrSet[$82], 0,  4, @Instr82_ADDAD);
  InitInstruction(@InstrSet[$83], 0,  4, @Instr83_ADDAE);
  InitInstruction(@InstrSet[$84], 0,  4, @Instr84_ADDAH);
  InitInstruction(@InstrSet[$85], 0,  4, @Instr85_ADDAL);
  InitInstruction(@InstrSet[$86], 0,  8, @Instr86_ADDARHL);
  InitInstruction(@InstrSet[$87], 0,  4, @Instr87_ADDAA);
  InitInstruction(@InstrSet[$88], 0,  4, @Instr88_ADCAB);
  InitInstruction(@InstrSet[$89], 0,  4, @Instr89_ADCAC);
  InitInstruction(@InstrSet[$8A], 0,  4, @Instr8A_ADCAD);
  InitInstruction(@InstrSet[$8B], 0,  4, @Instr8B_ADCAE);
  InitInstruction(@InstrSet[$8C], 0,  4, @Instr8C_ADCAH);
  InitInstruction(@InstrSet[$8D], 0,  4, @Instr8D_ADCAL);
  InitInstruction(@InstrSet[$8E], 0,  8, @Instr8E_ADCARHL);
  InitInstruction(@InstrSet[$8F], 0,  4, @Instr8F_ADCAA);

  InitInstruction(@InstrSet[$90], 0,  4, @Instr90_SUBB);
  InitInstruction(@InstrSet[$91], 0,  4, @Instr91_SUBC);
  InitInstruction(@InstrSet[$92], 0,  4, @Instr92_SUBD);
  InitInstruction(@InstrSet[$93], 0,  4, @Instr93_SUBE);
  InitInstruction(@InstrSet[$94], 0,  4, @Instr94_SUBH);
  InitInstruction(@InstrSet[$95], 0,  4, @Instr95_SUBL);
  InitInstruction(@InstrSet[$96], 0,  8, @Instr96_SUBRHL);
  InitInstruction(@InstrSet[$97], 0,  4, @Instr97_SUBA);
  InitInstruction(@InstrSet[$98], 0,  4, @Instr98_SBCAB);
  InitInstruction(@InstrSet[$99], 0,  4, @Instr99_SBCAC);
  InitInstruction(@InstrSet[$9A], 0,  4, @Instr9A_SBCAD);
  InitInstruction(@InstrSet[$9B], 0,  4, @Instr9B_SBCAE);
  InitInstruction(@InstrSet[$9C], 0,  4, @Instr9C_SBCAH);
  InitInstruction(@InstrSet[$9D], 0,  4, @Instr9D_SBCAL);
  InitInstruction(@InstrSet[$9E], 0,  8, @Instr9E_SBCARHL);
  InitInstruction(@InstrSet[$9F], 0,  4, @Instr9F_SBCAA);

  InitInstruction(@InstrSet[$A0], 0,  4, @InstrA0_ANDB);
  InitInstruction(@InstrSet[$A1], 0,  4, @InstrA1_ANDC);
  InitInstruction(@InstrSet[$A2], 0,  4, @InstrA2_ANDD);
  InitInstruction(@InstrSet[$A3], 0,  4, @InstrA3_ANDE);
  InitInstruction(@InstrSet[$A4], 0,  4, @InstrA4_ANDH);
  InitInstruction(@InstrSet[$A5], 0,  4, @InstrA5_ANDL);
  InitInstruction(@InstrSet[$A6], 0,  8, @InstrA6_ANDRHL);
  InitInstruction(@InstrSet[$A7], 0,  4, @InstrA7_ANDA);
  InitInstruction(@InstrSet[$A8], 0,  4, @InstrA8_XORB);
  InitInstruction(@InstrSet[$A9], 0,  4, @InstrA9_XORC);
  InitInstruction(@InstrSet[$AA], 0,  4, @InstrAA_XORD);
  InitInstruction(@InstrSet[$AB], 0,  4, @InstrAB_XORE);
  InitInstruction(@InstrSet[$AC], 0,  4, @InstrAC_XORH);
  InitInstruction(@InstrSet[$AD], 0,  4, @InstrAD_XORL);
  InitInstruction(@InstrSet[$AE], 0,  8, @InstrAE_XORRHL);
  InitInstruction(@InstrSet[$AF], 0,  4, @InstrAF_XORA);

  InitInstruction(@InstrSet[$B0], 0,  4, @InstrB0_ORB);
  InitInstruction(@InstrSet[$B1], 0,  4, @InstrB1_ORC);
  InitInstruction(@InstrSet[$B2], 0,  4, @InstrB2_ORD);
  InitInstruction(@InstrSet[$B3], 0,  4, @InstrB3_ORE);
  InitInstruction(@InstrSet[$B4], 0,  4, @InstrB4_ORH);
  InitInstruction(@InstrSet[$B5], 0,  4, @InstrB5_ORL);
  InitInstruction(@InstrSet[$B6], 0,  8, @InstrB6_ORRHL);
  InitInstruction(@InstrSet[$B7], 0,  4, @InstrB7_ORA);
  InitInstruction(@InstrSet[$B8], 0,  4, @InstrB8_CPB);
  InitInstruction(@InstrSet[$B9], 0,  4, @InstrB9_CPC);
  InitInstruction(@InstrSet[$BA], 0,  4, @InstrBA_CPD);
  InitInstruction(@InstrSet[$BB], 0,  4, @InstrBB_CPE);
  InitInstruction(@InstrSet[$BC], 0,  4, @InstrBC_CPH);
  InitInstruction(@InstrSet[$BD], 0,  4, @InstrBD_CPL);
  InitInstruction(@InstrSet[$BE], 0,  8, @InstrBE_CPRHL);
  InitInstruction(@InstrSet[$BF], 0,  4, @InstrBF_CPA);

  InitInstruction(@InstrSet[$C0], 0,  8, @InstrC0_RETNZ);
  InitInstruction(@InstrSet[$C1], 0, 12, @InstrC1_POPBC);
  InitInstruction(@InstrSet[$C2], 2, 12, @InstrC2_JPNZA16);
  InitInstruction(@InstrSet[$C3], 2, 16, @InstrC3_JPA16);
  InitInstruction(@InstrSet[$C4], 2, 12, @InstrC4_CALLNZA16);
  InitInstruction(@InstrSet[$C5], 0, 16, @InstrC5_PUSHBC);
  InitInstruction(@InstrSet[$C6], 1,  8, @InstrC6_ADDAD8);
  InitInstruction(@InstrSet[$C7], 0, 16, @InstrC7_RST00);
  InitInstruction(@InstrSet[$C8], 0,  8, @InstrC8_RETZ);
  InitInstruction(@InstrSet[$C9], 0, 16, @InstrC9_RET);
  InitInstruction(@InstrSet[$CA], 2, 12, @InstrCA_JPZA16);
  InitInstruction(@InstrSet[$CB], 1,  4, @InstrCB_PREFCB);
  InitInstruction(@InstrSet[$CC], 2, 12, @InstrCC_CALLZA16);
  InitInstruction(@InstrSet[$CD], 2, 24, @InstrCD_CALLA16);
  InitInstruction(@InstrSet[$CE], 1,  8, @InstrCE_ADCAD8);
  InitInstruction(@InstrSet[$CF], 0, 16, @InstrCF_RST08);

  InitInstruction(@InstrSet[$D0], 0,  8, @InstrD0_RETNC);
  InitInstruction(@InstrSet[$D1], 0, 12, @InstrD1_POPDE);
  InitInstruction(@InstrSet[$D2], 2, 12, @InstrD2_JPNCA16);
  //
  InitInstruction(@InstrSet[$D4], 2, 12, @InstrD4_CALLNCA16);
  InitInstruction(@InstrSet[$D5], 0, 16, @InstrD5_PUSHDE);
  InitInstruction(@InstrSet[$D6], 1,  8, @InstrD6_SUBD8);
  InitInstruction(@InstrSet[$D7], 0, 16, @InstrD7_RST10);
  InitInstruction(@InstrSet[$D8], 0,  8, @InstrD8_RETC);
  InitInstruction(@InstrSet[$D9], 0, 16, @InstrD9_RETI);
  InitInstruction(@InstrSet[$DA], 2, 12, @InstrDA_JPCA16);
  //
  InitInstruction(@InstrSet[$DC], 2, 12, @InstrDC_CALLCA16);
  //
  InitInstruction(@InstrSet[$DE], 1,  8, @InstrDE_SBCAD8);
  InitInstruction(@InstrSet[$DF], 0, 16, @InstrDF_RST18);

  InitInstruction(@InstrSet[$E0], 1, 12, @InstrE0_LDHR8A);
  InitInstruction(@InstrSet[$E1], 0, 12, @InstrE1_POPHL);
  InitInstruction(@InstrSet[$E2], 1,  8, @InstrE2_LDRCA);
  //
  //
  InitInstruction(@InstrSet[$E5], 0, 16, @InstrE5_PUSHHL);
  InitInstruction(@InstrSet[$E6], 1,  8, @InstrE6_ANDD8);
  InitInstruction(@InstrSet[$E7], 0, 16, @InstrE7_RST20);
  InitInstruction(@InstrSet[$E8], 1, 16, @InstrE8_ADDSPR8);
  InitInstruction(@InstrSet[$E9], 0,  4, @InstrE9_JPHL);
  InitInstruction(@InstrSet[$EA], 2, 16, @InstrEA_LDA16A);
  //
  //
  //
  InitInstruction(@InstrSet[$EE], 1,  8, @InstrEE_XORD8);
  InitInstruction(@InstrSet[$EF], 0, 16, @InstrEF_RST28);

  InitInstruction(@InstrSet[$F0], 1, 12, @InstrF0_LDHAA8);
  InitInstruction(@InstrSet[$F1], 0, 12, @InstrF1_POPAF);
  InitInstruction(@InstrSet[$F2], 1,  8, @InstrF2_LDARC);
  InitInstruction(@InstrSet[$F3], 0,  4, @InstrF3_DI);
  //
  InitInstruction(@InstrSet[$F5], 0, 16, @InstrF5_PUSHAF);
  InitInstruction(@InstrSet[$F6], 1,  8, @InstrF6_ORD8);
  InitInstruction(@InstrSet[$F7], 0, 16, @InstrF7_RST30);
  InitInstruction(@InstrSet[$F8], 1, 12, @InstrF8_LDHLSPR8);
  InitInstruction(@InstrSet[$F9], 0,  8, @InstrF9_LDSPHL);
  InitInstruction(@InstrSet[$FA], 2, 16, @InstrFA_LDAA16);
  InitInstruction(@InstrSet[$FB], 0,  4, @InstrFB_EI);
  //
  //
  InitInstruction(@InstrSet[$FE], 1,  8, @InstrFE_CPD8);
  InitInstruction(@InstrSet[$FF], 0, 16, @InstrFF_RST38);
end;

end.

