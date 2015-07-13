unit instructions;

{$mode objfpc}{$H+}
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
  Result := Value and $00FF;
end;

function GetHigh(Value: Word): Byte; inline;
begin
  Result := Value and $FF00;
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

procedure CheckHalfCarry(CPU: PCPU; Data: Word); inline;
begin
  CPU^.HalfCarry := (Data and CPU_HALFCARRY_MASK) > 0;
end;

// Affects: H Z N C
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

// Affects: H Z N C
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

// Affects: H Z N C
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

// Affects: H Z N C
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

// Affects: H N
procedure Add8(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  Target^ += Data;
  CPU^.Negative := False;
  CheckHalfCarry(CPU, Target^);
end;

// Affects: H N
procedure Add16(Target: PWord; Data: Word; CPU: PCPU); inline;
begin
  Target^ += Data;
  CPU^.Negative := False;
  CheckHalfCarry(CPU, Target^);
end;

// Affects: H N
procedure Sub8(Target: PByte; Data: Byte; CPU: PCPU); inline;
begin
  Target^ -= Data;
  CPU^.Negative := True;
  CheckHalfCarry(CPU, Target^);
end;

// Affects: H N
procedure Sub16(Target: PWord; Data: Word; CPU: PCPU); inline;
begin
  Target^ -= Data;
  CPU^.Negative := True;
  CheckHalfCarry(CPU, Target^);
end;

// Affects: N Z H
procedure IncX4(CPU: PCPU; Target: PByte); inline;
begin
  Target^ += 1;
  CheckZero(CPU, Target^);
  CheckHalfCarry(CPU, Target^);
  CPU^.Negative := False;
end;

// Affects: N Z H
procedure DecX5(CPU: PCPU; Target: PByte); inline;
begin
  Target^ -= 1;
  CheckZero(CPU, Target^);
  CheckHalfCarry(CPU, Target^);
  CPU^.Negative := True;
end;

function GetByteArg(CPU: PCPU; Offset: Byte): Byte; inline;
begin
  Result := GetByte(CPU^.Memory, CPU^.IP + Offset);
end;

function GetWordArg(CPU: PCPU; Offset: Byte): Byte; inline;
begin
  Result := GetWord(CPU^.Memory, CPU^.IP + Offset);
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
  SetHigh(PByte(CPU^.Memory + CPU^.HL), GetByteArg(CPU, 1));
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
end;

end.

