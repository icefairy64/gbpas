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
end;

end.

