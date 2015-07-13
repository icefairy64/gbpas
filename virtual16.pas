unit virtual16;

{$mode fpc}{$H+}
{$inline on}

interface

const
  VIRTUAL16_BLOCKSIZE = $10000;

// Block routines
function AllocVirtual16Block(Clear: Boolean): Pointer;
procedure DumpVirtual16Block(Block: Pointer; FileName: ShortString);
procedure LoadVirtual16Block(Block: Pointer; FileName: ShortString);

// Data routines
function GetByte(Mem: Pointer; Offset: Word): Byte;
procedure SetByte(Mem: Pointer; Offset: Word; Data: Byte);
function GetWord(Mem: Pointer; Offset: Word): Word;
procedure SetWord(Mem: Pointer; Offset: Word; Data: Word);

implementation

// Block routines

function AllocVirtual16Block(Clear: Boolean): Pointer;
begin
  AllocVirtual16Block := GetMem(VIRTUAL16_BLOCKSIZE);
  if Clear then
    FillDWord(AllocVirtual16Block^, VIRTUAL16_BLOCKSIZE shr 2, $00000000);
end;

procedure DumpVirtual16Block(Block: Pointer; FileName: ShortString);
const
  BLOCK_SIZE = $1000;
  BLOCK_COUNT = VIRTUAL16_BLOCKSIZE div BLOCK_SIZE;
var
  target: file;
  i: Byte;
  buf: array [0 .. (BLOCK_SIZE shr 2) - 1] of DWord;
begin
  Assign(target, FileName);
  Rewrite(target, 4);
  for i := 0 to BLOCK_COUNT - 1 do begin
    buf := PDWord(Block + i * BLOCK_SIZE);
    BlockWrite(target, buf, BLOCK_SIZE shr 2);
  end;
  Close(target);
end;

procedure LoadVirtual16Block(Block: Pointer; FileName: ShortString);
const
  BLOCK_SIZE = $1000;
  BLOCK_COUNT = VIRTUAL16_BLOCKSIZE div BLOCK_SIZE;
var
  target: file;
  i: Byte;
  buf: array [0 .. (BLOCK_SIZE shr 2) - 1] of DWord;
begin
  Assign(target, FileName);
  Reset(target, 4);
  for i := 0 to BLOCK_COUNT - 1 do begin
    buf := PDWord(Block + i * BLOCK_SIZE);
    BlockRead(target, buf, BLOCK_SIZE shr 2);
  end;
  Close(target);
end;

// Data routines

procedure ValidateOffset(var Offset: Word); inline;
begin
  // $E000 - $FDFF: Mirror of $C000 - $DDFF
  if (Offset >= $E000) and (Offset <= $FDFF) then
    Offset -= $2000;
end;

function GetByte(Mem: Pointer; Offset: Word): Byte;
begin
  ValidateOffset(Offset);
  GetByte := PByte(Mem + Offset)^;
end;

procedure SetByte(Mem: Pointer; Offset: Word; Data: Byte);
begin
  ValidateOffset(Offset);
  PByte(Mem + Offset)^ := Data;
end;

function GetWord(Mem: Pointer; Offset: Word): Word;
begin
  ValidateOffset(Offset);
  GetWord := PWord(Mem + Offset)^;
end;

procedure SetWord(Mem: Pointer; Offset: Word; Data: Word);
begin
  ValidateOffset(Offset);
  PWord(Mem + Offset)^ := Data;
end;

end.

