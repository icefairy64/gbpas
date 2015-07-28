unit lcd;

{$mode fpc}

interface

uses
  virtual16, cpu;

const
  LCD_INTERRUPT_VBLANK   = $0040;
  LCD_INTERRUPT_LCDCSTAT = $0048;

  LCD_TILEPATTERNTABLE1  = $8000;
  LCD_TILEPATTERNTABLE2  = $9000;
  LCD_SPRITEATTRTABLE    = $FE00;
  LCD_CONTROL            = $FF40;

  LCD_SPRITEATTRRECSIZE  = $04;

type
  PSpriteAttr = ^TSpriteAttr;
  TSpriteAttr = record
    Y: Byte;
    X: Byte;
    Pattern: Byte;
    Priority: Boolean;
    YFlip: Boolean;
    XFlip: Boolean;
    PaletteNumber: Boolean;
  end;

  PLCDControl = ^TLCDControl;
  TLCDControl = record
    Operation: Boolean;
    WindowTileMap: Boolean;
    WindowDisplay: Boolean;
    BGWindowTable: Boolean;
    BGTileMap: Boolean;
    SpriteSize: Boolean;
    SpriteDisplay: Boolean;
    BGDisplay: Boolean;
  end;

procedure LCD_ReadSpriteAttr(CPU: PCPU; Index: Byte; Dest: PSpriteAttr);
procedure LCD_WriteSpriteAttr(CPU: PCPU; Index: Byte; Data: PSpriteAttr);
procedure LCD_ReadLCDControl(CPU: PCPU; Dest: PLCDControl);
procedure LCD_WriteLCDControl(CPU: PCPU; Dest: PLCDControl);

implementation

procedure LCD_ReadSpriteAttr(CPU: PCPU; Index: Byte; Dest: PSpriteAttr);
var
  pt: PByte;
begin
  pt := PByte(CPU^.Memory + LCD_SPRITEATTRTABLE + Index * LCD_SPRITEATTRRECSIZE);
  Dest^.Y := pt^;
  pt += 1;
  Dest^.X := pt^;
  pt += 1;
  Dest^.Pattern := pt^;
  pt += 1;
  Dest^.Priority      := pt^ and $80 > 0;
  Dest^.YFlip         := pt^ and $40 > 0;
  Dest^.XFlip         := pt^ and $20 > 0;
  Dest^.PaletteNumber := pt^ and $10 > 0;
end;

procedure LCD_WriteSpriteAttr(CPU: PCPU; Index: Byte; Data: PSpriteAttr);
var
  pt: PByte;
begin
  pt := PByte(CPU^.Memory + LCD_SPRITEATTRTABLE + Index * LCD_SPRITEATTRRECSIZE);
  pt^ := Data^.Y;
  pt += 1;
  pt^ := Data^.X;
  pt += 1;
  pt^ := Data^.Pattern;
  pt += 1;
  pt^ := 0;
  if Data^.Priority then
    pt^ += $80;
  if Data^.YFlip then
    pt^ += $40;
  if Data^.XFlip then
    pt^ += $20;
  if Data^.PaletteNumber then
    pt^ += $10;
end;

procedure LCD_ReadLCDControl(CPU: PCPU; Dest: PLCDControl);
var
  data: Byte;
begin
  data := GetByte(CPU, LCD_CONTROL);
  Dest^.Operation     := data and $80 > 0;
  Dest^.WindowTileMap := data and $40 > 0;
  Dest^.WindowDisplay := data and $20 > 0;
  Dest^.BGWindowTable := data and $10 > 0;
  Dest^.BGTileMap     := data and $08 > 0;
  Dest^.SpriteSize    := data and $04 > 0;
  Dest^.SpriteDisplay := data and $02 > 0;
  Dest^.BGDisplay     := data and $01 > 0;
end;

procedure LCD_WriteLCDControl(CPU: PCPU; Dest: PLCDControl);
var
  data: Byte;
begin
  data := 0;
  if Dest^.Operation then
    data += $80;
  if Dest^.WindowTileMap then
    data += $40;
  if Dest^.WindowDisplay then
    data += $20;
  if Dest^.BGWindowTable then
    data += $10;
  if Dest^.BGTileMap then
    data += $08;
  if Dest^.SpriteSize then
    data += $04;
  if Dest^.SpriteDisplay then
    data += $02;
  if Dest^.BGDisplay then
    data += $01;
  SetByte(CPU, LCD_CONTROL, data);
end;

end.

