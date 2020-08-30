unit HlpHaval;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
  HlpHashBuffer,
{$ENDIF DELPHI}
  HlpBits,
  HlpHashSize,
  HlpHashRounds,
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpHashCryptoNotBuildIn;

resourcestring
  SInvalidHavalRound = 'Haval Round Must be 3, 4 or 5';
  SInvalidHavalHashSize =
    'Haval HashSize Must be Either 128 bit(16 byte), 160 bit(20 byte), 192 bit(24 byte), 224 bit(28 byte) or 256 bit(32 byte)';

type
  THaval = class abstract(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private
  const
    HAVAL_VERSION = Int32(1);

  var
    FRounds: Int32;

    procedure TailorDigestBits();

  strict protected
  var
    FHash: THashLibUInt32Array;

    constructor Create(ARounds: THashRounds; AHashSize: THashSize);

    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;

  public
    procedure Initialize(); override;
  end;

type

  THaval3 = class abstract(THaval)

  strict protected
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create(AHashSize: THashSize);

  end;

type

  THaval4 = class abstract(THaval)

  strict protected
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create(AHashSize: THashSize);

  end;

type

  THaval5 = class abstract(THaval)

  strict protected
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create(AHashSize: THashSize);

  end;

type

  THaval_3_128 = class sealed(THaval3)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_4_128 = class sealed(THaval4)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_5_128 = class sealed(THaval5)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_3_160 = class sealed(THaval3)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_4_160 = class sealed(THaval4)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_5_160 = class sealed(THaval5)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_3_192 = class sealed(THaval3)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_4_192 = class sealed(THaval4)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_5_192 = class sealed(THaval5)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_3_224 = class sealed(THaval3)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_4_224 = class sealed(THaval4)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_5_224 = class sealed(THaval5)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_3_256 = class sealed(THaval3)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_4_256 = class sealed(THaval4)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

type

  THaval_5_256 = class sealed(THaval5)

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ THaval }

constructor THaval.Create(ARounds: THashRounds; AHashSize: THashSize);
begin
  inherited Create(Int32(AHashSize), 128);
  System.SetLength(FHash, 8);
  FRounds := Int32(ARounds);
end;

procedure THaval.Finish;
var
  LBits: UInt64;
  LPadIndex: Int32;
  LPad: THashLibByteArray;
begin
  LBits := FProcessedBytesCount * 8;
  if (FBuffer.Position < 118) then
  begin
    LPadIndex := (118 - FBuffer.Position)
  end
  else
  begin
    LPadIndex := (246 - FBuffer.Position);
  end;
  System.SetLength(LPad, LPadIndex + 10);

  LPad[0] := Byte($01);

  LPad[LPadIndex] := Byte((FRounds shl 3) or (HAVAL_VERSION and $07));
  System.Inc(LPadIndex);
  LPad[LPadIndex] := Byte(HashSize shl 1);
  System.Inc(LPadIndex);

  LBits := TConverters.le2me_64(LBits);

  TConverters.ReadUInt64AsBytesLE(LBits, LPad, LPadIndex);

  LPadIndex := LPadIndex + 8;

  TransformBytes(LPad, 0, LPadIndex);
end;

function THaval.GetResult: THashLibByteArray;
begin
  TailorDigestBits();
  System.SetLength(result, (HashSize shr 2) * System.SizeOf(UInt32));
  TConverters.le32_copy(PCardinal(FHash), 0, PByte(result), 0,
    System.Length(result));
end;

procedure THaval.Initialize;
begin
  FHash[0] := $243F6A88;
  FHash[1] := $85A308D3;
  FHash[2] := $13198A2E;
  FHash[3] := $03707344;
  FHash[4] := $A4093822;
  FHash[5] := $299F31D0;
  FHash[6] := $082EFA98;
  FHash[7] := $EC4E6C89;

  inherited Initialize();
end;

procedure THaval.TailorDigestBits;
var
  LT: UInt32;
begin

  case HashSize of
    16:
      begin
        LT := (FHash[7] and $000000FF) or (FHash[6] and $FF000000) or
          (FHash[5] and $00FF0000) or (FHash[4] and $0000FF00);
        FHash[0] := FHash[0] + TBits.RotateRight32(LT, 8);
        LT := (FHash[7] and $0000FF00) or (FHash[6] and $000000FF) or
          (FHash[5] and $FF000000) or (FHash[4] and $00FF0000);
        FHash[1] := FHash[1] + TBits.RotateRight32(LT, 16);
        LT := (FHash[7] and $00FF0000) or (FHash[6] and $0000FF00) or
          (FHash[5] and $000000FF) or (FHash[4] and $FF000000);
        FHash[2] := FHash[2] + TBits.RotateRight32(LT, 24);
        LT := (FHash[7] and $FF000000) or (FHash[6] and $00FF0000) or
          (FHash[5] and $0000FF00) or (FHash[4] and $000000FF);
        FHash[3] := FHash[3] + LT;
      end;

    20:
      begin
        LT := UInt32(FHash[7] and $3F) or UInt32(FHash[6] and ($7F shl 25)) or
          UInt32(FHash[5] and ($3F shl 19));
        FHash[0] := FHash[0] + TBits.RotateRight32(LT, 19);
        LT := UInt32(FHash[7] and ($3F shl 6)) or UInt32(FHash[6] and $3F) or
          UInt32(FHash[5] and ($7F shl 25));
        FHash[1] := FHash[1] + TBits.RotateRight32(LT, 25);
        LT := (FHash[7] and ($7F shl 12)) or (FHash[6] and ($3F shl 6)) or
          (FHash[5] and $3F);
        FHash[2] := FHash[2] + LT;
        LT := (FHash[7] and ($3F shl 19)) or (FHash[6] and ($7F shl 12)) or
          (FHash[5] and ($3F shl 6));
        FHash[3] := FHash[3] + (LT shr 6);
        LT := (FHash[7] and (UInt32($7F) shl 25)) or
          UInt32(FHash[6] and ($3F shl 19)) or
          UInt32(FHash[5] and ($7F shl 12));
        FHash[4] := FHash[4] + (LT shr 12);
      end;

    24:
      begin
        LT := UInt32(FHash[7] and $1F) or UInt32(FHash[6] and ($3F shl 26));
        FHash[0] := FHash[0] + TBits.RotateRight32(LT, 26);
        LT := (FHash[7] and ($1F shl 5)) or (FHash[6] and $1F);
        FHash[1] := FHash[1] + LT;
        LT := (FHash[7] and ($3F shl 10)) or (FHash[6] and ($1F shl 5));
        FHash[2] := FHash[2] + (LT shr 5);
        LT := (FHash[7] and ($1F shl 16)) or (FHash[6] and ($3F shl 10));
        FHash[3] := FHash[3] + (LT shr 10);
        LT := (FHash[7] and ($1F shl 21)) or (FHash[6] and ($1F shl 16));
        FHash[4] := FHash[4] + (LT shr 16);
        LT := UInt32(FHash[7] and ($3F shl 26)) or
          UInt32(FHash[6] and ($1F shl 21));
        FHash[5] := FHash[5] + (LT shr 21);
      end;

    28:
      begin
        FHash[0] := FHash[0] + ((FHash[7] shr 27) and $1F);
        FHash[1] := FHash[1] + ((FHash[7] shr 22) and $1F);
        FHash[2] := FHash[2] + ((FHash[7] shr 18) and $0F);
        FHash[3] := FHash[3] + ((FHash[7] shr 13) and $1F);
        FHash[4] := FHash[4] + ((FHash[7] shr 9) and $0F);
        FHash[5] := FHash[5] + ((FHash[7] shr 4) and $1F);
        FHash[6] := FHash[6] + (FHash[7] and $0F);
      end;
  end;

end;

{ THaval3 }

constructor THaval3.Create(AHashSize: THashSize);
begin
  inherited Create(THashRounds.hrRounds3, AHashSize);
end;

procedure THaval3.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  a, b, c, d, e, f, g, h, t: UInt32;
  LTemp: array [0 .. 31] of UInt32;
begin
  TConverters.le32_copy(AData, AIndex, @(LTemp[0]), 0, ADataLength);

  a := FHash[0];
  b := FHash[1];
  c := FHash[2];
  d := FHash[3];
  e := FHash[4];
  f := FHash[5];
  g := FHash[6];
  h := FHash[7];

  t := c and (e xor d) xor g and a xor f and b xor e;
  h := LTemp[0] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (d xor c) xor f and h xor e and a xor d;
  g := LTemp[1] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (c xor b) xor e and g xor d and h xor c;
  f := LTemp[2] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (b xor a) xor d and f xor c and g xor b;
  e := LTemp[3] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (a xor h) xor c and e xor b and f xor a;
  d := LTemp[4] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (h xor g) xor b and d xor a and e xor h;
  c := LTemp[5] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (g xor f) xor a and c xor h and d xor g;
  b := LTemp[6] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (f xor e) xor h and b xor g and c xor f;
  a := LTemp[7] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (e xor d) xor g and a xor f and b xor e;
  h := LTemp[8] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (d xor c) xor f and h xor e and a xor d;
  g := LTemp[9] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (c xor b) xor e and g xor d and h xor c;
  f := LTemp[10] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (b xor a) xor d and f xor c and g xor b;
  e := LTemp[11] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (a xor h) xor c and e xor b and f xor a;
  d := LTemp[12] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (h xor g) xor b and d xor a and e xor h;
  c := LTemp[13] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (g xor f) xor a and c xor h and d xor g;
  b := LTemp[14] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (f xor e) xor h and b xor g and c xor f;
  a := LTemp[15] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (e xor d) xor g and a xor f and b xor e;
  h := LTemp[16] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (d xor c) xor f and h xor e and a xor d;
  g := LTemp[17] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (c xor b) xor e and g xor d and h xor c;
  f := LTemp[18] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (b xor a) xor d and f xor c and g xor b;
  e := LTemp[19] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (a xor h) xor c and e xor b and f xor a;
  d := LTemp[20] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (h xor g) xor b and d xor a and e xor h;
  c := LTemp[21] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (g xor f) xor a and c xor h and d xor g;
  b := LTemp[22] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (f xor e) xor h and b xor g and c xor f;
  a := LTemp[23] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (e xor d) xor g and a xor f and b xor e;
  h := LTemp[24] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (d xor c) xor f and h xor e and a xor d;
  g := LTemp[25] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (c xor b) xor e and g xor d and h xor c;
  f := LTemp[26] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (b xor a) xor d and f xor c and g xor b;
  e := LTemp[27] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (a xor h) xor c and e xor b and f xor a;
  d := LTemp[28] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (h xor g) xor b and d xor a and e xor h;
  c := LTemp[29] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (g xor f) xor a and c xor h and d xor g;
  b := LTemp[30] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (f xor e) xor h and b xor g and c xor f;
  a := LTemp[31] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := f and (d and not a xor b and c xor e xor g) xor b and (d xor c)
    xor a and c xor g;
  h := LTemp[5] + $452821E6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := e and (c and not h xor a and b xor d xor f) xor a and (c xor b)
    xor h and b xor f;
  g := LTemp[14] + $38D01377 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := d and (b and not g xor h and a xor c xor e) xor h and (b xor a)
    xor g and a xor e;
  f := LTemp[26] + $BE5466CF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := c and (a and not f xor g and h xor b xor d) xor g and (a xor h)
    xor f and h xor d;
  e := LTemp[18] + $34E90C6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := b and (h and not e xor f and g xor a xor c) xor f and (h xor g)
    xor e and g xor c;
  d := LTemp[11] + $C0AC29B7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := a and (g and not d xor e and f xor h xor b) xor e and (g xor f)
    xor d and f xor b;
  c := LTemp[28] + $C97C50DD + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := h and (f and not c xor d and e xor g xor a) xor d and (f xor e)
    xor c and e xor a;
  b := LTemp[7] + $3F84D5B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := g and (e and not b xor c and d xor f xor h) xor c and (e xor d)
    xor b and d xor h;
  a := LTemp[16] + $B5470917 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := f and (d and not a xor b and c xor e xor g) xor b and (d xor c)
    xor a and c xor g;
  h := LTemp[0] + $9216D5D9 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := e and (c and not h xor a and b xor d xor f) xor a and (c xor b)
    xor h and b xor f;
  g := LTemp[23] + $8979FB1B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := d and (b and not g xor h and a xor c xor e) xor h and (b xor a)
    xor g and a xor e;
  f := LTemp[20] + $D1310BA6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := c and (a and not f xor g and h xor b xor d) xor g and (a xor h)
    xor f and h xor d;
  e := LTemp[22] + $98DFB5AC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := b and (h and not e xor f and g xor a xor c) xor f and (h xor g)
    xor e and g xor c;
  d := LTemp[1] + $2FFD72DB + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := a and (g and not d xor e and f xor h xor b) xor e and (g xor f)
    xor d and f xor b;
  c := LTemp[10] + $D01ADFB7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := h and (f and not c xor d and e xor g xor a) xor d and (f xor e)
    xor c and e xor a;
  b := LTemp[4] + $B8E1AFED + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := g and (e and not b xor c and d xor f xor h) xor c and (e xor d)
    xor b and d xor h;
  a := LTemp[8] + $6A267E96 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := f and (d and not a xor b and c xor e xor g) xor b and (d xor c)
    xor a and c xor g;
  h := LTemp[30] + $BA7C9045 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := e and (c and not h xor a and b xor d xor f) xor a and (c xor b)
    xor h and b xor f;
  g := LTemp[3] + $F12C7F99 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := d and (b and not g xor h and a xor c xor e) xor h and (b xor a)
    xor g and a xor e;
  f := LTemp[21] + $24A19947 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := c and (a and not f xor g and h xor b xor d) xor g and (a xor h)
    xor f and h xor d;
  e := LTemp[9] + $B3916CF7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := b and (h and not e xor f and g xor a xor c) xor f and (h xor g)
    xor e and g xor c;
  d := LTemp[17] + $0801F2E2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := a and (g and not d xor e and f xor h xor b) xor e and (g xor f)
    xor d and f xor b;
  c := LTemp[24] + $858EFC16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := h and (f and not c xor d and e xor g xor a) xor d and (f xor e)
    xor c and e xor a;
  b := LTemp[29] + $636920D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := g and (e and not b xor c and d xor f xor h) xor c and (e xor d)
    xor b and d xor h;
  a := LTemp[6] + $71574E69 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := f and (d and not a xor b and c xor e xor g) xor b and (d xor c)
    xor a and c xor g;
  h := LTemp[19] + $A458FEA3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := e and (c and not h xor a and b xor d xor f) xor a and (c xor b)
    xor h and b xor f;
  g := LTemp[12] + $F4933D7E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := d and (b and not g xor h and a xor c xor e) xor h and (b xor a)
    xor g and a xor e;
  f := LTemp[15] + $0D95748F + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := c and (a and not f xor g and h xor b xor d) xor g and (a xor h)
    xor f and h xor d;
  e := LTemp[13] + $728EB658 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := b and (h and not e xor f and g xor a xor c) xor f and (h xor g)
    xor e and g xor c;
  d := LTemp[2] + $718BCD58 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := a and (g and not d xor e and f xor h xor b) xor e and (g xor f)
    xor d and f xor b;
  c := LTemp[25] + $82154AEE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := h and (f and not c xor d and e xor g xor a) xor d and (f xor e)
    xor c and e xor a;
  b := LTemp[31] + $7B54A41D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := g and (e and not b xor c and d xor f xor h) xor c and (e xor d)
    xor b and d xor h;
  a := LTemp[27] + $C25A59B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and e xor g xor a) xor f and c xor e and b xor a;
  h := LTemp[19] + $9C30D539 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and d xor f xor h) xor e and b xor d and a xor h;
  g := LTemp[9] + $2AF26013 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and c xor e xor g) xor d and a xor c and h xor g;
  f := LTemp[4] + $C5D1B023 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and b xor d xor f) xor c and h xor b and g xor f;
  e := LTemp[20] + $286085F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and a xor c xor e) xor b and g xor a and f xor e;
  d := LTemp[28] + $CA417918 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and h xor b xor d) xor a and f xor h and e xor d;
  c := LTemp[17] + $B8DB38EF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and g xor a xor c) xor h and e xor g and d xor c;
  b := LTemp[8] + $8E79DCB0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and f xor h xor b) xor g and d xor f and c xor b;
  a := LTemp[22] + $603A180E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and e xor g xor a) xor f and c xor e and b xor a;
  h := LTemp[29] + $6C9E0E8B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and d xor f xor h) xor e and b xor d and a xor h;
  g := LTemp[14] + $B01E8A3E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and c xor e xor g) xor d and a xor c and h xor g;
  f := LTemp[25] + $D71577C1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and b xor d xor f) xor c and h xor b and g xor f;
  e := LTemp[12] + $BD314B27 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and a xor c xor e) xor b and g xor a and f xor e;
  d := LTemp[24] + $78AF2FDA + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and h xor b xor d) xor a and f xor h and e xor d;
  c := LTemp[30] + $55605C60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and g xor a xor c) xor h and e xor g and d xor c;
  b := LTemp[16] + $E65525F3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and f xor h xor b) xor g and d xor f and c xor b;
  a := LTemp[26] + $AA55AB94 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and e xor g xor a) xor f and c xor e and b xor a;
  h := LTemp[31] + $57489862 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and d xor f xor h) xor e and b xor d and a xor h;
  g := LTemp[15] + $63E81440 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and c xor e xor g) xor d and a xor c and h xor g;
  f := LTemp[7] + $55CA396A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and b xor d xor f) xor c and h xor b and g xor f;
  e := LTemp[3] + $2AAB10B6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and a xor c xor e) xor b and g xor a and f xor e;
  d := LTemp[1] + $B4CC5C34 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and h xor b xor d) xor a and f xor h and e xor d;
  c := LTemp[0] + $1141E8CE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and g xor a xor c) xor h and e xor g and d xor c;
  b := LTemp[18] + $A15486AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and f xor h xor b) xor g and d xor f and c xor b;
  a := LTemp[27] + $7C72E993 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and e xor g xor a) xor f and c xor e and b xor a;
  h := LTemp[13] + $B3EE1411 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and d xor f xor h) xor e and b xor d and a xor h;
  g := LTemp[6] + $636FBC2A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and c xor e xor g) xor d and a xor c and h xor g;
  f := LTemp[21] + $2BA9C55D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and b xor d xor f) xor c and h xor b and g xor f;
  e := LTemp[10] + $741831F6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and a xor c xor e) xor b and g xor a and f xor e;
  d := LTemp[23] + $CE5C3E16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and h xor b xor d) xor a and f xor h and e xor d;
  c := LTemp[11] + $9B87931E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and g xor a xor c) xor h and e xor g and d xor c;
  b := LTemp[5] + $AFD6BA33 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and f xor h xor b) xor g and d xor f and c xor b;
  a := LTemp[2] + $6C24CF5C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  FHash[0] := FHash[0] + a;
  FHash[1] := FHash[1] + b;
  FHash[2] := FHash[2] + c;
  FHash[3] := FHash[3] + d;
  FHash[4] := FHash[4] + e;
  FHash[5] := FHash[5] + f;
  FHash[6] := FHash[6] + g;
  FHash[7] := FHash[7] + h;

  System.FillChar(LTemp, System.SizeOf(LTemp), UInt32(0));
end;

{ THaval4 }

constructor THaval4.Create(AHashSize: THashSize);
begin
  inherited Create(THashRounds.hrRounds4, AHashSize);
end;

procedure THaval4.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  a, b, c, d, e, f, g, h, t: UInt32;
  LTemp: array [0 .. 31] of UInt32;
begin
  TConverters.le32_copy(AData, AIndex, @(LTemp[0]), 0, ADataLength);

  a := FHash[0];
  b := FHash[1];
  c := FHash[2];
  d := FHash[3];
  e := FHash[4];
  f := FHash[5];
  g := FHash[6];
  h := FHash[7];

  t := d and (a xor b) xor f and g xor e and c xor a;
  h := LTemp[0] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := c and (h xor a) xor e and f xor d and b xor h;
  g := LTemp[1] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := b and (g xor h) xor d and e xor c and a xor g;
  f := LTemp[2] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := a and (f xor g) xor c and d xor b and h xor f;
  e := LTemp[3] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := h and (e xor f) xor b and c xor a and g xor e;
  d := LTemp[4] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := g and (d xor e) xor a and b xor h and f xor d;
  c := LTemp[5] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := f and (c xor d) xor h and a xor g and e xor c;
  b := LTemp[6] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := e and (b xor c) xor g and h xor f and d xor b;
  a := LTemp[7] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := d and (a xor b) xor f and g xor e and c xor a;
  h := LTemp[8] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := c and (h xor a) xor e and f xor d and b xor h;
  g := LTemp[9] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := b and (g xor h) xor d and e xor c and a xor g;
  f := LTemp[10] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := a and (f xor g) xor c and d xor b and h xor f;
  e := LTemp[11] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := h and (e xor f) xor b and c xor a and g xor e;
  d := LTemp[12] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := g and (d xor e) xor a and b xor h and f xor d;
  c := LTemp[13] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := f and (c xor d) xor h and a xor g and e xor c;
  b := LTemp[14] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := e and (b xor c) xor g and h xor f and d xor b;
  a := LTemp[15] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := d and (a xor b) xor f and g xor e and c xor a;
  h := LTemp[16] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := c and (h xor a) xor e and f xor d and b xor h;
  g := LTemp[17] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := b and (g xor h) xor d and e xor c and a xor g;
  f := LTemp[18] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := a and (f xor g) xor c and d xor b and h xor f;
  e := LTemp[19] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := h and (e xor f) xor b and c xor a and g xor e;
  d := LTemp[20] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := g and (d xor e) xor a and b xor h and f xor d;
  c := LTemp[21] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := f and (c xor d) xor h and a xor g and e xor c;
  b := LTemp[22] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := e and (b xor c) xor g and h xor f and d xor b;
  a := LTemp[23] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := d and (a xor b) xor f and g xor e and c xor a;
  h := LTemp[24] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := c and (h xor a) xor e and f xor d and b xor h;
  g := LTemp[25] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := b and (g xor h) xor d and e xor c and a xor g;
  f := LTemp[26] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := a and (f xor g) xor c and d xor b and h xor f;
  e := LTemp[27] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := h and (e xor f) xor b and c xor a and g xor e;
  d := LTemp[28] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := g and (d xor e) xor a and b xor h and f xor d;
  c := LTemp[29] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := f and (c xor d) xor h and a xor g and e xor c;
  b := LTemp[30] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := e and (b xor c) xor g and h xor f and d xor b;
  a := LTemp[31] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := b and (g and not a xor c and f xor d xor e) xor c and (g xor f)
    xor a and f xor e;
  h := LTemp[5] + $452821E6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (f and not h xor b and e xor c xor d) xor b and (f xor e)
    xor h and e xor d;
  g := LTemp[14] + $38D01377 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (e and not g xor a and d xor b xor c) xor a and (e xor d)
    xor g and d xor c;
  f := LTemp[26] + $BE5466CF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (d and not f xor h and c xor a xor b) xor h and (d xor c)
    xor f and c xor b;
  e := LTemp[18] + $34E90C6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (c and not e xor g and b xor h xor a) xor g and (c xor b)
    xor e and b xor a;
  d := LTemp[11] + $C0AC29B7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (b and not d xor f and a xor g xor h) xor f and (b xor a)
    xor d and a xor h;
  c := LTemp[28] + $C97C50DD + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (a and not c xor e and h xor f xor g) xor e and (a xor h)
    xor c and h xor g;
  b := LTemp[7] + $3F84D5B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (h and not b xor d and g xor e xor f) xor d and (h xor g)
    xor b and g xor f;
  a := LTemp[16] + $B5470917 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (g and not a xor c and f xor d xor e) xor c and (g xor f)
    xor a and f xor e;
  h := LTemp[0] + $9216D5D9 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (f and not h xor b and e xor c xor d) xor b and (f xor e)
    xor h and e xor d;
  g := LTemp[23] + $8979FB1B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (e and not g xor a and d xor b xor c) xor a and (e xor d)
    xor g and d xor c;
  f := LTemp[20] + $D1310BA6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (d and not f xor h and c xor a xor b) xor h and (d xor c)
    xor f and c xor b;
  e := LTemp[22] + $98DFB5AC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (c and not e xor g and b xor h xor a) xor g and (c xor b)
    xor e and b xor a;
  d := LTemp[1] + $2FFD72DB + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (b and not d xor f and a xor g xor h) xor f and (b xor a)
    xor d and a xor h;
  c := LTemp[10] + $D01ADFB7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (a and not c xor e and h xor f xor g) xor e and (a xor h)
    xor c and h xor g;
  b := LTemp[4] + $B8E1AFED + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (h and not b xor d and g xor e xor f) xor d and (h xor g)
    xor b and g xor f;
  a := LTemp[8] + $6A267E96 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (g and not a xor c and f xor d xor e) xor c and (g xor f)
    xor a and f xor e;
  h := LTemp[30] + $BA7C9045 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (f and not h xor b and e xor c xor d) xor b and (f xor e)
    xor h and e xor d;
  g := LTemp[3] + $F12C7F99 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (e and not g xor a and d xor b xor c) xor a and (e xor d)
    xor g and d xor c;
  f := LTemp[21] + $24A19947 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (d and not f xor h and c xor a xor b) xor h and (d xor c)
    xor f and c xor b;
  e := LTemp[9] + $B3916CF7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (c and not e xor g and b xor h xor a) xor g and (c xor b)
    xor e and b xor a;
  d := LTemp[17] + $0801F2E2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (b and not d xor f and a xor g xor h) xor f and (b xor a)
    xor d and a xor h;
  c := LTemp[24] + $858EFC16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (a and not c xor e and h xor f xor g) xor e and (a xor h)
    xor c and h xor g;
  b := LTemp[29] + $636920D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (h and not b xor d and g xor e xor f) xor d and (h xor g)
    xor b and g xor f;
  a := LTemp[6] + $71574E69 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (g and not a xor c and f xor d xor e) xor c and (g xor f)
    xor a and f xor e;
  h := LTemp[19] + $A458FEA3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (f and not h xor b and e xor c xor d) xor b and (f xor e)
    xor h and e xor d;
  g := LTemp[12] + $F4933D7E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (e and not g xor a and d xor b xor c) xor a and (e xor d)
    xor g and d xor c;
  f := LTemp[15] + $0D95748F + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (d and not f xor h and c xor a xor b) xor h and (d xor c)
    xor f and c xor b;
  e := LTemp[13] + $728EB658 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (c and not e xor g and b xor h xor a) xor g and (c xor b)
    xor e and b xor a;
  d := LTemp[2] + $718BCD58 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (b and not d xor f and a xor g xor h) xor f and (b xor a)
    xor d and a xor h;
  c := LTemp[25] + $82154AEE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (a and not c xor e and h xor f xor g) xor e and (a xor h)
    xor c and h xor g;
  b := LTemp[31] + $7B54A41D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (h and not b xor d and g xor e xor f) xor d and (h xor g)
    xor b and g xor f;
  a := LTemp[27] + $C25A59B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := g and (c and a xor b xor f) xor c and d xor a and e xor f;
  h := LTemp[19] + $9C30D539 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := f and (b and h xor a xor e) xor b and c xor h and d xor e;
  g := LTemp[9] + $2AF26013 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := e and (a and g xor h xor d) xor a and b xor g and c xor d;
  f := LTemp[4] + $C5D1B023 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := d and (h and f xor g xor c) xor h and a xor f and b xor c;
  e := LTemp[20] + $286085F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := c and (g and e xor f xor b) xor g and h xor e and a xor b;
  d := LTemp[28] + $CA417918 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := b and (f and d xor e xor a) xor f and g xor d and h xor a;
  c := LTemp[17] + $B8DB38EF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := a and (e and c xor d xor h) xor e and f xor c and g xor h;
  b := LTemp[8] + $8E79DCB0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := h and (d and b xor c xor g) xor d and e xor b and f xor g;
  a := LTemp[22] + $603A180E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := g and (c and a xor b xor f) xor c and d xor a and e xor f;
  h := LTemp[29] + $6C9E0E8B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := f and (b and h xor a xor e) xor b and c xor h and d xor e;
  g := LTemp[14] + $B01E8A3E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := e and (a and g xor h xor d) xor a and b xor g and c xor d;
  f := LTemp[25] + $D71577C1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := d and (h and f xor g xor c) xor h and a xor f and b xor c;
  e := LTemp[12] + $BD314B27 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := c and (g and e xor f xor b) xor g and h xor e and a xor b;
  d := LTemp[24] + $78AF2FDA + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := b and (f and d xor e xor a) xor f and g xor d and h xor a;
  c := LTemp[30] + $55605C60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := a and (e and c xor d xor h) xor e and f xor c and g xor h;
  b := LTemp[16] + $E65525F3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := h and (d and b xor c xor g) xor d and e xor b and f xor g;
  a := LTemp[26] + $AA55AB94 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := g and (c and a xor b xor f) xor c and d xor a and e xor f;
  h := LTemp[31] + $57489862 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := f and (b and h xor a xor e) xor b and c xor h and d xor e;
  g := LTemp[15] + $63E81440 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := e and (a and g xor h xor d) xor a and b xor g and c xor d;
  f := LTemp[7] + $55CA396A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := d and (h and f xor g xor c) xor h and a xor f and b xor c;
  e := LTemp[3] + $2AAB10B6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := c and (g and e xor f xor b) xor g and h xor e and a xor b;
  d := LTemp[1] + $B4CC5C34 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := b and (f and d xor e xor a) xor f and g xor d and h xor a;
  c := LTemp[0] + $1141E8CE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := a and (e and c xor d xor h) xor e and f xor c and g xor h;
  b := LTemp[18] + $A15486AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := h and (d and b xor c xor g) xor d and e xor b and f xor g;
  a := LTemp[27] + $7C72E993 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := g and (c and a xor b xor f) xor c and d xor a and e xor f;
  h := LTemp[13] + $B3EE1411 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := f and (b and h xor a xor e) xor b and c xor h and d xor e;
  g := LTemp[6] + $636FBC2A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := e and (a and g xor h xor d) xor a and b xor g and c xor d;
  f := LTemp[21] + $2BA9C55D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := d and (h and f xor g xor c) xor h and a xor f and b xor c;
  e := LTemp[10] + $741831F6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := c and (g and e xor f xor b) xor g and h xor e and a xor b;
  d := LTemp[23] + $CE5C3E16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := b and (f and d xor e xor a) xor f and g xor d and h xor a;
  c := LTemp[11] + $9B87931E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := a and (e and c xor d xor h) xor e and f xor c and g xor h;
  b := LTemp[5] + $AFD6BA33 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := h and (d and b xor c xor g) xor d and e xor b and f xor g;
  a := LTemp[2] + $6C24CF5C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := a and (e and not c xor f and not g xor b xor g xor d) xor f and
    (b and c xor e xor g) xor c and g xor d;
  h := LTemp[24] + $7A325381 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := h and (d and not b xor e and not f xor a xor f xor c) xor e and
    (a and b xor d xor f) xor b and f xor c;
  g := LTemp[4] + $28958677 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := g and (c and not a xor d and not e xor h xor e xor b) xor d and
    (h and a xor c xor e) xor a and e xor b;
  f := LTemp[0] + $3B8F4898 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := f and (b and not h xor c and not d xor g xor d xor a) xor c and
    (g and h xor b xor d) xor h and d xor a;
  e := LTemp[14] + $6B4BB9AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := e and (a and not g xor b and not c xor f xor c xor h) xor b and
    (f and g xor a xor c) xor g and c xor h;
  d := LTemp[2] + $C4BFE81B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := d and (h and not f xor a and not b xor e xor b xor g) xor a and
    (e and f xor h xor b) xor f and b xor g;
  c := LTemp[7] + $66282193 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := c and (g and not e xor h and not a xor d xor a xor f) xor h and
    (d and e xor g xor a) xor e and a xor f;
  b := LTemp[28] + $61D809CC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := b and (f and not d xor g and not h xor c xor h xor e) xor g and
    (c and d xor f xor h) xor d and h xor e;
  a := LTemp[23] + $FB21A991 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := a and (e and not c xor f and not g xor b xor g xor d) xor f and
    (b and c xor e xor g) xor c and g xor d;
  h := LTemp[26] + $487CAC60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := h and (d and not b xor e and not f xor a xor f xor c) xor e and
    (a and b xor d xor f) xor b and f xor c;
  g := LTemp[6] + $5DEC8032 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := g and (c and not a xor d and not e xor h xor e xor b) xor d and
    (h and a xor c xor e) xor a and e xor b;
  f := LTemp[30] + $EF845D5D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := f and (b and not h xor c and not d xor g xor d xor a) xor c and
    (g and h xor b xor d) xor h and d xor a;
  e := LTemp[20] + $E98575B1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := e and (a and not g xor b and not c xor f xor c xor h) xor b and
    (f and g xor a xor c) xor g and c xor h;
  d := LTemp[18] + $DC262302 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := d and (h and not f xor a and not b xor e xor b xor g) xor a and
    (e and f xor h xor b) xor f and b xor g;
  c := LTemp[25] + $EB651B88 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := c and (g and not e xor h and not a xor d xor a xor f) xor h and
    (d and e xor g xor a) xor e and a xor f;
  b := LTemp[19] + $23893E81 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := b and (f and not d xor g and not h xor c xor h xor e) xor g and
    (c and d xor f xor h) xor d and h xor e;
  a := LTemp[3] + $D396ACC5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := a and (e and not c xor f and not g xor b xor g xor d) xor f and
    (b and c xor e xor g) xor c and g xor d;
  h := LTemp[22] + $0F6D6FF3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := h and (d and not b xor e and not f xor a xor f xor c) xor e and
    (a and b xor d xor f) xor b and f xor c;
  g := LTemp[11] + $83F44239 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := g and (c and not a xor d and not e xor h xor e xor b) xor d and
    (h and a xor c xor e) xor a and e xor b;
  f := LTemp[31] + $2E0B4482 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := f and (b and not h xor c and not d xor g xor d xor a) xor c and
    (g and h xor b xor d) xor h and d xor a;
  e := LTemp[21] + $A4842004 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := e and (a and not g xor b and not c xor f xor c xor h) xor b and
    (f and g xor a xor c) xor g and c xor h;
  d := LTemp[8] + $69C8F04A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := d and (h and not f xor a and not b xor e xor b xor g) xor a and
    (e and f xor h xor b) xor f and b xor g;
  c := LTemp[27] + $9E1F9B5E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := c and (g and not e xor h and not a xor d xor a xor f) xor h and
    (d and e xor g xor a) xor e and a xor f;
  b := LTemp[12] + $21C66842 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := b and (f and not d xor g and not h xor c xor h xor e) xor g and
    (c and d xor f xor h) xor d and h xor e;
  a := LTemp[9] + $F6E96C9A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := a and (e and not c xor f and not g xor b xor g xor d) xor f and
    (b and c xor e xor g) xor c and g xor d;
  h := LTemp[1] + $670C9C61 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := h and (d and not b xor e and not f xor a xor f xor c) xor e and
    (a and b xor d xor f) xor b and f xor c;
  g := LTemp[29] + $ABD388F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := g and (c and not a xor d and not e xor h xor e xor b) xor d and
    (h and a xor c xor e) xor a and e xor b;
  f := LTemp[5] + $6A51A0D2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := f and (b and not h xor c and not d xor g xor d xor a) xor c and
    (g and h xor b xor d) xor h and d xor a;
  e := LTemp[15] + $D8542F68 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := e and (a and not g xor b and not c xor f xor c xor h) xor b and
    (f and g xor a xor c) xor g and c xor h;
  d := LTemp[17] + $960FA728 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := d and (h and not f xor a and not b xor e xor b xor g) xor a and
    (e and f xor h xor b) xor f and b xor g;
  c := LTemp[10] + $AB5133A3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := c and (g and not e xor h and not a xor d xor a xor f) xor h and
    (d and e xor g xor a) xor e and a xor f;
  b := LTemp[16] + $6EEF0B6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := b and (f and not d xor g and not h xor c xor h xor e) xor g and
    (c and d xor f xor h) xor d and h xor e;
  a := LTemp[13] + $137A3BE4 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  FHash[0] := FHash[0] + a;
  FHash[1] := FHash[1] + b;
  FHash[2] := FHash[2] + c;
  FHash[3] := FHash[3] + d;
  FHash[4] := FHash[4] + e;
  FHash[5] := FHash[5] + f;
  FHash[6] := FHash[6] + g;
  FHash[7] := FHash[7] + h;

  System.FillChar(LTemp, System.SizeOf(LTemp), UInt32(0));
end;

{ THaval5 }

constructor THaval5.Create(AHashSize: THashSize);
begin
  inherited Create(THashRounds.hrRounds5, AHashSize);
end;

procedure THaval5.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  a, b, c, d, e, f, g, h, t: UInt32;
  LTemp: array [0 .. 31] of UInt32;
begin
  TConverters.le32_copy(AData, AIndex, @(LTemp[0]), 0, ADataLength);

  a := FHash[0];
  b := FHash[1];
  c := FHash[2];
  d := FHash[3];
  e := FHash[4];
  f := FHash[5];
  g := FHash[6];
  h := FHash[7];

  t := c and (g xor b) xor f and e xor a and d xor g;
  h := LTemp[0] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);
  t := b and (f xor a) xor e and d xor h and c xor f;
  g := LTemp[1] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (e xor h) xor d and c xor g and b xor e;
  f := LTemp[2] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (d xor g) xor c and b xor f and a xor d;
  e := LTemp[3] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (c xor f) xor b and a xor e and h xor c;
  d := LTemp[4] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (b xor e) xor a and h xor d and g xor b;
  c := LTemp[5] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (a xor d) xor h and g xor c and f xor a;
  b := LTemp[6] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (h xor c) xor g and f xor b and e xor h;
  a := LTemp[7] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (g xor b) xor f and e xor a and d xor g;
  h := LTemp[8] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (f xor a) xor e and d xor h and c xor f;
  g := LTemp[9] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (e xor h) xor d and c xor g and b xor e;
  f := LTemp[10] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (d xor g) xor c and b xor f and a xor d;
  e := LTemp[11] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (c xor f) xor b and a xor e and h xor c;
  d := LTemp[12] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (b xor e) xor a and h xor d and g xor b;
  c := LTemp[13] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (a xor d) xor h and g xor c and f xor a;
  b := LTemp[14] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (h xor c) xor g and f xor b and e xor h;
  a := LTemp[15] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (g xor b) xor f and e xor a and d xor g;
  h := LTemp[16] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (f xor a) xor e and d xor h and c xor f;
  g := LTemp[17] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (e xor h) xor d and c xor g and b xor e;
  f := LTemp[18] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (d xor g) xor c and b xor f and a xor d;
  e := LTemp[19] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (c xor f) xor b and a xor e and h xor c;
  d := LTemp[20] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (b xor e) xor a and h xor d and g xor b;
  c := LTemp[21] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (a xor d) xor h and g xor c and f xor a;
  b := LTemp[22] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (h xor c) xor g and f xor b and e xor h;
  a := LTemp[23] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (g xor b) xor f and e xor a and d xor g;
  h := LTemp[24] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (f xor a) xor e and d xor h and c xor f;
  g := LTemp[25] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (e xor h) xor d and c xor g and b xor e;
  f := LTemp[26] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (d xor g) xor c and b xor f and a xor d;
  e := LTemp[27] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (c xor f) xor b and a xor e and h xor c;
  d := LTemp[28] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (b xor e) xor a and h xor d and g xor b;
  c := LTemp[29] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (a xor d) xor h and g xor c and f xor a;
  b := LTemp[30] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (h xor c) xor g and f xor b and e xor h;
  a := LTemp[31] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := d and (e and not a xor b and c xor g xor f) xor b and (e xor c)
    xor a and c xor f;
  h := LTemp[5] + $452821E6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (d and not h xor a and b xor f xor e) xor a and (d xor b)
    xor h and b xor e;
  g := LTemp[14] + $38D01377 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (c and not g xor h and a xor e xor d) xor h and (c xor a)
    xor g and a xor d;
  f := LTemp[26] + $BE5466CF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (b and not f xor g and h xor d xor c) xor g and (b xor h)
    xor f and h xor c;
  e := LTemp[18] + $34E90C6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (a and not e xor f and g xor c xor b) xor f and (a xor g)
    xor e and g xor b;
  d := LTemp[11] + $C0AC29B7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (h and not d xor e and f xor b xor a) xor e and (h xor f)
    xor d and f xor a;
  c := LTemp[28] + $C97C50DD + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (g and not c xor d and e xor a xor h) xor d and (g xor e)
    xor c and e xor h;
  b := LTemp[7] + $3F84D5B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (f and not b xor c and d xor h xor g) xor c and (f xor d)
    xor b and d xor g;
  a := LTemp[16] + $B5470917 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (e and not a xor b and c xor g xor f) xor b and (e xor c)
    xor a and c xor f;
  h := LTemp[0] + $9216D5D9 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (d and not h xor a and b xor f xor e) xor a and (d xor b)
    xor h and b xor e;
  g := LTemp[23] + $8979FB1B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (c and not g xor h and a xor e xor d) xor h and (c xor a)
    xor g and a xor d;
  f := LTemp[20] + $D1310BA6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (b and not f xor g and h xor d xor c) xor g and (b xor h)
    xor f and h xor c;
  e := LTemp[22] + $98DFB5AC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (a and not e xor f and g xor c xor b) xor f and (a xor g)
    xor e and g xor b;
  d := LTemp[1] + $2FFD72DB + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (h and not d xor e and f xor b xor a) xor e and (h xor f)
    xor d and f xor a;
  c := LTemp[10] + $D01ADFB7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (g and not c xor d and e xor a xor h) xor d and (g xor e)
    xor c and e xor h;
  b := LTemp[4] + $B8E1AFED + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (f and not b xor c and d xor h xor g) xor c and (f xor d)
    xor b and d xor g;
  a := LTemp[8] + $6A267E96 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (e and not a xor b and c xor g xor f) xor b and (e xor c)
    xor a and c xor f;
  h := LTemp[30] + $BA7C9045 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (d and not h xor a and b xor f xor e) xor a and (d xor b)
    xor h and b xor e;
  g := LTemp[3] + $F12C7F99 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (c and not g xor h and a xor e xor d) xor h and (c xor a)
    xor g and a xor d;
  f := LTemp[21] + $24A19947 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (b and not f xor g and h xor d xor c) xor g and (b xor h)
    xor f and h xor c;
  e := LTemp[9] + $B3916CF7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (a and not e xor f and g xor c xor b) xor f and (a xor g)
    xor e and g xor b;
  d := LTemp[17] + $0801F2E2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (h and not d xor e and f xor b xor a) xor e and (h xor f)
    xor d and f xor a;
  c := LTemp[24] + $858EFC16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (g and not c xor d and e xor a xor h) xor d and (g xor e)
    xor c and e xor h;
  b := LTemp[29] + $636920D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (f and not b xor c and d xor h xor g) xor c and (f xor d)
    xor b and d xor g;
  a := LTemp[6] + $71574E69 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (e and not a xor b and c xor g xor f) xor b and (e xor c)
    xor a and c xor f;
  h := LTemp[19] + $A458FEA3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (d and not h xor a and b xor f xor e) xor a and (d xor b)
    xor h and b xor e;
  g := LTemp[12] + $F4933D7E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (c and not g xor h and a xor e xor d) xor h and (c xor a)
    xor g and a xor d;
  f := LTemp[15] + $0D95748F + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (b and not f xor g and h xor d xor c) xor g and (b xor h)
    xor f and h xor c;
  e := LTemp[13] + $728EB658 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (a and not e xor f and g xor c xor b) xor f and (a xor g)
    xor e and g xor b;
  d := LTemp[2] + $718BCD58 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (h and not d xor e and f xor b xor a) xor e and (h xor f)
    xor d and f xor a;
  c := LTemp[25] + $82154AEE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (g and not c xor d and e xor a xor h) xor d and (g xor e)
    xor c and e xor h;
  b := LTemp[31] + $7B54A41D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (f and not b xor c and d xor h xor g) xor c and (f xor d)
    xor b and d xor g;
  a := LTemp[27] + $C25A59B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := e and (b and d xor c xor f) xor b and a xor d and g xor f;
  h := LTemp[19] + $9C30D539 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := d and (a and c xor b xor e) xor a and h xor c and f xor e;
  g := LTemp[9] + $2AF26013 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := c and (h and b xor a xor d) xor h and g xor b and e xor d;
  f := LTemp[4] + $C5D1B023 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := b and (g and a xor h xor c) xor g and f xor a and d xor c;
  e := LTemp[20] + $286085F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := a and (f and h xor g xor b) xor f and e xor h and c xor b;
  d := LTemp[28] + $CA417918 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := h and (e and g xor f xor a) xor e and d xor g and b xor a;
  c := LTemp[17] + $B8DB38EF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := g and (d and f xor e xor h) xor d and c xor f and a xor h;
  b := LTemp[8] + $8E79DCB0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := f and (c and e xor d xor g) xor c and b xor e and h xor g;
  a := LTemp[22] + $603A180E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := e and (b and d xor c xor f) xor b and a xor d and g xor f;
  h := LTemp[29] + $6C9E0E8B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := d and (a and c xor b xor e) xor a and h xor c and f xor e;
  g := LTemp[14] + $B01E8A3E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := c and (h and b xor a xor d) xor h and g xor b and e xor d;
  f := LTemp[25] + $D71577C1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := b and (g and a xor h xor c) xor g and f xor a and d xor c;
  e := LTemp[12] + $BD314B27 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := a and (f and h xor g xor b) xor f and e xor h and c xor b;
  d := LTemp[24] + $78AF2FDA + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := h and (e and g xor f xor a) xor e and d xor g and b xor a;
  c := LTemp[30] + $55605C60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := g and (d and f xor e xor h) xor d and c xor f and a xor h;
  b := LTemp[16] + $E65525F3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := f and (c and e xor d xor g) xor c and b xor e and h xor g;
  a := LTemp[26] + $AA55AB94 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := e and (b and d xor c xor f) xor b and a xor d and g xor f;
  h := LTemp[31] + $57489862 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := d and (a and c xor b xor e) xor a and h xor c and f xor e;
  g := LTemp[15] + $63E81440 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := c and (h and b xor a xor d) xor h and g xor b and e xor d;
  f := LTemp[7] + $55CA396A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := b and (g and a xor h xor c) xor g and f xor a and d xor c;
  e := LTemp[3] + $2AAB10B6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := a and (f and h xor g xor b) xor f and e xor h and c xor b;
  d := LTemp[1] + $B4CC5C34 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := h and (e and g xor f xor a) xor e and d xor g and b xor a;
  c := LTemp[0] + $1141E8CE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := g and (d and f xor e xor h) xor d and c xor f and a xor h;
  b := LTemp[18] + $A15486AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := f and (c and e xor d xor g) xor c and b xor e and h xor g;
  a := LTemp[27] + $7C72E993 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := e and (b and d xor c xor f) xor b and a xor d and g xor f;
  h := LTemp[13] + $B3EE1411 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := d and (a and c xor b xor e) xor a and h xor c and f xor e;
  g := LTemp[6] + $636FBC2A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := c and (h and b xor a xor d) xor h and g xor b and e xor d;
  f := LTemp[21] + $2BA9C55D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := b and (g and a xor h xor c) xor g and f xor a and d xor c;
  e := LTemp[10] + $741831F6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := a and (f and h xor g xor b) xor f and e xor h and c xor b;
  d := LTemp[23] + $CE5C3E16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := h and (e and g xor f xor a) xor e and d xor g and b xor a;
  c := LTemp[11] + $9B87931E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := g and (d and f xor e xor h) xor d and c xor f and a xor h;
  b := LTemp[5] + $AFD6BA33 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := f and (c and e xor d xor g) xor c and b xor e and h xor g;
  a := LTemp[2] + $6C24CF5C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and not a xor c and not b xor e xor b xor g) xor c and
    (e and a xor f xor b) xor a and b xor g;
  h := LTemp[24] + $7A325381 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and not h xor b and not a xor d xor a xor f) xor b and
    (d and h xor e xor a) xor h and a xor f;
  g := LTemp[4] + $28958677 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and not g xor a and not h xor c xor h xor e) xor a and
    (c and g xor d xor h) xor g and h xor e;
  f := LTemp[0] + $3B8F4898 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and not f xor h and not g xor b xor g xor d) xor h and
    (b and f xor c xor g) xor f and g xor d;
  e := LTemp[14] + $6B4BB9AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and not e xor g and not f xor a xor f xor c) xor g and
    (a and e xor b xor f) xor e and f xor c;
  d := LTemp[2] + $C4BFE81B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and not d xor f and not e xor h xor e xor b) xor f and
    (h and d xor a xor e) xor d and e xor b;
  c := LTemp[7] + $66282193 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and not c xor e and not d xor g xor d xor a) xor e and
    (g and c xor h xor d) xor c and d xor a;
  b := LTemp[28] + $61D809CC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and not b xor d and not c xor f xor c xor h) xor d and
    (f and b xor g xor c) xor b and c xor h;
  a := LTemp[23] + $FB21A991 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and not a xor c and not b xor e xor b xor g) xor c and
    (e and a xor f xor b) xor a and b xor g;
  h := LTemp[26] + $487CAC60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and not h xor b and not a xor d xor a xor f) xor b and
    (d and h xor e xor a) xor h and a xor f;
  g := LTemp[6] + $5DEC8032 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and not g xor a and not h xor c xor h xor e) xor a and
    (c and g xor d xor h) xor g and h xor e;
  f := LTemp[30] + $EF845D5D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and not f xor h and not g xor b xor g xor d) xor h and
    (b and f xor c xor g) xor f and g xor d;
  e := LTemp[20] + $E98575B1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and not e xor g and not f xor a xor f xor c) xor g and
    (a and e xor b xor f) xor e and f xor c;
  d := LTemp[18] + $DC262302 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and not d xor f and not e xor h xor e xor b) xor f and
    (h and d xor a xor e) xor d and e xor b;
  c := LTemp[25] + $EB651B88 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and not c xor e and not d xor g xor d xor a) xor e and
    (g and c xor h xor d) xor c and d xor a;
  b := LTemp[19] + $23893E81 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and not b xor d and not c xor f xor c xor h) xor d and
    (f and b xor g xor c) xor b and c xor h;
  a := LTemp[3] + $D396ACC5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and not a xor c and not b xor e xor b xor g) xor c and
    (e and a xor f xor b) xor a and b xor g;
  h := LTemp[22] + $0F6D6FF3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and not h xor b and not a xor d xor a xor f) xor b and
    (d and h xor e xor a) xor h and a xor f;
  g := LTemp[11] + $83F44239 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and not g xor a and not h xor c xor h xor e) xor a and
    (c and g xor d xor h) xor g and h xor e;
  f := LTemp[31] + $2E0B4482 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and not f xor h and not g xor b xor g xor d) xor h and
    (b and f xor c xor g) xor f and g xor d;
  e := LTemp[21] + $A4842004 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and not e xor g and not f xor a xor f xor c) xor g and
    (a and e xor b xor f) xor e and f xor c;
  d := LTemp[8] + $69C8F04A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and not d xor f and not e xor h xor e xor b) xor f and
    (h and d xor a xor e) xor d and e xor b;
  c := LTemp[27] + $9E1F9B5E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and not c xor e and not d xor g xor d xor a) xor e and
    (g and c xor h xor d) xor c and d xor a;
  b := LTemp[12] + $21C66842 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and not b xor d and not c xor f xor c xor h) xor d and
    (f and b xor g xor c) xor b and c xor h;
  a := LTemp[9] + $F6E96C9A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and not a xor c and not b xor e xor b xor g) xor c and
    (e and a xor f xor b) xor a and b xor g;
  h := LTemp[1] + $670C9C61 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and not h xor b and not a xor d xor a xor f) xor b and
    (d and h xor e xor a) xor h and a xor f;
  g := LTemp[29] + $ABD388F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and not g xor a and not h xor c xor h xor e) xor a and
    (c and g xor d xor h) xor g and h xor e;
  f := LTemp[5] + $6A51A0D2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and not f xor h and not g xor b xor g xor d) xor h and
    (b and f xor c xor g) xor f and g xor d;
  e := LTemp[15] + $D8542F68 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and not e xor g and not f xor a xor f xor c) xor g and
    (a and e xor b xor f) xor e and f xor c;
  d := LTemp[17] + $960FA728 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and not d xor f and not e xor h xor e xor b) xor f and
    (h and d xor a xor e) xor d and e xor b;
  c := LTemp[10] + $AB5133A3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and not c xor e and not d xor g xor d xor a) xor e and
    (g and c xor h xor d) xor c and d xor a;
  b := LTemp[16] + $6EEF0B6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and not b xor d and not c xor f xor c xor h) xor d and
    (f and b xor g xor c) xor b and c xor h;
  a := LTemp[13] + $137A3BE4 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (d and e and g xor not f) xor d and a xor e and f xor g and c;
  h := LTemp[27] + $BA3BF050 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (c and d and f xor not e) xor c and h xor d and e xor f and b;
  g := LTemp[3] + $7EFB2A98 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (b and c and e xor not d) xor b and g xor c and d xor e and a;
  f := LTemp[21] + $A1F1651D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (a and b and d xor not c) xor a and f xor b and c xor d and h;
  e := LTemp[26] + $39AF0176 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (h and a and c xor not b) xor h and e xor a and b xor c and g;
  d := LTemp[17] + $66CA593E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (g and h and b xor not a) xor g and d xor h and a xor b and f;
  c := LTemp[11] + $82430E88 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (f and g and a xor not h) xor f and c xor g and h xor a and e;
  b := LTemp[20] + $8CEE8619 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (e and f and h xor not g) xor e and b xor f and g xor h and d;
  a := LTemp[29] + $456F9FB4 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (d and e and g xor not f) xor d and a xor e and f xor g and c;
  h := LTemp[19] + $7D84A5C3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (c and d and f xor not e) xor c and h xor d and e xor f and b;
  g := LTemp[0] + $3B8B5EBE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (b and c and e xor not d) xor b and g xor c and d xor e and a;
  f := LTemp[12] + $E06F75D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (a and b and d xor not c) xor a and f xor b and c xor d and h;
  e := LTemp[7] + $85C12073 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (h and a and c xor not b) xor h and e xor a and b xor c and g;
  d := LTemp[13] + $401A449F + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (g and h and b xor not a) xor g and d xor h and a xor b and f;
  c := LTemp[8] + $56C16AA6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (f and g and a xor not h) xor f and c xor g and h xor a and e;
  b := LTemp[31] + $4ED3AA62 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (e and f and h xor not g) xor e and b xor f and g xor h and d;
  a := LTemp[10] + $363F7706 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (d and e and g xor not f) xor d and a xor e and f xor g and c;
  h := LTemp[5] + $1BFEDF72 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (c and d and f xor not e) xor c and h xor d and e xor f and b;
  g := LTemp[9] + $429B023D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (b and c and e xor not d) xor b and g xor c and d xor e and a;
  f := LTemp[14] + $37D0D724 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (a and b and d xor not c) xor a and f xor b and c xor d and h;
  e := LTemp[30] + $D00A1248 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (h and a and c xor not b) xor h and e xor a and b xor c and g;
  d := LTemp[18] + $DB0FEAD3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (g and h and b xor not a) xor g and d xor h and a xor b and f;
  c := LTemp[6] + $49F1C09B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (f and g and a xor not h) xor f and c xor g and h xor a and e;
  b := LTemp[28] + $075372C9 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (e and f and h xor not g) xor e and b xor f and g xor h and d;
  a := LTemp[24] + $80991B7B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (d and e and g xor not f) xor d and a xor e and f xor g and c;
  h := LTemp[2] + $25D479D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (c and d and f xor not e) xor c and h xor d and e xor f and b;
  g := LTemp[23] + $F6E8DEF7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (b and c and e xor not d) xor b and g xor c and d xor e and a;
  f := LTemp[16] + $E3FE501A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (a and b and d xor not c) xor a and f xor b and c xor d and h;
  e := LTemp[22] + $B6794C3B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (h and a and c xor not b) xor h and e xor a and b xor c and g;
  d := LTemp[4] + $976CE0BD + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (g and h and b xor not a) xor g and d xor h and a xor b and f;
  c := LTemp[1] + $04C006BA + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (f and g and a xor not h) xor f and c xor g and h xor a and e;
  b := LTemp[25] + $C1A94FB6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (e and f and h xor not g) xor e and b xor f and g xor h and d;
  a := LTemp[15] + $409F60C4 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  FHash[0] := FHash[0] + a;
  FHash[1] := FHash[1] + b;
  FHash[2] := FHash[2] + c;
  FHash[3] := FHash[3] + d;
  FHash[4] := FHash[4] + e;
  FHash[5] := FHash[5] + f;
  FHash[6] := FHash[6] + g;
  FHash[7] := FHash[7] + h;

  System.FillChar(LTemp, System.SizeOf(LTemp), UInt32(0));
end;

{ THaval_3_128 }

function THaval_3_128.Clone(): IHash;
var
  LHashInstance: THaval_3_128;
begin
  LHashInstance := THaval_3_128.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_128.Create;
begin
  inherited Create(THashSize.hsHashSize128);
end;

{ THaval_4_128 }

function THaval_4_128.Clone(): IHash;
var
  LHashInstance: THaval_4_128;
begin
  LHashInstance := THaval_4_128.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_128.Create;
begin
  inherited Create(THashSize.hsHashSize128);
end;

{ THaval_5_128 }

function THaval_5_128.Clone(): IHash;
var
  LHashInstance: THaval_5_128;
begin
  LHashInstance := THaval_5_128.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_128.Create;
begin
  inherited Create(THashSize.hsHashSize128);
end;

{ THaval_3_160 }

function THaval_3_160.Clone(): IHash;
var
  LHashInstance: THaval_3_160;
begin
  LHashInstance := THaval_3_160.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_160.Create;
begin
  inherited Create(THashSize.hsHashSize160);
end;

{ THaval_4_160 }

function THaval_4_160.Clone(): IHash;
var
  LHashInstance: THaval_4_160;
begin
  LHashInstance := THaval_4_160.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_160.Create;
begin
  inherited Create(THashSize.hsHashSize160);
end;

{ THaval_5_160 }

function THaval_5_160.Clone(): IHash;
var
  LHashInstance: THaval_5_160;
begin
  LHashInstance := THaval_5_160.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_160.Create;
begin
  inherited Create(THashSize.hsHashSize160);
end;

{ THaval_3_192 }

function THaval_3_192.Clone(): IHash;
var
  LHashInstance: THaval_3_192;
begin
  LHashInstance := THaval_3_192.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_192.Create;
begin
  inherited Create(THashSize.hsHashSize192);
end;

{ THaval_4_192 }

function THaval_4_192.Clone(): IHash;
var
  LHashInstance: THaval_4_192;
begin
  LHashInstance := THaval_4_192.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_192.Create;
begin
  inherited Create(THashSize.hsHashSize192);
end;

{ THaval_5_192 }

function THaval_5_192.Clone(): IHash;
var
  LHashInstance: THaval_5_192;
begin
  LHashInstance := THaval_5_192.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_192.Create;
begin
  inherited Create(THashSize.hsHashSize192);
end;

{ THaval_3_224 }

function THaval_3_224.Clone(): IHash;
var
  LHashInstance: THaval_3_224;
begin
  LHashInstance := THaval_3_224.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_224.Create;
begin
  inherited Create(THashSize.hsHashSize224);
end;

{ THaval_4_224 }

function THaval_4_224.Clone(): IHash;
var
  LHashInstance: THaval_4_224;
begin
  LHashInstance := THaval_4_224.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_224.Create;
begin
  inherited Create(THashSize.hsHashSize224);
end;

{ THaval_5_224 }

function THaval_5_224.Clone(): IHash;
var
  LHashInstance: THaval_5_224;
begin
  LHashInstance := THaval_5_224.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_224.Create;
begin
  inherited Create(THashSize.hsHashSize224);
end;

{ THaval_3_256 }

function THaval_3_256.Clone(): IHash;
var
  LHashInstance: THaval_3_256;
begin
  LHashInstance := THaval_3_256.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_256.Create;
begin
  inherited Create(THashSize.hsHashSize256);
end;

{ THaval_4_256 }

function THaval_4_256.Clone(): IHash;
var
  LHashInstance: THaval_4_256;
begin
  LHashInstance := THaval_4_256.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_256.Create;
begin
  inherited Create(THashSize.hsHashSize256);
end;

{ THaval_5_256 }

function THaval_5_256.Clone(): IHash;
var
  LHashInstance: THaval_5_256;
begin
  LHashInstance := THaval_5_256.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_256.Create;
begin
  inherited Create(THashSize.hsHashSize256);
end;

end.
