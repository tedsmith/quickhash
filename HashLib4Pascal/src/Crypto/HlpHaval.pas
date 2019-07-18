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
  HlpBitConverter,
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

    procedure TailorDigestBits();

  strict protected
    Fm_rounds, FHashSize: Int32;
    Fm_hash: THashLibUInt32Array;

    constructor Create(a_rounds: THashRounds; a_hash_size: THashSize);

    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;

  public
    procedure Initialize(); override;
  end;

type

  THaval3 = class abstract(THaval)

  strict protected
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create(a_hash_size: THashSize);

  end;

type

  THaval4 = class abstract(THaval)

  strict protected
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create(a_hash_size: THashSize);

  end;

type

  THaval5 = class abstract(THaval)

  strict protected
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create(a_hash_size: THashSize);

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

constructor THaval.Create(a_rounds: THashRounds; a_hash_size: THashSize);
begin
  inherited Create(Int32(a_hash_size), 128);
  FHashSize := HashSize;
  System.SetLength(Fm_hash, 8);
  Fm_rounds := Int32(a_rounds);

end;

procedure THaval.Finish;
var
  bits: UInt64;
  padindex: Int32;
  pad: THashLibByteArray;
begin
  bits := Fm_processed_bytes * 8;
  if (Fm_buffer.Pos < 118) then
    padindex := (118 - Fm_buffer.Pos)
  else
    padindex := (246 - Fm_buffer.Pos);
  System.SetLength(pad, padindex + 10);

  pad[0] := Byte($01);

  pad[padindex] := Byte((Fm_rounds shl 3) or (HAVAL_VERSION and $07));
  System.Inc(padindex);
  pad[padindex] := Byte(FHashSize shl 1);
  System.Inc(padindex);

  bits := TConverters.le2me_64(bits);

  TConverters.ReadUInt64AsBytesLE(bits, pad, padindex);

  padindex := padindex + 8;

  TransformBytes(pad, 0, padindex);

end;

function THaval.GetResult: THashLibByteArray;
begin
  TailorDigestBits();

  System.SetLength(result, (FHashSize shr 2) * System.SizeOf(UInt32));

  TConverters.le32_copy(PCardinal(Fm_hash), 0, PByte(result), 0,
    System.Length(result));

end;

procedure THaval.Initialize;
begin
  Fm_hash[0] := $243F6A88;
  Fm_hash[1] := $85A308D3;
  Fm_hash[2] := $13198A2E;
  Fm_hash[3] := $03707344;
  Fm_hash[4] := $A4093822;
  Fm_hash[5] := $299F31D0;
  Fm_hash[6] := $082EFA98;
  Fm_hash[7] := $EC4E6C89;

  inherited Initialize();

end;

procedure THaval.TailorDigestBits;
var
  t: UInt32;
begin

  case FHashSize of

    16:
      begin
        t := (Fm_hash[7] and $000000FF) or (Fm_hash[6] and $FF000000) or
          (Fm_hash[5] and $00FF0000) or (Fm_hash[4] and $0000FF00);
        Fm_hash[0] := Fm_hash[0] + TBits.RotateRight32(t, 8);
        t := (Fm_hash[7] and $0000FF00) or (Fm_hash[6] and $000000FF) or
          (Fm_hash[5] and $FF000000) or (Fm_hash[4] and $00FF0000);
        Fm_hash[1] := Fm_hash[1] + TBits.RotateRight32(t, 16);
        t := (Fm_hash[7] and $00FF0000) or (Fm_hash[6] and $0000FF00) or
          (Fm_hash[5] and $000000FF) or (Fm_hash[4] and $FF000000);
        Fm_hash[2] := Fm_hash[2] + TBits.RotateRight32(t, 24);
        t := (Fm_hash[7] and $FF000000) or (Fm_hash[6] and $00FF0000) or
          (Fm_hash[5] and $0000FF00) or (Fm_hash[4] and $000000FF);
        Fm_hash[3] := Fm_hash[3] + t;
      end;

    20:
      begin
        t := UInt32(Fm_hash[7] and $3F) or UInt32(Fm_hash[6] and ($7F shl 25))
          or UInt32(Fm_hash[5] and ($3F shl 19));
        Fm_hash[0] := Fm_hash[0] + TBits.RotateRight32(t, 19);
        t := UInt32(Fm_hash[7] and ($3F shl 6)) or UInt32(Fm_hash[6] and $3F) or
          UInt32(Fm_hash[5] and ($7F shl 25));
        Fm_hash[1] := Fm_hash[1] + TBits.RotateRight32(t, 25);
        t := (Fm_hash[7] and ($7F shl 12)) or (Fm_hash[6] and ($3F shl 6)) or
          (Fm_hash[5] and $3F);
        Fm_hash[2] := Fm_hash[2] + t;
        t := (Fm_hash[7] and ($3F shl 19)) or (Fm_hash[6] and ($7F shl 12)) or
          (Fm_hash[5] and ($3F shl 6));
        Fm_hash[3] := Fm_hash[3] + (t shr 6);
        t := (Fm_hash[7] and (UInt32($7F) shl 25)) or
          UInt32(Fm_hash[6] and ($3F shl 19)) or
          UInt32(Fm_hash[5] and ($7F shl 12));
        Fm_hash[4] := Fm_hash[4] + (t shr 12);
      end;

    24:
      begin
        t := UInt32(Fm_hash[7] and $1F) or UInt32(Fm_hash[6] and ($3F shl 26));
        Fm_hash[0] := Fm_hash[0] + TBits.RotateRight32(t, 26);
        t := (Fm_hash[7] and ($1F shl 5)) or (Fm_hash[6] and $1F);
        Fm_hash[1] := Fm_hash[1] + t;
        t := (Fm_hash[7] and ($3F shl 10)) or (Fm_hash[6] and ($1F shl 5));
        Fm_hash[2] := Fm_hash[2] + (t shr 5);
        t := (Fm_hash[7] and ($1F shl 16)) or (Fm_hash[6] and ($3F shl 10));
        Fm_hash[3] := Fm_hash[3] + (t shr 10);
        t := (Fm_hash[7] and ($1F shl 21)) or (Fm_hash[6] and ($1F shl 16));
        Fm_hash[4] := Fm_hash[4] + (t shr 16);
        t := UInt32(Fm_hash[7] and ($3F shl 26)) or
          UInt32(Fm_hash[6] and ($1F shl 21));
        Fm_hash[5] := Fm_hash[5] + (t shr 21);
      end;

    28:
      begin
        Fm_hash[0] := Fm_hash[0] + ((Fm_hash[7] shr 27) and $1F);
        Fm_hash[1] := Fm_hash[1] + ((Fm_hash[7] shr 22) and $1F);
        Fm_hash[2] := Fm_hash[2] + ((Fm_hash[7] shr 18) and $0F);
        Fm_hash[3] := Fm_hash[3] + ((Fm_hash[7] shr 13) and $1F);
        Fm_hash[4] := Fm_hash[4] + ((Fm_hash[7] shr 9) and $0F);
        Fm_hash[5] := Fm_hash[5] + ((Fm_hash[7] shr 4) and $1F);
        Fm_hash[6] := Fm_hash[6] + (Fm_hash[7] and $0F);
      end;

  end;

end;

{ THaval3 }

constructor THaval3.Create(a_hash_size: THashSize);
begin
  inherited Create(THashRounds.hrRounds3, a_hash_size);
end;

procedure THaval3.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  a, b, c, d, e, f, g, h, t: UInt32;
  temp: array [0 .. 31] of UInt32;
begin

  TConverters.le32_copy(a_data, a_index, @(temp[0]), 0, a_data_length);

  a := Fm_hash[0];
  b := Fm_hash[1];
  c := Fm_hash[2];
  d := Fm_hash[3];
  e := Fm_hash[4];
  f := Fm_hash[5];
  g := Fm_hash[6];
  h := Fm_hash[7];

  t := c and (e xor d) xor g and a xor f and b xor e;
  h := temp[0] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (d xor c) xor f and h xor e and a xor d;
  g := temp[1] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (c xor b) xor e and g xor d and h xor c;
  f := temp[2] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (b xor a) xor d and f xor c and g xor b;
  e := temp[3] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (a xor h) xor c and e xor b and f xor a;
  d := temp[4] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (h xor g) xor b and d xor a and e xor h;
  c := temp[5] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (g xor f) xor a and c xor h and d xor g;
  b := temp[6] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (f xor e) xor h and b xor g and c xor f;
  a := temp[7] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (e xor d) xor g and a xor f and b xor e;
  h := temp[8] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (d xor c) xor f and h xor e and a xor d;
  g := temp[9] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (c xor b) xor e and g xor d and h xor c;
  f := temp[10] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (b xor a) xor d and f xor c and g xor b;
  e := temp[11] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (a xor h) xor c and e xor b and f xor a;
  d := temp[12] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (h xor g) xor b and d xor a and e xor h;
  c := temp[13] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (g xor f) xor a and c xor h and d xor g;
  b := temp[14] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (f xor e) xor h and b xor g and c xor f;
  a := temp[15] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (e xor d) xor g and a xor f and b xor e;
  h := temp[16] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (d xor c) xor f and h xor e and a xor d;
  g := temp[17] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (c xor b) xor e and g xor d and h xor c;
  f := temp[18] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (b xor a) xor d and f xor c and g xor b;
  e := temp[19] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (a xor h) xor c and e xor b and f xor a;
  d := temp[20] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (h xor g) xor b and d xor a and e xor h;
  c := temp[21] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (g xor f) xor a and c xor h and d xor g;
  b := temp[22] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (f xor e) xor h and b xor g and c xor f;
  a := temp[23] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (e xor d) xor g and a xor f and b xor e;
  h := temp[24] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (d xor c) xor f and h xor e and a xor d;
  g := temp[25] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (c xor b) xor e and g xor d and h xor c;
  f := temp[26] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (b xor a) xor d and f xor c and g xor b;
  e := temp[27] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (a xor h) xor c and e xor b and f xor a;
  d := temp[28] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (h xor g) xor b and d xor a and e xor h;
  c := temp[29] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (g xor f) xor a and c xor h and d xor g;
  b := temp[30] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (f xor e) xor h and b xor g and c xor f;
  a := temp[31] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := f and (d and not a xor b and c xor e xor g) xor b and (d xor c)
    xor a and c xor g;
  h := temp[5] + $452821E6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := e and (c and not h xor a and b xor d xor f) xor a and (c xor b)
    xor h and b xor f;
  g := temp[14] + $38D01377 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := d and (b and not g xor h and a xor c xor e) xor h and (b xor a)
    xor g and a xor e;
  f := temp[26] + $BE5466CF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := c and (a and not f xor g and h xor b xor d) xor g and (a xor h)
    xor f and h xor d;
  e := temp[18] + $34E90C6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := b and (h and not e xor f and g xor a xor c) xor f and (h xor g)
    xor e and g xor c;
  d := temp[11] + $C0AC29B7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := a and (g and not d xor e and f xor h xor b) xor e and (g xor f)
    xor d and f xor b;
  c := temp[28] + $C97C50DD + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := h and (f and not c xor d and e xor g xor a) xor d and (f xor e)
    xor c and e xor a;
  b := temp[7] + $3F84D5B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := g and (e and not b xor c and d xor f xor h) xor c and (e xor d)
    xor b and d xor h;
  a := temp[16] + $B5470917 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := f and (d and not a xor b and c xor e xor g) xor b and (d xor c)
    xor a and c xor g;
  h := temp[0] + $9216D5D9 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := e and (c and not h xor a and b xor d xor f) xor a and (c xor b)
    xor h and b xor f;
  g := temp[23] + $8979FB1B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := d and (b and not g xor h and a xor c xor e) xor h and (b xor a)
    xor g and a xor e;
  f := temp[20] + $D1310BA6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := c and (a and not f xor g and h xor b xor d) xor g and (a xor h)
    xor f and h xor d;
  e := temp[22] + $98DFB5AC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := b and (h and not e xor f and g xor a xor c) xor f and (h xor g)
    xor e and g xor c;
  d := temp[1] + $2FFD72DB + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := a and (g and not d xor e and f xor h xor b) xor e and (g xor f)
    xor d and f xor b;
  c := temp[10] + $D01ADFB7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := h and (f and not c xor d and e xor g xor a) xor d and (f xor e)
    xor c and e xor a;
  b := temp[4] + $B8E1AFED + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := g and (e and not b xor c and d xor f xor h) xor c and (e xor d)
    xor b and d xor h;
  a := temp[8] + $6A267E96 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := f and (d and not a xor b and c xor e xor g) xor b and (d xor c)
    xor a and c xor g;
  h := temp[30] + $BA7C9045 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := e and (c and not h xor a and b xor d xor f) xor a and (c xor b)
    xor h and b xor f;
  g := temp[3] + $F12C7F99 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := d and (b and not g xor h and a xor c xor e) xor h and (b xor a)
    xor g and a xor e;
  f := temp[21] + $24A19947 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := c and (a and not f xor g and h xor b xor d) xor g and (a xor h)
    xor f and h xor d;
  e := temp[9] + $B3916CF7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := b and (h and not e xor f and g xor a xor c) xor f and (h xor g)
    xor e and g xor c;
  d := temp[17] + $0801F2E2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := a and (g and not d xor e and f xor h xor b) xor e and (g xor f)
    xor d and f xor b;
  c := temp[24] + $858EFC16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := h and (f and not c xor d and e xor g xor a) xor d and (f xor e)
    xor c and e xor a;
  b := temp[29] + $636920D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := g and (e and not b xor c and d xor f xor h) xor c and (e xor d)
    xor b and d xor h;
  a := temp[6] + $71574E69 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := f and (d and not a xor b and c xor e xor g) xor b and (d xor c)
    xor a and c xor g;
  h := temp[19] + $A458FEA3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := e and (c and not h xor a and b xor d xor f) xor a and (c xor b)
    xor h and b xor f;
  g := temp[12] + $F4933D7E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := d and (b and not g xor h and a xor c xor e) xor h and (b xor a)
    xor g and a xor e;
  f := temp[15] + $0D95748F + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := c and (a and not f xor g and h xor b xor d) xor g and (a xor h)
    xor f and h xor d;
  e := temp[13] + $728EB658 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := b and (h and not e xor f and g xor a xor c) xor f and (h xor g)
    xor e and g xor c;
  d := temp[2] + $718BCD58 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := a and (g and not d xor e and f xor h xor b) xor e and (g xor f)
    xor d and f xor b;
  c := temp[25] + $82154AEE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := h and (f and not c xor d and e xor g xor a) xor d and (f xor e)
    xor c and e xor a;
  b := temp[31] + $7B54A41D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := g and (e and not b xor c and d xor f xor h) xor c and (e xor d)
    xor b and d xor h;
  a := temp[27] + $C25A59B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and e xor g xor a) xor f and c xor e and b xor a;
  h := temp[19] + $9C30D539 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and d xor f xor h) xor e and b xor d and a xor h;
  g := temp[9] + $2AF26013 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and c xor e xor g) xor d and a xor c and h xor g;
  f := temp[4] + $C5D1B023 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and b xor d xor f) xor c and h xor b and g xor f;
  e := temp[20] + $286085F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and a xor c xor e) xor b and g xor a and f xor e;
  d := temp[28] + $CA417918 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and h xor b xor d) xor a and f xor h and e xor d;
  c := temp[17] + $B8DB38EF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and g xor a xor c) xor h and e xor g and d xor c;
  b := temp[8] + $8E79DCB0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and f xor h xor b) xor g and d xor f and c xor b;
  a := temp[22] + $603A180E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and e xor g xor a) xor f and c xor e and b xor a;
  h := temp[29] + $6C9E0E8B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and d xor f xor h) xor e and b xor d and a xor h;
  g := temp[14] + $B01E8A3E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and c xor e xor g) xor d and a xor c and h xor g;
  f := temp[25] + $D71577C1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and b xor d xor f) xor c and h xor b and g xor f;
  e := temp[12] + $BD314B27 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and a xor c xor e) xor b and g xor a and f xor e;
  d := temp[24] + $78AF2FDA + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and h xor b xor d) xor a and f xor h and e xor d;
  c := temp[30] + $55605C60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and g xor a xor c) xor h and e xor g and d xor c;
  b := temp[16] + $E65525F3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and f xor h xor b) xor g and d xor f and c xor b;
  a := temp[26] + $AA55AB94 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and e xor g xor a) xor f and c xor e and b xor a;
  h := temp[31] + $57489862 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and d xor f xor h) xor e and b xor d and a xor h;
  g := temp[15] + $63E81440 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and c xor e xor g) xor d and a xor c and h xor g;
  f := temp[7] + $55CA396A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and b xor d xor f) xor c and h xor b and g xor f;
  e := temp[3] + $2AAB10B6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and a xor c xor e) xor b and g xor a and f xor e;
  d := temp[1] + $B4CC5C34 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and h xor b xor d) xor a and f xor h and e xor d;
  c := temp[0] + $1141E8CE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and g xor a xor c) xor h and e xor g and d xor c;
  b := temp[18] + $A15486AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and f xor h xor b) xor g and d xor f and c xor b;
  a := temp[27] + $7C72E993 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and e xor g xor a) xor f and c xor e and b xor a;
  h := temp[13] + $B3EE1411 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and d xor f xor h) xor e and b xor d and a xor h;
  g := temp[6] + $636FBC2A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and c xor e xor g) xor d and a xor c and h xor g;
  f := temp[21] + $2BA9C55D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and b xor d xor f) xor c and h xor b and g xor f;
  e := temp[10] + $741831F6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and a xor c xor e) xor b and g xor a and f xor e;
  d := temp[23] + $CE5C3E16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and h xor b xor d) xor a and f xor h and e xor d;
  c := temp[11] + $9B87931E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and g xor a xor c) xor h and e xor g and d xor c;
  b := temp[5] + $AFD6BA33 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and f xor h xor b) xor g and d xor f and c xor b;
  a := temp[2] + $6C24CF5C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  Fm_hash[0] := Fm_hash[0] + a;
  Fm_hash[1] := Fm_hash[1] + b;
  Fm_hash[2] := Fm_hash[2] + c;
  Fm_hash[3] := Fm_hash[3] + d;
  Fm_hash[4] := Fm_hash[4] + e;
  Fm_hash[5] := Fm_hash[5] + f;
  Fm_hash[6] := Fm_hash[6] + g;
  Fm_hash[7] := Fm_hash[7] + h;

  System.FillChar(temp, System.SizeOf(temp), UInt32(0));
end;

{ THaval4 }

constructor THaval4.Create(a_hash_size: THashSize);
begin
  inherited Create(THashRounds.hrRounds4, a_hash_size);
end;

procedure THaval4.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  a, b, c, d, e, f, g, h, t: UInt32;
  temp: array [0 .. 31] of UInt32;
begin

  TConverters.le32_copy(a_data, a_index, @(temp[0]), 0, a_data_length);

  a := Fm_hash[0];
  b := Fm_hash[1];
  c := Fm_hash[2];
  d := Fm_hash[3];
  e := Fm_hash[4];
  f := Fm_hash[5];
  g := Fm_hash[6];
  h := Fm_hash[7];

  t := d and (a xor b) xor f and g xor e and c xor a;
  h := temp[0] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := c and (h xor a) xor e and f xor d and b xor h;
  g := temp[1] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := b and (g xor h) xor d and e xor c and a xor g;
  f := temp[2] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := a and (f xor g) xor c and d xor b and h xor f;
  e := temp[3] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := h and (e xor f) xor b and c xor a and g xor e;
  d := temp[4] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := g and (d xor e) xor a and b xor h and f xor d;
  c := temp[5] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := f and (c xor d) xor h and a xor g and e xor c;
  b := temp[6] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := e and (b xor c) xor g and h xor f and d xor b;
  a := temp[7] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := d and (a xor b) xor f and g xor e and c xor a;
  h := temp[8] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := c and (h xor a) xor e and f xor d and b xor h;
  g := temp[9] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := b and (g xor h) xor d and e xor c and a xor g;
  f := temp[10] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := a and (f xor g) xor c and d xor b and h xor f;
  e := temp[11] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := h and (e xor f) xor b and c xor a and g xor e;
  d := temp[12] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := g and (d xor e) xor a and b xor h and f xor d;
  c := temp[13] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := f and (c xor d) xor h and a xor g and e xor c;
  b := temp[14] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := e and (b xor c) xor g and h xor f and d xor b;
  a := temp[15] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := d and (a xor b) xor f and g xor e and c xor a;
  h := temp[16] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := c and (h xor a) xor e and f xor d and b xor h;
  g := temp[17] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := b and (g xor h) xor d and e xor c and a xor g;
  f := temp[18] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := a and (f xor g) xor c and d xor b and h xor f;
  e := temp[19] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := h and (e xor f) xor b and c xor a and g xor e;
  d := temp[20] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := g and (d xor e) xor a and b xor h and f xor d;
  c := temp[21] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := f and (c xor d) xor h and a xor g and e xor c;
  b := temp[22] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := e and (b xor c) xor g and h xor f and d xor b;
  a := temp[23] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := d and (a xor b) xor f and g xor e and c xor a;
  h := temp[24] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := c and (h xor a) xor e and f xor d and b xor h;
  g := temp[25] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := b and (g xor h) xor d and e xor c and a xor g;
  f := temp[26] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := a and (f xor g) xor c and d xor b and h xor f;
  e := temp[27] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := h and (e xor f) xor b and c xor a and g xor e;
  d := temp[28] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := g and (d xor e) xor a and b xor h and f xor d;
  c := temp[29] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := f and (c xor d) xor h and a xor g and e xor c;
  b := temp[30] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := e and (b xor c) xor g and h xor f and d xor b;
  a := temp[31] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := b and (g and not a xor c and f xor d xor e) xor c and (g xor f)
    xor a and f xor e;
  h := temp[5] + $452821E6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (f and not h xor b and e xor c xor d) xor b and (f xor e)
    xor h and e xor d;
  g := temp[14] + $38D01377 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (e and not g xor a and d xor b xor c) xor a and (e xor d)
    xor g and d xor c;
  f := temp[26] + $BE5466CF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (d and not f xor h and c xor a xor b) xor h and (d xor c)
    xor f and c xor b;
  e := temp[18] + $34E90C6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (c and not e xor g and b xor h xor a) xor g and (c xor b)
    xor e and b xor a;
  d := temp[11] + $C0AC29B7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (b and not d xor f and a xor g xor h) xor f and (b xor a)
    xor d and a xor h;
  c := temp[28] + $C97C50DD + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (a and not c xor e and h xor f xor g) xor e and (a xor h)
    xor c and h xor g;
  b := temp[7] + $3F84D5B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (h and not b xor d and g xor e xor f) xor d and (h xor g)
    xor b and g xor f;
  a := temp[16] + $B5470917 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (g and not a xor c and f xor d xor e) xor c and (g xor f)
    xor a and f xor e;
  h := temp[0] + $9216D5D9 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (f and not h xor b and e xor c xor d) xor b and (f xor e)
    xor h and e xor d;
  g := temp[23] + $8979FB1B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (e and not g xor a and d xor b xor c) xor a and (e xor d)
    xor g and d xor c;
  f := temp[20] + $D1310BA6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (d and not f xor h and c xor a xor b) xor h and (d xor c)
    xor f and c xor b;
  e := temp[22] + $98DFB5AC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (c and not e xor g and b xor h xor a) xor g and (c xor b)
    xor e and b xor a;
  d := temp[1] + $2FFD72DB + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (b and not d xor f and a xor g xor h) xor f and (b xor a)
    xor d and a xor h;
  c := temp[10] + $D01ADFB7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (a and not c xor e and h xor f xor g) xor e and (a xor h)
    xor c and h xor g;
  b := temp[4] + $B8E1AFED + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (h and not b xor d and g xor e xor f) xor d and (h xor g)
    xor b and g xor f;
  a := temp[8] + $6A267E96 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (g and not a xor c and f xor d xor e) xor c and (g xor f)
    xor a and f xor e;
  h := temp[30] + $BA7C9045 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (f and not h xor b and e xor c xor d) xor b and (f xor e)
    xor h and e xor d;
  g := temp[3] + $F12C7F99 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (e and not g xor a and d xor b xor c) xor a and (e xor d)
    xor g and d xor c;
  f := temp[21] + $24A19947 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (d and not f xor h and c xor a xor b) xor h and (d xor c)
    xor f and c xor b;
  e := temp[9] + $B3916CF7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (c and not e xor g and b xor h xor a) xor g and (c xor b)
    xor e and b xor a;
  d := temp[17] + $0801F2E2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (b and not d xor f and a xor g xor h) xor f and (b xor a)
    xor d and a xor h;
  c := temp[24] + $858EFC16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (a and not c xor e and h xor f xor g) xor e and (a xor h)
    xor c and h xor g;
  b := temp[29] + $636920D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (h and not b xor d and g xor e xor f) xor d and (h xor g)
    xor b and g xor f;
  a := temp[6] + $71574E69 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (g and not a xor c and f xor d xor e) xor c and (g xor f)
    xor a and f xor e;
  h := temp[19] + $A458FEA3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (f and not h xor b and e xor c xor d) xor b and (f xor e)
    xor h and e xor d;
  g := temp[12] + $F4933D7E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (e and not g xor a and d xor b xor c) xor a and (e xor d)
    xor g and d xor c;
  f := temp[15] + $0D95748F + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (d and not f xor h and c xor a xor b) xor h and (d xor c)
    xor f and c xor b;
  e := temp[13] + $728EB658 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (c and not e xor g and b xor h xor a) xor g and (c xor b)
    xor e and b xor a;
  d := temp[2] + $718BCD58 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (b and not d xor f and a xor g xor h) xor f and (b xor a)
    xor d and a xor h;
  c := temp[25] + $82154AEE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (a and not c xor e and h xor f xor g) xor e and (a xor h)
    xor c and h xor g;
  b := temp[31] + $7B54A41D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (h and not b xor d and g xor e xor f) xor d and (h xor g)
    xor b and g xor f;
  a := temp[27] + $C25A59B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := g and (c and a xor b xor f) xor c and d xor a and e xor f;
  h := temp[19] + $9C30D539 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := f and (b and h xor a xor e) xor b and c xor h and d xor e;
  g := temp[9] + $2AF26013 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := e and (a and g xor h xor d) xor a and b xor g and c xor d;
  f := temp[4] + $C5D1B023 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := d and (h and f xor g xor c) xor h and a xor f and b xor c;
  e := temp[20] + $286085F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := c and (g and e xor f xor b) xor g and h xor e and a xor b;
  d := temp[28] + $CA417918 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := b and (f and d xor e xor a) xor f and g xor d and h xor a;
  c := temp[17] + $B8DB38EF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := a and (e and c xor d xor h) xor e and f xor c and g xor h;
  b := temp[8] + $8E79DCB0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := h and (d and b xor c xor g) xor d and e xor b and f xor g;
  a := temp[22] + $603A180E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := g and (c and a xor b xor f) xor c and d xor a and e xor f;
  h := temp[29] + $6C9E0E8B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := f and (b and h xor a xor e) xor b and c xor h and d xor e;
  g := temp[14] + $B01E8A3E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := e and (a and g xor h xor d) xor a and b xor g and c xor d;
  f := temp[25] + $D71577C1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := d and (h and f xor g xor c) xor h and a xor f and b xor c;
  e := temp[12] + $BD314B27 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := c and (g and e xor f xor b) xor g and h xor e and a xor b;
  d := temp[24] + $78AF2FDA + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := b and (f and d xor e xor a) xor f and g xor d and h xor a;
  c := temp[30] + $55605C60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := a and (e and c xor d xor h) xor e and f xor c and g xor h;
  b := temp[16] + $E65525F3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := h and (d and b xor c xor g) xor d and e xor b and f xor g;
  a := temp[26] + $AA55AB94 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := g and (c and a xor b xor f) xor c and d xor a and e xor f;
  h := temp[31] + $57489862 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := f and (b and h xor a xor e) xor b and c xor h and d xor e;
  g := temp[15] + $63E81440 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := e and (a and g xor h xor d) xor a and b xor g and c xor d;
  f := temp[7] + $55CA396A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := d and (h and f xor g xor c) xor h and a xor f and b xor c;
  e := temp[3] + $2AAB10B6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := c and (g and e xor f xor b) xor g and h xor e and a xor b;
  d := temp[1] + $B4CC5C34 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := b and (f and d xor e xor a) xor f and g xor d and h xor a;
  c := temp[0] + $1141E8CE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := a and (e and c xor d xor h) xor e and f xor c and g xor h;
  b := temp[18] + $A15486AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := h and (d and b xor c xor g) xor d and e xor b and f xor g;
  a := temp[27] + $7C72E993 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := g and (c and a xor b xor f) xor c and d xor a and e xor f;
  h := temp[13] + $B3EE1411 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := f and (b and h xor a xor e) xor b and c xor h and d xor e;
  g := temp[6] + $636FBC2A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := e and (a and g xor h xor d) xor a and b xor g and c xor d;
  f := temp[21] + $2BA9C55D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := d and (h and f xor g xor c) xor h and a xor f and b xor c;
  e := temp[10] + $741831F6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := c and (g and e xor f xor b) xor g and h xor e and a xor b;
  d := temp[23] + $CE5C3E16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := b and (f and d xor e xor a) xor f and g xor d and h xor a;
  c := temp[11] + $9B87931E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := a and (e and c xor d xor h) xor e and f xor c and g xor h;
  b := temp[5] + $AFD6BA33 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := h and (d and b xor c xor g) xor d and e xor b and f xor g;
  a := temp[2] + $6C24CF5C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := a and (e and not c xor f and not g xor b xor g xor d) xor f and
    (b and c xor e xor g) xor c and g xor d;
  h := temp[24] + $7A325381 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := h and (d and not b xor e and not f xor a xor f xor c) xor e and
    (a and b xor d xor f) xor b and f xor c;
  g := temp[4] + $28958677 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := g and (c and not a xor d and not e xor h xor e xor b) xor d and
    (h and a xor c xor e) xor a and e xor b;
  f := temp[0] + $3B8F4898 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := f and (b and not h xor c and not d xor g xor d xor a) xor c and
    (g and h xor b xor d) xor h and d xor a;
  e := temp[14] + $6B4BB9AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := e and (a and not g xor b and not c xor f xor c xor h) xor b and
    (f and g xor a xor c) xor g and c xor h;
  d := temp[2] + $C4BFE81B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := d and (h and not f xor a and not b xor e xor b xor g) xor a and
    (e and f xor h xor b) xor f and b xor g;
  c := temp[7] + $66282193 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := c and (g and not e xor h and not a xor d xor a xor f) xor h and
    (d and e xor g xor a) xor e and a xor f;
  b := temp[28] + $61D809CC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := b and (f and not d xor g and not h xor c xor h xor e) xor g and
    (c and d xor f xor h) xor d and h xor e;
  a := temp[23] + $FB21A991 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := a and (e and not c xor f and not g xor b xor g xor d) xor f and
    (b and c xor e xor g) xor c and g xor d;
  h := temp[26] + $487CAC60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := h and (d and not b xor e and not f xor a xor f xor c) xor e and
    (a and b xor d xor f) xor b and f xor c;
  g := temp[6] + $5DEC8032 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := g and (c and not a xor d and not e xor h xor e xor b) xor d and
    (h and a xor c xor e) xor a and e xor b;
  f := temp[30] + $EF845D5D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := f and (b and not h xor c and not d xor g xor d xor a) xor c and
    (g and h xor b xor d) xor h and d xor a;
  e := temp[20] + $E98575B1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := e and (a and not g xor b and not c xor f xor c xor h) xor b and
    (f and g xor a xor c) xor g and c xor h;
  d := temp[18] + $DC262302 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := d and (h and not f xor a and not b xor e xor b xor g) xor a and
    (e and f xor h xor b) xor f and b xor g;
  c := temp[25] + $EB651B88 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := c and (g and not e xor h and not a xor d xor a xor f) xor h and
    (d and e xor g xor a) xor e and a xor f;
  b := temp[19] + $23893E81 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := b and (f and not d xor g and not h xor c xor h xor e) xor g and
    (c and d xor f xor h) xor d and h xor e;
  a := temp[3] + $D396ACC5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := a and (e and not c xor f and not g xor b xor g xor d) xor f and
    (b and c xor e xor g) xor c and g xor d;
  h := temp[22] + $0F6D6FF3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := h and (d and not b xor e and not f xor a xor f xor c) xor e and
    (a and b xor d xor f) xor b and f xor c;
  g := temp[11] + $83F44239 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := g and (c and not a xor d and not e xor h xor e xor b) xor d and
    (h and a xor c xor e) xor a and e xor b;
  f := temp[31] + $2E0B4482 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := f and (b and not h xor c and not d xor g xor d xor a) xor c and
    (g and h xor b xor d) xor h and d xor a;
  e := temp[21] + $A4842004 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := e and (a and not g xor b and not c xor f xor c xor h) xor b and
    (f and g xor a xor c) xor g and c xor h;
  d := temp[8] + $69C8F04A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := d and (h and not f xor a and not b xor e xor b xor g) xor a and
    (e and f xor h xor b) xor f and b xor g;
  c := temp[27] + $9E1F9B5E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := c and (g and not e xor h and not a xor d xor a xor f) xor h and
    (d and e xor g xor a) xor e and a xor f;
  b := temp[12] + $21C66842 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := b and (f and not d xor g and not h xor c xor h xor e) xor g and
    (c and d xor f xor h) xor d and h xor e;
  a := temp[9] + $F6E96C9A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := a and (e and not c xor f and not g xor b xor g xor d) xor f and
    (b and c xor e xor g) xor c and g xor d;
  h := temp[1] + $670C9C61 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := h and (d and not b xor e and not f xor a xor f xor c) xor e and
    (a and b xor d xor f) xor b and f xor c;
  g := temp[29] + $ABD388F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := g and (c and not a xor d and not e xor h xor e xor b) xor d and
    (h and a xor c xor e) xor a and e xor b;
  f := temp[5] + $6A51A0D2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := f and (b and not h xor c and not d xor g xor d xor a) xor c and
    (g and h xor b xor d) xor h and d xor a;
  e := temp[15] + $D8542F68 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := e and (a and not g xor b and not c xor f xor c xor h) xor b and
    (f and g xor a xor c) xor g and c xor h;
  d := temp[17] + $960FA728 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := d and (h and not f xor a and not b xor e xor b xor g) xor a and
    (e and f xor h xor b) xor f and b xor g;
  c := temp[10] + $AB5133A3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := c and (g and not e xor h and not a xor d xor a xor f) xor h and
    (d and e xor g xor a) xor e and a xor f;
  b := temp[16] + $6EEF0B6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := b and (f and not d xor g and not h xor c xor h xor e) xor g and
    (c and d xor f xor h) xor d and h xor e;
  a := temp[13] + $137A3BE4 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  Fm_hash[0] := Fm_hash[0] + a;
  Fm_hash[1] := Fm_hash[1] + b;
  Fm_hash[2] := Fm_hash[2] + c;
  Fm_hash[3] := Fm_hash[3] + d;
  Fm_hash[4] := Fm_hash[4] + e;
  Fm_hash[5] := Fm_hash[5] + f;
  Fm_hash[6] := Fm_hash[6] + g;
  Fm_hash[7] := Fm_hash[7] + h;

  System.FillChar(temp, System.SizeOf(temp), UInt32(0));

end;

{ THaval5 }

constructor THaval5.Create(a_hash_size: THashSize);
begin
  inherited Create(THashRounds.hrRounds5, a_hash_size);
end;

procedure THaval5.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  a, b, c, d, e, f, g, h, t: UInt32;
  temp: array [0 .. 31] of UInt32;
begin

  TConverters.le32_copy(a_data, a_index, @(temp[0]), 0, a_data_length);

  a := Fm_hash[0];
  b := Fm_hash[1];
  c := Fm_hash[2];
  d := Fm_hash[3];
  e := Fm_hash[4];
  f := Fm_hash[5];
  g := Fm_hash[6];
  h := Fm_hash[7];

  t := c and (g xor b) xor f and e xor a and d xor g;
  h := temp[0] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);
  t := b and (f xor a) xor e and d xor h and c xor f;
  g := temp[1] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (e xor h) xor d and c xor g and b xor e;
  f := temp[2] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (d xor g) xor c and b xor f and a xor d;
  e := temp[3] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (c xor f) xor b and a xor e and h xor c;
  d := temp[4] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (b xor e) xor a and h xor d and g xor b;
  c := temp[5] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (a xor d) xor h and g xor c and f xor a;
  b := temp[6] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (h xor c) xor g and f xor b and e xor h;
  a := temp[7] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (g xor b) xor f and e xor a and d xor g;
  h := temp[8] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (f xor a) xor e and d xor h and c xor f;
  g := temp[9] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (e xor h) xor d and c xor g and b xor e;
  f := temp[10] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (d xor g) xor c and b xor f and a xor d;
  e := temp[11] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (c xor f) xor b and a xor e and h xor c;
  d := temp[12] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (b xor e) xor a and h xor d and g xor b;
  c := temp[13] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (a xor d) xor h and g xor c and f xor a;
  b := temp[14] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (h xor c) xor g and f xor b and e xor h;
  a := temp[15] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (g xor b) xor f and e xor a and d xor g;
  h := temp[16] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (f xor a) xor e and d xor h and c xor f;
  g := temp[17] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (e xor h) xor d and c xor g and b xor e;
  f := temp[18] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (d xor g) xor c and b xor f and a xor d;
  e := temp[19] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (c xor f) xor b and a xor e and h xor c;
  d := temp[20] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (b xor e) xor a and h xor d and g xor b;
  c := temp[21] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (a xor d) xor h and g xor c and f xor a;
  b := temp[22] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (h xor c) xor g and f xor b and e xor h;
  a := temp[23] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := c and (g xor b) xor f and e xor a and d xor g;
  h := temp[24] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(h, 11);

  t := b and (f xor a) xor e and d xor h and c xor f;
  g := temp[25] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(g, 11);

  t := a and (e xor h) xor d and c xor g and b xor e;
  f := temp[26] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(f, 11);

  t := h and (d xor g) xor c and b xor f and a xor d;
  e := temp[27] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(e, 11);

  t := g and (c xor f) xor b and a xor e and h xor c;
  d := temp[28] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(d, 11);

  t := f and (b xor e) xor a and h xor d and g xor b;
  c := temp[29] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(c, 11);

  t := e and (a xor d) xor h and g xor c and f xor a;
  b := temp[30] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(b, 11);

  t := d and (h xor c) xor g and f xor b and e xor h;
  a := temp[31] + TBits.RotateRight32(t, 7) + TBits.RotateRight32(a, 11);

  t := d and (e and not a xor b and c xor g xor f) xor b and (e xor c)
    xor a and c xor f;
  h := temp[5] + $452821E6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (d and not h xor a and b xor f xor e) xor a and (d xor b)
    xor h and b xor e;
  g := temp[14] + $38D01377 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (c and not g xor h and a xor e xor d) xor h and (c xor a)
    xor g and a xor d;
  f := temp[26] + $BE5466CF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (b and not f xor g and h xor d xor c) xor g and (b xor h)
    xor f and h xor c;
  e := temp[18] + $34E90C6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (a and not e xor f and g xor c xor b) xor f and (a xor g)
    xor e and g xor b;
  d := temp[11] + $C0AC29B7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (h and not d xor e and f xor b xor a) xor e and (h xor f)
    xor d and f xor a;
  c := temp[28] + $C97C50DD + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (g and not c xor d and e xor a xor h) xor d and (g xor e)
    xor c and e xor h;
  b := temp[7] + $3F84D5B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (f and not b xor c and d xor h xor g) xor c and (f xor d)
    xor b and d xor g;
  a := temp[16] + $B5470917 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (e and not a xor b and c xor g xor f) xor b and (e xor c)
    xor a and c xor f;
  h := temp[0] + $9216D5D9 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (d and not h xor a and b xor f xor e) xor a and (d xor b)
    xor h and b xor e;
  g := temp[23] + $8979FB1B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (c and not g xor h and a xor e xor d) xor h and (c xor a)
    xor g and a xor d;
  f := temp[20] + $D1310BA6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (b and not f xor g and h xor d xor c) xor g and (b xor h)
    xor f and h xor c;
  e := temp[22] + $98DFB5AC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (a and not e xor f and g xor c xor b) xor f and (a xor g)
    xor e and g xor b;
  d := temp[1] + $2FFD72DB + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (h and not d xor e and f xor b xor a) xor e and (h xor f)
    xor d and f xor a;
  c := temp[10] + $D01ADFB7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (g and not c xor d and e xor a xor h) xor d and (g xor e)
    xor c and e xor h;
  b := temp[4] + $B8E1AFED + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (f and not b xor c and d xor h xor g) xor c and (f xor d)
    xor b and d xor g;
  a := temp[8] + $6A267E96 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (e and not a xor b and c xor g xor f) xor b and (e xor c)
    xor a and c xor f;
  h := temp[30] + $BA7C9045 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (d and not h xor a and b xor f xor e) xor a and (d xor b)
    xor h and b xor e;
  g := temp[3] + $F12C7F99 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (c and not g xor h and a xor e xor d) xor h and (c xor a)
    xor g and a xor d;
  f := temp[21] + $24A19947 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (b and not f xor g and h xor d xor c) xor g and (b xor h)
    xor f and h xor c;
  e := temp[9] + $B3916CF7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (a and not e xor f and g xor c xor b) xor f and (a xor g)
    xor e and g xor b;
  d := temp[17] + $0801F2E2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (h and not d xor e and f xor b xor a) xor e and (h xor f)
    xor d and f xor a;
  c := temp[24] + $858EFC16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (g and not c xor d and e xor a xor h) xor d and (g xor e)
    xor c and e xor h;
  b := temp[29] + $636920D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (f and not b xor c and d xor h xor g) xor c and (f xor d)
    xor b and d xor g;
  a := temp[6] + $71574E69 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (e and not a xor b and c xor g xor f) xor b and (e xor c)
    xor a and c xor f;
  h := temp[19] + $A458FEA3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (d and not h xor a and b xor f xor e) xor a and (d xor b)
    xor h and b xor e;
  g := temp[12] + $F4933D7E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (c and not g xor h and a xor e xor d) xor h and (c xor a)
    xor g and a xor d;
  f := temp[15] + $0D95748F + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (b and not f xor g and h xor d xor c) xor g and (b xor h)
    xor f and h xor c;
  e := temp[13] + $728EB658 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (a and not e xor f and g xor c xor b) xor f and (a xor g)
    xor e and g xor b;
  d := temp[2] + $718BCD58 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (h and not d xor e and f xor b xor a) xor e and (h xor f)
    xor d and f xor a;
  c := temp[25] + $82154AEE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (g and not c xor d and e xor a xor h) xor d and (g xor e)
    xor c and e xor h;
  b := temp[31] + $7B54A41D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (f and not b xor c and d xor h xor g) xor c and (f xor d)
    xor b and d xor g;
  a := temp[27] + $C25A59B5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := e and (b and d xor c xor f) xor b and a xor d and g xor f;
  h := temp[19] + $9C30D539 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := d and (a and c xor b xor e) xor a and h xor c and f xor e;
  g := temp[9] + $2AF26013 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := c and (h and b xor a xor d) xor h and g xor b and e xor d;
  f := temp[4] + $C5D1B023 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := b and (g and a xor h xor c) xor g and f xor a and d xor c;
  e := temp[20] + $286085F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := a and (f and h xor g xor b) xor f and e xor h and c xor b;
  d := temp[28] + $CA417918 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := h and (e and g xor f xor a) xor e and d xor g and b xor a;
  c := temp[17] + $B8DB38EF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := g and (d and f xor e xor h) xor d and c xor f and a xor h;
  b := temp[8] + $8E79DCB0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := f and (c and e xor d xor g) xor c and b xor e and h xor g;
  a := temp[22] + $603A180E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := e and (b and d xor c xor f) xor b and a xor d and g xor f;
  h := temp[29] + $6C9E0E8B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := d and (a and c xor b xor e) xor a and h xor c and f xor e;
  g := temp[14] + $B01E8A3E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := c and (h and b xor a xor d) xor h and g xor b and e xor d;
  f := temp[25] + $D71577C1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := b and (g and a xor h xor c) xor g and f xor a and d xor c;
  e := temp[12] + $BD314B27 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := a and (f and h xor g xor b) xor f and e xor h and c xor b;
  d := temp[24] + $78AF2FDA + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := h and (e and g xor f xor a) xor e and d xor g and b xor a;
  c := temp[30] + $55605C60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := g and (d and f xor e xor h) xor d and c xor f and a xor h;
  b := temp[16] + $E65525F3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := f and (c and e xor d xor g) xor c and b xor e and h xor g;
  a := temp[26] + $AA55AB94 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := e and (b and d xor c xor f) xor b and a xor d and g xor f;
  h := temp[31] + $57489862 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := d and (a and c xor b xor e) xor a and h xor c and f xor e;
  g := temp[15] + $63E81440 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := c and (h and b xor a xor d) xor h and g xor b and e xor d;
  f := temp[7] + $55CA396A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := b and (g and a xor h xor c) xor g and f xor a and d xor c;
  e := temp[3] + $2AAB10B6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := a and (f and h xor g xor b) xor f and e xor h and c xor b;
  d := temp[1] + $B4CC5C34 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := h and (e and g xor f xor a) xor e and d xor g and b xor a;
  c := temp[0] + $1141E8CE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := g and (d and f xor e xor h) xor d and c xor f and a xor h;
  b := temp[18] + $A15486AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := f and (c and e xor d xor g) xor c and b xor e and h xor g;
  a := temp[27] + $7C72E993 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := e and (b and d xor c xor f) xor b and a xor d and g xor f;
  h := temp[13] + $B3EE1411 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := d and (a and c xor b xor e) xor a and h xor c and f xor e;
  g := temp[6] + $636FBC2A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := c and (h and b xor a xor d) xor h and g xor b and e xor d;
  f := temp[21] + $2BA9C55D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := b and (g and a xor h xor c) xor g and f xor a and d xor c;
  e := temp[10] + $741831F6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := a and (f and h xor g xor b) xor f and e xor h and c xor b;
  d := temp[23] + $CE5C3E16 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := h and (e and g xor f xor a) xor e and d xor g and b xor a;
  c := temp[11] + $9B87931E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := g and (d and f xor e xor h) xor d and c xor f and a xor h;
  b := temp[5] + $AFD6BA33 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := f and (c and e xor d xor g) xor c and b xor e and h xor g;
  a := temp[2] + $6C24CF5C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and not a xor c and not b xor e xor b xor g) xor c and
    (e and a xor f xor b) xor a and b xor g;
  h := temp[24] + $7A325381 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and not h xor b and not a xor d xor a xor f) xor b and
    (d and h xor e xor a) xor h and a xor f;
  g := temp[4] + $28958677 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and not g xor a and not h xor c xor h xor e) xor a and
    (c and g xor d xor h) xor g and h xor e;
  f := temp[0] + $3B8F4898 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and not f xor h and not g xor b xor g xor d) xor h and
    (b and f xor c xor g) xor f and g xor d;
  e := temp[14] + $6B4BB9AF + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and not e xor g and not f xor a xor f xor c) xor g and
    (a and e xor b xor f) xor e and f xor c;
  d := temp[2] + $C4BFE81B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and not d xor f and not e xor h xor e xor b) xor f and
    (h and d xor a xor e) xor d and e xor b;
  c := temp[7] + $66282193 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and not c xor e and not d xor g xor d xor a) xor e and
    (g and c xor h xor d) xor c and d xor a;
  b := temp[28] + $61D809CC + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and not b xor d and not c xor f xor c xor h) xor d and
    (f and b xor g xor c) xor b and c xor h;
  a := temp[23] + $FB21A991 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and not a xor c and not b xor e xor b xor g) xor c and
    (e and a xor f xor b) xor a and b xor g;
  h := temp[26] + $487CAC60 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and not h xor b and not a xor d xor a xor f) xor b and
    (d and h xor e xor a) xor h and a xor f;
  g := temp[6] + $5DEC8032 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and not g xor a and not h xor c xor h xor e) xor a and
    (c and g xor d xor h) xor g and h xor e;
  f := temp[30] + $EF845D5D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and not f xor h and not g xor b xor g xor d) xor h and
    (b and f xor c xor g) xor f and g xor d;
  e := temp[20] + $E98575B1 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and not e xor g and not f xor a xor f xor c) xor g and
    (a and e xor b xor f) xor e and f xor c;
  d := temp[18] + $DC262302 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and not d xor f and not e xor h xor e xor b) xor f and
    (h and d xor a xor e) xor d and e xor b;
  c := temp[25] + $EB651B88 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and not c xor e and not d xor g xor d xor a) xor e and
    (g and c xor h xor d) xor c and d xor a;
  b := temp[19] + $23893E81 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and not b xor d and not c xor f xor c xor h) xor d and
    (f and b xor g xor c) xor b and c xor h;
  a := temp[3] + $D396ACC5 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and not a xor c and not b xor e xor b xor g) xor c and
    (e and a xor f xor b) xor a and b xor g;
  h := temp[22] + $0F6D6FF3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and not h xor b and not a xor d xor a xor f) xor b and
    (d and h xor e xor a) xor h and a xor f;
  g := temp[11] + $83F44239 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and not g xor a and not h xor c xor h xor e) xor a and
    (c and g xor d xor h) xor g and h xor e;
  f := temp[31] + $2E0B4482 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and not f xor h and not g xor b xor g xor d) xor h and
    (b and f xor c xor g) xor f and g xor d;
  e := temp[21] + $A4842004 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and not e xor g and not f xor a xor f xor c) xor g and
    (a and e xor b xor f) xor e and f xor c;
  d := temp[8] + $69C8F04A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and not d xor f and not e xor h xor e xor b) xor f and
    (h and d xor a xor e) xor d and e xor b;
  c := temp[27] + $9E1F9B5E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and not c xor e and not d xor g xor d xor a) xor e and
    (g and c xor h xor d) xor c and d xor a;
  b := temp[12] + $21C66842 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and not b xor d and not c xor f xor c xor h) xor d and
    (f and b xor g xor c) xor b and c xor h;
  a := temp[9] + $F6E96C9A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := d and (f and not a xor c and not b xor e xor b xor g) xor c and
    (e and a xor f xor b) xor a and b xor g;
  h := temp[1] + $670C9C61 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := c and (e and not h xor b and not a xor d xor a xor f) xor b and
    (d and h xor e xor a) xor h and a xor f;
  g := temp[29] + $ABD388F0 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := b and (d and not g xor a and not h xor c xor h xor e) xor a and
    (c and g xor d xor h) xor g and h xor e;
  f := temp[5] + $6A51A0D2 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := a and (c and not f xor h and not g xor b xor g xor d) xor h and
    (b and f xor c xor g) xor f and g xor d;
  e := temp[15] + $D8542F68 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := h and (b and not e xor g and not f xor a xor f xor c) xor g and
    (a and e xor b xor f) xor e and f xor c;
  d := temp[17] + $960FA728 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := g and (a and not d xor f and not e xor h xor e xor b) xor f and
    (h and d xor a xor e) xor d and e xor b;
  c := temp[10] + $AB5133A3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := f and (h and not c xor e and not d xor g xor d xor a) xor e and
    (g and c xor h xor d) xor c and d xor a;
  b := temp[16] + $6EEF0B6C + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := e and (g and not b xor d and not c xor f xor c xor h) xor d and
    (f and b xor g xor c) xor b and c xor h;
  a := temp[13] + $137A3BE4 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (d and e and g xor not f) xor d and a xor e and f xor g and c;
  h := temp[27] + $BA3BF050 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (c and d and f xor not e) xor c and h xor d and e xor f and b;
  g := temp[3] + $7EFB2A98 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (b and c and e xor not d) xor b and g xor c and d xor e and a;
  f := temp[21] + $A1F1651D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (a and b and d xor not c) xor a and f xor b and c xor d and h;
  e := temp[26] + $39AF0176 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (h and a and c xor not b) xor h and e xor a and b xor c and g;
  d := temp[17] + $66CA593E + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (g and h and b xor not a) xor g and d xor h and a xor b and f;
  c := temp[11] + $82430E88 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (f and g and a xor not h) xor f and c xor g and h xor a and e;
  b := temp[20] + $8CEE8619 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (e and f and h xor not g) xor e and b xor f and g xor h and d;
  a := temp[29] + $456F9FB4 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (d and e and g xor not f) xor d and a xor e and f xor g and c;
  h := temp[19] + $7D84A5C3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (c and d and f xor not e) xor c and h xor d and e xor f and b;
  g := temp[0] + $3B8B5EBE + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (b and c and e xor not d) xor b and g xor c and d xor e and a;
  f := temp[12] + $E06F75D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (a and b and d xor not c) xor a and f xor b and c xor d and h;
  e := temp[7] + $85C12073 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (h and a and c xor not b) xor h and e xor a and b xor c and g;
  d := temp[13] + $401A449F + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (g and h and b xor not a) xor g and d xor h and a xor b and f;
  c := temp[8] + $56C16AA6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (f and g and a xor not h) xor f and c xor g and h xor a and e;
  b := temp[31] + $4ED3AA62 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (e and f and h xor not g) xor e and b xor f and g xor h and d;
  a := temp[10] + $363F7706 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (d and e and g xor not f) xor d and a xor e and f xor g and c;
  h := temp[5] + $1BFEDF72 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (c and d and f xor not e) xor c and h xor d and e xor f and b;
  g := temp[9] + $429B023D + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (b and c and e xor not d) xor b and g xor c and d xor e and a;
  f := temp[14] + $37D0D724 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (a and b and d xor not c) xor a and f xor b and c xor d and h;
  e := temp[30] + $D00A1248 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (h and a and c xor not b) xor h and e xor a and b xor c and g;
  d := temp[18] + $DB0FEAD3 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (g and h and b xor not a) xor g and d xor h and a xor b and f;
  c := temp[6] + $49F1C09B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (f and g and a xor not h) xor f and c xor g and h xor a and e;
  b := temp[28] + $075372C9 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (e and f and h xor not g) xor e and b xor f and g xor h and d;
  a := temp[24] + $80991B7B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  t := b and (d and e and g xor not f) xor d and a xor e and f xor g and c;
  h := temp[2] + $25D479D8 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(h, 11);

  t := a and (c and d and f xor not e) xor c and h xor d and e xor f and b;
  g := temp[23] + $F6E8DEF7 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(g, 11);

  t := h and (b and c and e xor not d) xor b and g xor c and d xor e and a;
  f := temp[16] + $E3FE501A + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(f, 11);

  t := g and (a and b and d xor not c) xor a and f xor b and c xor d and h;
  e := temp[22] + $B6794C3B + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(e, 11);

  t := f and (h and a and c xor not b) xor h and e xor a and b xor c and g;
  d := temp[4] + $976CE0BD + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(d, 11);

  t := e and (g and h and b xor not a) xor g and d xor h and a xor b and f;
  c := temp[1] + $04C006BA + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(c, 11);

  t := d and (f and g and a xor not h) xor f and c xor g and h xor a and e;
  b := temp[25] + $C1A94FB6 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(b, 11);

  t := c and (e and f and h xor not g) xor e and b xor f and g xor h and d;
  a := temp[15] + $409F60C4 + TBits.RotateRight32(t, 7) +
    TBits.RotateRight32(a, 11);

  Fm_hash[0] := Fm_hash[0] + a;
  Fm_hash[1] := Fm_hash[1] + b;
  Fm_hash[2] := Fm_hash[2] + c;
  Fm_hash[3] := Fm_hash[3] + d;
  Fm_hash[4] := Fm_hash[4] + e;
  Fm_hash[5] := Fm_hash[5] + f;
  Fm_hash[6] := Fm_hash[6] + g;
  Fm_hash[7] := Fm_hash[7] + h;

  System.FillChar(temp, System.SizeOf(temp), UInt32(0));

end;

{ THaval_3_128 }

function THaval_3_128.Clone(): IHash;
var
  HashInstance: THaval_3_128;
begin
  HashInstance := THaval_3_128.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_128.Create;
begin
  inherited Create(THashSize.hsHashSize128);
end;

{ THaval_4_128 }

function THaval_4_128.Clone(): IHash;
var
  HashInstance: THaval_4_128;
begin
  HashInstance := THaval_4_128.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_128.Create;
begin
  inherited Create(THashSize.hsHashSize128);
end;

{ THaval_5_128 }

function THaval_5_128.Clone(): IHash;
var
  HashInstance: THaval_5_128;
begin
  HashInstance := THaval_5_128.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_128.Create;
begin
  inherited Create(THashSize.hsHashSize128);
end;

{ THaval_3_160 }

function THaval_3_160.Clone(): IHash;
var
  HashInstance: THaval_3_160;
begin
  HashInstance := THaval_3_160.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_160.Create;
begin
  inherited Create(THashSize.hsHashSize160);
end;

{ THaval_4_160 }

function THaval_4_160.Clone(): IHash;
var
  HashInstance: THaval_4_160;
begin
  HashInstance := THaval_4_160.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_160.Create;
begin
  inherited Create(THashSize.hsHashSize160);
end;

{ THaval_5_160 }

function THaval_5_160.Clone(): IHash;
var
  HashInstance: THaval_5_160;
begin
  HashInstance := THaval_5_160.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_160.Create;
begin
  inherited Create(THashSize.hsHashSize160);
end;

{ THaval_3_192 }

function THaval_3_192.Clone(): IHash;
var
  HashInstance: THaval_3_192;
begin
  HashInstance := THaval_3_192.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_192.Create;
begin
  inherited Create(THashSize.hsHashSize192);
end;

{ THaval_4_192 }

function THaval_4_192.Clone(): IHash;
var
  HashInstance: THaval_4_192;
begin
  HashInstance := THaval_4_192.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_192.Create;
begin
  inherited Create(THashSize.hsHashSize192);
end;

{ THaval_5_192 }

function THaval_5_192.Clone(): IHash;
var
  HashInstance: THaval_5_192;
begin
  HashInstance := THaval_5_192.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_192.Create;
begin
  inherited Create(THashSize.hsHashSize192);
end;

{ THaval_3_224 }

function THaval_3_224.Clone(): IHash;
var
  HashInstance: THaval_3_224;
begin
  HashInstance := THaval_3_224.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_224.Create;
begin
  inherited Create(THashSize.hsHashSize224);
end;

{ THaval_4_224 }

function THaval_4_224.Clone(): IHash;
var
  HashInstance: THaval_4_224;
begin
  HashInstance := THaval_4_224.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_224.Create;
begin
  inherited Create(THashSize.hsHashSize224);
end;

{ THaval_5_224 }

function THaval_5_224.Clone(): IHash;
var
  HashInstance: THaval_5_224;
begin
  HashInstance := THaval_5_224.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_224.Create;
begin
  inherited Create(THashSize.hsHashSize224);
end;

{ THaval_3_256 }

function THaval_3_256.Clone(): IHash;
var
  HashInstance: THaval_3_256;
begin
  HashInstance := THaval_3_256.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_3_256.Create;
begin
  inherited Create(THashSize.hsHashSize256);
end;

{ THaval_4_256 }

function THaval_4_256.Clone(): IHash;
var
  HashInstance: THaval_4_256;
begin
  HashInstance := THaval_4_256.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_4_256.Create;
begin
  inherited Create(THashSize.hsHashSize256);
end;

{ THaval_5_256 }

function THaval_5_256.Clone(): IHash;
var
  HashInstance: THaval_5_256;
begin
  HashInstance := THaval_5_256.Create();
  HashInstance.Fm_rounds := Fm_rounds;
  HashInstance.FHashSize := FHashSize;
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THaval_5_256.Create;
begin
  inherited Create(THashSize.hsHashSize256);

end;

end.
