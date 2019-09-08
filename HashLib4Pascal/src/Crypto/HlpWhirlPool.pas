unit HlpWhirlPool;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpBitConverter,
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpHashCryptoNotBuildIn,
  HlpArrayUtils;

type
  TWhirlPool = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private

    Fm_hash: THashLibUInt64Array;

    class var

      Fs_C0, Fs_C1, Fs_C2, Fs_C3, Fs_C4, Fs_C5, Fs_C6, Fs_C7,
      Fs_rc: THashLibUInt64Array;

{$REGION 'Consts'}

  const

    ROUNDS = Int32(10);
    REDUCTION_POLYNOMIAL = UInt32($011D);

    s_SBOX: array [0 .. 255] of UInt32 = ($18, $23, $C6, $E8, $87, $B8, $01,
      $4F, $36, $A6, $D2, $F5, $79, $6F, $91, $52, $60, $BC, $9B, $8E, $A3, $0C,
      $7B, $35, $1D, $E0, $D7, $C2, $2E, $4B, $FE, $57, $15, $77, $37, $E5, $9F,
      $F0, $4A, $DA, $58, $C9, $29, $0A, $B1, $A0, $6B, $85, $BD, $5D, $10, $F4,
      $CB, $3E, $05, $67, $E4, $27, $41, $8B, $A7, $7D, $95, $D8, $FB, $EE, $7C,
      $66, $DD, $17, $47, $9E, $CA, $2D, $BF, $07, $AD, $5A, $83, $33, $63, $02,
      $AA, $71, $C8, $19, $49, $D9, $F2, $E3, $5B, $88, $9A, $26, $32, $B0, $E9,
      $0F, $D5, $80, $BE, $CD, $34, $48, $FF, $7A, $90, $5F, $20, $68, $1A, $AE,
      $B4, $54, $93, $22, $64, $F1, $73, $12, $40, $08, $C3, $EC, $DB, $A1, $8D,
      $3D, $97, $00, $CF, $2B, $76, $82, $D6, $1B, $B5, $AF, $6A, $50, $45, $F3,
      $30, $EF, $3F, $55, $A2, $EA, $65, $BA, $2F, $C0, $DE, $1C, $FD, $4D, $92,
      $75, $06, $8A, $B2, $E6, $0E, $1F, $62, $D4, $A8, $96, $F9, $C5, $25, $59,
      $84, $72, $39, $4C, $5E, $78, $38, $8C, $D1, $A5, $E2, $61, $B3, $21, $9C,
      $1E, $43, $C7, $FC, $04, $51, $99, $6D, $0D, $FA, $DF, $7E, $24, $3B, $AB,
      $CE, $11, $8F, $4E, $B7, $EB, $3C, $81, $94, $F7, $B9, $13, $2C, $D3, $E7,
      $6E, $C4, $03, $56, $44, $7F, $A9, $2A, $BB, $C1, $53, $DC, $0B, $9D, $6C,
      $31, $74, $F6, $46, $AC, $89, $14, $E1, $16, $3A, $69, $09, $70, $B6, $D0,
      $ED, $CC, $42, $98, $A4, $28, $5C, $F8, $86);

{$ENDREGION}
    class constructor WhirlPool;

    class function PackIntoUInt64(b7, b6, b5, b4, b3, b2, b1, b0: UInt32)
      : UInt64; static; inline;
    class function MaskWithReductionPolynomial(input: UInt32): UInt32;
      static; inline;

  strict protected
    function GetResult(): THashLibByteArray; override;
    procedure Finish(); override;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TWhirlPool }

function TWhirlPool.Clone(): IHash;
var
  HashInstance: TWhirlPool;
begin
  HashInstance := TWhirlPool.Create();
  HashInstance.Fm_hash := System.Copy(Fm_hash);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TWhirlPool.Create;
begin
  Inherited Create(64, 64);

  System.SetLength(Fm_hash, 8);

end;

procedure TWhirlPool.Finish;
var
  bits: UInt64;
  padindex: Int32;
  pad: THashLibByteArray;
begin
  bits := Fm_processed_bytes * 8;
  if (Fm_buffer.Pos > 31) then

    padindex := (120 - Fm_buffer.Pos)
  else
    padindex := (56 - Fm_buffer.Pos);

  System.SetLength(pad, padindex + 8);

  pad[0] := $80;

  bits := TConverters.be2me_64(bits);

  TConverters.ReadUInt64AsBytesLE(bits, pad, padindex);

  padindex := padindex + 8;

  TransformBytes(pad, 0, padindex);

end;

function TWhirlPool.GetResult: THashLibByteArray;
begin
  System.SetLength(result, System.Length(Fm_hash) * System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(Fm_hash), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TWhirlPool.Initialize;
begin
  TArrayUtils.ZeroFill(Fm_hash);
  Inherited Initialize();
end;

class function TWhirlPool.MaskWithReductionPolynomial(input: UInt32): UInt32;
begin
  if (input >= $100) then
    input := input xor REDUCTION_POLYNOMIAL;
  result := input;
end;

class function TWhirlPool.PackIntoUInt64(b7, b6, b5, b4, b3, b2, b1,
  b0: UInt32): UInt64;
begin
  result := (UInt64(b7) shl 56) xor (UInt64(b6) shl 48) xor (UInt64(b5) shl 40)
    xor (UInt64(b4) shl 32) xor (UInt64(b3) shl 24) xor (UInt64(b2) shl 16)
    xor (UInt64(b1) shl 8) xor b0;
end;

procedure TWhirlPool.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  data, k, m, temp: array [0 .. 7] of UInt64;
  i, round: Int32;

begin

  TConverters.be64_copy(a_data, a_index, @(data[0]), 0, a_data_length);

  i := 0;
  while i < 8 do
  begin
    k[i] := Fm_hash[i];
    temp[i] := data[i] xor k[i];
    System.Inc(i);
  end;

  round := 1;

  while round <= ROUNDS do
  begin

    i := 0;

    while i < 8 do
    begin

      m[i] := 0;
      m[i] := m[i] xor (Fs_C0[Byte(k[(i - 0) and 7] shr 56)]);
      m[i] := m[i] xor (Fs_C1[Byte(k[(i - 1) and 7] shr 48)]);
      m[i] := m[i] xor (Fs_C2[Byte(k[(i - 2) and 7] shr 40)]);
      m[i] := m[i] xor (Fs_C3[Byte(k[(i - 3) and 7] shr 32)]);
      m[i] := m[i] xor (Fs_C4[Byte(k[(i - 4) and 7] shr 24)]);
      m[i] := m[i] xor (Fs_C5[Byte(k[(i - 5) and 7] shr 16)]);
      m[i] := m[i] xor (Fs_C6[Byte(k[(i - 6) and 7] shr 8)]);
      m[i] := m[i] xor (Fs_C7[Byte(k[(i - 7) and 7])]);

      System.Inc(i);
    end;

    System.Move(m[0], k[0], 8 * System.SizeOf(UInt64));

    k[0] := k[0] xor Fs_rc[round];

    i := 0;

    while i < 8 do

    begin
      m[i] := k[i];

      m[i] := m[i] xor (Fs_C0[Byte(temp[(i - 0) and 7] shr 56)]);
      m[i] := m[i] xor (Fs_C1[Byte(temp[(i - 1) and 7] shr 48)]);
      m[i] := m[i] xor (Fs_C2[Byte(temp[(i - 2) and 7] shr 40)]);
      m[i] := m[i] xor (Fs_C3[Byte(temp[(i - 3) and 7] shr 32)]);
      m[i] := m[i] xor (Fs_C4[Byte(temp[(i - 4) and 7] shr 24)]);
      m[i] := m[i] xor (Fs_C5[Byte(temp[(i - 5) and 7] shr 16)]);
      m[i] := m[i] xor (Fs_C6[Byte(temp[(i - 6) and 7] shr 8)]);
      m[i] := m[i] xor (Fs_C7[Byte(temp[(i - 7) and 7])]);

      System.Inc(i);
    end;

    System.Move(m[0], temp[0], System.Length(temp) * System.SizeOf(UInt64));

    System.Inc(round);

  end;

  i := 0;

  while i < 8 do
  begin
    Fm_hash[i] := Fm_hash[i] xor (temp[i] xor data[i]);

    System.Inc(i);
  end;

  System.FillChar(data, System.SizeOf(data), UInt64(0));

end;

class constructor TWhirlPool.WhirlPool;
var
  i, r: Int32;
  v1, v2, v4, v5, v8, v9: UInt32;
begin

  System.SetLength(Fs_C0, 256);
  System.SetLength(Fs_C1, 256);
  System.SetLength(Fs_C2, 256);
  System.SetLength(Fs_C3, 256);
  System.SetLength(Fs_C4, 256);
  System.SetLength(Fs_C5, 256);
  System.SetLength(Fs_C6, 256);
  System.SetLength(Fs_C7, 256);

  System.SetLength(Fs_rc, ROUNDS + 1);

  i := 0;
  while i < 256 do

  begin
    v1 := s_SBOX[i];
    v2 := MaskWithReductionPolynomial(v1 shl 1);
    v4 := MaskWithReductionPolynomial(v2 shl 1);
    v5 := v4 xor v1;
    v8 := MaskWithReductionPolynomial(v4 shl 1);
    v9 := v8 xor v1;

    Fs_C0[i] := PackIntoUInt64(v1, v1, v4, v1, v8, v5, v2, v9);
    Fs_C1[i] := PackIntoUInt64(v9, v1, v1, v4, v1, v8, v5, v2);
    Fs_C2[i] := PackIntoUInt64(v2, v9, v1, v1, v4, v1, v8, v5);
    Fs_C3[i] := PackIntoUInt64(v5, v2, v9, v1, v1, v4, v1, v8);
    Fs_C4[i] := PackIntoUInt64(v8, v5, v2, v9, v1, v1, v4, v1);
    Fs_C5[i] := PackIntoUInt64(v1, v8, v5, v2, v9, v1, v1, v4);
    Fs_C6[i] := PackIntoUInt64(v4, v1, v8, v5, v2, v9, v1, v1);
    Fs_C7[i] := PackIntoUInt64(v1, v4, v1, v8, v5, v2, v9, v1);

    System.Inc(i);
  end;

  Fs_rc[0] := 0;

  r := 1;

  while r <= ROUNDS do

  begin

    i := 8 * (r - 1);
    Fs_rc[r] := (Fs_C0[i] and $FF00000000000000)
      xor (Fs_C1[i + 1] and $00FF000000000000)
      xor (Fs_C2[i + 2] and $0000FF0000000000)
      xor (Fs_C3[i + 3] and $000000FF00000000)
      xor (Fs_C4[i + 4] and $00000000FF000000)
      xor (Fs_C5[i + 5] and $0000000000FF0000)
      xor (Fs_C6[i + 6] and $000000000000FF00)
      xor (Fs_C7[i + 7] and $00000000000000FF);

    System.Inc(r);
  end;
end;

end.
