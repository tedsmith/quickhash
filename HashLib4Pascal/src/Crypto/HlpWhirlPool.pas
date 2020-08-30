unit HlpWhirlPool;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
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
  var
    FHash: THashLibUInt64Array;

    class var

      FSC0, FSC1, FSC2, FSC3, FSC4, FSC5, FSC6, FSC7, FSRC: THashLibUInt64Array;

{$REGION 'Consts'}

  const
    ROUNDS = Int32(10);
    REDUCTION_POLYNOMIAL = UInt32($011D);

    SSBOX: array [0 .. 255] of UInt32 = ($18, $23, $C6, $E8, $87, $B8, $01, $4F,
      $36, $A6, $D2, $F5, $79, $6F, $91, $52, $60, $BC, $9B, $8E, $A3, $0C, $7B,
      $35, $1D, $E0, $D7, $C2, $2E, $4B, $FE, $57, $15, $77, $37, $E5, $9F, $F0,
      $4A, $DA, $58, $C9, $29, $0A, $B1, $A0, $6B, $85, $BD, $5D, $10, $F4, $CB,
      $3E, $05, $67, $E4, $27, $41, $8B, $A7, $7D, $95, $D8, $FB, $EE, $7C, $66,
      $DD, $17, $47, $9E, $CA, $2D, $BF, $07, $AD, $5A, $83, $33, $63, $02, $AA,
      $71, $C8, $19, $49, $D9, $F2, $E3, $5B, $88, $9A, $26, $32, $B0, $E9, $0F,
      $D5, $80, $BE, $CD, $34, $48, $FF, $7A, $90, $5F, $20, $68, $1A, $AE, $B4,
      $54, $93, $22, $64, $F1, $73, $12, $40, $08, $C3, $EC, $DB, $A1, $8D, $3D,
      $97, $00, $CF, $2B, $76, $82, $D6, $1B, $B5, $AF, $6A, $50, $45, $F3, $30,
      $EF, $3F, $55, $A2, $EA, $65, $BA, $2F, $C0, $DE, $1C, $FD, $4D, $92, $75,
      $06, $8A, $B2, $E6, $0E, $1F, $62, $D4, $A8, $96, $F9, $C5, $25, $59, $84,
      $72, $39, $4C, $5E, $78, $38, $8C, $D1, $A5, $E2, $61, $B3, $21, $9C, $1E,
      $43, $C7, $FC, $04, $51, $99, $6D, $0D, $FA, $DF, $7E, $24, $3B, $AB, $CE,
      $11, $8F, $4E, $B7, $EB, $3C, $81, $94, $F7, $B9, $13, $2C, $D3, $E7, $6E,
      $C4, $03, $56, $44, $7F, $A9, $2A, $BB, $C1, $53, $DC, $0B, $9D, $6C, $31,
      $74, $F6, $46, $AC, $89, $14, $E1, $16, $3A, $69, $09, $70, $B6, $D0, $ED,
      $CC, $42, $98, $A4, $28, $5C, $F8, $86);

{$ENDREGION}
    class constructor WhirlPool;

    class function PackIntoUInt64(b7, b6, b5, b4, b3, b2, b1, b0: UInt32)
      : UInt64; static; inline;
    class function MaskWithReductionPolynomial(AInput: UInt32): UInt32;
      static; inline;

  strict protected
    function GetResult(): THashLibByteArray; override;
    procedure Finish(); override;
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TWhirlPool }

function TWhirlPool.Clone(): IHash;
var
  LHashInstance: TWhirlPool;
begin
  LHashInstance := TWhirlPool.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TWhirlPool.Create;
begin
  Inherited Create(64, 64);
  System.SetLength(FHash, 8);
end;

procedure TWhirlPool.Finish;
var
  LBits: UInt64;
  LPadIndex: Int32;
  LPad: THashLibByteArray;
begin
  LBits := FProcessedBytesCount * 8;
  if (FBuffer.Position > 31) then
  begin
    LPadIndex := (120 - FBuffer.Position)
  end
  else
  begin
    LPadIndex := (56 - FBuffer.Position);
  end;

  System.SetLength(LPad, LPadIndex + 8);

  LPad[0] := $80;

  LBits := TConverters.be2me_64(LBits);

  TConverters.ReadUInt64AsBytesLE(LBits, LPad, LPadIndex);

  LPadIndex := LPadIndex + 8;

  TransformBytes(LPad, 0, LPadIndex);
end;

function TWhirlPool.GetResult: THashLibByteArray;
begin
  System.SetLength(result, System.Length(FHash) * System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(FHash), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TWhirlPool.Initialize;
begin
  TArrayUtils.ZeroFill(FHash);
  Inherited Initialize();
end;

class function TWhirlPool.MaskWithReductionPolynomial(AInput: UInt32): UInt32;
begin
  if (AInput >= $100) then
  begin
    AInput := AInput xor REDUCTION_POLYNOMIAL;
  end;
  result := AInput;
end;

class function TWhirlPool.PackIntoUInt64(b7, b6, b5, b4, b3, b2, b1,
  b0: UInt32): UInt64;
begin
  result := (UInt64(b7) shl 56) xor (UInt64(b6) shl 48) xor (UInt64(b5) shl 40)
    xor (UInt64(b4) shl 32) xor (UInt64(b3) shl 24) xor (UInt64(b2) shl 16)
    xor (UInt64(b1) shl 8) xor b0;
end;

procedure TWhirlPool.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  LData, LK, LM, LTemp: array [0 .. 7] of UInt64;
  LIdx, LRound: Int32;
begin
  TConverters.be64_copy(AData, AIndex, @(LData[0]), 0, ADataLength);

  LIdx := 0;
  while LIdx < 8 do
  begin
    LK[LIdx] := FHash[LIdx];
    LTemp[LIdx] := LData[LIdx] xor LK[LIdx];
    System.Inc(LIdx);
  end;

  LRound := 1;

  while LRound <= ROUNDS do
  begin
    LIdx := 0;

    while LIdx < 8 do
    begin
      LM[LIdx] := 0;
      LM[LIdx] := LM[LIdx] xor (FSC0[Byte(LK[(LIdx - 0) and 7] shr 56)]);
      LM[LIdx] := LM[LIdx] xor (FSC1[Byte(LK[(LIdx - 1) and 7] shr 48)]);
      LM[LIdx] := LM[LIdx] xor (FSC2[Byte(LK[(LIdx - 2) and 7] shr 40)]);
      LM[LIdx] := LM[LIdx] xor (FSC3[Byte(LK[(LIdx - 3) and 7] shr 32)]);
      LM[LIdx] := LM[LIdx] xor (FSC4[Byte(LK[(LIdx - 4) and 7] shr 24)]);
      LM[LIdx] := LM[LIdx] xor (FSC5[Byte(LK[(LIdx - 5) and 7] shr 16)]);
      LM[LIdx] := LM[LIdx] xor (FSC6[Byte(LK[(LIdx - 6) and 7] shr 8)]);
      LM[LIdx] := LM[LIdx] xor (FSC7[Byte(LK[(LIdx - 7) and 7])]);

      System.Inc(LIdx);
    end;

    System.Move(LM[0], LK[0], 8 * System.SizeOf(UInt64));

    LK[0] := LK[0] xor FSRC[LRound];

    LIdx := 0;

    while LIdx < 8 do
    begin
      LM[LIdx] := LK[LIdx];

      LM[LIdx] := LM[LIdx] xor (FSC0[Byte(LTemp[(LIdx - 0) and 7] shr 56)]);
      LM[LIdx] := LM[LIdx] xor (FSC1[Byte(LTemp[(LIdx - 1) and 7] shr 48)]);
      LM[LIdx] := LM[LIdx] xor (FSC2[Byte(LTemp[(LIdx - 2) and 7] shr 40)]);
      LM[LIdx] := LM[LIdx] xor (FSC3[Byte(LTemp[(LIdx - 3) and 7] shr 32)]);
      LM[LIdx] := LM[LIdx] xor (FSC4[Byte(LTemp[(LIdx - 4) and 7] shr 24)]);
      LM[LIdx] := LM[LIdx] xor (FSC5[Byte(LTemp[(LIdx - 5) and 7] shr 16)]);
      LM[LIdx] := LM[LIdx] xor (FSC6[Byte(LTemp[(LIdx - 6) and 7] shr 8)]);
      LM[LIdx] := LM[LIdx] xor (FSC7[Byte(LTemp[(LIdx - 7) and 7])]);

      System.Inc(LIdx);
    end;

    System.Move(LM[0], LTemp[0], System.Length(LTemp) * System.SizeOf(UInt64));

    System.Inc(LRound);
  end;

  LIdx := 0;

  while LIdx < 8 do
  begin
    FHash[LIdx] := FHash[LIdx] xor (LTemp[LIdx] xor LData[LIdx]);

    System.Inc(LIdx);
  end;

  System.FillChar(LData, System.SizeOf(LData), UInt64(0));
end;

class constructor TWhirlPool.WhirlPool;
var
  LIdx, LR: Int32;
  v1, v2, v4, v5, v8, v9: UInt32;
begin
  System.SetLength(FSC0, 256);
  System.SetLength(FSC1, 256);
  System.SetLength(FSC2, 256);
  System.SetLength(FSC3, 256);
  System.SetLength(FSC4, 256);
  System.SetLength(FSC5, 256);
  System.SetLength(FSC6, 256);
  System.SetLength(FSC7, 256);

  System.SetLength(FSRC, ROUNDS + 1);

  LIdx := 0;
  while LIdx < 256 do
  begin
    v1 := SSBOX[LIdx];
    v2 := MaskWithReductionPolynomial(v1 shl 1);
    v4 := MaskWithReductionPolynomial(v2 shl 1);
    v5 := v4 xor v1;
    v8 := MaskWithReductionPolynomial(v4 shl 1);
    v9 := v8 xor v1;

    FSC0[LIdx] := PackIntoUInt64(v1, v1, v4, v1, v8, v5, v2, v9);
    FSC1[LIdx] := PackIntoUInt64(v9, v1, v1, v4, v1, v8, v5, v2);
    FSC2[LIdx] := PackIntoUInt64(v2, v9, v1, v1, v4, v1, v8, v5);
    FSC3[LIdx] := PackIntoUInt64(v5, v2, v9, v1, v1, v4, v1, v8);
    FSC4[LIdx] := PackIntoUInt64(v8, v5, v2, v9, v1, v1, v4, v1);
    FSC5[LIdx] := PackIntoUInt64(v1, v8, v5, v2, v9, v1, v1, v4);
    FSC6[LIdx] := PackIntoUInt64(v4, v1, v8, v5, v2, v9, v1, v1);
    FSC7[LIdx] := PackIntoUInt64(v1, v4, v1, v8, v5, v2, v9, v1);

    System.Inc(LIdx);
  end;

  FSRC[0] := 0;

  LR := 1;

  while LR <= ROUNDS do
  begin
    LIdx := 8 * (LR - 1);
    FSRC[LR] := (FSC0[LIdx] and $FF00000000000000)
      xor (FSC1[LIdx + 1] and $00FF000000000000)
      xor (FSC2[LIdx + 2] and $0000FF0000000000)
      xor (FSC3[LIdx + 3] and $000000FF00000000)
      xor (FSC4[LIdx + 4] and $00000000FF000000)
      xor (FSC5[LIdx + 5] and $0000000000FF0000)
      xor (FSC6[LIdx + 6] and $000000000000FF00)
      xor (FSC7[LIdx + 7] and $00000000000000FF);

    System.Inc(LR);
  end;
end;

end.
