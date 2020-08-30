unit HlpHAS160;

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
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpHashCryptoNotBuildIn;

type
  THAS160 = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private
  var
    FHash: THashLibUInt32Array;

{$REGION 'Consts'}

  const
    SRot: array [0 .. 19] of Int32 = (5, 11, 7, 15, 6, 13, 8, 14, 7, 12, 9, 11,
      8, 15, 6, 12, 9, 14, 5, 13);

    Stor: array [0 .. 19] of Int32 = (27, 21, 25, 17, 26, 19, 24, 18, 25, 20,
      23, 21, 24, 17, 26, 20, 23, 18, 27, 19);

    SIndex: array [0 .. 79] of Int32 = (18, 0, 1, 2, 3, 19, 4, 5, 6, 7, 16, 8,
      9, 10, 11, 17, 12, 13, 14, 15, 18, 3, 6, 9, 12, 19, 15, 2, 5, 8, 16, 11,
      14, 1, 4, 17, 7, 10, 13, 0, 18, 12, 5, 14, 7, 19, 0, 9, 2, 11, 16, 4, 13,
      6, 15, 17, 8, 1, 10, 3, 18, 7, 2, 13, 8, 19, 3, 14, 9, 4, 16, 15, 10, 5,
      0, 17, 11, 6, 1, 12);

{$ENDREGION}
  strict protected
    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;
  end;

implementation

{ THAS160 }

function THAS160.Clone(): IHash;
var
  LHashInstance: THAS160;
begin
  LHashInstance := THAS160.Create();
  LHashInstance.FHash := System.Copy(FHash);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THAS160.Create;
begin
  Inherited Create(20, 64);
  System.SetLength(FHash, 5);
end;

procedure THAS160.Finish;
var
  LPadIndex: Int32;
  LBits: UInt64;
  LPad: THashLibByteArray;
begin
  LBits := FProcessedBytesCount * 8;
  if (FBuffer.Position < 56) then
  begin
    LPadIndex := (56 - FBuffer.Position)
  end
  else
  begin
    LPadIndex := (120 - FBuffer.Position);
  end;

  System.SetLength(LPad, LPadIndex + 8);

  LPad[0] := $80;

  LBits := TConverters.le2me_64(LBits);

  TConverters.ReadUInt64AsBytesLE(LBits, LPad, LPadIndex);

  LPadIndex := LPadIndex + 8;

  TransformBytes(LPad, 0, LPadIndex);
end;

function THAS160.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 5 * System.SizeOf(UInt32));
  TConverters.le32_copy(PCardinal(FHash), 0, PByte(result), 0,
    System.Length(result));
end;

procedure THAS160.Initialize;
begin
  FHash[0] := $67452301;
  FHash[1] := $EFCDAB89;
  FHash[2] := $98BADCFE;
  FHash[3] := $10325476;
  FHash[4] := $C3D2E1F0;
  Inherited Initialize();
end;

procedure THAS160.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  A, B, C, D, E, T: UInt32;
  R: Int32;
  LData: array [0 .. 19] of UInt32;
begin
  A := FHash[0];
  B := FHash[1];
  C := FHash[2];
  D := FHash[3];
  E := FHash[4];

  TConverters.le32_copy(AData, AIndex, @(LData[0]), 0, ADataLength);

  LData[16] := LData[0] xor LData[1] xor LData[2] xor LData[3];
  LData[17] := LData[4] xor LData[5] xor LData[6] xor LData[7];
  LData[18] := LData[8] xor LData[9] xor LData[10] xor LData[11];
  LData[19] := LData[12] xor LData[13] xor LData[14] xor LData[15];

  R := 0;
  while R < 20 do
  begin
    T := LData[SIndex[R]] + (A shl SRot[R] or A shr Stor[R]) +
      ((B and C) or (not B and D)) + E;
    E := D;
    D := C;
    C := B shl 10 or B shr 22;
    B := A;
    A := T;
    System.Inc(R);
  end;

  LData[16] := LData[3] xor LData[6] xor LData[9] xor LData[12];
  LData[17] := LData[2] xor LData[5] xor LData[8] xor LData[15];
  LData[18] := LData[1] xor LData[4] xor LData[11] xor LData[14];
  LData[19] := LData[0] xor LData[7] xor LData[10] xor LData[13];

  R := 20;
  while R < 40 do
  begin
    T := LData[SIndex[R]] + $5A827999 +
      (A shl SRot[R - 20] or A shr Stor[R - 20]) + (B xor C xor D) + E;
    E := D;
    D := C;
    C := B shl 17 or B shr 15;
    B := A;
    A := T;
    System.Inc(R);
  end;

  LData[16] := LData[5] xor LData[7] xor LData[12] xor LData[14];
  LData[17] := LData[0] xor LData[2] xor LData[9] xor LData[11];
  LData[18] := LData[4] xor LData[6] xor LData[13] xor LData[15];
  LData[19] := LData[1] xor LData[3] xor LData[8] xor LData[10];

  R := 40;
  while R < 60 do
  begin
    T := LData[SIndex[R]] + $6ED9EBA1 +
      (A shl SRot[R - 40] or A shr Stor[R - 40]) + (C xor (B or not D)) + E;
    E := D;
    D := C;
    C := B shl 25 or B shr 7;
    B := A;
    A := T;
    System.Inc(R);
  end;

  LData[16] := LData[2] xor LData[7] xor LData[8] xor LData[13];
  LData[17] := LData[3] xor LData[4] xor LData[9] xor LData[14];
  LData[18] := LData[0] xor LData[5] xor LData[10] xor LData[15];
  LData[19] := LData[1] xor LData[6] xor LData[11] xor LData[12];

  R := 60;
  while R < 80 do
  begin
    T := LData[SIndex[R]] + $8F1BBCDC +
      (A shl SRot[R - 60] or A shr Stor[R - 60]) + (B xor C xor D) + E;
    E := D;
    D := C;
    C := (B shl 30) or (B shr 2);
    B := A;
    A := T;
    System.Inc(R);
  end;

  FHash[0] := FHash[0] + A;
  FHash[1] := FHash[1] + B;
  FHash[2] := FHash[2] + C;
  FHash[3] := FHash[3] + D;
  FHash[4] := FHash[4] + E;

  System.FillChar(LData, System.SizeOf(LData), UInt32(0));
end;

end.
