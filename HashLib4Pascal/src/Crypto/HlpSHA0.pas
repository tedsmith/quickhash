unit HlpSHA0;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpBits,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpHashLibTypes,
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpHashCryptoNotBuildIn;

type
  TSHA0 = class(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict protected
  var
    FState: THashLibUInt32Array;

{$REGION 'Consts'}

  const

    C1 = UInt32($5A827999);
    C2 = UInt32($6ED9EBA1);
    C3 = UInt32($8F1BBCDC);
    C4 = UInt32($CA62C1D6);

{$ENDREGION}
    procedure Finish(); override;
    procedure Expand(AData: PCardinal); virtual;
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TSHA0 }

function TSHA0.Clone(): IHash;
var
  LHashInstance: TSHA0;
begin
  LHashInstance := TSHA0.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA0.Create;
begin
  Inherited Create(20, 64);
  System.SetLength(FState, 5);
end;

procedure TSHA0.Expand(AData: PCardinal);
{$IFNDEF USE_UNROLLED_VARIANT}
var
  LJdx: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
begin
{$IFDEF USE_UNROLLED_VARIANT}
  AData[16] := ((AData[16 - 3] xor AData[16 - 8]) xor AData[16 - 14])
    xor AData[0];
  AData[17] := ((AData[17 - 3] xor AData[17 - 8]) xor AData[17 - 14])
    xor AData[17 - 16];
  AData[18] := ((AData[18 - 3] xor AData[18 - 8]) xor AData[18 - 14])
    xor AData[18 - 16];
  AData[19] := ((AData[19 - 3] xor AData[19 - 8]) xor AData[19 - 14])
    xor AData[19 - 16];
  AData[20] := ((AData[20 - 3] xor AData[20 - 8]) xor AData[20 - 14])
    xor AData[20 - 16];
  AData[21] := ((AData[21 - 3] xor AData[21 - 8]) xor AData[21 - 14])
    xor AData[21 - 16];
  AData[22] := ((AData[22 - 3] xor AData[22 - 8]) xor AData[22 - 14])
    xor AData[22 - 16];
  AData[23] := ((AData[23 - 3] xor AData[23 - 8]) xor AData[23 - 14])
    xor AData[23 - 16];
  AData[24] := ((AData[24 - 3] xor AData[24 - 8]) xor AData[24 - 14])
    xor AData[24 - 16];
  AData[25] := ((AData[25 - 3] xor AData[25 - 8]) xor AData[25 - 14])
    xor AData[25 - 16];
  AData[26] := ((AData[26 - 3] xor AData[26 - 8]) xor AData[26 - 14])
    xor AData[26 - 16];
  AData[27] := ((AData[27 - 3] xor AData[27 - 8]) xor AData[27 - 14])
    xor AData[27 - 16];
  AData[28] := ((AData[28 - 3] xor AData[28 - 8]) xor AData[28 - 14])
    xor AData[28 - 16];
  AData[29] := ((AData[29 - 3] xor AData[29 - 8]) xor AData[29 - 14])
    xor AData[29 - 16];
  AData[30] := ((AData[30 - 3] xor AData[30 - 8]) xor AData[30 - 14])
    xor AData[30 - 16];
  AData[31] := ((AData[31 - 3] xor AData[31 - 8]) xor AData[31 - 14])
    xor AData[31 - 16];
  AData[32] := ((AData[32 - 3] xor AData[32 - 8]) xor AData[32 - 14])
    xor AData[32 - 16];
  AData[33] := ((AData[33 - 3] xor AData[33 - 8]) xor AData[33 - 14])
    xor AData[33 - 16];
  AData[34] := ((AData[34 - 3] xor AData[34 - 8]) xor AData[34 - 14])
    xor AData[34 - 16];
  AData[35] := ((AData[35 - 3] xor AData[35 - 8]) xor AData[35 - 14])
    xor AData[35 - 16];
  AData[36] := ((AData[36 - 3] xor AData[36 - 8]) xor AData[36 - 14])
    xor AData[36 - 16];
  AData[37] := ((AData[37 - 3] xor AData[37 - 8]) xor AData[37 - 14])
    xor AData[37 - 16];
  AData[38] := ((AData[38 - 3] xor AData[38 - 8]) xor AData[38 - 14])
    xor AData[38 - 16];
  AData[39] := ((AData[39 - 3] xor AData[39 - 8]) xor AData[39 - 14])
    xor AData[39 - 16];
  AData[40] := ((AData[40 - 3] xor AData[40 - 8]) xor AData[40 - 14])
    xor AData[40 - 16];
  AData[41] := ((AData[41 - 3] xor AData[41 - 8]) xor AData[41 - 14])
    xor AData[41 - 16];
  AData[42] := ((AData[42 - 3] xor AData[42 - 8]) xor AData[42 - 14])
    xor AData[42 - 16];
  AData[43] := ((AData[43 - 3] xor AData[43 - 8]) xor AData[43 - 14])
    xor AData[43 - 16];
  AData[44] := ((AData[44 - 3] xor AData[44 - 8]) xor AData[44 - 14])
    xor AData[44 - 16];
  AData[45] := ((AData[45 - 3] xor AData[45 - 8]) xor AData[45 - 14])
    xor AData[45 - 16];
  AData[46] := ((AData[46 - 3] xor AData[46 - 8]) xor AData[46 - 14])
    xor AData[46 - 16];
  AData[47] := ((AData[47 - 3] xor AData[47 - 8]) xor AData[47 - 14])
    xor AData[47 - 16];
  AData[48] := ((AData[48 - 3] xor AData[48 - 8]) xor AData[48 - 14])
    xor AData[48 - 16];
  AData[49] := ((AData[49 - 3] xor AData[49 - 8]) xor AData[49 - 14])
    xor AData[49 - 16];
  AData[50] := ((AData[50 - 3] xor AData[50 - 8]) xor AData[50 - 14])
    xor AData[50 - 16];
  AData[51] := ((AData[51 - 3] xor AData[51 - 8]) xor AData[51 - 14])
    xor AData[51 - 16];
  AData[52] := ((AData[52 - 3] xor AData[52 - 8]) xor AData[52 - 14])
    xor AData[52 - 16];
  AData[53] := ((AData[53 - 3] xor AData[53 - 8]) xor AData[53 - 14])
    xor AData[53 - 16];
  AData[54] := ((AData[54 - 3] xor AData[54 - 8]) xor AData[54 - 14])
    xor AData[54 - 16];
  AData[55] := ((AData[55 - 3] xor AData[55 - 8]) xor AData[55 - 14])
    xor AData[55 - 16];
  AData[56] := ((AData[56 - 3] xor AData[56 - 8]) xor AData[56 - 14])
    xor AData[56 - 16];
  AData[57] := ((AData[57 - 3] xor AData[57 - 8]) xor AData[57 - 14])
    xor AData[57 - 16];
  AData[58] := ((AData[58 - 3] xor AData[58 - 8]) xor AData[58 - 14])
    xor AData[58 - 16];
  AData[59] := ((AData[59 - 3] xor AData[59 - 8]) xor AData[59 - 14])
    xor AData[59 - 16];
  AData[60] := ((AData[60 - 3] xor AData[60 - 8]) xor AData[60 - 14])
    xor AData[60 - 16];
  AData[61] := ((AData[61 - 3] xor AData[61 - 8]) xor AData[61 - 14])
    xor AData[61 - 16];
  AData[62] := ((AData[62 - 3] xor AData[62 - 8]) xor AData[62 - 14])
    xor AData[62 - 16];
  AData[63] := ((AData[63 - 3] xor AData[63 - 8]) xor AData[63 - 14])
    xor AData[63 - 16];
  AData[64] := ((AData[64 - 3] xor AData[64 - 8]) xor AData[64 - 14])
    xor AData[64 - 16];
  AData[65] := ((AData[65 - 3] xor AData[65 - 8]) xor AData[65 - 14])
    xor AData[65 - 16];
  AData[66] := ((AData[66 - 3] xor AData[66 - 8]) xor AData[66 - 14])
    xor AData[66 - 16];
  AData[67] := ((AData[67 - 3] xor AData[67 - 8]) xor AData[67 - 14])
    xor AData[67 - 16];
  AData[68] := ((AData[68 - 3] xor AData[68 - 8]) xor AData[68 - 14])
    xor AData[68 - 16];
  AData[69] := ((AData[69 - 3] xor AData[69 - 8]) xor AData[69 - 14])
    xor AData[69 - 16];
  AData[70] := ((AData[70 - 3] xor AData[70 - 8]) xor AData[70 - 14])
    xor AData[70 - 16];
  AData[71] := ((AData[71 - 3] xor AData[71 - 8]) xor AData[71 - 14])
    xor AData[71 - 16];
  AData[72] := ((AData[72 - 3] xor AData[72 - 8]) xor AData[72 - 14])
    xor AData[72 - 16];
  AData[73] := ((AData[73 - 3] xor AData[73 - 8]) xor AData[73 - 14])
    xor AData[73 - 16];
  AData[74] := ((AData[74 - 3] xor AData[74 - 8]) xor AData[74 - 14])
    xor AData[74 - 16];
  AData[75] := ((AData[75 - 3] xor AData[75 - 8]) xor AData[75 - 14])
    xor AData[75 - 16];
  AData[76] := ((AData[76 - 3] xor AData[76 - 8]) xor AData[76 - 14])
    xor AData[76 - 16];
  AData[77] := ((AData[77 - 3] xor AData[77 - 8]) xor AData[77 - 14])
    xor AData[77 - 16];
  AData[78] := ((AData[78 - 3] xor AData[78 - 8]) xor AData[78 - 14])
    xor AData[78 - 16];
  AData[79] := ((AData[79 - 3] xor AData[79 - 8]) xor AData[79 - 14])
    xor AData[79 - 16];

{$ELSE}
  for LJdx := 16 to 79 do
  begin
    AData[LJdx] := ((AData[LJdx - 3] xor AData[LJdx - 8]) xor AData[LJdx - 14])
      xor AData[LJdx - 16];
  end;
{$ENDIF USE_UNROLLED_VARIANT}
end;

procedure TSHA0.Finish;
var
  LBits: UInt64;
  LPadIndex: Int32;
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

  LBits := TConverters.be2me_64(LBits);

  TConverters.ReadUInt64AsBytesLE(LBits, LPad, LPadIndex);

  LPadIndex := LPadIndex + 8;

  TransformBytes(LPad, 0, LPadIndex);
end;

function TSHA0.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 5 * System.SizeOf(UInt32));
  TConverters.be32_copy(PCardinal(FState), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TSHA0.Initialize;
begin
  FState[0] := $67452301;
  FState[1] := $EFCDAB89;
  FState[2] := $98BADCFE;
  FState[3] := $10325476;
  FState[4] := $C3D2E1F0;
  Inherited Initialize();
end;

procedure TSHA0.TransformBlock(AData: PByte; ADataLength: Int32; AIndex: Int32);
var
  A, B, C, D, E: UInt32;
  LData: array [0 .. 79] of UInt32;
  LPtrData: PCardinal;
begin
  LPtrData := @(LData[0]);

  TConverters.be32_copy(AData, AIndex, LPtrData, 0, ADataLength);

  Expand(LPtrData);

  A := FState[0];
  B := FState[1];
  C := FState[2];
  D := FState[3];
  E := FState[4];

  E := (LData[0] + C1 + TBits.RotateLeft32(A, 5) +
    (D xor (B and (C xor D)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[1] + C1 + TBits.RotateLeft32(E, 5) +
    (C xor (A and (B xor C)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[2] + C1 + TBits.RotateLeft32(D, 5) +
    (B xor (E and (A xor B)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[3] + C1 + TBits.RotateLeft32(C, 5) +
    (A xor (D and (E xor A)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[4] + C1 + TBits.RotateLeft32(B, 5) +
    (E xor (C and (D xor E)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[5] + C1 + TBits.RotateLeft32(A, 5) +
    (D xor (B and (C xor D)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[6] + C1 + TBits.RotateLeft32(E, 5) +
    (C xor (A and (B xor C)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[7] + C1 + TBits.RotateLeft32(D, 5) +
    (B xor (E and (A xor B)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[8] + C1 + TBits.RotateLeft32(C, 5) +
    (A xor (D and (E xor A)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[9] + C1 + TBits.RotateLeft32(B, 5) +
    (E xor (C and (D xor E)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[10] + C1 + TBits.RotateLeft32(A, 5) +
    (D xor (B and (C xor D)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[11] + C1 + TBits.RotateLeft32(E, 5) +
    (C xor (A and (B xor C)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[12] + C1 + TBits.RotateLeft32(D, 5) +
    (B xor (E and (A xor B)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[13] + C1 + TBits.RotateLeft32(C, 5) +
    (A xor (D and (E xor A)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[14] + C1 + TBits.RotateLeft32(B, 5) +
    (E xor (C and (D xor E)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[15] + C1 + TBits.RotateLeft32(A, 5) +
    (D xor (B and (C xor D)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[16] + C1 + TBits.RotateLeft32(E, 5) +
    (C xor (A and (B xor C)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[17] + C1 + TBits.RotateLeft32(D, 5) +
    (B xor (E and (A xor B)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[18] + C1 + TBits.RotateLeft32(C, 5) +
    (A xor (D and (E xor A)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[19] + C1 + TBits.RotateLeft32(B, 5) +
    (E xor (C and (D xor E)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[20] + C2 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[21] + C2 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[22] + C2 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[23] + C2 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[24] + C2 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[25] + C2 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[26] + C2 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[27] + C2 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[28] + C2 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[29] + C2 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[30] + C2 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[31] + C2 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[32] + C2 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[33] + C2 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[34] + C2 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[35] + C2 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[36] + C2 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[37] + C2 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[38] + C2 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[39] + C2 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[40] + C3 + TBits.RotateLeft32(A, 5) +
    ((B and C) or (D and (B or C)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[41] + C3 + TBits.RotateLeft32(E, 5) +
    ((A and B) or (C and (A or B)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[42] + C3 + TBits.RotateLeft32(D, 5) +
    ((E and A) or (B and (E or A)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[43] + C3 + TBits.RotateLeft32(C, 5) +
    ((D and E) or (A and (D or E)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[44] + C3 + TBits.RotateLeft32(B, 5) +
    ((C and D) or (E and (C or D)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[45] + C3 + TBits.RotateLeft32(A, 5) +
    ((B and C) or (D and (B or C)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[46] + C3 + TBits.RotateLeft32(E, 5) +
    ((A and B) or (C and (A or B)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[47] + C3 + TBits.RotateLeft32(D, 5) +
    ((E and A) or (B and (E or A)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[48] + C3 + TBits.RotateLeft32(C, 5) +
    ((D and E) or (A and (D or E)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[49] + C3 + TBits.RotateLeft32(B, 5) +
    ((C and D) or (E and (C or D)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[50] + C3 + TBits.RotateLeft32(A, 5) +
    ((B and C) or (D and (B or C)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[51] + C3 + TBits.RotateLeft32(E, 5) +
    ((A and B) or (C and (A or B)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[52] + C3 + TBits.RotateLeft32(D, 5) +
    ((E and A) or (B and (E or A)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[53] + C3 + TBits.RotateLeft32(C, 5) +
    ((D and E) or (A and (D or E)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[54] + C3 + TBits.RotateLeft32(B, 5) +
    ((C and D) or (E and (C or D)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[55] + C3 + TBits.RotateLeft32(A, 5) +
    ((B and C) or (D and (B or C)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[56] + C3 + TBits.RotateLeft32(E, 5) +
    ((A and B) or (C and (A or B)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[57] + C3 + TBits.RotateLeft32(D, 5) +
    ((E and A) or (B and (E or A)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[58] + C3 + TBits.RotateLeft32(C, 5) +
    ((D and E) or (A and (D or E)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[59] + C3 + TBits.RotateLeft32(B, 5) +
    ((C and D) or (E and (C or D)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[60] + C4 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[61] + C4 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[62] + C4 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[63] + C4 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[64] + C4 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[65] + C4 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[66] + C4 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[67] + C4 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[68] + C4 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[69] + C4 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[70] + C4 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[71] + C4 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[72] + C4 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[73] + C4 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[74] + C4 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (LData[75] + C4 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (LData[76] + C4 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (LData[77] + C4 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (LData[78] + C4 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (LData[79] + C4 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);

  FState[0] := FState[0] + A;
  FState[1] := FState[1] + B;
  FState[2] := FState[2] + C;
  FState[3] := FState[3] + D;
  FState[4] := FState[4] + E;

  System.FillChar(LData, System.SizeOf(LData), UInt32(0));
end;

end.
