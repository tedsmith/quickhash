unit HlpSHA2_256Base;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHashBuffer,
{$ENDIF DELPHI}
  HlpBits,
  HlpConverters,
  HlpIHashInfo,
  HlpHashCryptoNotBuildIn;

type
  TSHA2_256Base = class abstract(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

{$IFNDEF USE_UNROLLED_VARIANT}
{$REGION 'Consts'}
  const
    SK: array [0 .. 63] of UInt32 = ($428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5,
      $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5, $D807AA98, $12835B01,
      $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
      $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA,
      $5CB0A9DC, $76F988DA, $983E5152, $A831C66D, $B00327C8, $BF597FC7,
      $C6E00BF3, $D5A79147, $06CA6351, $14292967, $27B70A85, $2E1B2138,
      $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
      $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624,
      $F40E3585, $106AA070, $19A4C116, $1E376C08, $2748774C, $34B0BCB5,
      $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3, $748F82EE, $78A5636F,
      $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2);

{$ENDREGION}
{$ENDIF USE_UNROLLED_VARIANT}
  strict protected
  var
    FState: THashLibUInt32Array;

    constructor Create(AHashSize: Int32);

    procedure Finish(); override;
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  end;

implementation

{ TSHA2_256Base }

constructor TSHA2_256Base.Create(AHashSize: Int32);
begin
  Inherited Create(AHashSize, 64);
  System.SetLength(FState, 8);
end;

procedure TSHA2_256Base.Finish;
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

procedure TSHA2_256Base.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  A, B, C, D, E, F, G, H, T, T2: UInt32;
{$IFNDEF USE_UNROLLED_VARIANT}
  LR: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
  LData: array [0 .. 63] of UInt32;
begin
  TConverters.be32_copy(AData, AIndex, @(LData[0]), 0, ADataLength);

  A := FState[0];
  B := FState[1];
  C := FState[2];
  D := FState[3];
  E := FState[4];
  F := FState[5];
  G := FState[6];
  H := FState[7];

  // Step 1

{$IFDEF USE_UNROLLED_VARIANT}
  T := LData[14];
  T2 := LData[1];
  LData[16] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[9] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[0];

  T := LData[15];
  T2 := LData[2];
  LData[17] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[10] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[1];

  T := LData[16];
  T2 := LData[3];
  LData[18] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[11] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[2];

  T := LData[17];
  T2 := LData[4];
  LData[19] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[12] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[3];

  T := LData[18];
  T2 := LData[5];
  LData[20] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[13] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[4];

  T := LData[19];
  T2 := LData[6];
  LData[21] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[14] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[5];

  T := LData[20];
  T2 := LData[7];
  LData[22] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[15] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[6];

  T := LData[21];
  T2 := LData[8];
  LData[23] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[16] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[7];

  T := LData[22];
  T2 := LData[9];
  LData[24] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[17] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[8];

  T := LData[23];
  T2 := LData[10];
  LData[25] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[18] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[9];

  T := LData[24];
  T2 := LData[11];
  LData[26] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[19] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[10];

  T := LData[25];
  T2 := LData[12];
  LData[27] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[20] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[11];

  T := LData[26];
  T2 := LData[13];
  LData[28] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[21] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[12];

  T := LData[27];
  T2 := LData[14];
  LData[29] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[22] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[13];

  T := LData[28];
  T2 := LData[15];
  LData[30] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[23] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[14];

  T := LData[29];
  T2 := LData[16];
  LData[31] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[24] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[15];

  T := LData[30];
  T2 := LData[17];
  LData[32] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[25] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[16];

  T := LData[31];
  T2 := LData[18];
  LData[33] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[26] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[17];

  T := LData[32];
  T2 := LData[19];
  LData[34] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[27] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[18];

  T := LData[33];
  T2 := LData[20];
  LData[35] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[28] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[19];

  T := LData[34];
  T2 := LData[21];
  LData[36] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[29] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[20];

  T := LData[35];
  T2 := LData[22];
  LData[37] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[30] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[21];

  T := LData[36];
  T2 := LData[23];
  LData[38] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[31] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[22];

  T := LData[37];
  T2 := LData[24];
  LData[39] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[32] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[23];

  T := LData[38];
  T2 := LData[25];
  LData[40] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[33] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[24];

  T := LData[39];
  T2 := LData[26];
  LData[41] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[34] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[25];

  T := LData[40];
  T2 := LData[27];
  LData[42] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[35] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[26];

  T := LData[41];
  T2 := LData[28];
  LData[43] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[36] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[27];

  T := LData[42];
  T2 := LData[29];
  LData[44] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[37] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[28];

  T := LData[43];
  T2 := LData[30];
  LData[45] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[38] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[29];

  T := LData[44];
  T2 := LData[31];
  LData[46] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[39] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[30];

  T := LData[45];
  T2 := LData[32];
  LData[47] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[40] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[31];

  T := LData[46];
  T2 := LData[33];
  LData[48] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[41] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[32];

  T := LData[47];
  T2 := LData[34];
  LData[49] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[42] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[33];

  T := LData[48];
  T2 := LData[35];
  LData[50] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[43] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[34];

  T := LData[49];
  T2 := LData[36];
  LData[51] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[44] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[35];

  T := LData[50];
  T2 := LData[37];
  LData[52] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[45] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[36];

  T := LData[51];
  T2 := LData[38];
  LData[53] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[46] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[37];

  T := LData[52];
  T2 := LData[39];
  LData[54] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[47] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[38];

  T := LData[53];
  T2 := LData[40];
  LData[55] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[48] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[39];

  T := LData[54];
  T2 := LData[41];
  LData[56] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[49] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[40];

  T := LData[55];
  T2 := LData[42];
  LData[57] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[50] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[41];

  T := LData[56];
  T2 := LData[43];
  LData[58] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[51] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[42];

  T := LData[57];
  T2 := LData[44];
  LData[59] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[52] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[43];

  T := LData[58];
  T2 := LData[45];
  LData[60] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[53] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[44];

  T := LData[59];
  T2 := LData[46];
  LData[61] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[54] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[45];

  T := LData[60];
  T2 := LData[47];
  LData[62] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[55] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[46];

  T := LData[61];
  T2 := LData[48];
  LData[63] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + LData[56] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + LData[47];

  // Step 2

  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $428A2F98 + LData[0];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $71374491 + LData[1];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $B5C0FBCF + LData[2];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $E9B5DBA5 + LData[3];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $3956C25B + LData[4];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $59F111F1 + LData[5];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $923F82A4 + LData[6];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $AB1C5ED5 + LData[7];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $D807AA98 + LData[8];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $12835B01 + LData[9];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $243185BE + LData[10];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $550C7DC3 + LData[11];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $72BE5D74 + LData[12];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $80DEB1FE + LData[13];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $9BDC06A7 + LData[14];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $C19BF174 + LData[15];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $E49B69C1 + LData[16];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $EFBE4786 + LData[17];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $0FC19DC6 + LData[18];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $240CA1CC + LData[19];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $2DE92C6F + LData[20];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $4A7484AA + LData[21];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $5CB0A9DC + LData[22];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $76F988DA + LData[23];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $983E5152 + LData[24];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $A831C66D + LData[25];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $B00327C8 + LData[26];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $BF597FC7 + LData[27];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $C6E00BF3 + LData[28];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $D5A79147 + LData[29];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $06CA6351 + LData[30];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $14292967 + LData[31];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $27B70A85 + LData[32];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $2E1B2138 + LData[33];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $4D2C6DFC + LData[34];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $53380D13 + LData[35];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $650A7354 + LData[36];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $766A0ABB + LData[37];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $81C2C92E + LData[38];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $92722C85 + LData[39];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $A2BFE8A1 + LData[40];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $A81A664B + LData[41];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $C24B8B70 + LData[42];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $C76C51A3 + LData[43];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $D192E819 + LData[44];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $D6990624 + LData[45];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $F40E3585 + LData[46];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $106AA070 + LData[47];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $19A4C116 + LData[48];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $1E376C08 + LData[49];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $2748774C + LData[50];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $34B0BCB5 + LData[51];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $391C0CB3 + LData[52];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $4ED8AA4A + LData[53];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $5B9CCA4F + LData[54];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $682E6FF3 + LData[55];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $748F82EE + LData[56];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $78A5636F + LData[57];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $84C87814 + LData[58];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $8CC70208 + LData[59];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $90BEFFFA + LData[60];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $A4506CEB + LData[61];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $BEF9A3F7 + LData[62];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $C67178F2 + LData[63];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;

{$ELSE}
  // Step 1
  for LR := 16 to 63 do
  begin
    T := LData[LR - 2];
    T2 := LData[LR - 15];
    LData[LR] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
      xor (T shr 10)) + LData[LR - 7] +
      ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
      xor (T2 shr 3)) + LData[LR - 16];
  end;

  // Step 2

  for LR := 0 to 63 do
  begin

    T := SK[LR] + LData[LR] + H +
      ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
      xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G));
    T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
      xor (TBits.RotateRight32(A, 22))) +
      ((A and B) xor (A and C) xor (B and C));

    H := G;
    G := F;
    F := E;
    E := D + T;
    D := C;
    C := B;
    B := A;
    A := T + T2;
  end;

{$ENDIF USE_UNROLLED_VARIANT}
  FState[0] := FState[0] + A;
  FState[1] := FState[1] + B;
  FState[2] := FState[2] + C;
  FState[3] := FState[3] + D;
  FState[4] := FState[4] + E;
  FState[5] := FState[5] + F;
  FState[6] := FState[6] + G;
  FState[7] := FState[7] + H;

  System.FillChar(LData, System.SizeOf(LData), UInt32(0));
end;

end.
