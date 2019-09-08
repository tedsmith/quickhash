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
  HlpBitConverter,
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

    s_K: array [0 .. 63] of UInt32 = ($428A2F98, $71374491, $B5C0FBCF,
      $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5, $D807AA98,
      $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7,
      $C19BF174, $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F,
      $4A7484AA, $5CB0A9DC, $76F988DA, $983E5152, $A831C66D, $B00327C8,
      $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967, $27B70A85,
      $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E,
      $92722C85, $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819,
      $D6990624, $F40E3585, $106AA070, $19A4C116, $1E376C08, $2748774C,
      $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3, $748F82EE,
      $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7,
      $C67178F2);

{$ENDREGION}
{$ENDIF USE_UNROLLED_VARIANT}
  strict protected
    Fm_state: THashLibUInt32Array;

    constructor Create(a_hash_size: Int32);

    procedure Finish(); override;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  end;

implementation

{ TSHA2_256Base }

constructor TSHA2_256Base.Create(a_hash_size: Int32);
begin
  Inherited Create(a_hash_size, 64);
  System.SetLength(Fm_state, 8);
end;

procedure TSHA2_256Base.Finish;
var
  bits: UInt64;
  padindex: Int32;
  pad: THashLibByteArray;
begin
  bits := Fm_processed_bytes * 8;
  if (Fm_buffer.Pos < 56) then

    padindex := (56 - Fm_buffer.Pos)
  else
    padindex := (120 - Fm_buffer.Pos);
  System.SetLength(pad, padindex + 8);
  pad[0] := $80;

  bits := TConverters.be2me_64(bits);

  TConverters.ReadUInt64AsBytesLE(bits, pad, padindex);

  padindex := padindex + 8;

  TransformBytes(pad, 0, padindex);

end;

procedure TSHA2_256Base.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  A, B, C, D, E, F, G, H, T, T2: UInt32;
{$IFNDEF USE_UNROLLED_VARIANT}
  r: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
  data: array [0 .. 63] of UInt32;
begin

  TConverters.be32_copy(a_data, a_index, @(data[0]), 0, a_data_length);

  A := Fm_state[0];
  B := Fm_state[1];
  C := Fm_state[2];
  D := Fm_state[3];
  E := Fm_state[4];
  F := Fm_state[5];
  G := Fm_state[6];
  H := Fm_state[7];

  // Step 1

{$IFDEF USE_UNROLLED_VARIANT}
  T := data[14];
  T2 := data[1];
  data[16] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[9] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[0];

  T := data[15];
  T2 := data[2];
  data[17] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[10] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[1];

  T := data[16];
  T2 := data[3];
  data[18] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[11] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[2];

  T := data[17];
  T2 := data[4];
  data[19] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[12] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[3];

  T := data[18];
  T2 := data[5];
  data[20] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[13] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[4];

  T := data[19];
  T2 := data[6];
  data[21] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[14] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[5];

  T := data[20];
  T2 := data[7];
  data[22] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[15] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[6];

  T := data[21];
  T2 := data[8];
  data[23] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[16] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[7];

  T := data[22];
  T2 := data[9];
  data[24] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[17] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[8];

  T := data[23];
  T2 := data[10];
  data[25] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[18] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[9];

  T := data[24];
  T2 := data[11];
  data[26] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[19] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[10];

  T := data[25];
  T2 := data[12];
  data[27] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[20] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[11];

  T := data[26];
  T2 := data[13];
  data[28] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[21] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[12];

  T := data[27];
  T2 := data[14];
  data[29] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[22] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[13];

  T := data[28];
  T2 := data[15];
  data[30] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[23] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[14];

  T := data[29];
  T2 := data[16];
  data[31] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[24] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[15];

  T := data[30];
  T2 := data[17];
  data[32] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[25] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[16];

  T := data[31];
  T2 := data[18];
  data[33] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[26] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[17];

  T := data[32];
  T2 := data[19];
  data[34] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[27] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[18];

  T := data[33];
  T2 := data[20];
  data[35] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[28] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[19];

  T := data[34];
  T2 := data[21];
  data[36] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[29] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[20];

  T := data[35];
  T2 := data[22];
  data[37] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[30] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[21];

  T := data[36];
  T2 := data[23];
  data[38] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[31] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[22];

  T := data[37];
  T2 := data[24];
  data[39] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[32] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[23];

  T := data[38];
  T2 := data[25];
  data[40] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[33] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[24];

  T := data[39];
  T2 := data[26];
  data[41] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[34] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[25];

  T := data[40];
  T2 := data[27];
  data[42] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[35] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[26];

  T := data[41];
  T2 := data[28];
  data[43] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[36] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[27];

  T := data[42];
  T2 := data[29];
  data[44] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[37] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[28];

  T := data[43];
  T2 := data[30];
  data[45] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[38] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[29];

  T := data[44];
  T2 := data[31];
  data[46] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[39] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[30];

  T := data[45];
  T2 := data[32];
  data[47] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[40] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[31];

  T := data[46];
  T2 := data[33];
  data[48] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[41] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[32];

  T := data[47];
  T2 := data[34];
  data[49] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[42] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[33];

  T := data[48];
  T2 := data[35];
  data[50] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[43] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[34];

  T := data[49];
  T2 := data[36];
  data[51] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[44] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[35];

  T := data[50];
  T2 := data[37];
  data[52] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[45] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[36];

  T := data[51];
  T2 := data[38];
  data[53] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[46] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[37];

  T := data[52];
  T2 := data[39];
  data[54] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[47] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[38];

  T := data[53];
  T2 := data[40];
  data[55] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[48] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[39];

  T := data[54];
  T2 := data[41];
  data[56] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[49] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[40];

  T := data[55];
  T2 := data[42];
  data[57] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[50] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[41];

  T := data[56];
  T2 := data[43];
  data[58] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[51] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[42];

  T := data[57];
  T2 := data[44];
  data[59] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[52] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[43];

  T := data[58];
  T2 := data[45];
  data[60] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[53] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[44];

  T := data[59];
  T2 := data[46];
  data[61] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[54] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[45];

  T := data[60];
  T2 := data[47];
  data[62] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[55] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[46];

  T := data[61];
  T2 := data[48];
  data[63] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
    xor (T shr 10)) + data[56] +
    ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
    xor (T2 shr 3)) + data[47];

  // Step 2

  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $428A2F98 + data[0];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $71374491 + data[1];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $B5C0FBCF + data[2];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $E9B5DBA5 + data[3];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $3956C25B + data[4];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $59F111F1 + data[5];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $923F82A4 + data[6];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $AB1C5ED5 + data[7];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $D807AA98 + data[8];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $12835B01 + data[9];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $243185BE + data[10];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $550C7DC3 + data[11];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $72BE5D74 + data[12];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $80DEB1FE + data[13];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $9BDC06A7 + data[14];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $C19BF174 + data[15];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $E49B69C1 + data[16];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $EFBE4786 + data[17];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $0FC19DC6 + data[18];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $240CA1CC + data[19];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $2DE92C6F + data[20];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $4A7484AA + data[21];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $5CB0A9DC + data[22];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $76F988DA + data[23];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $983E5152 + data[24];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $A831C66D + data[25];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $B00327C8 + data[26];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $BF597FC7 + data[27];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $C6E00BF3 + data[28];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $D5A79147 + data[29];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $06CA6351 + data[30];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $14292967 + data[31];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $27B70A85 + data[32];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $2E1B2138 + data[33];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $4D2C6DFC + data[34];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $53380D13 + data[35];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $650A7354 + data[36];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $766A0ABB + data[37];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $81C2C92E + data[38];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $92722C85 + data[39];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $A2BFE8A1 + data[40];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $A81A664B + data[41];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $C24B8B70 + data[42];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $C76C51A3 + data[43];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $D192E819 + data[44];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $D6990624 + data[45];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $F40E3585 + data[46];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $106AA070 + data[47];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $19A4C116 + data[48];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $1E376C08 + data[49];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $2748774C + data[50];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $34B0BCB5 + data[51];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $391C0CB3 + data[52];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $4ED8AA4A + data[53];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $5B9CCA4F + data[54];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $682E6FF3 + data[55];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;
  T := H + ((TBits.RotateRight32(E, 6)) xor (TBits.RotateRight32(E, 11))
    xor (TBits.RotateRight32(E, 25))) + ((E and F) xor (not E and G)) +
    $748F82EE + data[56];
  T2 := ((TBits.RotateRight32(A, 2)) xor (TBits.RotateRight32(A, 13))
    xor ((A shr 22) xor (A shl 10))) + ((A and B) xor (A and C) xor (B and C));
  H := T + T2;
  D := D + T;
  T := G + ((TBits.RotateRight32(D, 6)) xor (TBits.RotateRight32(D, 11))
    xor (TBits.RotateRight32(D, 25))) + ((D and E) xor (not D and F)) +
    $78A5636F + data[57];
  T2 := ((TBits.RotateRight32(H, 2)) xor (TBits.RotateRight32(H, 13))
    xor ((H shr 22) xor (H shl 10))) + ((H and A) xor (H and B) xor (A and B));
  G := T + T2;
  C := C + T;
  T := F + ((TBits.RotateRight32(C, 6)) xor (TBits.RotateRight32(C, 11))
    xor (TBits.RotateRight32(C, 25))) + ((C and D) xor (not C and E)) +
    $84C87814 + data[58];
  T2 := ((TBits.RotateRight32(G, 2)) xor (TBits.RotateRight32(G, 13))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and A) xor (H and A));
  F := T + T2;
  B := B + T;
  T := E + ((TBits.RotateRight32(B, 6)) xor (TBits.RotateRight32(B, 11))
    xor (TBits.RotateRight32(B, 25))) + ((B and C) xor (not B and D)) +
    $8CC70208 + data[59];
  T2 := ((TBits.RotateRight32(F, 2)) xor (TBits.RotateRight32(F, 13))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := T + T2;
  A := A + T;
  T := D + ((TBits.RotateRight32(A, 6)) xor (TBits.RotateRight32(A, 11))
    xor (TBits.RotateRight32(A, 25))) + ((A and B) xor (not A and C)) +
    $90BEFFFA + data[60];
  T2 := ((TBits.RotateRight32(E, 2)) xor (TBits.RotateRight32(E, 13))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  D := T + T2;
  H := H + T;
  T := C + ((TBits.RotateRight32(H, 6)) xor (TBits.RotateRight32(H, 11))
    xor (TBits.RotateRight32(H, 25))) + ((H and A) xor (not H and B)) +
    $A4506CEB + data[61];
  T2 := ((TBits.RotateRight32(D, 2)) xor (TBits.RotateRight32(D, 13))
    xor ((D shr 22) xor (D shl 10))) + ((D and E) xor (D and F) xor (E and F));
  C := T + T2;
  G := G + T;
  T := B + ((TBits.RotateRight32(G, 6)) xor (TBits.RotateRight32(G, 11))
    xor (TBits.RotateRight32(G, 25))) + ((G and H) xor (not G and A)) +
    $BEF9A3F7 + data[62];
  T2 := ((TBits.RotateRight32(C, 2)) xor (TBits.RotateRight32(C, 13))
    xor ((C shr 22) xor (C shl 10))) + ((C and D) xor (C and E) xor (D and E));
  B := T + T2;
  F := F + T;
  T := A + ((TBits.RotateRight32(F, 6)) xor (TBits.RotateRight32(F, 11))
    xor (TBits.RotateRight32(F, 25))) + ((F and G) xor (not F and H)) +
    $C67178F2 + data[63];
  T2 := ((TBits.RotateRight32(B, 2)) xor (TBits.RotateRight32(B, 13))
    xor ((B shr 22) xor (B shl 10))) + ((B and C) xor (B and D) xor (C and D));
  A := T + T2;
  E := E + T;

{$ELSE}
  // Step 1
  for r := 16 to 63 do
  begin
    T := data[r - 2];
    T2 := data[r - 15];
    data[r] := ((TBits.RotateRight32(T, 17)) xor (TBits.RotateRight32(T, 19))
      xor (T shr 10)) + data[r - 7] +
      ((TBits.RotateRight32(T2, 7)) xor (TBits.RotateRight32(T2, 18))
      xor (T2 shr 3)) + data[r - 16];
  end;

  // Step 2

  for r := 0 to 63 do
  begin

    T := s_K[r] + data[r] + H +
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
  Fm_state[0] := Fm_state[0] + A;
  Fm_state[1] := Fm_state[1] + B;
  Fm_state[2] := Fm_state[2] + C;
  Fm_state[3] := Fm_state[3] + D;
  Fm_state[4] := Fm_state[4] + E;
  Fm_state[5] := Fm_state[5] + F;
  Fm_state[6] := Fm_state[6] + G;
  Fm_state[7] := Fm_state[7] + H;

  System.FillChar(data, System.SizeOf(data), UInt32(0));

end;

end.
