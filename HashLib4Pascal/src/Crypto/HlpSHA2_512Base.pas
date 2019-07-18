unit HlpSHA2_512Base;

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
  TSHA2_512Base = class abstract(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

{$IFNDEF USE_UNROLLED_VARIANT}
{$REGION 'Consts'}
  const

    s_K: array [0 .. 79] of UInt64 = (UInt64($428A2F98D728AE22),
      UInt64($7137449123EF65CD), UInt64($B5C0FBCFEC4D3B2F),
      UInt64($E9B5DBA58189DBBC), UInt64($3956C25BF348B538),
      UInt64($59F111F1B605D019), UInt64($923F82A4AF194F9B),
      UInt64($AB1C5ED5DA6D8118), UInt64($D807AA98A3030242),
      UInt64($12835B0145706FBE), UInt64($243185BE4EE4B28C),
      UInt64($550C7DC3D5FFB4E2), UInt64($72BE5D74F27B896F),
      UInt64($80DEB1FE3B1696B1), UInt64($9BDC06A725C71235),
      UInt64($C19BF174CF692694), UInt64($E49B69C19EF14AD2),
      UInt64($EFBE4786384F25E3), UInt64($0FC19DC68B8CD5B5),
      UInt64($240CA1CC77AC9C65), UInt64($2DE92C6F592B0275),
      UInt64($4A7484AA6EA6E483), UInt64($5CB0A9DCBD41FBD4),
      UInt64($76F988DA831153B5), UInt64($983E5152EE66DFAB),
      UInt64($A831C66D2DB43210), UInt64($B00327C898FB213F),
      UInt64($BF597FC7BEEF0EE4), UInt64($C6E00BF33DA88FC2),
      UInt64($D5A79147930AA725), UInt64($06CA6351E003826F),
      UInt64($142929670A0E6E70), UInt64($27B70A8546D22FFC),
      UInt64($2E1B21385C26C926), UInt64($4D2C6DFC5AC42AED),
      UInt64($53380D139D95B3DF), UInt64($650A73548BAF63DE),
      UInt64($766A0ABB3C77B2A8), UInt64($81C2C92E47EDAEE6),
      UInt64($92722C851482353B), UInt64($A2BFE8A14CF10364),
      UInt64($A81A664BBC423001), UInt64($C24B8B70D0F89791),
      UInt64($C76C51A30654BE30), UInt64($D192E819D6EF5218),
      UInt64($D69906245565A910), UInt64($F40E35855771202A),
      UInt64($106AA07032BBD1B8), UInt64($19A4C116B8D2D0C8),
      UInt64($1E376C085141AB53), UInt64($2748774CDF8EEB99),
      UInt64($34B0BCB5E19B48A8), UInt64($391C0CB3C5C95A63),
      UInt64($4ED8AA4AE3418ACB), UInt64($5B9CCA4F7763E373),
      UInt64($682E6FF3D6B2B8A3), UInt64($748F82EE5DEFB2FC),
      UInt64($78A5636F43172F60), UInt64($84C87814A1F0AB72),
      UInt64($8CC702081A6439EC), UInt64($90BEFFFA23631E28),
      UInt64($A4506CEBDE82BDE9), UInt64($BEF9A3F7B2C67915),
      UInt64($C67178F2E372532B), UInt64($CA273ECEEA26619C),
      UInt64($D186B8C721C0C207), UInt64($EADA7DD6CDE0EB1E),
      UInt64($F57D4F7FEE6ED178), UInt64($06F067AA72176FBA),
      UInt64($0A637DC5A2C898A6), UInt64($113F9804BEF90DAE),
      UInt64($1B710B35131C471B), UInt64($28DB77F523047D84),
      UInt64($32CAAB7B40C72493), UInt64($3C9EBE0A15C9BEBC),
      UInt64($431D67C49C100D4C), UInt64($4CC5D4BECB3E42B6),
      UInt64($597F299CFC657E2A), UInt64($5FCB6FAB3AD6FAEC),
      UInt64($6C44198C4A475817));

{$ENDREGION}
{$ENDIF USE_UNROLLED_VARIANT}
  strict protected
    Fm_state: THashLibUInt64Array;

    constructor Create(a_hash_size: Int32);

    procedure Finish(); override;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;
  end;

implementation

{ TSHA2_512Base }

constructor TSHA2_512Base.Create(a_hash_size: Int32);
begin
  Inherited Create(a_hash_size, 128);
  System.SetLength(Fm_state, 8);
end;

procedure TSHA2_512Base.Finish;
var
  lowBits, hiBits: UInt64;
  padindex: Int32;
  pad: THashLibByteArray;
begin
  lowBits := Fm_processed_bytes shl 3;
  hiBits := Fm_processed_bytes shr 61;

  if (Fm_buffer.Pos < 112) then

    padindex := (111 - Fm_buffer.Pos)
  else
    padindex := (239 - Fm_buffer.Pos);

  System.Inc(padindex);
  System.SetLength(pad, padindex + 16);
  pad[0] := $80;

  hiBits := TConverters.be2me_64(hiBits);

  TConverters.ReadUInt64AsBytesLE(hiBits, pad, padindex);

  padindex := padindex + 8;

  lowBits := TConverters.be2me_64(lowBits);

  TConverters.ReadUInt64AsBytesLE(lowBits, pad, padindex);

  padindex := padindex + 8;

  TransformBytes(pad, 0, padindex);

end;

procedure TSHA2_512Base.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
{$IFNDEF USE_UNROLLED_VARIANT}
  i, t: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
  T0, T1, a, b, c, d, e, f, g, h: UInt64;
  data: array [0 .. 79] of UInt64;
begin

  TConverters.be64_copy(a_data, a_index, @(data[0]), 0, a_data_length);

  // Step 1

{$IFDEF USE_UNROLLED_VARIANT}
  T0 := data[16 - 15];
  T1 := data[16 - 2];
  data[16] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[16 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[0];
  T0 := data[17 - 15];
  T1 := data[17 - 2];
  data[17] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[17 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[17 - 16];
  T0 := data[18 - 15];
  T1 := data[18 - 2];
  data[18] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[18 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[18 - 16];
  T0 := data[19 - 15];
  T1 := data[19 - 2];
  data[19] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[19 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[19 - 16];
  T0 := data[20 - 15];
  T1 := data[20 - 2];
  data[20] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[20 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[20 - 16];
  T0 := data[21 - 15];
  T1 := data[21 - 2];
  data[21] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[21 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[21 - 16];
  T0 := data[22 - 15];
  T1 := data[22 - 2];
  data[22] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[22 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[22 - 16];
  T0 := data[23 - 15];
  T1 := data[23 - 2];
  data[23] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[23 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[23 - 16];
  T0 := data[24 - 15];
  T1 := data[24 - 2];
  data[24] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[24 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[24 - 16];
  T0 := data[25 - 15];
  T1 := data[25 - 2];
  data[25] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[25 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[25 - 16];
  T0 := data[26 - 15];
  T1 := data[26 - 2];
  data[26] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[26 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[26 - 16];
  T0 := data[27 - 15];
  T1 := data[27 - 2];
  data[27] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[27 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[27 - 16];
  T0 := data[28 - 15];
  T1 := data[28 - 2];
  data[28] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[28 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[28 - 16];
  T0 := data[29 - 15];
  T1 := data[29 - 2];
  data[29] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[29 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[29 - 16];
  T0 := data[30 - 15];
  T1 := data[30 - 2];
  data[30] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[30 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[30 - 16];
  T0 := data[31 - 15];
  T1 := data[31 - 2];
  data[31] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[31 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[31 - 16];
  T0 := data[32 - 15];
  T1 := data[32 - 2];
  data[32] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[32 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[32 - 16];
  T0 := data[33 - 15];
  T1 := data[33 - 2];
  data[33] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[33 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[33 - 16];
  T0 := data[34 - 15];
  T1 := data[34 - 2];
  data[34] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[34 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[34 - 16];
  T0 := data[35 - 15];
  T1 := data[35 - 2];
  data[35] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[35 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[35 - 16];
  T0 := data[36 - 15];
  T1 := data[36 - 2];
  data[36] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[36 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[36 - 16];
  T0 := data[37 - 15];
  T1 := data[37 - 2];
  data[37] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[37 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[37 - 16];
  T0 := data[38 - 15];
  T1 := data[38 - 2];
  data[38] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[38 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[38 - 16];
  T0 := data[39 - 15];
  T1 := data[39 - 2];
  data[39] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[39 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[39 - 16];
  T0 := data[40 - 15];
  T1 := data[40 - 2];
  data[40] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[40 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[40 - 16];
  T0 := data[41 - 15];
  T1 := data[41 - 2];
  data[41] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[41 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[41 - 16];
  T0 := data[42 - 15];
  T1 := data[42 - 2];
  data[42] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[42 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[42 - 16];
  T0 := data[43 - 15];
  T1 := data[43 - 2];
  data[43] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[43 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[43 - 16];
  T0 := data[44 - 15];
  T1 := data[44 - 2];
  data[44] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[44 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[44 - 16];
  T0 := data[45 - 15];
  T1 := data[45 - 2];
  data[45] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[45 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[45 - 16];
  T0 := data[46 - 15];
  T1 := data[46 - 2];
  data[46] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[46 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[46 - 16];
  T0 := data[47 - 15];
  T1 := data[47 - 2];
  data[47] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[47 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[47 - 16];
  T0 := data[48 - 15];
  T1 := data[48 - 2];
  data[48] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[48 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[48 - 16];
  T0 := data[49 - 15];
  T1 := data[49 - 2];
  data[49] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[49 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[49 - 16];
  T0 := data[50 - 15];
  T1 := data[50 - 2];
  data[50] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[50 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[50 - 16];
  T0 := data[51 - 15];
  T1 := data[51 - 2];
  data[51] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[51 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[51 - 16];
  T0 := data[52 - 15];
  T1 := data[52 - 2];
  data[52] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[52 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[52 - 16];
  T0 := data[53 - 15];
  T1 := data[53 - 2];
  data[53] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[53 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[53 - 16];
  T0 := data[54 - 15];
  T1 := data[54 - 2];
  data[54] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[54 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[54 - 16];
  T0 := data[55 - 15];
  T1 := data[55 - 2];
  data[55] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[55 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[55 - 16];
  T0 := data[56 - 15];
  T1 := data[56 - 2];
  data[56] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[56 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[56 - 16];
  T0 := data[57 - 15];
  T1 := data[57 - 2];
  data[57] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[57 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[57 - 16];
  T0 := data[58 - 15];
  T1 := data[58 - 2];
  data[58] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[58 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[58 - 16];
  T0 := data[59 - 15];
  T1 := data[59 - 2];
  data[59] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[59 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[59 - 16];
  T0 := data[60 - 15];
  T1 := data[60 - 2];
  data[60] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[60 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[60 - 16];
  T0 := data[61 - 15];
  T1 := data[61 - 2];
  data[61] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[61 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[61 - 16];
  T0 := data[62 - 15];
  T1 := data[62 - 2];
  data[62] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[62 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[62 - 16];
  T0 := data[63 - 15];
  T1 := data[63 - 2];
  data[63] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[63 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[63 - 16];
  T0 := data[64 - 15];
  T1 := data[64 - 2];
  data[64] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[64 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[64 - 16];
  T0 := data[65 - 15];
  T1 := data[65 - 2];
  data[65] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[65 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[65 - 16];
  T0 := data[66 - 15];
  T1 := data[66 - 2];
  data[66] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[66 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[66 - 16];
  T0 := data[67 - 15];
  T1 := data[67 - 2];
  data[67] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[67 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[67 - 16];
  T0 := data[68 - 15];
  T1 := data[68 - 2];
  data[68] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[68 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[68 - 16];
  T0 := data[69 - 15];
  T1 := data[69 - 2];
  data[69] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[69 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[69 - 16];
  T0 := data[70 - 15];
  T1 := data[70 - 2];
  data[70] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[70 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[70 - 16];
  T0 := data[71 - 15];
  T1 := data[71 - 2];
  data[71] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[71 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[71 - 16];
  T0 := data[72 - 15];
  T1 := data[72 - 2];
  data[72] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[72 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[72 - 16];
  T0 := data[73 - 15];
  T1 := data[73 - 2];
  data[73] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[73 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[73 - 16];
  T0 := data[74 - 15];
  T1 := data[74 - 2];
  data[74] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[74 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[74 - 16];
  T0 := data[75 - 15];
  T1 := data[75 - 2];
  data[75] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[75 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[75 - 16];
  T0 := data[76 - 15];
  T1 := data[76 - 2];
  data[76] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[76 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[76 - 16];
  T0 := data[77 - 15];
  T1 := data[77 - 2];
  data[77] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[77 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[77 - 16];
  T0 := data[78 - 15];
  T1 := data[78 - 2];
  data[78] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[78 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[78 - 16];
  T0 := data[79 - 15];
  T1 := data[79 - 2];
  data[79] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
    xor (T1 shr 6)) + data[79 - 7] +
    ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
    xor (T0 shr 7)) + data[79 - 16];

  a := Fm_state[0];
  b := Fm_state[1];
  c := Fm_state[2];
  d := Fm_state[3];
  e := Fm_state[4];
  f := Fm_state[5];
  g := Fm_state[6];
  h := Fm_state[7];

  // Step 2

  // R0
  h := h + (UInt64($428A2F98D728AE22) + data[0] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($7137449123EF65CD) + data[1] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($B5C0FBCFEC4D3B2F) + data[2] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($E9B5DBA58189DBBC) + data[3] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($3956C25BF348B538) + data[4] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($59F111F1B605D019) + data[5] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($923F82A4AF194F9B) + data[6] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($AB1C5ED5DA6D8118) + data[7] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R1
  h := h + (UInt64($D807AA98A3030242) + data[8] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($12835B0145706FBE) + data[9] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($243185BE4EE4B28C) + data[10] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($550C7DC3D5FFB4E2) + data[11] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($72BE5D74F27B896F) + data[12] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($80DEB1FE3B1696B1) + data[13] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($9BDC06A725C71235) + data[14] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($C19BF174CF692694) + data[15] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R2

  h := h + (UInt64($E49B69C19EF14AD2) + data[16] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($EFBE4786384F25E3) + data[17] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($0FC19DC68B8CD5B5) + data[18] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($240CA1CC77AC9C65) + data[19] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($2DE92C6F592B0275) + data[20] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($4A7484AA6EA6E483) + data[21] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($5CB0A9DCBD41FBD4) + data[22] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($76F988DA831153B5) + data[23] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R3

  h := h + (UInt64($983E5152EE66DFAB) + data[24] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($A831C66D2DB43210) + data[25] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($B00327C898FB213F) + data[26] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($BF597FC7BEEF0EE4) + data[27] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($C6E00BF33DA88FC2) + data[28] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($D5A79147930AA725) + data[29] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($06CA6351E003826F) + data[30] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($142929670A0E6E70) + data[31] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R4

  h := h + (UInt64($27B70A8546D22FFC) + data[32] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($2E1B21385C26C926) + data[33] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($4D2C6DFC5AC42AED) + data[34] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($53380D139D95B3DF) + data[35] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($650A73548BAF63DE) + data[36] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($766A0ABB3C77B2A8) + data[37] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($81C2C92E47EDAEE6) + data[38] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($92722C851482353B) + data[39] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R5

  h := h + (UInt64($A2BFE8A14CF10364) + data[40] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($A81A664BBC423001) + data[41] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($C24B8B70D0F89791) + data[42] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($C76C51A30654BE30) + data[43] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($D192E819D6EF5218) + data[44] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($D69906245565A910) + data[45] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($F40E35855771202A) + data[46] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($106AA07032BBD1B8) + data[47] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R6

  h := h + (UInt64($19A4C116B8D2D0C8) + data[48] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($1E376C085141AB53) + data[49] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($2748774CDF8EEB99) + data[50] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($34B0BCB5E19B48A8) + data[51] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($391C0CB3C5C95A63) + data[52] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($4ED8AA4AE3418ACB) + data[53] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($5B9CCA4F7763E373) + data[54] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($682E6FF3D6B2B8A3) + data[55] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R7

  h := h + (UInt64($748F82EE5DEFB2FC) + data[56] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($78A5636F43172F60) + data[57] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($84C87814A1F0AB72) + data[58] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($8CC702081A6439EC) + data[59] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($90BEFFFA23631E28) + data[60] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($A4506CEBDE82BDE9) + data[61] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($BEF9A3F7B2C67915) + data[62] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($C67178F2E372532B) + data[63] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R8

  h := h + (UInt64($CA273ECEEA26619C) + data[64] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($D186B8C721C0C207) + data[65] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($EADA7DD6CDE0EB1E) + data[66] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($F57D4F7FEE6ED178) + data[67] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($06F067AA72176FBA) + data[68] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($0A637DC5A2C898A6) + data[69] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($113F9804BEF90DAE) + data[70] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($1B710B35131C471B) + data[71] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

  // R9

  h := h + (UInt64($28DB77F523047D84) + data[72] + ((TBits.RotateLeft64(e, 50))
    xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
    ((e and f) xor (not e and g)));

  d := d + h;
  h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
    xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c) xor (b and c)));

  g := g + (UInt64($32CAAB7B40C72493) + data[73] + ((TBits.RotateLeft64(d, 50))
    xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
    ((d and e) xor (not d and f)));

  c := c + g;
  g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
    xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b) xor (a and b)));

  f := f + (UInt64($3C9EBE0A15C9BEBC) + data[74] + ((TBits.RotateLeft64(c, 50))
    xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
    ((c and d) xor (not c and e)));

  b := b + f;
  f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
    xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a) xor (h and a)));

  e := e + (UInt64($431D67C49C100D4C) + data[75] + ((TBits.RotateLeft64(b, 50))
    xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
    ((b and c) xor (not b and d)));

  a := a + e;
  e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
    xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h) xor (g and h)));

  d := d + (UInt64($4CC5D4BECB3E42B6) + data[76] + ((TBits.RotateLeft64(a, 50))
    xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
    ((a and b) xor (not a and c)));

  h := h + d;
  d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
    xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g) xor (f and g)));

  c := c + (UInt64($597F299CFC657E2A) + data[77] + ((TBits.RotateLeft64(h, 50))
    xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
    ((h and a) xor (not h and b)));

  g := g + c;
  c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
    xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f) xor (e and f)));

  b := b + (UInt64($5FCB6FAB3AD6FAEC) + data[78] + ((TBits.RotateLeft64(g, 50))
    xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
    ((g and h) xor (not g and a)));

  f := f + b;
  b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
    xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e) xor (d and e)));

  a := a + (UInt64($6C44198C4A475817) + data[79] + ((TBits.RotateLeft64(f, 50))
    xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
    ((f and g) xor (not f and h)));

  e := e + a;
  a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
    xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d) xor (c and d)));

{$ELSE}
  a := Fm_state[0];
  b := Fm_state[1];
  c := Fm_state[2];
  d := Fm_state[3];
  e := Fm_state[4];
  f := Fm_state[5];
  g := Fm_state[6];
  h := Fm_state[7];

  // Step 1

  for i := 16 to 79 do
  begin
    T0 := data[i - 15];
    T1 := data[i - 2];
    data[i] := ((TBits.RotateLeft64(T1, 45)) xor (TBits.RotateLeft64(T1, 3))
      xor (T1 shr 6)) + data[i - 7] +
      ((TBits.RotateLeft64(T0, 63)) xor (TBits.RotateLeft64(T0, 56))
      xor (T0 shr 7)) + data[i - 16];
  end;


  // Step 2

  t := 0;
  i := 0;

  while i <= 9 do

  begin

    h := h + (s_K[t] + data[t] + ((TBits.RotateLeft64(e, 50))
      xor (TBits.RotateLeft64(e, 46)) xor (TBits.RotateLeft64(e, 23))) +
      ((e and f) xor (not e and g)));
    System.Inc(t);
    d := d + h;
    h := h + (((TBits.RotateLeft64(a, 36)) xor (TBits.RotateLeft64(a, 30))
      xor (TBits.RotateLeft64(a, 25))) + ((a and b) xor (a and c)
      xor (b and c)));

    g := g + (s_K[t] + data[t] + ((TBits.RotateLeft64(d, 50))
      xor (TBits.RotateLeft64(d, 46)) xor (TBits.RotateLeft64(d, 23))) +
      ((d and e) xor (not d and f)));
    System.Inc(t);
    c := c + g;
    g := g + (((TBits.RotateLeft64(h, 36)) xor (TBits.RotateLeft64(h, 30))
      xor (TBits.RotateLeft64(h, 25))) + ((h and a) xor (h and b)
      xor (a and b)));

    f := f + (s_K[t] + data[t] + ((TBits.RotateLeft64(c, 50))
      xor (TBits.RotateLeft64(c, 46)) xor (TBits.RotateLeft64(c, 23))) +
      ((c and d) xor (not c and e)));
    System.Inc(t);
    b := b + f;
    f := f + (((TBits.RotateLeft64(g, 36)) xor (TBits.RotateLeft64(g, 30))
      xor (TBits.RotateLeft64(g, 25))) + ((g and h) xor (g and a)
      xor (h and a)));

    e := e + (s_K[t] + data[t] + ((TBits.RotateLeft64(b, 50))
      xor (TBits.RotateLeft64(b, 46)) xor (TBits.RotateLeft64(b, 23))) +
      ((b and c) xor (not b and d)));
    System.Inc(t);
    a := a + e;
    e := e + (((TBits.RotateLeft64(f, 36)) xor (TBits.RotateLeft64(f, 30))
      xor (TBits.RotateLeft64(f, 25))) + ((f and g) xor (f and h)
      xor (g and h)));

    d := d + (s_K[t] + data[t] + ((TBits.RotateLeft64(a, 50))
      xor (TBits.RotateLeft64(a, 46)) xor (TBits.RotateLeft64(a, 23))) +
      ((a and b) xor (not a and c)));
    System.Inc(t);
    h := h + d;
    d := d + (((TBits.RotateLeft64(e, 36)) xor (TBits.RotateLeft64(e, 30))
      xor (TBits.RotateLeft64(e, 25))) + ((e and f) xor (e and g)
      xor (f and g)));

    c := c + (s_K[t] + data[t] + ((TBits.RotateLeft64(h, 50))
      xor (TBits.RotateLeft64(h, 46)) xor (TBits.RotateLeft64(h, 23))) +
      ((h and a) xor (not h and b)));
    System.Inc(t);
    g := g + c;
    c := c + (((TBits.RotateLeft64(d, 36)) xor (TBits.RotateLeft64(d, 30))
      xor (TBits.RotateLeft64(d, 25))) + ((d and e) xor (d and f)
      xor (e and f)));

    b := b + (s_K[t] + data[t] + ((TBits.RotateLeft64(g, 50))
      xor (TBits.RotateLeft64(g, 46)) xor (TBits.RotateLeft64(g, 23))) +
      ((g and h) xor (not g and a)));
    System.Inc(t);
    f := f + b;
    b := b + (((TBits.RotateLeft64(c, 36)) xor (TBits.RotateLeft64(c, 30))
      xor (TBits.RotateLeft64(c, 25))) + ((c and d) xor (c and e)
      xor (d and e)));

    a := a + (s_K[t] + data[t] + ((TBits.RotateLeft64(f, 50))
      xor (TBits.RotateLeft64(f, 46)) xor (TBits.RotateLeft64(f, 23))) +
      ((f and g) xor (not f and h)));
    System.Inc(t);
    e := e + a;
    a := a + (((TBits.RotateLeft64(b, 36)) xor (TBits.RotateLeft64(b, 30))
      xor (TBits.RotateLeft64(b, 25))) + ((b and c) xor (b and d)
      xor (c and d)));

    System.Inc(i);
  end;

{$ENDIF USE_UNROLLED_VARIANT}
  Fm_state[0] := Fm_state[0] + a;
  Fm_state[1] := Fm_state[1] + b;
  Fm_state[2] := Fm_state[2] + c;
  Fm_state[3] := Fm_state[3] + d;
  Fm_state[4] := Fm_state[4] + e;
  Fm_state[5] := Fm_state[5] + f;
  Fm_state[6] := Fm_state[6] + g;
  Fm_state[7] := Fm_state[7] + h;

  System.FillChar(data, System.SizeOf(data), UInt64(0));

end;

end.
