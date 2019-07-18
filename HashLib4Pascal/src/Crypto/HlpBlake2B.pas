unit HlpBlake2B;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF HAS_UNITSCOPE}
{$IFDEF DELPHI}
  HlpBitConverter,
  HlpHashBuffer,
{$ENDIF DELPHI}
  HlpBits,
  HlpHash,
  HlpHashResult,
  HlpIHashResult,
  HlpIBlake2BConfig,
  HlpBlake2BConfig,
  HlpIBlake2BTreeConfig,
  HlpBlake2BIvBuilder,
  HlpIHash,
  HlpIHashInfo,
  HlpConverters,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SInvalidConfigLength = 'Config Length Must Be 8 Words';

type
  TBlake2B = class sealed(THash, ICryptoNotBuildIn, ITransformBlock)
  strict private

{$REGION 'Consts'}
  const

{$IFNDEF USE_UNROLLED_VARIANT}
    NumberOfRounds = Int32(12);
{$ENDIF USE_UNROLLED_VARIANT}
    BlockSizeInBytes = Int32(128);

    IV0 = UInt64($6A09E667F3BCC908);
    IV1 = UInt64($BB67AE8584CAA73B);
    IV2 = UInt64($3C6EF372FE94F82B);
    IV3 = UInt64($A54FF53A5F1D36F1);
    IV4 = UInt64($510E527FADE682D1);
    IV5 = UInt64($9B05688C2B3E6C1F);
    IV6 = UInt64($1F83D9ABFB41BD6B);
    IV7 = UInt64($5BE0CD19137E2179);

{$IFNDEF USE_UNROLLED_VARIANT}
    Sigma: array [0 .. ((NumberOfRounds * 16) - 1)] of Int32 = (0, 1, 2, 3, 4,
      5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 10, 4, 8, 9, 15, 13, 6, 1, 12,
      0, 2, 11, 7, 5, 3, 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4,
      7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8, 9, 0, 5, 7, 2, 4,
      10, 15, 14, 1, 11, 12, 6, 8, 3, 13, 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7,
      5, 15, 14, 1, 9, 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11, 13,
      11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10, 6, 15, 14, 9, 11, 3, 0,
      8, 12, 2, 13, 7, 1, 4, 10, 5, 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3,
      12, 13, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 10,
      4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3);
{$ENDIF USE_UNROLLED_VARIANT}
{$ENDREGION}
    class var

      FDefaultConfig: IBlake2BConfig;

  var
    F_m: array [0 .. 15] of UInt64;
    FrawConfig, Fm_state: THashLibUInt64Array;
    FKey, F_buf: THashLibByteArray;
{$IFNDEF USE_UNROLLED_VARIANT}
    F_v: array [0 .. 15] of UInt64;
{$ENDIF USE_UNROLLED_VARIANT}
    F_bufferFilled, FHashSize, FBlockSize: Int32;
    F_counter0, F_counter1, F_finalizationFlag0, F_finalizationFlag1: UInt64;
    FtreeConfig: IBlake2BTreeConfig;

    class constructor Blake2BConfig();

{$IFNDEF USE_UNROLLED_VARIANT}
    procedure G(a, b, c, d, r, i: Int32); inline;
{$ENDIF USE_UNROLLED_VARIANT}
    procedure Compress(block: PByte; start: Int32);

    procedure Finish(); inline;

  strict protected

    function GetName: String; override;

  public
    constructor Create(); overload;
    constructor Create(const config: IBlake2BConfig); overload;
    constructor Create(const config: IBlake2BConfig;
      const treeConfig: IBlake2BTreeConfig); overload;
    procedure Initialize; override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_data_length: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;

  end;

implementation

{ TBlake2B }

class constructor TBlake2B.Blake2BConfig;
begin
  FDefaultConfig := TBlake2BConfig.Create();
end;

constructor TBlake2B.Create();
begin
  Create(TBlake2BConfig.Create() as IBlake2BConfig);
end;

{$IFNDEF USE_UNROLLED_VARIANT}

procedure TBlake2B.G(a, b, c, d, r, i: Int32);
var
  p, p0, p1: Int32;
begin
  p := (r shl 4) + i;
  p0 := Sigma[p];
  p1 := Sigma[p + 1];

  F_v[a] := F_v[a] + (F_v[b] + F_m[p0]);
  F_v[d] := TBits.RotateRight64(F_v[d] xor F_v[a], 32);
  F_v[c] := F_v[c] + F_v[d];
  F_v[b] := TBits.RotateRight64(F_v[b] xor F_v[c], 24);
  F_v[a] := F_v[a] + (F_v[b] + F_m[p1]);
  F_v[d] := TBits.RotateRight64(F_v[d] xor F_v[a], 16);
  F_v[c] := F_v[c] + F_v[d];
  F_v[b] := TBits.RotateRight64(F_v[b] xor F_v[c], 63);
end;

{$ENDIF USE_UNROLLED_VARIANT}

function TBlake2B.Clone(): IHash;
var
  HashInstance: TBlake2B;
begin
  HashInstance := TBlake2B.Create(TBlake2BConfig.Create(FHashSize)
    as IBlake2BConfig);
  System.Move(F_m, HashInstance.F_m, System.SizeOf(F_m));
  HashInstance.FrawConfig := System.Copy(FrawConfig);
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.FKey := System.Copy(FKey);
  HashInstance.F_buf := System.Copy(F_buf);
{$IFNDEF USE_UNROLLED_VARIANT}
  System.Move(F_v, HashInstance.F_v, System.SizeOf(F_v));
{$ENDIF USE_UNROLLED_VARIANT}
  HashInstance.F_bufferFilled := F_bufferFilled;
  HashInstance.F_counter0 := F_counter0;
  HashInstance.F_counter1 := F_counter1;
  HashInstance.F_finalizationFlag0 := F_finalizationFlag0;
  HashInstance.F_finalizationFlag1 := F_finalizationFlag1;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

procedure TBlake2B.Compress(block: PByte; start: Int32);
var
{$IFDEF USE_UNROLLED_VARIANT}
  m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, v0, v1,
    v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: UInt64;

{$ELSE}
  i, r: Int32;

{$ENDIF USE_UNROLLED_VARIANT}
begin
  TConverters.le64_copy(block, start, @(F_m[0]), 0, FBlockSize);

{$IFDEF USE_UNROLLED_VARIANT}
  m0 := F_m[0];
  m1 := F_m[1];
  m2 := F_m[2];
  m3 := F_m[3];
  m4 := F_m[4];
  m5 := F_m[5];
  m6 := F_m[6];
  m7 := F_m[7];
  m8 := F_m[8];
  m9 := F_m[9];
  m10 := F_m[10];
  m11 := F_m[11];
  m12 := F_m[12];
  m13 := F_m[13];
  m14 := F_m[14];
  m15 := F_m[15];

  v0 := Fm_state[0];
  v1 := Fm_state[1];
  v2 := Fm_state[2];
  v3 := Fm_state[3];
  v4 := Fm_state[4];
  v5 := Fm_state[5];
  v6 := Fm_state[6];
  v7 := Fm_state[7];

  v8 := IV0;
  v9 := IV1;
  v10 := IV2;
  v11 := IV3;
  v12 := IV4 xor F_counter0;
  v13 := IV5 xor F_counter1;
  v14 := IV6 xor F_finalizationFlag0;
  v15 := IV7 xor F_finalizationFlag1;

  // Rounds

  // ##### Round(0)
  // G(0, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m0;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m1;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(0, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m2;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m3;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(0, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m4;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m5;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(0, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m6;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(0, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m8;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m9;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(0, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m10;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m11;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(0, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m12;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m13;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(0, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m14;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m15;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(1)
  // G(1, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m14;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m10;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(1, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m4;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m8;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(1, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m9;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m15;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(1, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m13;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m6;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(1, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m1;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m12;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(1, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m0;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m2;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(1, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m11;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(1, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m5;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m3;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(2)
  // G(2, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m11;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m8;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(2, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m12;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m0;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(2, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m5;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m2;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(2, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m15;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m13;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(2, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m10;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m14;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(2, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m3;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(2, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m1;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(2, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m9;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(3)
  // G(3, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m7;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m9;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(3, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m3;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m1;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(3, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m13;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m12;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(3, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m11;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m14;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(3, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m2;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m6;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(3, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m5;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m10;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(3, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m4;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m0;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(3, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m15;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m8;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(4)
  // G(4, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m9;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m0;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(4, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m7;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(4, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m2;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m4;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(4, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m10;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m15;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(4, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m14;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m1;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(4, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m11;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m12;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(4, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m6;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m8;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(4, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m3;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m13;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(5)
  // G(5, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m2;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m12;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(5, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m6;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m10;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(5, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m0;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m11;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(5, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m8;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m3;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(5, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m4;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m13;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(5, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m7;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m5;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(5, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m15;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m14;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(5, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m1;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m9;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(6)
  // G(6, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m12;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m5;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(6, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m1;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m15;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(6, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m14;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m13;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(6, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m4;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m10;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(6, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m0;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m7;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(6, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m3;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(6, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m9;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m2;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(6, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m8;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m11;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(7)
  // G(7, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m13;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m11;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(7, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m7;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m14;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(7, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m12;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m1;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(7, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m3;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m9;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(7, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m0;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(7, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m15;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m4;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(7, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m8;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m6;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(7, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m2;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m10;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(8)
  // G(8, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m6;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m15;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(8, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m14;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m9;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(8, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m11;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m3;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(8, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m0;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m8;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(8, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m12;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m2;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(8, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m13;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m7;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(8, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m1;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m4;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(8, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m10;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m5;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(9)
  // G(9, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m10;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m2;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(9, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m8;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m4;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(9, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m7;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(9, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m1;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m5;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(9, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m15;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m11;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(9, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m9;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m14;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(9, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m3;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m12;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(9, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m13;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m0;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(10)
  // G(10, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m0;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m1;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(10, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m2;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m3;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(10, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m4;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m5;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(10, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m6;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(10, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m8;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m9;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(10, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m10;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m11;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(10, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m12;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m13;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(10, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m14;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m15;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // ##### Round(11)
  // G(11, 0, v0, v4, v8, v12)
  v0 := v0 + v4 + m14;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 32);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 24);
  v0 := v0 + v4 + m10;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight64(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight64(v4, 63);

  // G(11, 1, v1, v5, v9, v13)
  v1 := v1 + v5 + m4;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 32);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 24);
  v1 := v1 + v5 + m8;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight64(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight64(v5, 63);

  // G(11, 2, v2, v6, v10, v14)
  v2 := v2 + v6 + m9;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 32);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 24);
  v2 := v2 + v6 + m15;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight64(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight64(v6, 63);

  // G(11, 3, v3, v7, v11, v15)
  v3 := v3 + v7 + m13;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 32);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 24);
  v3 := v3 + v7 + m6;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight64(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight64(v7, 63);

  // G(11, 4, v0, v5, v10, v15)
  v0 := v0 + v5 + m1;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 32);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 24);
  v0 := v0 + v5 + m12;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight64(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight64(v5, 63);

  // G(11, 5, v1, v6, v11, v12)
  v1 := v1 + v6 + m0;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 32);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 24);
  v1 := v1 + v6 + m2;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight64(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight64(v6, 63);

  // G(11, 6, v2, v7, v8, v13)
  v2 := v2 + v7 + m11;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 32);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 24);
  v2 := v2 + v7 + m7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight64(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight64(v7, 63);

  // G(11, 7, v3, v4, v9, v14)
  v3 := v3 + v4 + m5;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 32);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 24);
  v3 := v3 + v4 + m3;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight64(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight64(v4, 63);

  // Finalization
  Fm_state[0] := Fm_state[0] xor (v0 xor v8);
  Fm_state[1] := Fm_state[1] xor (v1 xor v9);
  Fm_state[2] := Fm_state[2] xor (v2 xor v10);
  Fm_state[3] := Fm_state[3] xor (v3 xor v11);
  Fm_state[4] := Fm_state[4] xor (v4 xor v12);
  Fm_state[5] := Fm_state[5] xor (v5 xor v13);
  Fm_state[6] := Fm_state[6] xor (v6 xor v14);
  Fm_state[7] := Fm_state[7] xor (v7 xor v15);

{$ELSE}
  F_v[0] := Fm_state[0];
  F_v[1] := Fm_state[1];
  F_v[2] := Fm_state[2];
  F_v[3] := Fm_state[3];
  F_v[4] := Fm_state[4];
  F_v[5] := Fm_state[5];
  F_v[6] := Fm_state[6];
  F_v[7] := Fm_state[7];

  F_v[8] := IV0;
  F_v[9] := IV1;
  F_v[10] := IV2;
  F_v[11] := IV3;
  F_v[12] := IV4 xor F_counter0;
  F_v[13] := IV5 xor F_counter1;

  F_v[14] := IV6 xor F_finalizationFlag0;

  F_v[15] := IV7 xor F_finalizationFlag1;

  for r := 0 to System.Pred(NumberOfRounds) do

  begin
    G(0, 4, 8, 12, r, 0);
    G(1, 5, 9, 13, r, 2);
    G(2, 6, 10, 14, r, 4);
    G(3, 7, 11, 15, r, 6);
    G(3, 4, 9, 14, r, 14);
    G(2, 7, 8, 13, r, 12);
    G(0, 5, 10, 15, r, 8);
    G(1, 6, 11, 12, r, 10);
  end;

  for i := 0 to 7 do
  begin
    Fm_state[i] := Fm_state[i] xor (F_v[i] xor F_v[i + 8]);
  end;

{$ENDIF USE_UNROLLED_VARIANT}
end;

constructor TBlake2B.Create(const config: IBlake2BConfig);
begin
  Create(config, Nil);
end;

constructor TBlake2B.Create(const config: IBlake2BConfig;
  const treeConfig: IBlake2BTreeConfig);
var
  Lconfig: IBlake2BConfig;
begin

  Lconfig := config;
  FtreeConfig := treeConfig;
  FBlockSize := BlockSizeInBytes;

  if (Lconfig = Nil) then
  begin
    Lconfig := FDefaultConfig;
  end;

  FrawConfig := TBlake2BIvBuilder.ConfigB(Lconfig, FtreeConfig);
  if ((Lconfig.Key <> Nil) and (System.Length(Lconfig.Key) <> 0)) then
  begin

    FKey := System.Copy(Lconfig.Key, System.Low(Lconfig.Key),
      System.Length(Lconfig.Key));

    System.SetLength(FKey, FBlockSize);

  end;
  FHashSize := Lconfig.HashSize;

  System.SetLength(Fm_state, 8);

  Inherited Create(FHashSize, FBlockSize);

end;

procedure TBlake2B.Finish;
var
  count: Int32;
begin

  // Last compression

  F_counter0 := F_counter0 + UInt64(F_bufferFilled);

  F_finalizationFlag0 := System.High(UInt64);

  if (FtreeConfig.IsLastNode) then
  begin
    F_finalizationFlag1 := System.High(UInt64);
  end;

  count := System.Length(F_buf) - F_bufferFilled;

  if count > 0 then
  begin
    TArrayUtils.Fill(F_buf, F_bufferFilled, count + F_bufferFilled, Byte(0));
  end;

  Compress(PByte(F_buf), 0);

end;

procedure TBlake2B.Initialize;
var
  i: Int32;
begin
  if (FrawConfig = Nil) then
    raise EArgumentNilHashLibException.Create('config');
  if (System.Length(FrawConfig) <> 8) then
  begin
    raise EArgumentHashLibException.CreateRes(@SInvalidConfigLength);
  end;

  Fm_state[0] := IV0;
  Fm_state[1] := IV1;
  Fm_state[2] := IV2;
  Fm_state[3] := IV3;
  Fm_state[4] := IV4;
  Fm_state[5] := IV5;
  Fm_state[6] := IV6;
  Fm_state[7] := IV7;

  F_counter0 := 0;
  F_counter1 := 0;
  F_finalizationFlag0 := 0;
  F_finalizationFlag1 := 0;

  F_bufferFilled := 0;

  System.SetLength(F_buf, BlockSizeInBytes);

  TArrayUtils.ZeroFill(F_buf);

  System.FillChar(F_m, System.SizeOf(F_m), UInt64(0));

{$IFNDEF USE_UNROLLED_VARIANT}
  System.FillChar(F_v, System.SizeOf(F_v), UInt64(0));
{$ENDIF USE_UNROLLED_VARIANT}
  for i := 0 to 7 do
  begin
    Fm_state[i] := Fm_state[i] xor FrawConfig[i];
  end;

  if (FKey <> Nil) then
  begin
    TransformBytes(FKey, 0, System.Length(FKey));
  end;

end;

procedure TBlake2B.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_data_length: Int32);
var
  offset, bufferRemaining: Int32;

begin
  offset := a_index;
  bufferRemaining := BlockSizeInBytes - F_bufferFilled;

  if ((F_bufferFilled > 0) and (a_data_length > bufferRemaining)) then
  begin

    if bufferRemaining > 0 then
    begin
      System.Move(a_data[offset], F_buf[F_bufferFilled], bufferRemaining);
    end;
    F_counter0 := F_counter0 + UInt64(BlockSizeInBytes);
    if (F_counter0 = 0) then
    begin
      System.Inc(F_counter1);
    end;
    Compress(PByte(F_buf), 0);
    offset := offset + bufferRemaining;
    a_data_length := a_data_length - bufferRemaining;
    F_bufferFilled := 0;
  end;

  while (a_data_length > BlockSizeInBytes) do
  begin
    F_counter0 := F_counter0 + UInt64(BlockSizeInBytes);
    if (F_counter0 = 0) then
    begin
      System.Inc(F_counter1);
    end;
    Compress(PByte(a_data), offset);
    offset := offset + BlockSizeInBytes;
    a_data_length := a_data_length - BlockSizeInBytes;
  end;

  if (a_data_length > 0) then
  begin
    System.Move(a_data[offset], F_buf[F_bufferFilled], a_data_length);
    F_bufferFilled := F_bufferFilled + a_data_length;
  end;
end;

function TBlake2B.TransformFinal: IHashResult;
var
  tempRes: THashLibByteArray;
begin
  Finish();
  System.SetLength(tempRes, FHashSize);
  TConverters.le64_copy(PUInt64(Fm_state), 0, PByte(tempRes), 0,
    System.Length(tempRes));
  Result := THashResult.Create(tempRes);
  Initialize();
end;

function TBlake2B.GetName: String;
begin
  Result := Format('%s_%u', [Self.ClassName, Self.HashSize * 8]);
end;

end.
