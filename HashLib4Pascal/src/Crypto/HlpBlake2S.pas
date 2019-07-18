unit HlpBlake2S;

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
  HlpIBlake2SConfig,
  HlpBlake2SConfig,
  HlpIBlake2STreeConfig,
  HlpBlake2SIvBuilder,
  HlpIHash,
  HlpIHashInfo,
  HlpConverters,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SInvalidConfigLength = 'Config Length Must Be 8 Words';

type
  TBlake2S = class sealed(THash, ICryptoNotBuildIn, ITransformBlock)
  strict private

{$REGION 'Consts'}
  const

{$IFNDEF USE_UNROLLED_VARIANT}
    NumberOfRounds = Int32(10);
{$ENDIF USE_UNROLLED_VARIANT}
    BlockSizeInBytes = Int32(64);

    IV0 = UInt32($66A09E667);
    IV1 = UInt32($BB67AE85);
    IV2 = UInt32($3C6EF372);
    IV3 = UInt32($A54FF53A);
    IV4 = UInt32($510E527F);
    IV5 = UInt32($9B05688C);
    IV6 = UInt32($1F83D9AB);
    IV7 = UInt32($5BE0CD19);

{$IFNDEF USE_UNROLLED_VARIANT}
    Sigma: array [0 .. 9, 0 .. 15] of Byte = ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      11, 12, 13, 14, 15), (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5,
      3), (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),
      (7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),
      (9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),
      (2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),
      (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),
      (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),
      (6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),
      (10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0));
{$ENDIF USE_UNROLLED_VARIANT}
{$ENDREGION}
    class var

      FDefaultConfig: IBlake2SConfig;

  var
    F_m: array [0 .. 15] of UInt32;
    FrawConfig, Fm_state: THashLibUInt32Array;
    FKey, F_buf: THashLibByteArray;
{$IFNDEF USE_UNROLLED_VARIANT}
    F_v: array [0 .. 15] of UInt32;
{$ENDIF USE_UNROLLED_VARIANT}
    F_bufferFilled, FHashSize, FBlockSize: Int32;
    F_counter0, F_counter1, F_finalizationFlag0, F_finalizationFlag1: UInt32;
    FtreeConfig: IBlake2STreeConfig;

    class constructor Blake2SConfig();

{$IFNDEF USE_UNROLLED_VARIANT}
    procedure G(a, b, c, d, r, i: Int32); inline;
{$ENDIF USE_UNROLLED_VARIANT}
    procedure Compress(block: PByte; start: Int32);

    procedure Finish(); inline;

  strict protected

    function GetName: String; override;

  public
    constructor Create(); overload;
    constructor Create(const config: IBlake2SConfig); overload;
    constructor Create(const config: IBlake2SConfig;
      const treeConfig: IBlake2STreeConfig); overload;
    procedure Initialize; override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_data_length: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;

  end;

implementation

{ TBlake2S }

class constructor TBlake2S.Blake2SConfig;
begin
  FDefaultConfig := TBlake2SConfig.Create();
end;

constructor TBlake2S.Create();
begin
  Create(TBlake2SConfig.Create() as IBlake2SConfig);
end;

{$IFNDEF USE_UNROLLED_VARIANT}

procedure TBlake2S.G(a, b, c, d, r, i: Int32);
begin

  F_v[a] := F_v[a] + (F_v[b] + F_m[Sigma[r][2 * i + 0]]);
  F_v[d] := TBits.RotateRight32(F_v[d] xor F_v[a], 16);
  F_v[c] := F_v[c] + F_v[d];
  F_v[b] := TBits.RotateRight32(F_v[b] xor F_v[c], 12);
  F_v[a] := F_v[a] + (F_v[b] + F_m[Sigma[r][2 * i + 1]]);
  F_v[d] := TBits.RotateRight32(F_v[d] xor F_v[a], 8);
  F_v[c] := F_v[c] + F_v[d];
  F_v[b] := TBits.RotateRight32(F_v[b] xor F_v[c], 7);
end;

{$ENDIF USE_UNROLLED_VARIANT}

function TBlake2S.Clone(): IHash;
var
  HashInstance: TBlake2S;
begin
  HashInstance := TBlake2S.Create(TBlake2SConfig.Create(FHashSize)
    as IBlake2SConfig);
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

procedure TBlake2S.Compress(block: PByte; start: Int32);
var
{$IFDEF USE_UNROLLED_VARIANT}
  m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, v0, v1,
    v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: UInt32;

{$ELSE}
  i, r: Int32;

{$ENDIF USE_UNROLLED_VARIANT}
begin
  TConverters.le32_copy(block, start, @(F_m[0]), 0, FBlockSize);

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
  // *
  // Round 1.
  v0 := v0 + m0;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m2;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m4;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m6;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m5;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m7;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m3;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m1;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m8;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m10;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m12;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m14;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m13;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m15;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m11;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m9;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 2.
  v0 := v0 + m14;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m4;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m9;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m13;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m15;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m6;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m8;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m10;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m1;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m0;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m11;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m5;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m7;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m3;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m2;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m12;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 3.
  v0 := v0 + m11;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m12;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m5;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m15;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m2;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m13;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m0;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m8;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m10;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m3;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m7;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m9;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m1;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m4;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m6;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m14;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 4.
  v0 := v0 + m7;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m3;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m13;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m11;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m12;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m14;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m1;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m9;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m2;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m5;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m4;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m15;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m0;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m8;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m10;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m6;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 5.
  v0 := v0 + m9;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m5;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m2;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m10;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m4;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m15;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m7;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m0;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m14;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m11;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m6;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m3;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m8;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m13;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m12;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m1;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 6.
  v0 := v0 + m2;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m6;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m0;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m8;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m11;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m3;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m10;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m12;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m4;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m7;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m15;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m1;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m14;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m9;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m5;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m13;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 7.
  v0 := v0 + m12;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m1;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m14;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m4;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m13;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m10;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m15;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m5;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m0;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m6;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m9;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m8;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m2;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m11;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m3;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m7;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 8.
  v0 := v0 + m13;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m7;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m12;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m3;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m1;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m9;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m14;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m11;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m5;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m15;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m8;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m2;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m6;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m10;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m4;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m0;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 9.
  v0 := v0 + m6;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m14;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m11;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m0;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m3;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m8;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m9;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m15;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m12;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m13;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m1;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m10;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m4;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m5;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m7;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m2;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 10.
  v0 := v0 + m10;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m8;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m7;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m1;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m6;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m5;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m4;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m2;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m15;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m9;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m3;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m13;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m12;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m0;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m14;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m11;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);
  // */
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
    G(1, 5, 9, 13, r, 1);
    G(2, 6, 10, 14, r, 2);
    G(3, 7, 11, 15, r, 3);
    G(0, 5, 10, 15, r, 4);
    G(1, 6, 11, 12, r, 5);
    G(2, 7, 8, 13, r, 6);
    G(3, 4, 9, 14, r, 7);

  end;

  for i := 0 to 7 do
  begin
    Fm_state[i] := Fm_state[i] xor (F_v[i] xor F_v[i + 8]);
  end;

{$ENDIF USE_UNROLLED_VARIANT}
end;

constructor TBlake2S.Create(const config: IBlake2SConfig);
begin
  Create(config, Nil);
end;

constructor TBlake2S.Create(const config: IBlake2SConfig;
  const treeConfig: IBlake2STreeConfig);
var
  Lconfig: IBlake2SConfig;
begin

  Lconfig := config;
  FtreeConfig := treeConfig;
  FBlockSize := BlockSizeInBytes;

  if (Lconfig = Nil) then
  begin
    Lconfig := FDefaultConfig;
  end;

  FrawConfig := TBlake2SIvBuilder.ConfigS(Lconfig, FtreeConfig);
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

procedure TBlake2S.Finish;
var
  count: Int32;
begin

  // Last compression

  F_counter0 := F_counter0 + UInt32(F_bufferFilled);

  F_finalizationFlag0 := System.High(UInt32);

  if (FtreeConfig.IsLastNode) then
  begin
    F_finalizationFlag1 := System.High(UInt32);
  end;

  count := System.Length(F_buf) - F_bufferFilled;

  if count > 0 then
  begin
    TArrayUtils.Fill(F_buf, F_bufferFilled, count + F_bufferFilled, Byte(0));
  end;

  Compress(PByte(F_buf), 0);

end;

procedure TBlake2S.Initialize;
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

  System.FillChar(F_m, System.SizeOf(F_m), UInt32(0));

{$IFNDEF USE_UNROLLED_VARIANT}
  System.FillChar(F_v, System.SizeOf(F_v), UInt32(0));
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

procedure TBlake2S.TransformBytes(const a_data: THashLibByteArray;
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
    F_counter0 := F_counter0 + UInt32(BlockSizeInBytes);
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
    F_counter0 := F_counter0 + UInt32(BlockSizeInBytes);
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

function TBlake2S.TransformFinal: IHashResult;
var
  tempRes: THashLibByteArray;
begin
  Finish();
  System.SetLength(tempRes, FHashSize);
  TConverters.le32_copy(PCardinal(Fm_state), 0, PByte(tempRes), 0,
    System.Length(tempRes));
  Result := THashResult.Create(tempRes);
  Initialize();
end;

function TBlake2S.GetName: String;
begin
  Result := Format('%s_%u', [Self.ClassName, Self.HashSize * 8]);
end;

end.
