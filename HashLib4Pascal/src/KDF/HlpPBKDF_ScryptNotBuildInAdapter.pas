unit HlpPBKDF_ScryptNotBuildInAdapter;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHIXE7_UP}
  System.Classes,
  System.SysUtils,
  System.Threading,
{$ENDIF DELPHIXE7_UP}
  HlpIHash,
  HlpKDF,
  HlpBits,
  HlpSHA2_256,
  HlpIHashInfo,
  HlpPBKDF2_HMACNotBuildInAdapter,
  HlpConverters,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SInvalidByteCount =
    '"bc (ByteCount)" Argument must be a value greater than zero.';
  SInvalidCost = 'Cost parameter must be > 1 and a power of 2.';
  SBlockSizeAndCostIncompatible = 'Cost parameter must be > 1 and < 65536.';
  SBlockSizeTooSmall = 'Block size must be >= 1.';
  SInvalidParallelism =
    'Parallelism parameter must be >= 1 and <= %d (based on block size of %d)';
  SRoundsMustBeEven = 'Number of Rounds Must be Even';

type
  /// <summary>Implementation of the scrypt a password-based key derivation function.</summary>
  /// <remarks>
  /// Scrypt was created by Colin Percival and is specified in
  /// <a href="http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-01">draft-josefsson-scrypt-kd</a>.
  /// </remarks>
  TPBKDF_ScryptNotBuildInAdapter = class sealed(TKDF, IPBKDF_Scrypt,
    IPBKDF_ScryptNotBuildIn)

  strict private
  var
    FPasswordBytes, FSaltBytes: THashLibByteArray;
    FCost, FBlockSize, FParallelism: Int32;

    class procedure ClearArray(const AInput: THashLibByteArray); overload;
      static; inline;
    class procedure ClearArray(const AInput: THashLibUInt32Array); overload;
      static; inline;

    class procedure ClearAllArrays(const AInputs
      : THashLibMatrixUInt32Array); static;

    class function IsPowerOf2(x: Int32): Boolean; static; inline;

    class function SingleIterationPBKDF2(const APasswordBytes,
      ASaltBytes: THashLibByteArray; AOutputLength: Int32)
      : THashLibByteArray; static;

    /// <summary>
    /// Rotate left
    /// </summary>
    /// <param name="x">
    /// value to rotate
    /// </param>
    /// <param name="y">
    /// amount to rotate x
    /// </param>
    /// <returns>
    /// rotated x
    /// </returns>
    class function R(x: UInt32; y: Int32): UInt32; static; inline;

    /// <summary>
    /// lifted from <c>ClpSalsa20Engine.pas</c> in CryptoLib4Pascal with
    /// minor modifications.
    /// </summary>
    class procedure SalsaCore(rounds: Int32;
      const input, x: THashLibUInt32Array); static;

    class procedure &Xor(const a, b: THashLibUInt32Array; bOff: Int32;
      const output: THashLibUInt32Array); static;

    class procedure SMix(const b: THashLibUInt32Array;
      bOff, N, R: Int32); static;

    class procedure BlockMix(const b, X1, X2, y: THashLibUInt32Array;
      R: Int32); static;

    class procedure DoSMix(const b: THashLibUInt32Array;
      AParallelism, ACost, ABlockSize: Int32); static;

    class function MFcrypt(const APasswordBytes, ASaltBytes: THashLibByteArray;
      ACost, ABlockSize, AParallelism, AOutputLength: Int32)
      : THashLibByteArray; static;

  public

    class procedure ValidatePBKDF_ScryptInputs(ACost, ABlockSize,
      AParallelism: Int32); static;

    constructor Create(const APasswordBytes, ASaltBytes: THashLibByteArray;
      ACost, ABlockSize, AParallelism: Int32);

    destructor Destroy; override;

    procedure Clear(); override;

    /// <summary>
    /// Returns the pseudo-random bytes for this object.
    /// </summary>
    /// <param name="bc">The number of pseudo-random key bytes to generate.</param>
    /// <returns>A byte array filled with pseudo-random key bytes.</returns>
    /// /// <exception cref="EArgumentOutOfRangeHashLibException">bc must be greater than zero.</exception>
    function GetBytes(bc: Int32): THashLibByteArray; override;

  end;

implementation

{ TPBKDF_ScryptNotBuildInAdapter }

class procedure TPBKDF_ScryptNotBuildInAdapter.ClearArray
  (const AInput: THashLibByteArray);
begin
  TArrayUtils.ZeroFill(AInput);
end;

class procedure TPBKDF_ScryptNotBuildInAdapter.ClearArray
  (const AInput: THashLibUInt32Array);
begin
  TArrayUtils.ZeroFill(AInput);
end;

class procedure TPBKDF_ScryptNotBuildInAdapter.ClearAllArrays
  (const AInputs: THashLibMatrixUInt32Array);
var
  Idx: Int32;
begin
  for Idx := System.Low(AInputs) to System.High(AInputs) do
  begin
    ClearArray(AInputs[Idx]);
  end;
end;

class function TPBKDF_ScryptNotBuildInAdapter.IsPowerOf2(x: Int32): Boolean;
begin
  result := (x > 0) and ((x and (x - 1)) = 0);
end;

class function TPBKDF_ScryptNotBuildInAdapter.SingleIterationPBKDF2
  (const APasswordBytes, ASaltBytes: THashLibByteArray; AOutputLength: Int32)
  : THashLibByteArray;
begin
  result := (TPBKDF2_HMACNotBuildInAdapter.Create(TSHA2_256.Create() as IHash,
    APasswordBytes, ASaltBytes, 1) as IPBKDF2_HMAC).GetBytes(AOutputLength);
end;

class procedure TPBKDF_ScryptNotBuildInAdapter.&Xor(const a,
  b: THashLibUInt32Array; bOff: Int32; const output: THashLibUInt32Array);
var
  i: Int32;
begin
  i := System.Length(output) - 1;
  while i >= 0 do
  begin
    output[i] := a[i] xor b[bOff + i];
    System.Dec(i);
  end;
end;

class function TPBKDF_ScryptNotBuildInAdapter.R(x: UInt32; y: Int32): UInt32;
begin
  result := TBits.RotateLeft32(x, y);
end;

class procedure TPBKDF_ScryptNotBuildInAdapter.SalsaCore(rounds: Int32;
  const input, x: THashLibUInt32Array);
var
  x00, x01, x02, x03, x04, x05, x06, x07, x08, x09, x10, x11, x12, x13, x14,
    x15: UInt32;
  Idx: Int32;
begin
  if (System.Length(input) <> 16) then
  begin
    raise EArgumentHashLibException.Create('');
  end;
  if (System.Length(x) <> 16) then
  begin
    raise EArgumentHashLibException.Create('');
  end;
  if ((rounds mod 2) <> 0) then
  begin
    raise EArgumentHashLibException.CreateRes(@SRoundsMustBeEven);
  end;

  x00 := input[0];
  x01 := input[1];
  x02 := input[2];
  x03 := input[3];
  x04 := input[4];
  x05 := input[5];
  x06 := input[6];
  x07 := input[7];
  x08 := input[8];
  x09 := input[9];
  x10 := input[10];
  x11 := input[11];
  x12 := input[12];
  x13 := input[13];
  x14 := input[14];
  x15 := input[15];

  Idx := rounds;
  while Idx > 0 do
  begin

    x04 := x04 xor (R((x00 + x12), 7));
    x08 := x08 xor (R((x04 + x00), 9));
    x12 := x12 xor (R((x08 + x04), 13));
    x00 := x00 xor (R((x12 + x08), 18));
    x09 := x09 xor (R((x05 + x01), 7));
    x13 := x13 xor (R((x09 + x05), 9));
    x01 := x01 xor (R((x13 + x09), 13));
    x05 := x05 xor (R((x01 + x13), 18));
    x14 := x14 xor (R((x10 + x06), 7));
    x02 := x02 xor (R((x14 + x10), 9));
    x06 := x06 xor (R((x02 + x14), 13));
    x10 := x10 xor (R((x06 + x02), 18));
    x03 := x03 xor (R((x15 + x11), 7));
    x07 := x07 xor (R((x03 + x15), 9));
    x11 := x11 xor (R((x07 + x03), 13));
    x15 := x15 xor (R((x11 + x07), 18));

    x01 := x01 xor (R((x00 + x03), 7));
    x02 := x02 xor (R((x01 + x00), 9));
    x03 := x03 xor (R((x02 + x01), 13));
    x00 := x00 xor (R((x03 + x02), 18));
    x06 := x06 xor (R((x05 + x04), 7));
    x07 := x07 xor (R((x06 + x05), 9));
    x04 := x04 xor (R((x07 + x06), 13));
    x05 := x05 xor (R((x04 + x07), 18));
    x11 := x11 xor (R((x10 + x09), 7));
    x08 := x08 xor (R((x11 + x10), 9));
    x09 := x09 xor (R((x08 + x11), 13));
    x10 := x10 xor (R((x09 + x08), 18));
    x12 := x12 xor (R((x15 + x14), 7));
    x13 := x13 xor (R((x12 + x15), 9));
    x14 := x14 xor (R((x13 + x12), 13));
    x15 := x15 xor (R((x14 + x13), 18));

    System.Dec(Idx, 2);
  end;

  x[0] := x00 + input[0];
  x[1] := x01 + input[1];
  x[2] := x02 + input[2];
  x[3] := x03 + input[3];
  x[4] := x04 + input[4];
  x[5] := x05 + input[5];
  x[6] := x06 + input[6];
  x[7] := x07 + input[7];
  x[8] := x08 + input[8];
  x[9] := x09 + input[9];
  x[10] := x10 + input[10];
  x[11] := x11 + input[11];
  x[12] := x12 + input[12];
  x[13] := x13 + input[13];
  x[14] := x14 + input[14];
  x[15] := x15 + input[15];

end;

class procedure TPBKDF_ScryptNotBuildInAdapter.BlockMix(const b, X1, X2,
  y: THashLibUInt32Array; R: Int32);
var
  bOff, YOff, halfLen, i: Int32;
begin
  System.Move(b[System.Length(b) - 16], X1[0], 16 * System.SizeOf(UInt32));

  bOff := 0;
  YOff := 0;
  halfLen := System.Length(b) div 2;

  i := 2 * R;

  while i > 0 do
  begin
    &Xor(X1, b, bOff, X2);

    SalsaCore(8, X2, X1);
    System.Move(X1[0], y[YOff], 16 * System.SizeOf(UInt32));

    YOff := halfLen + bOff - YOff;
    bOff := bOff + 16;
    System.Dec(i);
  end;

  System.Move(y[0], b[0], System.Length(y) * System.SizeOf(UInt32));
end;

class procedure TPBKDF_ScryptNotBuildInAdapter.SMix
  (const b: THashLibUInt32Array; bOff, N, R: Int32);
var
  BCount, i: Int32;
  mask, j: UInt32;
  blockX1, blockX2, blockY, x: THashLibUInt32Array;
  V: THashLibMatrixUInt32Array;
begin
  BCount := R * 32;
  System.SetLength(blockX1, 16);
  System.SetLength(blockX2, 16);
  System.SetLength(blockY, BCount);

  System.SetLength(x, BCount);

  System.SetLength(V, N);

  try
    System.Move(b[bOff], x[0], BCount * System.SizeOf(UInt32));

    for i := 0 to System.Pred(N) do
    begin
      V[i] := System.Copy(x);
      BlockMix(x, blockX1, blockX2, blockY, R);
    end;

    mask := UInt32(N) - 1;

    i := 0;
    while i < N do
    begin
      j := x[BCount - 16] and mask;
      &Xor(x, V[j], 0, x);
      BlockMix(x, blockX1, blockX2, blockY, R);
      System.Inc(i);
    end;

    System.Move(x[0], b[bOff], BCount * System.SizeOf(UInt32));
  finally
    ClearAllArrays(V);
    ClearAllArrays(THashLibMatrixUInt32Array.Create(x, blockX1,
      blockX2, blockY));
  end;
end;

{$IFDEF DELPHIXE7_UP}

class procedure TPBKDF_ScryptNotBuildInAdapter.DoSMix
  (const b: THashLibUInt32Array; AParallelism, ACost, ABlockSize: Int32);

  function CreateTask(AOffset: Int32): ITask;
  begin
    result := TTask.Create(
      procedure()
      begin
        SMix(b, AOffset, ACost, ABlockSize);
      end);
  end;

var
  LIdx, LTaskIdx: Int32;
  LArrayTasks: array of ITask;
begin
  System.SetLength(LArrayTasks, AParallelism);
  for LIdx := 0 to System.Pred(AParallelism) do
  begin
    LArrayTasks[LIdx] := CreateTask(LIdx * 32 * ABlockSize);
  end;
  for LTaskIdx := System.Low(LArrayTasks) to System.High(LArrayTasks) do
  begin
    LArrayTasks[LTaskIdx].Start;
  end;
  TTask.WaitForAll(LArrayTasks);
end;

{$ELSE}

class procedure TPBKDF_ScryptNotBuildInAdapter.DoSMix
  (const b: THashLibUInt32Array; AParallelism, ACost, ABlockSize: Int32);
var
  i: Int32;
begin

  for i := 0 to System.Pred(AParallelism) do
  begin
    SMix(b, i * 32 * ABlockSize, ACost, ABlockSize);
  end;
end;

{$ENDIF}

class function TPBKDF_ScryptNotBuildInAdapter.MFcrypt(const APasswordBytes,
  ASaltBytes: THashLibByteArray; ACost, ABlockSize, AParallelism,
  AOutputLength: Int32): THashLibByteArray;
var
  MFLenBytes, BLen: Int32;
  bytes: THashLibByteArray;
  b: THashLibUInt32Array;
begin
  MFLenBytes := ABlockSize * 128;
  bytes := SingleIterationPBKDF2(APasswordBytes, ASaltBytes,
    AParallelism * MFLenBytes);

  try
    BLen := System.Length(bytes) div 4;
    System.SetLength(b, BLen);

    TConverters.le32_copy(PByte(bytes), 0, PCardinal(b), 0,
      System.Length(bytes) * System.SizeOf(Byte));

    DoSMix(b, AParallelism, ACost, ABlockSize);

    TConverters.le32_copy(PCardinal(b), 0, PByte(bytes), 0,
      System.Length(b) * System.SizeOf(UInt32));

    result := SingleIterationPBKDF2(APasswordBytes, bytes, AOutputLength);
  finally
    ClearArray(b);
    ClearArray(bytes);
  end;

end;

class procedure TPBKDF_ScryptNotBuildInAdapter.ValidatePBKDF_ScryptInputs(ACost,
  ABlockSize, AParallelism: Int32);
var
  maxParallel: Int32;
begin

  if ((ACost <= 1) or (not IsPowerOf2(ACost))) then
    raise EArgumentHashLibException.CreateRes(@SInvalidCost);

  // Only value of ABlockSize that cost (as an int) could be exceeded for is 1
  if ((ABlockSize = 1) and (ACost >= 65536)) then
    raise EArgumentHashLibException.CreateRes(@SBlockSizeAndCostIncompatible);

  if (ABlockSize < 1) then
    raise EArgumentHashLibException.CreateRes(@SBlockSizeTooSmall);

  maxParallel := System.High(Int32) div (128 * ABlockSize * 8);

  if ((AParallelism < 1) or (AParallelism > maxParallel)) then
    raise EArgumentHashLibException.CreateResFmt(@SInvalidParallelism,
      [maxParallel, ABlockSize]);
end;

procedure TPBKDF_ScryptNotBuildInAdapter.Clear();
begin
  TArrayUtils.ZeroFill(FPasswordBytes);
  TArrayUtils.ZeroFill(FSaltBytes);
end;

constructor TPBKDF_ScryptNotBuildInAdapter.Create(const APasswordBytes,
  ASaltBytes: THashLibByteArray; ACost, ABlockSize, AParallelism: Int32);
begin
  Inherited Create();
  ValidatePBKDF_ScryptInputs(ACost, ABlockSize, AParallelism);
  FPasswordBytes := System.Copy(APasswordBytes);
  FSaltBytes := System.Copy(ASaltBytes);
  FCost := ACost;
  FBlockSize := ABlockSize;
  FParallelism := AParallelism;
end;

destructor TPBKDF_ScryptNotBuildInAdapter.Destroy;
begin
  Clear();
  inherited Destroy;
end;

function TPBKDF_ScryptNotBuildInAdapter.GetBytes(bc: Int32): THashLibByteArray;
begin
  if (bc <= 0) then
    raise EArgumentHashLibException.CreateRes(@SInvalidByteCount);

  result := MFcrypt(FPasswordBytes, FSaltBytes, FCost, FBlockSize,
    FParallelism, bc);
end;

end.
