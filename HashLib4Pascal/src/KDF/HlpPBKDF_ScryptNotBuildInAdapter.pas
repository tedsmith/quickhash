unit HlpPBKDF_ScryptNotBuildInAdapter;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF USE_DELPHI_PPL}
  System.Classes,
  System.SysUtils,
  System.Threading,
{$ENDIF USE_DELPHI_PPL}
{$IFDEF USE_PASMP}
  PasMP,
{$ENDIF USE_PASMP}
{$IFDEF USE_MTPROCS}
  MTProcs,
{$ENDIF USE_MTPROCS}
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
    '"(AByteCount)" Argument must be a value greater than zero.';
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
  type
    PDataContainer = ^TDataContainer;

    TDataContainer = record
      PtrB: PCardinal;
      Parallelism, Cost, BlockSize: Int32;
    end;

  var
    FPasswordBytes, FSaltBytes: THashLibByteArray;
    FCost, FBlockSize, FParallelism: Int32;

    class procedure ClearArray(const AInput: THashLibByteArray); overload;
      static; inline;
    class procedure ClearArray(const AInput: THashLibUInt32Array); overload;
      static; inline;

    class procedure ClearAllArrays(const AInputs
      : THashLibMatrixUInt32Array); static;

    class function IsPowerOf2(Ax: Int32): Boolean; static; inline;

    class function SingleIterationPBKDF2(const APasswordBytes,
      ASaltBytes: THashLibByteArray; AOutputLength: Int32)
      : THashLibByteArray; static;

    /// <summary>
    /// Rotate left
    /// </summary>
    /// <param name="AValue">
    /// value to rotate
    /// </param>
    /// <param name="ADistance">
    /// distance to rotate AValue
    /// </param>
    /// <returns>
    /// rotated AValue
    /// </returns>
    class function Rotl(AValue: UInt32; ADistance: Int32): UInt32;
      static; inline;

    /// <summary>
    /// lifted from <c>ClpSalsa20Engine.pas</c> in CryptoLib4Pascal with
    /// minor modifications.
    /// </summary>
    class procedure SalsaCore(ARounds: Int32;
      const AInput, Ax: THashLibUInt32Array); static;

    class procedure &Xor(const Aa, Ab: THashLibUInt32Array; AbOff: Int32;
      const AOutput: THashLibUInt32Array); static;

    class procedure SMix(AIdx: Int32;
      APtrDataContainer: PDataContainer); static;

    class procedure BlockMix(const Ab, AX1, AX2, Ay: THashLibUInt32Array;
      AR: Int32); static;

    class procedure DoParallelSMix(ADataContainer: PDataContainer); static;

{$IFDEF USE_PASMP}
    class procedure PasMPSMixWrapper(const AJob: PPasMPJob;
      const AThreadIndex: LongInt; const ADataContainer: Pointer;
      const AFromIndex, AToIndex: TPasMPNativeInt); inline;
{$ENDIF USE_PASMP}
{$IFDEF USE_MTPROCS}
    class procedure MTProcsSMixWrapper(AIdx: PtrInt; ADataContainer: Pointer;
      AItem: TMultiThreadProcItem); inline;
{$ENDIF USE_MTPROCS}
    class function MFCrypt(const APasswordBytes, ASaltBytes: THashLibByteArray;
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
    /// <param name="AByteCount">The number of pseudo-random key bytes to generate.</param>
    /// <returns>A byte array filled with pseudo-random key bytes.</returns>
    /// /// <exception cref="EArgumentOutOfRangeHashLibException">AByteCount must be greater than zero.</exception>
    function GetBytes(AByteCount: Int32): THashLibByteArray; override;

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
  LIdx: Int32;
begin
  for LIdx := System.Low(AInputs) to System.High(AInputs) do
  begin
    ClearArray(AInputs[LIdx]);
  end;
end;

class function TPBKDF_ScryptNotBuildInAdapter.IsPowerOf2(Ax: Int32): Boolean;
begin
  result := (Ax > 0) and ((Ax and (Ax - 1)) = 0);
end;

class function TPBKDF_ScryptNotBuildInAdapter.SingleIterationPBKDF2
  (const APasswordBytes, ASaltBytes: THashLibByteArray; AOutputLength: Int32)
  : THashLibByteArray;
begin
  result := (TPBKDF2_HMACNotBuildInAdapter.Create(TSHA2_256.Create() as IHash,
    APasswordBytes, ASaltBytes, 1) as IPBKDF2_HMAC).GetBytes(AOutputLength);
end;

class procedure TPBKDF_ScryptNotBuildInAdapter.&Xor(const Aa,
  Ab: THashLibUInt32Array; AbOff: Int32; const AOutput: THashLibUInt32Array);
var
  LIdx: Int32;
begin
  LIdx := System.Length(AOutput) - 1;
  while LIdx >= 0 do
  begin
    AOutput[LIdx] := Aa[LIdx] xor Ab[AbOff + LIdx];
    System.Dec(LIdx);
  end;
end;

class function TPBKDF_ScryptNotBuildInAdapter.Rotl(AValue: UInt32;
  ADistance: Int32): UInt32;
begin
  result := TBits.RotateLeft32(AValue, ADistance);
end;

class procedure TPBKDF_ScryptNotBuildInAdapter.SalsaCore(ARounds: Int32;
  const AInput, Ax: THashLibUInt32Array);
var
  x00, x01, x02, x03, x04, x05, x06, x07, x08, x09, x10, x11, x12, x13, x14,
    x15: UInt32;
  LIdx: Int32;
begin
  if (System.Length(AInput) <> 16) then
  begin
    raise EArgumentHashLibException.Create('');
  end;
  if (System.Length(Ax) <> 16) then
  begin
    raise EArgumentHashLibException.Create('');
  end;
  if ((ARounds mod 2) <> 0) then
  begin
    raise EArgumentHashLibException.CreateRes(@SRoundsMustBeEven);
  end;

  x00 := AInput[0];
  x01 := AInput[1];
  x02 := AInput[2];
  x03 := AInput[3];
  x04 := AInput[4];
  x05 := AInput[5];
  x06 := AInput[6];
  x07 := AInput[7];
  x08 := AInput[8];
  x09 := AInput[9];
  x10 := AInput[10];
  x11 := AInput[11];
  x12 := AInput[12];
  x13 := AInput[13];
  x14 := AInput[14];
  x15 := AInput[15];

  LIdx := ARounds;
  while LIdx > 0 do
  begin

    x04 := x04 xor (Rotl((x00 + x12), 7));
    x08 := x08 xor (Rotl((x04 + x00), 9));
    x12 := x12 xor (Rotl((x08 + x04), 13));
    x00 := x00 xor (Rotl((x12 + x08), 18));
    x09 := x09 xor (Rotl((x05 + x01), 7));
    x13 := x13 xor (Rotl((x09 + x05), 9));
    x01 := x01 xor (Rotl((x13 + x09), 13));
    x05 := x05 xor (Rotl((x01 + x13), 18));
    x14 := x14 xor (Rotl((x10 + x06), 7));
    x02 := x02 xor (Rotl((x14 + x10), 9));
    x06 := x06 xor (Rotl((x02 + x14), 13));
    x10 := x10 xor (Rotl((x06 + x02), 18));
    x03 := x03 xor (Rotl((x15 + x11), 7));
    x07 := x07 xor (Rotl((x03 + x15), 9));
    x11 := x11 xor (Rotl((x07 + x03), 13));
    x15 := x15 xor (Rotl((x11 + x07), 18));

    x01 := x01 xor (Rotl((x00 + x03), 7));
    x02 := x02 xor (Rotl((x01 + x00), 9));
    x03 := x03 xor (Rotl((x02 + x01), 13));
    x00 := x00 xor (Rotl((x03 + x02), 18));
    x06 := x06 xor (Rotl((x05 + x04), 7));
    x07 := x07 xor (Rotl((x06 + x05), 9));
    x04 := x04 xor (Rotl((x07 + x06), 13));
    x05 := x05 xor (Rotl((x04 + x07), 18));
    x11 := x11 xor (Rotl((x10 + x09), 7));
    x08 := x08 xor (Rotl((x11 + x10), 9));
    x09 := x09 xor (Rotl((x08 + x11), 13));
    x10 := x10 xor (Rotl((x09 + x08), 18));
    x12 := x12 xor (Rotl((x15 + x14), 7));
    x13 := x13 xor (Rotl((x12 + x15), 9));
    x14 := x14 xor (Rotl((x13 + x12), 13));
    x15 := x15 xor (Rotl((x14 + x13), 18));

    System.Dec(LIdx, 2);
  end;

  Ax[0] := x00 + AInput[0];
  Ax[1] := x01 + AInput[1];
  Ax[2] := x02 + AInput[2];
  Ax[3] := x03 + AInput[3];
  Ax[4] := x04 + AInput[4];
  Ax[5] := x05 + AInput[5];
  Ax[6] := x06 + AInput[6];
  Ax[7] := x07 + AInput[7];
  Ax[8] := x08 + AInput[8];
  Ax[9] := x09 + AInput[9];
  Ax[10] := x10 + AInput[10];
  Ax[11] := x11 + AInput[11];
  Ax[12] := x12 + AInput[12];
  Ax[13] := x13 + AInput[13];
  Ax[14] := x14 + AInput[14];
  Ax[15] := x15 + AInput[15];

end;

class procedure TPBKDF_ScryptNotBuildInAdapter.BlockMix(const Ab, AX1, AX2,
  Ay: THashLibUInt32Array; AR: Int32);
var
  LbOff, LYOff, LHalfLen, LIdx: Int32;
begin
  System.Move(Ab[System.Length(Ab) - 16], AX1[0], 16 * System.SizeOf(UInt32));

  LbOff := 0;
  LYOff := 0;
  LHalfLen := System.Length(Ab) div 2;

  LIdx := 2 * AR;

  while LIdx > 0 do
  begin
    &Xor(AX1, Ab, LbOff, AX2);

    SalsaCore(8, AX2, AX1);
    System.Move(AX1[0], Ay[LYOff], 16 * System.SizeOf(UInt32));

    LYOff := LHalfLen + LbOff - LYOff;
    LbOff := LbOff + 16;
    System.Dec(LIdx);
  end;
end;

class procedure TPBKDF_ScryptNotBuildInAdapter.SMix(AIdx: Int32;
  APtrDataContainer: PDataContainer);
var
  LBCount, LIdx, LJdx, LOffset, LBlockSize, LCost: Int32;
  LMask: UInt32;
  LBlockX1, LBlockX2, LBlockY, LX, LV: THashLibUInt32Array;
  LPtrB: PCardinal;
begin
  LPtrB := APtrDataContainer^.PtrB;
  LCost := APtrDataContainer^.Cost;
  LBlockSize := APtrDataContainer^.BlockSize;
  AIdx := AIdx * 32 * LBlockSize;
  LBCount := LBlockSize * 32;
  System.SetLength(LBlockX1, 16);
  System.SetLength(LBlockX2, 16);
  System.SetLength(LBlockY, LBCount);

  System.SetLength(LX, LBCount);

  System.SetLength(LV, LCost * LBCount);

  try
    System.Move(LPtrB[AIdx], LX[0], LBCount * System.SizeOf(UInt32));

    LOffset := 0;
    LIdx := 0;
    while LIdx < LCost do
    begin
      System.Move(LX[0], LV[LOffset], LBCount * System.SizeOf(UInt32));
      LOffset := LOffset + LBCount;
      BlockMix(LX, LBlockX1, LBlockX2, LBlockY, LBlockSize);
      System.Move(LBlockY[0], LV[LOffset], LBCount * System.SizeOf(UInt32));
      LOffset := LOffset + LBCount;
      BlockMix(LBlockY, LBlockX1, LBlockX2, LX, LBlockSize);
      System.Inc(LIdx, 2);
    end;

    LMask := UInt32(LCost) - 1;

    LIdx := 0;
    while LIdx < LCost do
    begin
      LJdx := Int32(LX[LBCount - 16] and LMask);
      System.Move(LV[LJdx * LBCount], LBlockY[0],
        LBCount * System.SizeOf(UInt32));
      &Xor(LBlockY, LX, 0, LBlockY);
      BlockMix(LBlockY, LBlockX1, LBlockX2, LX, LBlockSize);
      System.Inc(LIdx);
    end;

    System.Move(LX[0], LPtrB[AIdx], LBCount * System.SizeOf(UInt32));
  finally
    ClearArray(LV);
    ClearAllArrays(THashLibMatrixUInt32Array.Create(LX, LBlockX1, LBlockX2,
      LBlockY));
  end;
end;

{$IFDEF USE_PASMP}

class procedure TPBKDF_ScryptNotBuildInAdapter.PasMPSMixWrapper
  (const AJob: PPasMPJob; const AThreadIndex: LongInt;
  const ADataContainer: Pointer; const AFromIndex, AToIndex: TPasMPNativeInt);
begin
  SMix(AFromIndex, ADataContainer);
end;
{$ENDIF}
{$IFDEF USE_MTPROCS}

class procedure TPBKDF_ScryptNotBuildInAdapter.MTProcsSMixWrapper(AIdx: PtrInt;
  ADataContainer: Pointer; AItem: TMultiThreadProcItem);
begin
  SMix(AIdx, ADataContainer);
end;
{$ENDIF}
{$IF DEFINED(USE_DELPHI_PPL)}

class procedure TPBKDF_ScryptNotBuildInAdapter.DoParallelSMix
  (ADataContainer: PDataContainer);

  function CreateTask(AIdx: Int32; ADataContainer: PDataContainer): ITask;
  begin
    result := TTask.Create(
      procedure()
      begin
        SMix(AIdx, ADataContainer);
      end);
  end;

var
  LIdx, LParallelism: Int32;
  LArrayTasks: array of ITask;
begin
  LParallelism := ADataContainer^.Parallelism;
  System.SetLength(LArrayTasks, LParallelism);
  for LIdx := 0 to System.Pred(LParallelism) do
  begin
    LArrayTasks[LIdx] := CreateTask(LIdx, ADataContainer);
    LArrayTasks[LIdx].Start;
  end;
  TTask.WaitForAll(LArrayTasks);
end;

{$ELSEIF DEFINED(USE_PASMP) OR DEFINED(USE_MTPROCS)}

class procedure TPBKDF_ScryptNotBuildInAdapter.DoParallelSMix
  (ADataContainer: PDataContainer);
var
  LParallelism: Int32;
begin
  LParallelism := ADataContainer^.Parallelism;
{$IF DEFINED(USE_PASMP)}
  TPasMP.CreateGlobalInstance;
  GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(ADataContainer, 0,
    LParallelism - 1, PasMPSMixWrapper));
{$ELSEIF DEFINED(USE_MTPROCS)}
  ProcThreadPool.DoParallel(MTProcsSMixWrapper, 0, LParallelism - 1,
    ADataContainer);
{$ELSE}
{$MESSAGE ERROR 'Unsupported Threading Library.'}
{$IFEND USE_PASMP}
end;

{$ELSE}

class procedure TPBKDF_ScryptNotBuildInAdapter.DoParallelSMix
  (ADataContainer: PDataContainer);
var
  LIdx, LParallelism: Int32;
begin
  LParallelism := ADataContainer^.Parallelism;
  for LIdx := 0 to System.Pred(LParallelism) do
  begin
    SMix(LIdx, ADataContainer);
  end;
end;

{$IFEND USE_DELPHI_PPL}

class function TPBKDF_ScryptNotBuildInAdapter.MFCrypt(const APasswordBytes,
  ASaltBytes: THashLibByteArray; ACost, ABlockSize, AParallelism,
  AOutputLength: Int32): THashLibByteArray;
var
  LMFLenBytes, LBLen: Int32;
  LBytes: THashLibByteArray;
  Lb: THashLibUInt32Array;
  LPtrDataContainer: PDataContainer;
begin
  LMFLenBytes := ABlockSize * 128;
  LBytes := SingleIterationPBKDF2(APasswordBytes, ASaltBytes,
    AParallelism * LMFLenBytes);

  try
    LBLen := System.Length(LBytes) div 4;
    System.SetLength(Lb, LBLen);

    TConverters.le32_copy(PByte(LBytes), 0, PCardinal(Lb), 0,
      System.Length(LBytes) * System.SizeOf(Byte));

    LPtrDataContainer := New(PDataContainer);
    try
      LPtrDataContainer^.PtrB := PCardinal(Lb);
      LPtrDataContainer^.Parallelism := AParallelism;
      LPtrDataContainer^.Cost := ACost;
      LPtrDataContainer^.BlockSize := ABlockSize;
      DoParallelSMix(LPtrDataContainer);
    finally
      Dispose(LPtrDataContainer);
    end;

    TConverters.le32_copy(PCardinal(Lb), 0, PByte(LBytes), 0,
      System.Length(Lb) * System.SizeOf(UInt32));

    result := SingleIterationPBKDF2(APasswordBytes, LBytes, AOutputLength);
  finally
    ClearArray(Lb);
    ClearArray(LBytes);
  end;

end;

class procedure TPBKDF_ScryptNotBuildInAdapter.ValidatePBKDF_ScryptInputs(ACost,
  ABlockSize, AParallelism: Int32);
var
  LMaxParallel: Int32;
begin

  if ((ACost <= 1) or (not IsPowerOf2(ACost))) then
  begin
    raise EArgumentHashLibException.CreateRes(@SInvalidCost);
  end;

  // Only value of ABlockSize that cost (as an int) could be exceeded for is 1
  if ((ABlockSize = 1) and (ACost >= 65536)) then
  begin
    raise EArgumentHashLibException.CreateRes(@SBlockSizeAndCostIncompatible);
  end;

  if (ABlockSize < 1) then
  begin
    raise EArgumentHashLibException.CreateRes(@SBlockSizeTooSmall);
  end;

  LMaxParallel := System.High(Int32) div (128 * ABlockSize * 8);

  if ((AParallelism < 1) or (AParallelism > LMaxParallel)) then
  begin
    raise EArgumentHashLibException.CreateResFmt(@SInvalidParallelism,
      [LMaxParallel, ABlockSize]);
  end;
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

function TPBKDF_ScryptNotBuildInAdapter.GetBytes(AByteCount: Int32)
  : THashLibByteArray;
begin
  if (AByteCount <= 0) then
  begin
    raise EArgumentHashLibException.CreateRes(@SInvalidByteCount);
  end;

  result := MFCrypt(FPasswordBytes, FSaltBytes, FCost, FBlockSize, FParallelism,
    AByteCount);
end;

end.
