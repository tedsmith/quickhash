unit HlpPBKDF2_HMACNotBuildInAdapter;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpIHash,
  HlpKDF,
  HlpIHashInfo,
  HlpHMACNotBuildInAdapter,
  HlpConverters,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SInvalidByteCount =
    '"(AByteCount)" Argument must be a value greater than zero.';
  SInvalidIndex = 'Invalid start or end index in the internal buffer';
  SNotInitializedIHashInstance = '"IHash" instance is uninitialized';
  SIterationtooSmall = 'Iteration must be greater than zero.';

type

  TPBKDF2_HMACNotBuildInAdapter = class sealed(TKDF, IPBKDF2_HMAC,
    IPBKDF2_HMACNotBuildIn)

  strict private
  var
    FHash: IHash;
    FHMAC: IHMAC;
    FPassword, FSalt, FBuffer: THashLibByteArray;
    FIterationCount, FBlock: UInt32;
    FBlockSize, FStartIndex, FEndIndex: Int32;

    // initializes the state of the operation.
    procedure Initialize();

    // iterative hash function
    function Func(): THashLibByteArray;

    /// <summary>
    /// Encodes an integer into a 4-byte array, in big endian.
    /// </summary>
    /// <param name="AInput">The integer to encode.</param>
    /// <returns>array of bytes, in big endian.</returns>
    class function GetBigEndianBytes(AInput: UInt32): THashLibByteArray;
      static; inline;

    class procedure ValidatePBKDF2_HMACInputs(const AHash: IHash;
      AIterations: UInt32); static;
  public

    constructor Create(const AUnderlyingHash: IHash;
      const APassword, ASalt: THashLibByteArray; AIterations: UInt32);

    destructor Destroy; override;

    procedure Clear(); override;

    /// <summary>
    /// Returns the pseudo-random bytes for this object.
    /// </summary>
    /// <param name="AByteCount">The number of pseudo-random key bytes to generate.</param>
    /// <returns>A byte array filled with pseudo-random key bytes.</returns>
    /// <exception cref="EArgumentOutOfRangeHashLibException">AByteCount must be greater than zero.</exception>
    /// <exception cref="EArgumentHashLibException">invalid start index or end index of internal buffer.</exception>
    function GetBytes(AByteCount: Int32): THashLibByteArray; override;

  end;

implementation

{ TPBKDF2_HMACNotBuildInAdapter }

class procedure TPBKDF2_HMACNotBuildInAdapter.ValidatePBKDF2_HMACInputs
  (const AHash: IHash; AIterations: UInt32);
begin
  if not(System.Assigned(AHash)) then
    raise EArgumentNilHashLibException.CreateRes(@SNotInitializedIHashInstance);

  if (AIterations < 1) then
    raise EArgumentHashLibException.CreateRes(@SIterationtooSmall);
end;

procedure TPBKDF2_HMACNotBuildInAdapter.Clear();
begin
  TArrayUtils.ZeroFill(FPassword);
  TArrayUtils.ZeroFill(FSalt);
end;

constructor TPBKDF2_HMACNotBuildInAdapter.Create(const AUnderlyingHash: IHash;
  const APassword, ASalt: THashLibByteArray; AIterations: UInt32);
begin
  Inherited Create();
  ValidatePBKDF2_HMACInputs(AUnderlyingHash, AIterations);
  FHash := AUnderlyingHash;
  FPassword := System.Copy(APassword);
  FSalt := System.Copy(ASalt);
  FIterationCount := AIterations;
  Initialize();
end;

destructor TPBKDF2_HMACNotBuildInAdapter.Destroy;
begin
  Clear();
  inherited Destroy;
end;

class function TPBKDF2_HMACNotBuildInAdapter.GetBigEndianBytes(AInput: UInt32)
  : THashLibByteArray;
begin
  System.SetLength(Result, System.SizeOf(UInt32));
  TConverters.ReadUInt32AsBytesBE(AInput, Result, 0);
end;

function TPBKDF2_HMACNotBuildInAdapter.Func: THashLibByteArray;
var
  LINT_Block, LTemp: THashLibByteArray;
  LIdx: UInt32;
  LJdx: Int32;
begin

  LINT_Block := TPBKDF2_HMACNotBuildInAdapter.GetBigEndianBytes(FBlock);
  FHMAC.Initialize();

  FHMAC.TransformBytes(FSalt, 0, System.Length(FSalt));
  FHMAC.TransformBytes(LINT_Block, 0, System.Length(LINT_Block));

  LTemp := FHMAC.TransformFinal().GetBytes();

  Result := System.Copy(LTemp);

  LIdx := 2;
  while LIdx <= FIterationCount do
  begin
    LTemp := FHMAC.ComputeBytes(LTemp).GetBytes();
    LJdx := 0;
    while LJdx < FBlockSize do
    begin
      Result[LJdx] := Result[LJdx] xor LTemp[LJdx];
      System.Inc(LJdx);
    end;
    System.Inc(LIdx);
  end;
  System.Inc(FBlock);
end;

function TPBKDF2_HMACNotBuildInAdapter.GetBytes(AByteCount: Int32)
  : THashLibByteArray;
var
  LKey, LT_Block: THashLibByteArray;
  LOffset, LSize, LRemainder, LRemCount: Int32;
begin

  if (AByteCount <= 0) then
  begin
    raise EArgumentHashLibException.CreateRes(@SInvalidByteCount);
  end;

  System.SetLength(LKey, AByteCount);

  LOffset := 0;
  LSize := FEndIndex - FStartIndex;
  if (LSize > 0) then
  begin
    if (AByteCount >= LSize) then
    begin
      System.Move(FBuffer[FStartIndex], LKey[0], LSize);
      FStartIndex := 0;
      FEndIndex := 0;
      LOffset := LOffset + LSize;
    end
    else
    begin
      System.Move(FBuffer[FStartIndex], LKey[0], AByteCount);
      FStartIndex := FStartIndex + AByteCount;
      Result := LKey;
      Exit;
    end;
  end;

  if ((FStartIndex <> 0) and (FEndIndex <> 0)) then
  begin
    raise EArgumentHashLibException.CreateRes(@SInvalidIndex);
  end;

  while (LOffset < AByteCount) do
  begin
    LT_Block := Func();
    LRemainder := AByteCount - LOffset;
    if (LRemainder > FBlockSize) then
    begin
      System.Move(LT_Block[0], LKey[LOffset], FBlockSize);
      LOffset := LOffset + FBlockSize;
    end
    else
    begin
      if (LRemainder > 0) then
      begin
        System.Move(LT_Block[0], LKey[LOffset], LRemainder);
      end;
      LRemCount := FBlockSize - LRemainder;
      if LRemCount > 0 then
      begin
        System.Move(LT_Block[LRemainder], FBuffer[FStartIndex], LRemCount);
      end;
      FEndIndex := FEndIndex + LRemCount;
      Result := LKey;
      Exit;
    end;
  end;
  Result := LKey;
end;

procedure TPBKDF2_HMACNotBuildInAdapter.Initialize;
begin
  TArrayUtils.ZeroFill(FBuffer);

  FHMAC := THMACNotBuildInAdapter.CreateHMAC(FHash, FPassword);

  FBlockSize := FHMAC.HashSize;
  System.SetLength(FBuffer, FBlockSize);
  FBlock := 1;
  FStartIndex := 0;
  FEndIndex := 0;
end;

end.
