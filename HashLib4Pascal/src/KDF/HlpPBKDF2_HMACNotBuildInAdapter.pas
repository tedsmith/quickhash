unit HlpPBKDF2_HMACNotBuildInAdapter;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpIHash,
  HlpKDF,
  HlpIHashInfo,
  HlpHMACNotBuildInAdapter,
  HlpBitConverter,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SInvalidByteCount =
    '"bc (ByteCount)" Argument must be a value greater than zero.';
  SInvalidIndex = 'Invalid start or end index in the internal buffer';
  SNotInitializedIHashInstance = '"IHash" instance is uninitialized';
  SIterationtooSmall = 'Iteration must be greater than zero.';

type

  TPBKDF2_HMACNotBuildInAdapter = class sealed(TKDF, IPBKDF2_HMAC,
    IPBKDF2_HMACNotBuildIn)

  strict private
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
    /// <param name="i">The integer to encode.</param>
    /// <returns>array of bytes, in big endian.</returns>
    class function GetBigEndianBytes(i: UInt32): THashLibByteArray;
      static; inline;

    class procedure ValidatePBKDF2_HMACInputs(const a_hash: IHash;
      a_iterations: UInt32); static;
  public

    constructor Create(const a_underlyingHash: IHash;
      const a_password, a_salt: THashLibByteArray; a_iterations: UInt32);

    destructor Destroy; override;

    procedure Clear(); override;

    /// <summary>
    /// Returns the pseudo-random bytes for this object.
    /// </summary>
    /// <param name="bc">The number of pseudo-random key bytes to generate.</param>
    /// <returns>A byte array filled with pseudo-random key bytes.</returns>
    /// <exception cref="EArgumentOutOfRangeHashLibException">bc must be greater than zero.</exception>
    /// <exception cref="EArgumentHashLibException">invalid start index or end index of internal buffer.</exception>
    function GetBytes(bc: Int32): THashLibByteArray; override;

  end;

implementation

{ TPBKDF2_HMACNotBuildInAdapter }

class procedure TPBKDF2_HMACNotBuildInAdapter.ValidatePBKDF2_HMACInputs
  (const a_hash: IHash; a_iterations: UInt32);
begin
  if not(System.Assigned(a_hash)) then
    raise EArgumentNilHashLibException.CreateRes(@SNotInitializedIHashInstance);

  if (a_iterations < 1) then
    raise EArgumentHashLibException.CreateRes(@SIterationtooSmall);
end;

procedure TPBKDF2_HMACNotBuildInAdapter.Clear();
begin
  TArrayUtils.ZeroFill(FPassword);
  TArrayUtils.ZeroFill(FSalt);
end;

constructor TPBKDF2_HMACNotBuildInAdapter.Create(const a_underlyingHash: IHash;
  const a_password, a_salt: THashLibByteArray; a_iterations: UInt32);
begin
  Inherited Create();
  ValidatePBKDF2_HMACInputs(a_underlyingHash, a_iterations);
  FHash := a_underlyingHash;
  FPassword := System.Copy(a_password);
  FSalt := System.Copy(a_salt);
  FIterationCount := a_iterations;
  Initialize();
end;

destructor TPBKDF2_HMACNotBuildInAdapter.Destroy;
begin
  Clear();
  inherited Destroy;
end;

class function TPBKDF2_HMACNotBuildInAdapter.GetBigEndianBytes(i: UInt32)
  : THashLibByteArray;
var
  b, invertedBytes: THashLibByteArray;
begin
  b := TBitConverter.GetBytes(i);
  invertedBytes := THashLibByteArray.Create(b[3], b[2], b[1], b[0]);
  if TBitConverter.IsLittleEndian then
    result := invertedBytes
  else
    result := b;
end;

function TPBKDF2_HMACNotBuildInAdapter.Func: THashLibByteArray;
var
  INT_block, temp, ret: THashLibByteArray;
  i: UInt32;
  j: Int32;
begin

  INT_block := TPBKDF2_HMACNotBuildInAdapter.GetBigEndianBytes(FBlock);
  FHMAC.Initialize();

  FHMAC.TransformBytes(FSalt, 0, System.Length(FSalt));
  FHMAC.TransformBytes(INT_block, 0, System.Length(INT_block));

  temp := FHMAC.TransformFinal().GetBytes();

  ret := temp;

  i := 2;
  while i <= FIterationCount do
  begin
    temp := FHMAC.ComputeBytes(temp).GetBytes();
    j := 0;
    while j < FBlockSize do
    begin
      ret[j] := ret[j] xor temp[j];
      System.Inc(j);
    end;
    System.Inc(i);
  end;
  System.Inc(FBlock);
  result := ret;
end;

function TPBKDF2_HMACNotBuildInAdapter.GetBytes(bc: Int32): THashLibByteArray;
var
  LKey, LT_block: THashLibByteArray;
  LOffset, LSize, LRemainder, LRemCount: Int32;
begin

  if (bc <= 0) then
    raise EArgumentHashLibException.CreateRes(@SInvalidByteCount);

  System.SetLength(LKey, bc);

  LOffset := 0;
  LSize := FEndIndex - FStartIndex;
  if (LSize > 0) then
  begin
    if (bc >= LSize) then
    begin
      System.Move(FBuffer[FStartIndex], LKey[0], LSize);
      FStartIndex := 0;
      FEndIndex := 0;
      LOffset := LOffset + LSize;
    end
    else
    begin
      System.Move(FBuffer[FStartIndex], LKey[0], bc);
      FStartIndex := FStartIndex + bc;
      result := LKey;
      Exit;
    end;
  end;

  if ((FStartIndex <> 0) and (FEndIndex <> 0)) then
    raise EArgumentHashLibException.CreateRes(@SInvalidIndex);

  while (LOffset < bc) do
  begin
    LT_block := Func();
    LRemainder := bc - LOffset;
    if (LRemainder > FBlockSize) then
    begin
      System.Move(LT_block[0], LKey[LOffset], FBlockSize);
      LOffset := LOffset + FBlockSize;
    end
    else
    begin
      if LRemainder > 0 then
      begin
        System.Move(LT_block[0], LKey[LOffset], LRemainder);
      end;
      LRemCount := FBlockSize - LRemainder;
      if LRemCount > 0 then
      begin
        System.Move(LT_block[LRemainder], FBuffer[FStartIndex], LRemCount);
      end;
      FEndIndex := FEndIndex + LRemCount;
      result := LKey;
      Exit;
    end;
  end;
  result := LKey;

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
