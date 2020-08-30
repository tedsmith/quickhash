unit HlpPBKDF_Argon2NotBuildInAdapter;

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
  HlpKDF,
  HlpBits,
  HlpIHash,
  HlpIHashInfo,
  HlpBlake2B,
  HlpIBlake2BParams,
  HlpBlake2BParams,
  HlpConverters,
  HlpArgon2TypeAndVersion,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SInvalidOutputByteCount = '"(AByteCount)" Argument Less Than "%d".';
  SBlockInstanceNotInitialized = 'Block Instance not Initialized';
  SInputLengthInvalid = 'Input Length "%d" is not Equal to BlockSize "%d"';
  SLanesTooSmall = 'Lanes Must be Greater Than "%d"';
  SLanesTooBig = 'Lanes Must be Less Than "%d"';
  SMemoryTooSmall = 'Memory is Less Than: "%d", Expected "%d"';
  SIterationsTooSmall = 'Iterations is Less Than: "%d"';
  SArgon2ParameterBuilderNotInitialized =
    'Argon2 Parameter Builder Not Initialized';

type
  TArgon2ParametersBuilder = class abstract(TInterfacedObject,
    IArgon2ParametersBuilder)

  strict private

  const
    DEFAULT_ITERATIONS = Int32(3);
    DEFAULT_MEMORY_COST = Int32(12);
    DEFAULT_LANES = Int32(1);
    DEFAULT_TYPE: TArgon2Type = TArgon2Type.a2tARGON2_i;
    DEFAULT_VERSION: TArgon2Version = TArgon2Version.a2vARGON2_VERSION_13;

  var
    FSalt, FSecret, FAdditional: THashLibByteArray;
    FIterations, FMemory, FLanes: Int32;
    FType: TArgon2Type;
    FVersion: TArgon2Version;

  type
    TArgon2Parameters = class sealed(TInterfacedObject, IArgon2Parameters)

    strict private
      FSalt, FSecret, FAdditional: THashLibByteArray;
      FIterations, FMemory, FLanes: Int32;
      FType: TArgon2Type;
      FVersion: TArgon2Version;

      function GetSalt(): THashLibByteArray; inline;
      function GetSecret(): THashLibByteArray; inline;
      function GetAdditional(): THashLibByteArray; inline;
      function GetIterations(): Int32; inline;
      function GetMemory(): Int32; inline;
      function GetLanes(): Int32; inline;
      function GetType(): TArgon2Type; inline;
      function GetVersion(): TArgon2Version; inline;

    public
      constructor Create(AType: TArgon2Type;
        const ASalt, ASecret, AAdditional: THashLibByteArray;
        AIterations, AMemory, ALanes: Int32; AVersion: TArgon2Version);

      procedure Clear(); inline;

      property Salt: THashLibByteArray read GetSalt;
      property Secret: THashLibByteArray read GetSecret;
      property Additional: THashLibByteArray read GetAdditional;
      property Iterations: Int32 read GetIterations;
      property Memory: Int32 read GetMemory;
      property Lanes: Int32 read GetLanes;
      property &Type: TArgon2Type read GetType;
      property Version: TArgon2Version read GetVersion;
    end;

  strict protected

    constructor Create(AType: TArgon2Type);

  public

    destructor Destroy(); override;

    function WithParallelism(AParallelism: Int32)
      : IArgon2ParametersBuilder; virtual;

    function WithSalt(const ASalt: THashLibByteArray)
      : IArgon2ParametersBuilder; virtual;

    function WithSecret(const ASecret: THashLibByteArray)
      : IArgon2ParametersBuilder; virtual;

    function WithAdditional(const AAdditional: THashLibByteArray)
      : IArgon2ParametersBuilder; virtual;

    function WithIterations(AIterations: Int32)
      : IArgon2ParametersBuilder; virtual;

    function WithMemoryAsKB(AMemory: Int32): IArgon2ParametersBuilder; virtual;

    function WithMemoryPowOfTwo(AMemory: Int32)
      : IArgon2ParametersBuilder; virtual;

    function WithVersion(AVersion: TArgon2Version)
      : IArgon2ParametersBuilder; virtual;

    procedure Clear(); virtual;

    function Build(): IArgon2Parameters; virtual;
  end;

type
  TArgon2iParametersBuilder = class sealed(TArgon2ParametersBuilder)

  strict private
    constructor Create();

  public
    class function Builder(): IArgon2ParametersBuilder; static; inline;

  end;

type
  TArgon2dParametersBuilder = class sealed(TArgon2ParametersBuilder)

  strict private
    constructor Create();

  public
    class function Builder(): IArgon2ParametersBuilder; static; inline;

  end;

type
  TArgon2idParametersBuilder = class sealed(TArgon2ParametersBuilder)

  strict private
    constructor Create();

  public
    class function Builder(): IArgon2ParametersBuilder; static; inline;

  end;

type

  /// <summary>
  /// Argon2 PBKDF - Based on the results of https://password-hashing.net/
  /// and https://www.ietf.org/archive/id/draft-irtf-cfrg-argon2-03.txt
  /// </summary>
  TPBKDF_Argon2NotBuildInAdapter = class sealed(TKDF, IPBKDF_Argon2,
    IPBKDF_Argon2NotBuildIn)

  strict private

  const

    ARGON2_BLOCK_SIZE = Int32(1024);
    ARGON2_QWORDS_IN_BLOCK = Int32(ARGON2_BLOCK_SIZE div 8);

    ARGON2_ADDRESSES_IN_BLOCK = Int32(128);

    ARGON2_PREHASH_DIGEST_LENGTH = Int32(64);
    ARGON2_PREHASH_SEED_LENGTH = Int32(72);

    ARGON2_SYNC_POINTS = Int32(4);

    // Minimum and maximum number of lanes (degree of parallelism)
    MIN_PARALLELISM = Int32(1);
    MAX_PARALLELISM = Int32(16777216);

    // Minimum digest size in bytes
    MIN_OUTLEN = Int32(4);

    // Minimum and maximum number of passes
    MIN_ITERATIONS = Int32(1);

  type
    TBlock = record

    private

      const
      SIZE = Int32(ARGON2_QWORDS_IN_BLOCK);

    var
      // 128 * 8 Byte QWords
      Fv: THashLibUInt64Array;
      FInitialized: Boolean;

      procedure CheckAreBlocksInitialized(const ABlocks
        : THashLibGenericArray<TBlock>);
      procedure CopyBlock(const AOther: TBlock); inline;
      procedure &Xor(const AB1, AB2: TBlock); overload;
      procedure XorWith(const AOther: TBlock);

    public
      class function CreateBlock(): TBlock; static;

      function Clear(): TBlock;
      procedure &Xor(const AB1, AB2, AB3: TBlock); overload;
      procedure FromBytes(const AInput: THashLibByteArray);

      function ToBytes(): THashLibByteArray;
      function ToString(): String;

    end;

  type
    TPosition = record

    private
    var
      FPass, FLane, FSlice, FIndex: Int32;

    public

      class function CreatePosition(): TPosition; static;

      procedure Update(APass, ALane, ASlice, AIndex: Int32);

    end;

  type
    TFillBlock = record

    private
    var
      FR, FZ, FAddressBlock, FZeroBlock, FInputBlock: TBlock;

      function GetR: TBlock; inline;
      function GetZ: TBlock; inline;
      function GetAddressBlock: TBlock; inline;
      function GetZeroBlock: TBlock; inline;
      function GetInputBlock: TBlock; inline;

      procedure ApplyBlake();

      procedure FillBlock(var Ax, Ay, ACurrentBlock: TBlock; AWithXor: Boolean);

      {
        *designed by the Lyra PHC team */
        /* a <- a + b + 2*aL*bL
        * + == addition modulo 2^64
        * aL = least 32 bit
        * }
      class procedure FBlaMka(var ABlock: TBlock; Ax, Ay: Int32);
        static; inline;
      class procedure Rotr64(var ABlock: TBlock; Av, Aw, Ac: Int32);
        static; inline;
      class procedure F(var ABlock: TBlock; Aa, Ab, Ac, Ad: Int32);
        static; inline;
      class procedure RoundFunction(var ABlock: TBlock;
        Av0, Av1, Av2, Av3, Av4, Av5, Av6, Av7, Av8, Av9, Av10, Av11, Av12,
        Av13, Av14, Av15: Int32); static; inline;

    public
      property R: TBlock read GetR;
      property Z: TBlock read GetZ;
      property AddressBlock: TBlock read GetAddressBlock;
      property ZeroBlock: TBlock read GetZeroBlock;
      property InputBlock: TBlock read GetInputBlock;

      class function CreateFillBlock(): TFillBlock; static;
    end;

  type
    PDataContainer = ^TDataContainer;

    TDataContainer = record
      Position: TPosition;
    end;

  var

    FMemory: THashLibGenericArray<TBlock>;
    FSegmentLength, FLaneLength: Int32;
    FParameters: IArgon2Parameters;
    FPassword, FResult: THashLibByteArray;

    class procedure AddIntToLittleEndian(const AHash: IHash; An: Int32);
      static; inline;

    class procedure AddByteString(const AHash: IHash;
      const AOctets: THashLibByteArray); static; inline;

    class function MakeBlake2BInstanceAndInitialize(AHashSize: Int32): IHash;
      static; inline;

    class function GetStartingIndex(const APosition: TPosition): Int32;
      static; inline;

    procedure InitializeMemory(AMemoryBlocks: Int32);
    procedure DoInit(const AParameters: IArgon2Parameters);
    // Clear memory.
    procedure Reset();

    function IsDataIndependentAddressing(const APosition: TPosition)
      : Boolean; inline;
    procedure NextAddresses(const AFiller: TFillBlock;
      var AZeroBlock, AInputBlock, AAddressBlock: TBlock); inline;
    function IntToUInt64(Ax: Int32): UInt64; inline;
    procedure InitAddressBlocks(const AFiller: TFillBlock;
      const APosition: TPosition; var AZeroBlock, AInputBlock,
      AAddressBlock: TBlock);
    (* 1.2 Computing the index of the reference block
      1.2.1 Taking pseudo-random value from the previous block *)
    function GetPseudoRandom(const AFiller: TFillBlock;
      const APosition: TPosition; var AAddressBlock, AInputBlock,
      AZeroBlock: TBlock; APrevOffset: Int32;
      ADataIndependentAddressing: Boolean): UInt64;
    function GetRefLane(const APosition: TPosition; APseudoRandom: UInt64)
      : Int32; inline;
    function GetRefColumn(const APosition: TPosition; APseudoRandom: UInt64;
      ASameLane: Boolean): Int32;
    function IsWithXor(const APosition: TPosition): Boolean; inline;
    function GetPrevOffset(ACurrentOffset: Int32): Int32; inline;
    function RotatePrevOffset(ACurrentOffset, APrevOffset: Int32)
      : Int32; inline;
    procedure FillSegment(AIdx: Int32; var APosition: TPosition);
    procedure FillMemoryBlocks(AIdx: Int32;
      ADataContainer: PDataContainer); inline;
    procedure DoParallelFillMemoryBlocks(ADataContainer: PDataContainer);
{$IFDEF USE_PASMP}
    procedure PasMPFillMemoryBlocksWrapper(const AJob: PPasMPJob;
      const AThreadIndex: LongInt; const ADataContainer: Pointer;
      const AFromIndex, AToIndex: TPasMPNativeInt); inline;
{$ENDIF USE_PASMP}
{$IFDEF USE_MTPROCS}
    procedure MTProcsFillMemoryBlocksWrapper(AIdx: PtrInt;
      ADataContainer: Pointer; AItem: TMultiThreadProcItem); inline;
{$ENDIF USE_MTPROCS}
    (* *

      * H0 = H64(p, τ, m, t, v, y, |P|, P, |S|, S, |L|, K, |X|, X)
      * -> 64 byte (ARGON2_PREHASH_DIGEST_LENGTH)
    *)
    function InitialHash(const AParameters: IArgon2Parameters;
      AOutputLength: Int32; const APassword: THashLibByteArray)
      : THashLibByteArray; inline;

    function GetInitialHashLong(const AInitialHash,
      AAppendix: THashLibByteArray): THashLibByteArray; inline;

    // H' - hash - variable length hash function
    function Hash(const AInput: THashLibByteArray; AOutputLength: Int32)
      : THashLibByteArray;
    procedure Digest(AOutputLength: Int32);
    (* *
      * (H0 || 0 || i) 72 byte -> 1024 byte
      * (H0 || 1 || i) 72 byte -> 1024 byte
    *)
    procedure FillFirstBlocks(const AInitialHash: THashLibByteArray);
    procedure Initialize(const APassword: THashLibByteArray;
      AOutputLength: Int32); inline;

    class procedure ValidatePBKDF_Argon2Inputs(const AArgon2Parameters
      : IArgon2Parameters); static;

  public

    /// <summary>
    /// Initialise the <see cref="HlpPBKDF_Argon2NotBuildInAdapter|TPBKDF_Argon2NotBuildInAdapter" />
    /// from the password and parameters.
    /// </summary>
    /// <param name="APassword">
    /// the password to use.
    /// </param>
    /// <param name="AParameters">
    /// Argon2 configuration.
    /// </param>
    constructor Create(const APassword: THashLibByteArray;
      const AParameters: IArgon2Parameters);

    destructor Destroy; override;

    procedure Clear(); override;

    /// <summary>
    /// Returns the pseudo-random bytes for this object.
    /// </summary>
    /// <param name="AByteCount">The number of pseudo-random key bytes to generate.</param>
    /// <returns>A byte array filled with pseudo-random key bytes.</returns>
    /// /// <exception cref="EArgumentOutOfRangeHashLibException">AByteCount must be greater than MIN_OUTLEN.</exception>
    function GetBytes(AByteCount: Int32): THashLibByteArray; override;

  end;

implementation

{ TPBKDF_Argon2NotBuildInAdapter.TBlock }

class function TPBKDF_Argon2NotBuildInAdapter.TBlock.CreateBlock: TBlock;
begin
  result := Default (TBlock);
  System.SetLength(result.Fv, SIZE);
  result.FInitialized := True;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TBlock.CheckAreBlocksInitialized
  (const ABlocks: THashLibGenericArray<TBlock>);
var
  LBlock: TBlock;
begin
  for LBlock in ABlocks do
  begin
    if not(LBlock.FInitialized) then
    begin
      raise EArgumentNilHashLibException.CreateRes
        (@SBlockInstanceNotInitialized);
    end;
  end;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TBlock.CopyBlock(const AOther: TBlock);
begin
  CheckAreBlocksInitialized(THashLibGenericArray<TBlock>.Create(Self, AOther));
  Fv := System.Copy(AOther.Fv);
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TBlock.&Xor(const AB1, AB2: TBlock);
var
  LIdx: Int32;
begin
  CheckAreBlocksInitialized(THashLibGenericArray<TBlock>.Create(Self,
    AB1, AB2));
  for LIdx := 0 to System.Pred(SIZE) do
  begin
    Fv[LIdx] := AB1.Fv[LIdx] xor AB2.Fv[LIdx];
  end;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TBlock.XorWith(const AOther: TBlock);
var
  LIdx: Int32;
begin
  CheckAreBlocksInitialized(THashLibGenericArray<TBlock>.Create(Self, AOther));
  for LIdx := 0 to System.Pred(System.Length(Fv)) do
  begin
    Fv[LIdx] := Fv[LIdx] xor AOther.Fv[LIdx];
  end;
end;

function TPBKDF_Argon2NotBuildInAdapter.TBlock.Clear;
begin
  CheckAreBlocksInitialized(THashLibGenericArray<TBlock>.Create(Self));
  TArrayUtils.ZeroFill(Fv);
  result := Self;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TBlock.&Xor(const AB1, AB2,
  AB3: TBlock);
var
  LIdx: Int32;
begin
  CheckAreBlocksInitialized(THashLibGenericArray<TBlock>.Create(Self, AB1,
    AB2, AB3));
  for LIdx := 0 to System.Pred(SIZE) do
  begin
    Fv[LIdx] := AB1.Fv[LIdx] xor AB2.Fv[LIdx] xor AB3.Fv[LIdx];
  end;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TBlock.FromBytes
  (const AInput: THashLibByteArray);
var
  LIdx: Int32;
  LPtrInput: PByte;
begin
  CheckAreBlocksInitialized(THashLibGenericArray<TBlock>.Create(Self));
  if (System.Length(AInput) <> ARGON2_BLOCK_SIZE) then
  begin
    raise EArgumentHashLibException.CreateResFmt(@SInputLengthInvalid,
      [System.Length(AInput), ARGON2_BLOCK_SIZE]);
  end;
  LPtrInput := PByte(AInput);
  for LIdx := 0 to System.Pred(SIZE) do
  begin
    Fv[LIdx] := TConverters.ReadBytesAsUInt64LE(LPtrInput, LIdx * 8);
  end;
end;

function TPBKDF_Argon2NotBuildInAdapter.TBlock.ToBytes: THashLibByteArray;
var
  LIdx: Int32;
begin
  CheckAreBlocksInitialized(THashLibGenericArray<TBlock>.Create(Self));
  System.SetLength(result, ARGON2_BLOCK_SIZE);
  for LIdx := 0 to System.Pred(SIZE) do
  begin
    TConverters.ReadUInt64AsBytesLE(Fv[LIdx], result, LIdx * 8);
  end;
end;

function TPBKDF_Argon2NotBuildInAdapter.TBlock.ToString: String;
var
  LIdx: Int32;
begin
  CheckAreBlocksInitialized(THashLibGenericArray<TBlock>.Create(Self));
  result := '';
  for LIdx := 0 to System.Pred(SIZE) do
  begin
    result := result + TConverters.ConvertBytesToHexString
      (TConverters.ReadUInt64AsBytesLE(Fv[LIdx]), False);
  end;
end;

{ TPBKDF_Argon2NotBuildInAdapter.TPosition }

class function TPBKDF_Argon2NotBuildInAdapter.TPosition.CreatePosition()
  : TPosition;
begin
  result := Default (TPosition);
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TPosition.Update(APass, ALane, ASlice,
  AIndex: Int32);
begin
  FPass := APass;
  FLane := ALane;
  FSlice := ASlice;
  FIndex := AIndex;
end;

{ TPBKDF_Argon2NotBuildInAdapter.TFillBlock }

class procedure TPBKDF_Argon2NotBuildInAdapter.TFillBlock.FBlaMka
  (var ABlock: TBlock; Ax, Ay: Int32);
var
  Lm: UInt32;
  Lxy: UInt64;
begin
  Lm := $FFFFFFFF;
  Lxy := (ABlock.Fv[Ax] and Lm) * (ABlock.Fv[Ay] and Lm);

  ABlock.Fv[Ax] := ABlock.Fv[Ax] + ABlock.Fv[Ay] + (2 * Lxy);
end;

class procedure TPBKDF_Argon2NotBuildInAdapter.TFillBlock.Rotr64
  (var ABlock: TBlock; Av, Aw, Ac: Int32);
var
  LTemp: UInt64;
begin
  LTemp := ABlock.Fv[Av] xor ABlock.Fv[Aw];
  ABlock.Fv[Av] := TBits.RotateRight64(LTemp, Ac);
end;

class procedure TPBKDF_Argon2NotBuildInAdapter.TFillBlock.F(var ABlock: TBlock;
  Aa, Ab, Ac, Ad: Int32);
begin
  FBlaMka(ABlock, Aa, Ab);
  Rotr64(ABlock, Ad, Aa, 32);

  FBlaMka(ABlock, Ac, Ad);
  Rotr64(ABlock, Ab, Ac, 24);

  FBlaMka(ABlock, Aa, Ab);
  Rotr64(ABlock, Ad, Aa, 16);

  FBlaMka(ABlock, Ac, Ad);
  Rotr64(ABlock, Ab, Ac, 63);
end;

class procedure TPBKDF_Argon2NotBuildInAdapter.TFillBlock.RoundFunction
  (var ABlock: TBlock; Av0, Av1, Av2, Av3, Av4, Av5, Av6, Av7, Av8, Av9, Av10,
  Av11, Av12, Av13, Av14, Av15: Int32);
begin
  F(ABlock, Av0, Av4, Av8, Av12);
  F(ABlock, Av1, Av5, Av9, Av13);
  F(ABlock, Av2, Av6, Av10, Av14);
  F(ABlock, Av3, Av7, Av11, Av15);

  F(ABlock, Av0, Av5, Av10, Av15);
  F(ABlock, Av1, Av6, Av11, Av12);
  F(ABlock, Av2, Av7, Av8, Av13);
  F(ABlock, Av3, Av4, Av9, Av14);
end;

function TPBKDF_Argon2NotBuildInAdapter.TFillBlock.GetAddressBlock: TBlock;
begin
  result := FAddressBlock;
end;

function TPBKDF_Argon2NotBuildInAdapter.TFillBlock.GetInputBlock: TBlock;
begin
  result := FInputBlock;
end;

function TPBKDF_Argon2NotBuildInAdapter.TFillBlock.GetR: TBlock;
begin
  result := FR;
end;

function TPBKDF_Argon2NotBuildInAdapter.TFillBlock.GetZ: TBlock;
begin
  result := FZ;
end;

function TPBKDF_Argon2NotBuildInAdapter.TFillBlock.GetZeroBlock: TBlock;
begin
  result := FZeroBlock;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TFillBlock.ApplyBlake();
var
  Li, Li16, Li2: Int32;
begin
  (* Apply Blake2 on columns of 64-bit words: (0,1,...,15) , then
    (16,17,..31)... finally (112,113,...127) *)

  for Li := 0 to System.Pred(8) do
  begin
    Li16 := 16 * Li;
    RoundFunction(FZ, Li16, Li16 + 1, Li16 + 2, Li16 + 3, Li16 + 4, Li16 + 5,
      Li16 + 6, Li16 + 7, Li16 + 8, Li16 + 9, Li16 + 10, Li16 + 11, Li16 + 12,
      Li16 + 13, Li16 + 14, Li16 + 15);
  end;

  (* Apply Blake2 on rows of 64-bit words: (0,1,16,17,...112,113), then
    (2,3,18,19,...,114,115).. finally (14,15,30,31,...,126,127) *)

  for Li := 0 to System.Pred(8) do
  begin
    Li2 := 2 * Li;
    RoundFunction(FZ, Li2, Li2 + 1, Li2 + 16, Li2 + 17, Li2 + 32, Li2 + 33,
      Li2 + 48, Li2 + 49, Li2 + 64, Li2 + 65, Li2 + 80, Li2 + 81, Li2 + 96,
      Li2 + 97, Li2 + 112, Li2 + 113);
  end;
end;

class function TPBKDF_Argon2NotBuildInAdapter.TFillBlock.CreateFillBlock
  : TFillBlock;
begin
  result := Default (TFillBlock);
  result.FR := TBlock.CreateBlock();
  result.FZ := TBlock.CreateBlock();
  result.FAddressBlock := TBlock.CreateBlock();
  result.FZeroBlock := TBlock.CreateBlock();
  result.FInputBlock := TBlock.CreateBlock();
end;

procedure TPBKDF_Argon2NotBuildInAdapter.TFillBlock.FillBlock(var Ax, Ay,
  ACurrentBlock: TBlock; AWithXor: Boolean);
begin
  R.&Xor(Ax, Ay);
  FZ.CopyBlock(R);

  ApplyBlake();

  if (AWithXor) then
  begin
    ACurrentBlock.&Xor(R, Z, ACurrentBlock);
  end
  else
  begin
    ACurrentBlock.&Xor(R, Z);
  end;
end;

{ TArgon2ParametersBuilder.TArgon2Parameters }

constructor TArgon2ParametersBuilder.TArgon2Parameters.Create
  (AType: TArgon2Type; const ASalt, ASecret, AAdditional: THashLibByteArray;
  AIterations, AMemory, ALanes: Int32; AVersion: TArgon2Version);
begin
  Inherited Create();
  FSalt := System.Copy(ASalt);
  FSecret := System.Copy(ASecret);
  FAdditional := System.Copy(AAdditional);
  FIterations := AIterations;
  FMemory := AMemory;
  FLanes := ALanes;
  FType := AType;
  FVersion := AVersion;
end;

function TArgon2ParametersBuilder.TArgon2Parameters.GetSalt: THashLibByteArray;
begin
  result := System.Copy(FSalt);
end;

function TArgon2ParametersBuilder.TArgon2Parameters.GetSecret
  : THashLibByteArray;
begin
  result := System.Copy(FSecret);
end;

function TArgon2ParametersBuilder.TArgon2Parameters.GetAdditional
  : THashLibByteArray;
begin
  result := System.Copy(FAdditional);
end;

function TArgon2ParametersBuilder.TArgon2Parameters.GetIterations: Int32;
begin
  result := FIterations;
end;

function TArgon2ParametersBuilder.TArgon2Parameters.GetMemory: Int32;
begin
  result := FMemory;
end;

function TArgon2ParametersBuilder.TArgon2Parameters.GetLanes: Int32;
begin
  result := FLanes;
end;

function TArgon2ParametersBuilder.TArgon2Parameters.GetVersion: TArgon2Version;
begin
  result := FVersion;
end;

function TArgon2ParametersBuilder.TArgon2Parameters.GetType: TArgon2Type;
begin
  result := FType;
end;

procedure TArgon2ParametersBuilder.TArgon2Parameters.Clear();
begin
  TArrayUtils.ZeroFill(FSalt);
  TArrayUtils.ZeroFill(FSecret);
  TArrayUtils.ZeroFill(FAdditional);
end;

{ TArgon2ParametersBuilder }

constructor TArgon2ParametersBuilder.Create(AType: TArgon2Type);
begin
  Inherited Create();
  FLanes := DEFAULT_LANES;
  FMemory := 1 shl DEFAULT_MEMORY_COST;
  FIterations := DEFAULT_ITERATIONS;
  FType := AType;
  FVersion := DEFAULT_VERSION;
end;

destructor TArgon2ParametersBuilder.Destroy;
begin
  Clear();
  inherited Destroy;
end;

function TArgon2ParametersBuilder.WithAdditional(const AAdditional
  : THashLibByteArray): IArgon2ParametersBuilder;
begin
  FAdditional := System.Copy(AAdditional);
  result := Self as IArgon2ParametersBuilder;
end;

function TArgon2ParametersBuilder.WithIterations(AIterations: Int32)
  : IArgon2ParametersBuilder;
begin
  FIterations := AIterations;
  result := Self as IArgon2ParametersBuilder;
end;

function TArgon2ParametersBuilder.WithMemoryAsKB(AMemory: Int32)
  : IArgon2ParametersBuilder;
begin
  FMemory := AMemory;
  result := Self as IArgon2ParametersBuilder;
end;

function TArgon2ParametersBuilder.WithMemoryPowOfTwo(AMemory: Int32)
  : IArgon2ParametersBuilder;
begin
  FMemory := 1 shl AMemory;
  result := Self as IArgon2ParametersBuilder;
end;

function TArgon2ParametersBuilder.WithParallelism(AParallelism: Int32)
  : IArgon2ParametersBuilder;
begin
  FLanes := AParallelism;
  result := Self as IArgon2ParametersBuilder;
end;

function TArgon2ParametersBuilder.WithSalt(const ASalt: THashLibByteArray)
  : IArgon2ParametersBuilder;
begin
  FSalt := System.Copy(ASalt);
  result := Self as IArgon2ParametersBuilder;
end;

function TArgon2ParametersBuilder.WithSecret(const ASecret: THashLibByteArray)
  : IArgon2ParametersBuilder;
begin
  FSecret := System.Copy(ASecret);
  result := Self as IArgon2ParametersBuilder;
end;

function TArgon2ParametersBuilder.WithVersion(AVersion: TArgon2Version)
  : IArgon2ParametersBuilder;
begin
  FVersion := AVersion;
  result := Self as IArgon2ParametersBuilder;
end;

function TArgon2ParametersBuilder.Build(): IArgon2Parameters;
begin
  result := TArgon2Parameters.Create(FType, FSalt, FSecret, FAdditional,
    FIterations, FMemory, FLanes, FVersion);
end;

procedure TArgon2ParametersBuilder.Clear();
begin
  TArrayUtils.ZeroFill(FSalt);
  TArrayUtils.ZeroFill(FSecret);
  TArrayUtils.ZeroFill(FAdditional);
end;

{ TArgon2iParametersBuilder }

constructor TArgon2iParametersBuilder.Create;
begin
  Inherited Create(TArgon2Type.a2tARGON2_i);
end;

class function TArgon2iParametersBuilder.Builder: IArgon2ParametersBuilder;
begin
  result := TArgon2iParametersBuilder.Create() as IArgon2ParametersBuilder;
end;

{ TArgon2dParametersBuilder }

constructor TArgon2dParametersBuilder.Create;
begin
  Inherited Create(TArgon2Type.a2tARGON2_d);
end;

class function TArgon2dParametersBuilder.Builder: IArgon2ParametersBuilder;
begin
  result := TArgon2dParametersBuilder.Create() as IArgon2ParametersBuilder;
end;

{ TArgon2idParametersBuilder }

constructor TArgon2idParametersBuilder.Create;
begin
  Inherited Create(TArgon2Type.a2tARGON2_id);
end;

class function TArgon2idParametersBuilder.Builder: IArgon2ParametersBuilder;
begin
  result := TArgon2idParametersBuilder.Create() as IArgon2ParametersBuilder;
end;

{ TPBKDF_Argon2NotBuildInAdapter }

class procedure TPBKDF_Argon2NotBuildInAdapter.ValidatePBKDF_Argon2Inputs
  (const AArgon2Parameters: IArgon2Parameters);
begin
  if not(System.Assigned(AArgon2Parameters)) then
  begin
    raise EArgumentNilHashLibException.CreateRes
      (@SArgon2ParameterBuilderNotInitialized);
  end;
end;

class procedure TPBKDF_Argon2NotBuildInAdapter.AddIntToLittleEndian
  (const AHash: IHash; An: Int32);
begin
  AHash.TransformBytes(TConverters.ReadUInt32AsBytesLE(UInt32(An)));
end;

class procedure TPBKDF_Argon2NotBuildInAdapter.AddByteString(const AHash: IHash;
  const AOctets: THashLibByteArray);
begin
  if (AOctets <> Nil) then
  begin
    AddIntToLittleEndian(AHash, System.Length(AOctets));
    AHash.TransformBytes(AOctets, 0, System.Length(AOctets));
  end
  else
  begin
    AddIntToLittleEndian(AHash, 0);
  end;
end;

class function TPBKDF_Argon2NotBuildInAdapter.MakeBlake2BInstanceAndInitialize
  (AHashSize: Int32): IHash;
begin
  result := TBlake2B.Create(TBlake2BConfig.Create(AHashSize) as IBlake2BConfig);
  result.Initialize;
end;

class function TPBKDF_Argon2NotBuildInAdapter.GetStartingIndex(const APosition
  : TPosition): Int32;
begin
  if ((APosition.FPass = 0) and (APosition.FSlice = 0)) then
  begin
    // we have already generated the first two blocks
    result := 2;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.InitializeMemory(AMemoryBlocks: Int32);
var
  LIdx: Int32;
begin
  System.SetLength(FMemory, AMemoryBlocks);
  for LIdx := 0 to System.Pred(System.Length(FMemory)) do
  begin
    FMemory[LIdx] := TBlock.CreateBlock();
  end;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.DoInit(const AParameters
  : IArgon2Parameters);
var
  LMemoryBlocks: Int32;
begin
  // 2. Align memory size
  // Minimum memoryBlocks = 8L blocks, where L is the number of lanes */
  LMemoryBlocks := AParameters.Memory;

  if (LMemoryBlocks < (2 * ARGON2_SYNC_POINTS * AParameters.Lanes)) then
  begin
    LMemoryBlocks := 2 * ARGON2_SYNC_POINTS * AParameters.Lanes;
  end;

  FSegmentLength := LMemoryBlocks div (FParameters.Lanes * ARGON2_SYNC_POINTS);
  FLaneLength := FSegmentLength * ARGON2_SYNC_POINTS;

  // Ensure that all segments have equal length
  LMemoryBlocks := FSegmentLength * (AParameters.Lanes * ARGON2_SYNC_POINTS);

  InitializeMemory(LMemoryBlocks);
end;

procedure TPBKDF_Argon2NotBuildInAdapter.Reset;
var
  LIdx: Int32;
begin
  // Reset memory.
  for LIdx := 0 to System.Pred(System.Length(FMemory)) do
  begin
    FMemory[LIdx].Clear;
    FMemory[LIdx] := Default (TBlock);
  end;
  FMemory := Nil;
  TArrayUtils.ZeroFill(FResult);
end;

function TPBKDF_Argon2NotBuildInAdapter.InitialHash(const AParameters
  : IArgon2Parameters; AOutputLength: Int32; const APassword: THashLibByteArray)
  : THashLibByteArray;
var
  LBlake2B: IHash;
begin
  LBlake2B := MakeBlake2BInstanceAndInitialize(ARGON2_PREHASH_DIGEST_LENGTH);

  AddIntToLittleEndian(LBlake2B, AParameters.Lanes);
  AddIntToLittleEndian(LBlake2B, AOutputLength);
  AddIntToLittleEndian(LBlake2B, AParameters.Memory);
  AddIntToLittleEndian(LBlake2B, AParameters.Iterations);
  AddIntToLittleEndian(LBlake2B, Int32(AParameters.Version));
  AddIntToLittleEndian(LBlake2B, Int32(AParameters.&Type));

  AddByteString(LBlake2B, APassword);
  AddByteString(LBlake2B, AParameters.Salt);
  AddByteString(LBlake2B, AParameters.Secret);
  AddByteString(LBlake2B, AParameters.Additional);

  result := LBlake2B.TransformFinal.GetBytes();
end;

function TPBKDF_Argon2NotBuildInAdapter.GetInitialHashLong(const AInitialHash,
  AAppendix: THashLibByteArray): THashLibByteArray;
begin
  System.SetLength(result, ARGON2_PREHASH_SEED_LENGTH);
  System.Move(AInitialHash[0], result[0], ARGON2_PREHASH_DIGEST_LENGTH *
    System.SizeOf(Byte));
  System.Move(AAppendix[0], result[ARGON2_PREHASH_DIGEST_LENGTH],
    4 * System.SizeOf(Byte));
end;

function TPBKDF_Argon2NotBuildInAdapter.Hash(const AInput: THashLibByteArray;
  AOutputLength: Int32): THashLibByteArray;
var
  LOutlenBytes, LOutBuffer: THashLibByteArray;
  LBlake2BLength, Lr, LPosition, LIdx, LLastLength: Int32;
  LBlake2B: IHash;
begin
  System.SetLength(result, AOutputLength);
  LOutlenBytes := TConverters.ReadUInt32AsBytesLE(UInt32(AOutputLength));

  LBlake2BLength := 64;

  if (AOutputLength <= LBlake2BLength) then
  begin

    LBlake2B := MakeBlake2BInstanceAndInitialize(AOutputLength);

    LBlake2B.TransformBytes(LOutlenBytes, 0, System.Length(LOutlenBytes));
    LBlake2B.TransformBytes(AInput, 0, System.Length(AInput));
    result := LBlake2B.TransformFinal.GetBytes();
  end
  else
  begin

    LBlake2B := MakeBlake2BInstanceAndInitialize(LBlake2BLength);

    System.SetLength(LOutBuffer, LBlake2BLength);

    // V1
    LBlake2B.TransformBytes(LOutlenBytes, 0, System.Length(LOutlenBytes));
    LBlake2B.TransformBytes(AInput, 0, System.Length(AInput));
    LOutBuffer := LBlake2B.TransformFinal.GetBytes();

    System.Move(LOutBuffer[0], result[0], (LBlake2BLength div 2) *
      System.SizeOf(Byte));

    Lr := ((AOutputLength + 31) div 32) - 2;

    LPosition := LBlake2BLength div 2;

    LIdx := 2;

    while LIdx <= Lr do
    begin
      // V2 to Vr
      LBlake2B.TransformBytes(LOutBuffer, 0, System.Length(LOutBuffer));
      LOutBuffer := LBlake2B.TransformFinal.GetBytes();

      System.Move(LOutBuffer[0], result[LPosition], (LBlake2BLength div 2) *
        System.SizeOf(Byte));

      System.Inc(LIdx);
      LPosition := LPosition + (LBlake2BLength div 2);
    end;

    LLastLength := AOutputLength - (32 * Lr);

    // Vr+1

    LBlake2B := MakeBlake2BInstanceAndInitialize(LLastLength);

    LBlake2B.TransformBytes(LOutBuffer, 0, System.Length(LOutBuffer));
    LOutBuffer := LBlake2B.TransformFinal.GetBytes();
    System.Move(LOutBuffer[0], result[LPosition],
      LLastLength * System.SizeOf(Byte));
  end;
{$IFDEF DEBUG}
  System.Assert(System.Length(result) = AOutputLength);
{$ENDIF DEBUG}
end;

procedure TPBKDF_Argon2NotBuildInAdapter.Digest(AOutputLength: Int32);
var
  LIdx, LLastBlockInLane: Int32;
  FFinalBlockBytes: THashLibByteArray;
  FFinalBlock: TBlock;
begin
  FFinalBlock := FMemory[FLaneLength - 1];

  // XOR the last blocks
  for LIdx := 1 to System.Pred(FParameters.Lanes) do
  begin
    LLastBlockInLane := (LIdx * FLaneLength) + (FLaneLength - 1);
    FFinalBlock.XorWith(FMemory[LLastBlockInLane]);
  end;

  FFinalBlockBytes := FFinalBlock.ToBytes();

  FResult := Hash(FFinalBlockBytes, AOutputLength);
end;

procedure TPBKDF_Argon2NotBuildInAdapter.FillFirstBlocks(const AInitialHash
  : THashLibByteArray);
var
  LZeroBytes, LOneBytes, LInitialHashWithZeros, LInitialHashWithOnes,
    LBlockHashBytes: THashLibByteArray;
  LIdx: Int32;
begin

  LZeroBytes := THashLibByteArray.Create(0, 0, 0, 0);
  LOneBytes := THashLibByteArray.Create(1, 0, 0, 0);

  LInitialHashWithZeros := GetInitialHashLong(AInitialHash, LZeroBytes);
  LInitialHashWithOnes := GetInitialHashLong(AInitialHash, LOneBytes);

  for LIdx := 0 to System.Pred(FParameters.Lanes) do
  begin
    TConverters.ReadUInt32AsBytesLE(UInt32(LIdx), LInitialHashWithZeros,
      ARGON2_PREHASH_DIGEST_LENGTH + 4);
    TConverters.ReadUInt32AsBytesLE(UInt32(LIdx), LInitialHashWithOnes,
      ARGON2_PREHASH_DIGEST_LENGTH + 4);

    LBlockHashBytes := Hash(LInitialHashWithZeros, ARGON2_BLOCK_SIZE);
    FMemory[LIdx * FLaneLength].FromBytes(LBlockHashBytes);

    LBlockHashBytes := Hash(LInitialHashWithOnes, ARGON2_BLOCK_SIZE);
    FMemory[(LIdx * FLaneLength) + 1].FromBytes(LBlockHashBytes);
  end;
end;

function TPBKDF_Argon2NotBuildInAdapter.IsDataIndependentAddressing
  (const APosition: TPosition): Boolean;
begin
  result := (FParameters.&Type = TArgon2Type.a2tARGON2_i) or
    ((FParameters.&Type = TArgon2Type.a2tARGON2_id) and (APosition.FPass = 0)
    and (APosition.FSlice < (ARGON2_SYNC_POINTS div 2)));
end;

procedure TPBKDF_Argon2NotBuildInAdapter.NextAddresses(const AFiller
  : TFillBlock; var AZeroBlock, AInputBlock, AAddressBlock: TBlock);
begin
  System.Inc(AInputBlock.Fv[6]);
  AFiller.FillBlock(AZeroBlock, AInputBlock, AAddressBlock, False);
  AFiller.FillBlock(AZeroBlock, AAddressBlock, AAddressBlock, False);
end;

function TPBKDF_Argon2NotBuildInAdapter.IntToUInt64(Ax: Int32): UInt64;
begin
  result := UInt64((Ax and UInt32($FFFFFFFF)))
end;

procedure TPBKDF_Argon2NotBuildInAdapter.InitAddressBlocks
  (const AFiller: TFillBlock; const APosition: TPosition;
  var AZeroBlock, AInputBlock, AAddressBlock: TBlock);
begin
  AInputBlock.Fv[0] := IntToUInt64(APosition.FPass);
  AInputBlock.Fv[1] := IntToUInt64(APosition.FLane);
  AInputBlock.Fv[2] := IntToUInt64(APosition.FSlice);
  AInputBlock.Fv[3] := IntToUInt64(System.Length(FMemory));
  AInputBlock.Fv[4] := IntToUInt64(FParameters.Iterations);
  AInputBlock.Fv[5] := IntToUInt64(Int32(FParameters.&Type));

  if ((APosition.FPass = 0) and (APosition.FSlice = 0)) then
  begin
    // Don't forget to generate the first block of addresses: */
    NextAddresses(AFiller, AZeroBlock, AInputBlock, AAddressBlock);
  end;
end;

function TPBKDF_Argon2NotBuildInAdapter.GetPseudoRandom(const AFiller
  : TFillBlock; const APosition: TPosition; var AAddressBlock, AInputBlock,
  AZeroBlock: TBlock; APrevOffset: Int32;
  ADataIndependentAddressing: Boolean): UInt64;
begin
  if (ADataIndependentAddressing) then
  begin
    if (APosition.FIndex mod ARGON2_ADDRESSES_IN_BLOCK = 0) then
    begin
      NextAddresses(AFiller, AZeroBlock, AInputBlock, AAddressBlock);
    end;
    result := AAddressBlock.Fv[APosition.FIndex mod ARGON2_ADDRESSES_IN_BLOCK];
    Exit;
  end
  else
  begin
    result := FMemory[APrevOffset].Fv[0];
  end;
end;

function TPBKDF_Argon2NotBuildInAdapter.GetRefLane(const APosition: TPosition;
  APseudoRandom: UInt64): Int32;
var
  LRefLane: Int32;
begin
  LRefLane := Int32((APseudoRandom shr 32) mod UInt64(FParameters.Lanes));

  if ((APosition.FPass = 0) and (APosition.FSlice = 0)) then
  begin
    // Can not reference other lanes yet
    LRefLane := APosition.FLane;
  end;
  result := LRefLane;
end;

function TPBKDF_Argon2NotBuildInAdapter.GetRefColumn(const APosition: TPosition;
  APseudoRandom: UInt64; ASameLane: Boolean): Int32;
var
  LReferenceAreaSize, LStartPosition, LTemp: Int32;
  LRelativePosition: UInt64;
begin

  if (APosition.FPass = 0) then
  begin
    LStartPosition := 0;

    if (ASameLane) then
    begin
      // The same lane => add current segment
      LReferenceAreaSize := ((APosition.FSlice) * FSegmentLength) +
        APosition.FIndex - 1;
    end
    else
    begin
      if (APosition.FIndex = 0) then
      begin
        LTemp := -1;
      end
      else
      begin
        LTemp := 0;
      end;
      LReferenceAreaSize := (APosition.FSlice * FSegmentLength) + LTemp;
    end
  end
  else
  begin
    LStartPosition := ((APosition.FSlice + 1) * FSegmentLength) mod FLaneLength;

    if (ASameLane) then
    begin
      LReferenceAreaSize := FLaneLength - FSegmentLength + APosition.FIndex - 1;
    end
    else
    begin
      if (APosition.FIndex = 0) then
      begin
        LTemp := -1;
      end
      else
      begin
        LTemp := 0;
      end;
      LReferenceAreaSize := FLaneLength - FSegmentLength + LTemp;
    end;
  end;

  LRelativePosition := APseudoRandom and UInt32($FFFFFFFF);
  LRelativePosition := (LRelativePosition * LRelativePosition) shr 32;
  LRelativePosition := UInt64(LReferenceAreaSize) - 1 -
    UInt64((UInt64(LReferenceAreaSize) * LRelativePosition) shr 32);

  result := Int32(UInt64(LStartPosition) + LRelativePosition) mod FLaneLength;
end;

function TPBKDF_Argon2NotBuildInAdapter.IsWithXor(const APosition
  : TPosition): Boolean;
begin
  result := not((APosition.FPass = 0) or
    (FParameters.Version = TArgon2Version.a2vARGON2_VERSION_10));
end;

function TPBKDF_Argon2NotBuildInAdapter.GetPrevOffset(ACurrentOffset
  : Int32): Int32;
begin
  if (ACurrentOffset mod FLaneLength = 0) then
  begin
    // Last block in this lane
    result := ACurrentOffset + FLaneLength - 1;
    Exit;
  end
  else
  begin
    // Previous block
    result := ACurrentOffset - 1;
    Exit;
  end;
end;

function TPBKDF_Argon2NotBuildInAdapter.RotatePrevOffset(ACurrentOffset,
  APrevOffset: Int32): Int32;
begin
  if (ACurrentOffset mod FLaneLength = 1) then
  begin
    APrevOffset := ACurrentOffset - 1;
  end;
  result := APrevOffset;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.Initialize(const APassword
  : THashLibByteArray; AOutputLength: Int32);
var
  LInitialHash: THashLibByteArray;
begin
  LInitialHash := InitialHash(FParameters, AOutputLength, APassword);
  FillFirstBlocks(LInitialHash);
end;

procedure TPBKDF_Argon2NotBuildInAdapter.FillSegment(AIdx: Int32;
  var APosition: TPosition);
var
  LAddressBlock, LInputBlock, LZeroBlock, LPrevBlock, LRefBlock,
    LCurrentBlock: TBlock;
  LDataIndependentAddressing, LWithXor: Boolean;
  LStartingIndex, LCurrentOffset, LPrevOffset, LRefLane, LRefColumn: Int32;
  LPseudoRandom: UInt64;
  LFiller: TFillBlock;
begin
  // line below not really needed, just added to fix compiler hint
  APosition.FLane := AIdx;
  LFiller := TFillBlock.CreateFillBlock();
  LDataIndependentAddressing := IsDataIndependentAddressing(APosition);
  LStartingIndex := GetStartingIndex(APosition);
  LCurrentOffset := (APosition.FLane * FLaneLength) +
    (APosition.FSlice * FSegmentLength) + LStartingIndex;
  LPrevOffset := GetPrevOffset(LCurrentOffset);

  LAddressBlock := Default (TBlock);
  LInputBlock := Default (TBlock);
  LZeroBlock := Default (TBlock);

  if (LDataIndependentAddressing) then
  begin
    LAddressBlock := LFiller.AddressBlock.Clear();
    LZeroBlock := LFiller.ZeroBlock.Clear();
    LInputBlock := LFiller.InputBlock.Clear();

    InitAddressBlocks(LFiller, APosition, LZeroBlock, LInputBlock,
      LAddressBlock);
  end;

  APosition.FIndex := LStartingIndex;

  while APosition.FIndex < FSegmentLength do
  begin
    LPrevOffset := RotatePrevOffset(LCurrentOffset, LPrevOffset);

    LPseudoRandom := GetPseudoRandom(LFiller, APosition, LAddressBlock,
      LInputBlock, LZeroBlock, LPrevOffset, LDataIndependentAddressing);
    LRefLane := GetRefLane(APosition, LPseudoRandom);
    LRefColumn := GetRefColumn(APosition, LPseudoRandom,
      LRefLane = APosition.FLane);

    // 2 Creating a new block
    LPrevBlock := FMemory[LPrevOffset];
    LRefBlock := FMemory[(((FLaneLength) * LRefLane) + LRefColumn)];
    LCurrentBlock := FMemory[LCurrentOffset];

    LWithXor := IsWithXor(APosition);
    LFiller.FillBlock(LPrevBlock, LRefBlock, LCurrentBlock, LWithXor);

    System.Inc(APosition.FIndex);
    System.Inc(LCurrentOffset);
    System.Inc(LPrevOffset);
  end;
end;

procedure TPBKDF_Argon2NotBuildInAdapter.FillMemoryBlocks(AIdx: Int32;
  ADataContainer: PDataContainer);
var
  LPosition: TPosition;
begin
  LPosition := ADataContainer^.Position;
  FillSegment(AIdx, LPosition);
end;

{$IFDEF USE_PASMP}

procedure TPBKDF_Argon2NotBuildInAdapter.PasMPFillMemoryBlocksWrapper
  (const AJob: PPasMPJob; const AThreadIndex: LongInt;
  const ADataContainer: Pointer; const AFromIndex, AToIndex: TPasMPNativeInt);
begin
  PDataContainer(ADataContainer)^.Position.FLane := AFromIndex;
  FillMemoryBlocks(AFromIndex, ADataContainer);
end;
{$ENDIF}
{$IFDEF USE_MTPROCS}

procedure TPBKDF_Argon2NotBuildInAdapter.MTProcsFillMemoryBlocksWrapper
  (AIdx: PtrInt; ADataContainer: Pointer; AItem: TMultiThreadProcItem);
begin
  PDataContainer(ADataContainer)^.Position.FLane := AIdx;
  FillMemoryBlocks(AIdx, ADataContainer);
end;
{$ENDIF}
{$IF DEFINED(USE_DELPHI_PPL)}

procedure TPBKDF_Argon2NotBuildInAdapter.DoParallelFillMemoryBlocks
  (ADataContainer: PDataContainer);

  function CreateTask(AIdx: Int32; ADataContainer: PDataContainer): ITask;
  begin
    result := TTask.Create(
      procedure()
      begin
        FillMemoryBlocks(AIdx, ADataContainer);
      end);
  end;

var
  LIdx, LJdx, LKdx, LIterations, LLanes: Int32;
  LArrayTasks: array of ITask;
begin
  LIterations := FParameters.Iterations;
  LLanes := FParameters.Lanes;
  System.SetLength(LArrayTasks, LLanes);

  for LIdx := 0 to System.Pred(LIterations) do
  begin
    for LJdx := 0 to System.Pred(ARGON2_SYNC_POINTS) do
    begin
      for LKdx := 0 to System.Pred(LLanes) do
      begin
        ADataContainer^.Position.Update(LIdx, LKdx, LJdx, 0);
        LArrayTasks[LKdx] := CreateTask(LKdx, ADataContainer);
        LArrayTasks[LKdx].Start;
      end;
      TTask.WaitForAll(LArrayTasks);
    end;
  end;

end;

{$ELSEIF DEFINED(USE_PASMP) OR DEFINED(USE_MTPROCS)}

procedure TPBKDF_Argon2NotBuildInAdapter.DoParallelFillMemoryBlocks
  (ADataContainer: PDataContainer);
var
  LIdx, LJdx, LIterations, LLanes: Int32;
begin
  LIterations := FParameters.Iterations;
  LLanes := FParameters.Lanes;
  for LIdx := 0 to System.Pred(LIterations) do
  begin
    for LJdx := 0 to System.Pred(ARGON2_SYNC_POINTS) do
    begin
      ADataContainer^.Position.Update(LIdx, 0, LJdx, 0);
{$IF DEFINED(USE_PASMP)}
      TPasMP.CreateGlobalInstance;
      GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(ADataContainer, 0, LLanes - 1,
        PasMPFillMemoryBlocksWrapper));
{$ELSEIF DEFINED(USE_MTPROCS)}
      ProcThreadPool.DoParallel(MTProcsFillMemoryBlocksWrapper, 0, LLanes - 1,
        ADataContainer);
{$ELSE}
{$MESSAGE ERROR 'Unsupported Threading Library.'}
{$IFEND USE_PASMP}
    end;
  end;

end;

{$ELSE}

procedure TPBKDF_Argon2NotBuildInAdapter.DoParallelFillMemoryBlocks
  (ADataContainer: PDataContainer);
var
  LIdx, LJdx, LKdx, LIterations, LLanes: Int32;
begin
  LIterations := FParameters.Iterations;
  LLanes := FParameters.Lanes;
  for LIdx := 0 to System.Pred(LIterations) do
  begin
    for LJdx := 0 to System.Pred(ARGON2_SYNC_POINTS) do
    begin
      for LKdx := 0 to System.Pred(LLanes) do
      begin
        ADataContainer^.Position.Update(LIdx, LKdx, LJdx, 0);
        FillMemoryBlocks(LKdx, ADataContainer);
      end;
    end;
  end;
end;

{$IFEND USE_DELPHI_PPL}

function TPBKDF_Argon2NotBuildInAdapter.GetBytes(AByteCount: Int32)
  : THashLibByteArray;
var
  LPtrDataContainer: PDataContainer;
  LPosition: TPosition;
begin
  if (AByteCount <= MIN_OUTLEN) then
  begin
    raise EArgumentHashLibException.CreateResFmt(@SInvalidOutputByteCount,
      [MIN_OUTLEN]);
  end;

  Initialize(FPassword, AByteCount);
  LPosition := TPosition.CreatePosition();
  LPtrDataContainer := New(PDataContainer);
  try
    LPtrDataContainer^.Position := LPosition;
    DoParallelFillMemoryBlocks(LPtrDataContainer);
  finally
    Dispose(LPtrDataContainer);
  end;
  Digest(AByteCount);
  System.SetLength(result, AByteCount);
  System.Move(FResult[0], result[0], AByteCount * System.SizeOf(Byte));

  Reset();
end;

procedure TPBKDF_Argon2NotBuildInAdapter.Clear();
begin
  TArrayUtils.ZeroFill(FPassword);
end;

constructor TPBKDF_Argon2NotBuildInAdapter.Create(const APassword
  : THashLibByteArray; const AParameters: IArgon2Parameters);
begin
  Inherited Create();
  ValidatePBKDF_Argon2Inputs(AParameters);
  FPassword := System.Copy(APassword);
  FParameters := AParameters;

  if (FParameters.Lanes < MIN_PARALLELISM) then
  begin
    raise EArgumentInvalidHashLibException.CreateResFmt(@SLanesTooSmall,
      [MIN_PARALLELISM]);
  end
  else if (FParameters.Lanes > MAX_PARALLELISM) then
  begin
    raise EArgumentInvalidHashLibException.CreateResFmt(@SLanesTooBig,
      [MAX_PARALLELISM]);
  end
  else if (FParameters.Memory < (2 * FParameters.Lanes)) then
  begin
    raise EArgumentInvalidHashLibException.CreateResFmt(@SMemoryTooSmall,
      [(2 * FParameters.Lanes), (2 * FParameters.Lanes)]);
  end
  else if (FParameters.Iterations < MIN_ITERATIONS) then
  begin
    raise EArgumentInvalidHashLibException.CreateResFmt(@SIterationsTooSmall,
      [MIN_ITERATIONS]);
  end;

  DoInit(AParameters);
end;

destructor TPBKDF_Argon2NotBuildInAdapter.Destroy;
begin
  Clear();
  inherited Destroy;
end;

end.
