unit HlpSHA3;

{$I ..\Include\HashLib.inc}

interface

uses
  SysUtils,
  HlpBits,
  HlpHash,
{$IFDEF DELPHI}
  HlpHashBuffer,
{$ENDIF DELPHI}
  HlpIHashInfo,
  HlpIHash,
  HlpHashResult,
  HlpIHashResult,
  HlpHashCryptoNotBuildIn,
  HlpConverters,
  HlpHashSize,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SInvalidXOFSize =
    'XOFSize in Bits must be Multiples of 8 and be Greater than Zero Bytes';
  SOutputLengthInvalid = 'Output Length is above the Digest Length';
  SOutputBufferTooShort = 'Output Buffer Too Short';
  SWritetoXofAfterReadError = '"%s" Write to Xof after Read not Allowed';

type
  TSHA3 = class abstract(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  type
{$SCOPEDENUMS ON}
    THashMode = (hmKeccak = $1, hmCShake = $4, hmSHA3 = $6, hmShake = $1F);
{$SCOPEDENUMS OFF}
  strict protected
  var
    FState: THashLibUInt64Array;

{$REGION 'Consts'}

  const

    RC: array [0 .. 23] of UInt64 = (UInt64($0000000000000001),
      UInt64($0000000000008082), UInt64($800000000000808A),
      UInt64($8000000080008000), UInt64($000000000000808B),
      UInt64($0000000080000001), UInt64($8000000080008081),
      UInt64($8000000000008009), UInt64($000000000000008A),
      UInt64($0000000000000088), UInt64($0000000080008009),
      UInt64($000000008000000A), UInt64($000000008000808B),
      UInt64($800000000000008B), UInt64($8000000000008089),
      UInt64($8000000000008003), UInt64($8000000000008002),
      UInt64($8000000000000080), UInt64($000000000000800A),
      UInt64($800000008000000A), UInt64($8000000080008081),
      UInt64($8000000000008080), UInt64($0000000080000001),
      UInt64($8000000080008008));

{$ENDREGION}
    procedure KeccakF1600_StatePermute();

    function GetName: String; override;
    constructor Create(AHashSize: THashSize);

    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

    function GetHashMode(): TSHA3.THashMode; virtual;

  public
    procedure Initialize; override;

  end;

type
  TSHA3_224 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TSHA3_256 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TSHA3_384 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TSHA3_512 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TKeccak = class abstract(TSHA3)

  strict protected

    function GetHashMode(): TSHA3.THashMode; override;
  end;

type
  TKeccak_224 = class sealed(TKeccak)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TKeccak_256 = class sealed(TKeccak)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TKeccak_288 = class sealed(TKeccak)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TKeccak_384 = class sealed(TKeccak)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TKeccak_512 = class sealed(TKeccak)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TShake = class abstract(TSHA3, IXOF)
  strict private
  var
    FXOFSizeInBits: UInt64;
    function GetXOFSizeInBits: UInt64; inline;
    procedure SetXOFSizeInBits(AXofSizeInBits: UInt64); inline;
    function SetXOFSizeInBitsInternal(AXofSizeInBits: UInt64): IXOF;
  strict protected
  var
    FBufferPosition, FDigestPosition: UInt64;
    FShakeBuffer: THashLibByteArray;
    FFinalized: Boolean;
    constructor Create(AHashSize: THashSize);
    function GetHashMode(): TSHA3.THashMode; override;
    property XOFSizeInBits: UInt64 read GetXOFSizeInBits write SetXOFSizeInBits;

  public
    procedure Initialize(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ADataLength: Int32); override;
    function TransformFinal(): IHashResult; override;
    procedure DoOutput(const ADestination: THashLibByteArray;
      ADestinationOffset, AOutputLength: UInt64);
  end;

type
  TShake_128 = class sealed(TShake)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TShake_256 = class sealed(TShake)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TCShake = class abstract(TShake)

  strict private

    // LeftEncode returns max 9 bytes
    class function LeftEncode(AInput: UInt64): THashLibByteArray; static;

  strict protected

  var
    FN, FS, FInitBlock: THashLibByteArray;

    function GetHashMode(): TSHA3.THashMode; override;

    /// <param name="AHashSize">
    /// the HashSize of the underlying Shake function
    /// </param>
    /// <param name="N">
    /// the function name string, note this is reserved for use by NIST.
    /// Avoid using if not required
    /// </param>
    /// <param name="S">
    /// the customization string - available for local use
    /// </param>
    constructor Create(AHashSize: THashSize; const N, S: THashLibByteArray);

  public

    procedure Initialize(); override;

    class function RightEncode(AInput: UInt64): THashLibByteArray; static;
    class function BytePad(const AInput: THashLibByteArray; AW: Int32)
      : THashLibByteArray; static;
    class function EncodeString(const AInput: THashLibByteArray)
      : THashLibByteArray; static;
  end;

type
  TCShake_128 = class sealed(TCShake)

  public

    constructor Create(const N, S: THashLibByteArray);
    function Clone(): IHash; override;
  end;

type
  TCShake_256 = class sealed(TCShake)

  public

    constructor Create(const N, S: THashLibByteArray);
    function Clone(): IHash; override;
  end;

type
  TKMACNotBuildInAdapter = class abstract(THash, IKMAC, IKMACNotBuildIn,
    ICrypto, ICryptoNotBuildIn)

  strict protected
  var
    FHash: IHash;
    FKey: THashLibByteArray;

    function GetName: String; override;

    function GetKey(): THashLibByteArray;
    procedure SetKey(const AValue: THashLibByteArray);

    procedure DoOutput(const ADestination: THashLibByteArray;
      ADestinationOffset, AOutputLength: UInt64);

    constructor Create(AHashSize: Int32);

    function GetResult(): THashLibByteArray;

  public

    destructor Destroy; override;

    procedure Clear();

    procedure Initialize(); override;
    function TransformFinal(): IHashResult; override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;

    property Key: THashLibByteArray read GetKey write SetKey;
    property Name: String read GetName;

  end;

type
  TKMAC128 = class(TKMACNotBuildInAdapter, IKMAC)

  strict private
    constructor Create(const AKMACKey, ACustomization: THashLibByteArray;
      AOutputLengthInBits: UInt64); overload;
    constructor Create(const AHash: IHash; const AKMACKey: THashLibByteArray;
      AOutputLengthInBits: UInt64); overload;

  public
    function Clone(): IHash; override;
    class function CreateKMAC128(const AKMACKey, ACustomization
      : THashLibByteArray; AOutputLengthInBits: UInt64): IKMAC; static;
  end;

type
  TKMAC128XOF = class sealed(TKMAC128, IKMAC, IXOF)
  strict private

    function GetXOFSizeInBits: UInt64; inline;
    procedure SetXOFSizeInBits(AXofSizeInBits: UInt64); inline;
    function SetXOFSizeInBitsInternal(AXofSizeInBits: UInt64): IXOF;

    constructor Create(const AKMACKey, ACustomization
      : THashLibByteArray); overload;
    constructor Create(const AHash: IHash;
      const AKMACKey: THashLibByteArray); overload;

  strict protected
    property XOFSizeInBits: UInt64 read GetXOFSizeInBits write SetXOFSizeInBits;

  public
    function Clone(): IHash; override;
    class function CreateKMAC128XOF(const AKMACKey, ACustomization
      : THashLibByteArray; AXofSizeInBits: UInt64): IKMAC; static;

  end;

type
  TKMAC256 = class(TKMACNotBuildInAdapter, IKMAC)

  strict private
    constructor Create(const AKMACKey, ACustomization: THashLibByteArray;
      AOutputLengthInBits: UInt64); overload;
    constructor Create(const AHash: IHash; const AKMACKey: THashLibByteArray;
      AOutputLengthInBits: UInt64); overload;

  public
    function Clone(): IHash; override;
    class function CreateKMAC256(const AKMACKey, ACustomization
      : THashLibByteArray; AOutputLengthInBits: UInt64): IKMAC; static;
  end;

type
  TKMAC256XOF = class sealed(TKMAC256, IKMAC, IXOF)
  strict private

    function GetXOFSizeInBits: UInt64; inline;
    procedure SetXOFSizeInBits(AXofSizeInBits: UInt64); inline;
    function SetXOFSizeInBitsInternal(AXofSizeInBits: UInt64): IXOF;

    constructor Create(const AKMACKey, ACustomization
      : THashLibByteArray); overload;
    constructor Create(const AHash: IHash;
      const AKMACKey: THashLibByteArray); overload;

  strict protected
    property XOFSizeInBits: UInt64 read GetXOFSizeInBits write SetXOFSizeInBits;

  public
    function Clone(): IHash; override;
    class function CreateKMAC256XOF(const AKMACKey, ACustomization
      : THashLibByteArray; AXofSizeInBits: UInt64): IKMAC; static;

  end;

implementation

{ TSHA3 }

function TSHA3.GetHashMode(): TSHA3.THashMode;
begin
  Result := TSHA3.THashMode.hmSHA3;
end;

constructor TSHA3.Create(AHashSize: THashSize);
begin
  Inherited Create(Int32(AHashSize), 200 - (Int32(AHashSize) * 2));
  System.SetLength(FState, 25);
end;

procedure TSHA3.Finish;
var
  LBufferPosition: Int32;
  LBlock: THashLibByteArray;
begin
  LBufferPosition := FBuffer.Position;
  LBlock := FBuffer.GetBytesZeroPadded();

  LBlock[LBufferPosition] := Int32(GetHashMode());
  LBlock[BlockSize - 1] := LBlock[BlockSize - 1] xor $80;

  TransformBlock(PByte(LBlock), System.Length(LBlock), 0);
end;

function TSHA3.GetName: String;
begin
  Result := Self.ClassName;
end;

function TSHA3.GetResult: THashLibByteArray;
begin
  System.SetLength(Result, HashSize);

  TConverters.le64_copy(PUInt64(FState), 0, PByte(Result), 0,
    System.Length(Result));
end;

procedure TSHA3.Initialize;
begin
  TArrayUtils.ZeroFill(FState);
  Inherited Initialize();
end;

procedure TSHA3.KeccakF1600_StatePermute;
var
  Da, De, Di, &Do, Du: UInt64;
{$IFDEF USE_UNROLLED_VARIANT}
  Aba, Abe, Abi, Abo, Abu, Aga, Age, Agi, Ago, Agu, Aka, Ake, Aki, Ako, Aku,
    Ama, Ame, Ami, Amo, Amu, Asa, Ase, Asi, Aso, Asu, BCa, BCe, BCi, BCo, BCu,
    Eba, Ebe, Ebi, Ebo, Ebu, Ega, Ege, Egi, Ego, Egu, Eka, Eke, Eki, Eko, Eku,
    Ema, Eme, Emi, Emo, Emu, Esa, Ese, Esi, Eso, Esu: UInt64;
  LRound: Int32;
{$ELSE}
  Ca, Ce, Ci, Co, Cu: UInt64;
  LTemp: array [0 .. 24] of UInt64;
  j: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
begin
{$IFDEF USE_UNROLLED_VARIANT}
  // copyFromState(A, state)
  Aba := FState[0];
  Abe := FState[1];
  Abi := FState[2];
  Abo := FState[3];
  Abu := FState[4];
  Aga := FState[5];
  Age := FState[6];
  Agi := FState[7];
  Ago := FState[8];
  Agu := FState[9];
  Aka := FState[10];
  Ake := FState[11];
  Aki := FState[12];
  Ako := FState[13];
  Aku := FState[14];
  Ama := FState[15];
  Ame := FState[16];
  Ami := FState[17];
  Amo := FState[18];
  Amu := FState[19];
  Asa := FState[20];
  Ase := FState[21];
  Asi := FState[22];
  Aso := FState[23];
  Asu := FState[24];

  LRound := 0;
  while LRound < 24 do
  begin
    // prepareTheta
    BCa := Aba xor Aga xor Aka xor Ama xor Asa;
    BCe := Abe xor Age xor Ake xor Ame xor Ase;
    BCi := Abi xor Agi xor Aki xor Ami xor Asi;
    BCo := Abo xor Ago xor Ako xor Amo xor Aso;
    BCu := Abu xor Agu xor Aku xor Amu xor Asu;

    // thetaRhoPiChiIotaPrepareTheta(LRound  , A, E)
    Da := BCu xor TBits.RotateLeft64(BCe, 1);
    De := BCa xor TBits.RotateLeft64(BCi, 1);
    Di := BCe xor TBits.RotateLeft64(BCo, 1);
    &Do := BCi xor TBits.RotateLeft64(BCu, 1);
    Du := BCo xor TBits.RotateLeft64(BCa, 1);

    Aba := Aba xor Da;
    BCa := Aba;
    Age := Age xor De;
    BCe := TBits.RotateLeft64(Age, 44);
    Aki := Aki xor Di;
    BCi := TBits.RotateLeft64(Aki, 43);
    Amo := Amo xor &Do;
    BCo := TBits.RotateLeft64(Amo, 21);
    Asu := Asu xor Du;
    BCu := TBits.RotateLeft64(Asu, 14);
    Eba := BCa xor ((not BCe) and BCi);
    Eba := Eba xor UInt64(RC[LRound]);
    Ebe := BCe xor ((not BCi) and BCo);
    Ebi := BCi xor ((not BCo) and BCu);
    Ebo := BCo xor ((not BCu) and BCa);
    Ebu := BCu xor ((not BCa) and BCe);

    Abo := Abo xor &Do;
    BCa := TBits.RotateLeft64(Abo, 28);
    Agu := Agu xor Du;
    BCe := TBits.RotateLeft64(Agu, 20);
    Aka := Aka xor Da;
    BCi := TBits.RotateLeft64(Aka, 3);
    Ame := Ame xor De;
    BCo := TBits.RotateLeft64(Ame, 45);
    Asi := Asi xor Di;
    BCu := TBits.RotateLeft64(Asi, 61);
    Ega := BCa xor ((not BCe) and BCi);
    Ege := BCe xor ((not BCi) and BCo);
    Egi := BCi xor ((not BCo) and BCu);
    Ego := BCo xor ((not BCu) and BCa);
    Egu := BCu xor ((not BCa) and BCe);

    Abe := Abe xor De;
    BCa := TBits.RotateLeft64(Abe, 1);
    Agi := Agi xor Di;
    BCe := TBits.RotateLeft64(Agi, 6);
    Ako := Ako xor &Do;
    BCi := TBits.RotateLeft64(Ako, 25);
    Amu := Amu xor Du;
    BCo := TBits.RotateLeft64(Amu, 8);
    Asa := Asa xor Da;
    BCu := TBits.RotateLeft64(Asa, 18);
    Eka := BCa xor ((not BCe) and BCi);
    Eke := BCe xor ((not BCi) and BCo);
    Eki := BCi xor ((not BCo) and BCu);
    Eko := BCo xor ((not BCu) and BCa);
    Eku := BCu xor ((not BCa) and BCe);

    Abu := Abu xor Du;
    BCa := TBits.RotateLeft64(Abu, 27);
    Aga := Aga xor Da;
    BCe := TBits.RotateLeft64(Aga, 36);
    Ake := Ake xor De;
    BCi := TBits.RotateLeft64(Ake, 10);
    Ami := Ami xor Di;
    BCo := TBits.RotateLeft64(Ami, 15);
    Aso := Aso xor &Do;
    BCu := TBits.RotateLeft64(Aso, 56);
    Ema := BCa xor ((not BCe) and BCi);
    Eme := BCe xor ((not BCi) and BCo);
    Emi := BCi xor ((not BCo) and BCu);
    Emo := BCo xor ((not BCu) and BCa);
    Emu := BCu xor ((not BCa) and BCe);

    Abi := Abi xor Di;
    BCa := TBits.RotateLeft64(Abi, 62);
    Ago := Ago xor &Do;
    BCe := TBits.RotateLeft64(Ago, 55);
    Aku := Aku xor Du;
    BCi := TBits.RotateLeft64(Aku, 39);
    Ama := Ama xor Da;
    BCo := TBits.RotateLeft64(Ama, 41);
    Ase := Ase xor De;
    BCu := TBits.RotateLeft64(Ase, 2);
    Esa := BCa xor ((not BCe) and BCi);
    Ese := BCe xor ((not BCi) and BCo);
    Esi := BCi xor ((not BCo) and BCu);
    Eso := BCo xor ((not BCu) and BCa);
    Esu := BCu xor ((not BCa) and BCe);

    // prepareTheta
    BCa := Eba xor Ega xor Eka xor Ema xor Esa;
    BCe := Ebe xor Ege xor Eke xor Eme xor Ese;
    BCi := Ebi xor Egi xor Eki xor Emi xor Esi;
    BCo := Ebo xor Ego xor Eko xor Emo xor Eso;
    BCu := Ebu xor Egu xor Eku xor Emu xor Esu;

    // thetaRhoPiChiIotaPrepareTheta(LRound+1, E, A)
    Da := BCu xor TBits.RotateLeft64(BCe, 1);
    De := BCa xor TBits.RotateLeft64(BCi, 1);
    Di := BCe xor TBits.RotateLeft64(BCo, 1);
    &Do := BCi xor TBits.RotateLeft64(BCu, 1);
    Du := BCo xor TBits.RotateLeft64(BCa, 1);

    Eba := Eba xor Da;
    BCa := Eba;
    Ege := Ege xor De;
    BCe := TBits.RotateLeft64(Ege, 44);
    Eki := Eki xor Di;
    BCi := TBits.RotateLeft64(Eki, 43);
    Emo := Emo xor &Do;
    BCo := TBits.RotateLeft64(Emo, 21);
    Esu := Esu xor Du;
    BCu := TBits.RotateLeft64(Esu, 14);
    Aba := BCa xor ((not BCe) and BCi);
    Aba := Aba xor UInt64(RC[LRound + 1]);
    Abe := BCe xor ((not BCi) and BCo);
    Abi := BCi xor ((not BCo) and BCu);
    Abo := BCo xor ((not BCu) and BCa);
    Abu := BCu xor ((not BCa) and BCe);

    Ebo := Ebo xor &Do;
    BCa := TBits.RotateLeft64(Ebo, 28);
    Egu := Egu xor Du;
    BCe := TBits.RotateLeft64(Egu, 20);
    Eka := Eka xor Da;
    BCi := TBits.RotateLeft64(Eka, 3);
    Eme := Eme xor De;
    BCo := TBits.RotateLeft64(Eme, 45);
    Esi := Esi xor Di;
    BCu := TBits.RotateLeft64(Esi, 61);
    Aga := BCa xor ((not BCe) and BCi);
    Age := BCe xor ((not BCi) and BCo);
    Agi := BCi xor ((not BCo) and BCu);
    Ago := BCo xor ((not BCu) and BCa);
    Agu := BCu xor ((not BCa) and BCe);

    Ebe := Ebe xor De;
    BCa := TBits.RotateLeft64(Ebe, 1);
    Egi := Egi xor Di;
    BCe := TBits.RotateLeft64(Egi, 6);
    Eko := Eko xor &Do;
    BCi := TBits.RotateLeft64(Eko, 25);
    Emu := Emu xor Du;
    BCo := TBits.RotateLeft64(Emu, 8);
    Esa := Esa xor Da;
    BCu := TBits.RotateLeft64(Esa, 18);
    Aka := BCa xor ((not BCe) and BCi);
    Ake := BCe xor ((not BCi) and BCo);
    Aki := BCi xor ((not BCo) and BCu);
    Ako := BCo xor ((not BCu) and BCa);
    Aku := BCu xor ((not BCa) and BCe);

    Ebu := Ebu xor Du;
    BCa := TBits.RotateLeft64(Ebu, 27);
    Ega := Ega xor Da;
    BCe := TBits.RotateLeft64(Ega, 36);
    Eke := Eke xor De;
    BCi := TBits.RotateLeft64(Eke, 10);
    Emi := Emi xor Di;
    BCo := TBits.RotateLeft64(Emi, 15);
    Eso := Eso xor &Do;
    BCu := TBits.RotateLeft64(Eso, 56);
    Ama := BCa xor ((not BCe) and BCi);
    Ame := BCe xor ((not BCi) and BCo);
    Ami := BCi xor ((not BCo) and BCu);
    Amo := BCo xor ((not BCu) and BCa);
    Amu := BCu xor ((not BCa) and BCe);

    Ebi := Ebi xor Di;
    BCa := TBits.RotateLeft64(Ebi, 62);
    Ego := Ego xor &Do;
    BCe := TBits.RotateLeft64(Ego, 55);
    Eku := Eku xor Du;
    BCi := TBits.RotateLeft64(Eku, 39);
    Ema := Ema xor Da;
    BCo := TBits.RotateLeft64(Ema, 41);
    Ese := Ese xor De;
    BCu := TBits.RotateLeft64(Ese, 2);
    Asa := BCa xor ((not BCe) and BCi);
    Ase := BCe xor ((not BCi) and BCo);
    Asi := BCi xor ((not BCo) and BCu);
    Aso := BCo xor ((not BCu) and BCa);
    Asu := BCu xor ((not BCa) and BCe);

    System.Inc(LRound, 2);
  end;

  // copyToState(state, A)
  FState[0] := Aba;
  FState[1] := Abe;
  FState[2] := Abi;
  FState[3] := Abo;
  FState[4] := Abu;
  FState[5] := Aga;
  FState[6] := Age;
  FState[7] := Agi;
  FState[8] := Ago;
  FState[9] := Agu;
  FState[10] := Aka;
  FState[11] := Ake;
  FState[12] := Aki;
  FState[13] := Ako;
  FState[14] := Aku;
  FState[15] := Ama;
  FState[16] := Ame;
  FState[17] := Ami;
  FState[18] := Amo;
  FState[19] := Amu;
  FState[20] := Asa;
  FState[21] := Ase;
  FState[22] := Asi;
  FState[23] := Aso;
  FState[24] := Asu;

{$ELSE}
  for j := 0 to 23 do
  begin
    Ca := FState[00] xor FState[05] xor FState[10] xor FState[15]
      xor FState[20];
    Ce := FState[01] xor FState[06] xor FState[11] xor FState[16]
      xor FState[21];
    Ci := FState[02] xor FState[07] xor FState[12] xor FState[17]
      xor FState[22];
    Co := FState[03] xor FState[08] xor FState[13] xor FState[18]
      xor FState[23];
    Cu := FState[04] xor FState[09] xor FState[14] xor FState[19]
      xor FState[24];
    Da := TBits.RotateLeft64(Ca, 1) xor Co;
    De := TBits.RotateLeft64(Ce, 1) xor Cu;
    Di := TBits.RotateLeft64(Ci, 1) xor Ca;
    &Do := TBits.RotateLeft64(Co, 1) xor Ce;
    Du := TBits.RotateLeft64(Cu, 1) xor Ci;
    LTemp[00] := FState[00] xor De;
    LTemp[01] := TBits.RotateLeft64(FState[06] xor Di, 44);
    LTemp[02] := TBits.RotateLeft64(FState[12] xor &Do, 43);
    LTemp[03] := TBits.RotateLeft64(FState[18] xor Du, 21);
    LTemp[04] := TBits.RotateLeft64(FState[24] xor Da, 14);
    LTemp[05] := TBits.RotateLeft64(FState[03] xor Du, 28);
    LTemp[06] := TBits.RotateLeft64(FState[09] xor Da, 20);
    LTemp[07] := TBits.RotateLeft64(FState[10] xor De, 3);
    LTemp[08] := TBits.RotateLeft64(FState[16] xor Di, 45);
    LTemp[09] := TBits.RotateLeft64(FState[22] xor &Do, 61);
    LTemp[10] := TBits.RotateLeft64(FState[01] xor Di, 1);
    LTemp[11] := TBits.RotateLeft64(FState[07] xor &Do, 6);
    LTemp[12] := TBits.RotateLeft64(FState[13] xor Du, 25);
    LTemp[13] := TBits.RotateLeft64(FState[19] xor Da, 8);
    LTemp[14] := TBits.RotateLeft64(FState[20] xor De, 18);
    LTemp[15] := TBits.RotateLeft64(FState[04] xor Da, 27);
    LTemp[16] := TBits.RotateLeft64(FState[05] xor De, 36);
    LTemp[17] := TBits.RotateLeft64(FState[11] xor Di, 10);
    LTemp[18] := TBits.RotateLeft64(FState[17] xor &Do, 15);
    LTemp[19] := TBits.RotateLeft64(FState[23] xor Du, 56);
    LTemp[20] := TBits.RotateLeft64(FState[02] xor &Do, 62);
    LTemp[21] := TBits.RotateLeft64(FState[08] xor Du, 55);
    LTemp[22] := TBits.RotateLeft64(FState[14] xor Da, 39);
    LTemp[23] := TBits.RotateLeft64(FState[15] xor De, 41);
    LTemp[24] := TBits.RotateLeft64(FState[21] xor Di, 2);
    FState[00] := LTemp[00] xor ((not LTemp[01]) and LTemp[02]);
    FState[01] := LTemp[01] xor ((not LTemp[02]) and LTemp[03]);
    FState[02] := LTemp[02] xor ((not LTemp[03]) and LTemp[04]);
    FState[03] := LTemp[03] xor ((not LTemp[04]) and LTemp[00]);
    FState[04] := LTemp[04] xor ((not LTemp[00]) and LTemp[01]);
    FState[05] := LTemp[05] xor ((not LTemp[06]) and LTemp[07]);
    FState[06] := LTemp[06] xor ((not LTemp[07]) and LTemp[08]);
    FState[07] := LTemp[07] xor ((not LTemp[08]) and LTemp[09]);
    FState[08] := LTemp[08] xor ((not LTemp[09]) and LTemp[05]);
    FState[09] := LTemp[09] xor ((not LTemp[05]) and LTemp[06]);
    FState[10] := LTemp[10] xor ((not LTemp[11]) and LTemp[12]);
    FState[11] := LTemp[11] xor ((not LTemp[12]) and LTemp[13]);
    FState[12] := LTemp[12] xor ((not LTemp[13]) and LTemp[14]);
    FState[13] := LTemp[13] xor ((not LTemp[14]) and LTemp[10]);
    FState[14] := LTemp[14] xor ((not LTemp[10]) and LTemp[11]);
    FState[15] := LTemp[15] xor ((not LTemp[16]) and LTemp[17]);
    FState[16] := LTemp[16] xor ((not LTemp[17]) and LTemp[18]);
    FState[17] := LTemp[17] xor ((not LTemp[18]) and LTemp[19]);
    FState[18] := LTemp[18] xor ((not LTemp[19]) and LTemp[15]);
    FState[19] := LTemp[19] xor ((not LTemp[15]) and LTemp[16]);
    FState[20] := LTemp[20] xor ((not LTemp[21]) and LTemp[22]);
    FState[21] := LTemp[21] xor ((not LTemp[22]) and LTemp[23]);
    FState[22] := LTemp[22] xor ((not LTemp[23]) and LTemp[24]);
    FState[23] := LTemp[23] xor ((not LTemp[24]) and LTemp[20]);
    FState[24] := LTemp[24] xor ((not LTemp[20]) and LTemp[21]);
    FState[00] := FState[00] xor RC[j];
  end;

  System.FillChar(LTemp, System.SizeOf(LTemp), UInt64(0));
{$ENDIF USE_UNROLLED_VARIANT}
end;

procedure TSHA3.TransformBlock(AData: PByte; ADataLength: Int32; AIndex: Int32);
var
  LData: array [0 .. 20] of UInt64;
  LJdx, LBlockCount: Int32;
begin
  TConverters.le64_copy(AData, AIndex, @(LData[0]), 0, ADataLength);
  LJdx := 0;
  LBlockCount := BlockSize shr 3;
  while LJdx < LBlockCount do
  begin
    FState[LJdx] := FState[LJdx] xor LData[LJdx];
    System.Inc(LJdx);
  end;

  KeccakF1600_StatePermute();
  System.FillChar(LData, System.SizeOf(LData), UInt64(0));
end;

{ TSHA3_224 }

function TSHA3_224.Clone(): IHash;
var
  LHashInstance: TSHA3_224;
begin
  LHashInstance := TSHA3_224.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSHA3_224.Create;
begin
  Inherited Create(THashSize.hsHashSize224);
end;

{ TSHA3_256 }

function TSHA3_256.Clone(): IHash;
var
  LHashInstance: TSHA3_256;
begin
  LHashInstance := TSHA3_256.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSHA3_256.Create;
begin
  Inherited Create(THashSize.hsHashSize256);
end;

{ TSHA3_384 }

function TSHA3_384.Clone(): IHash;
var
  LHashInstance: TSHA3_384;
begin
  LHashInstance := TSHA3_384.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSHA3_384.Create;
begin
  Inherited Create(THashSize.hsHashSize384);
end;

{ TSHA3_512 }

function TSHA3_512.Clone(): IHash;
var
  LHashInstance: TSHA3_512;
begin
  LHashInstance := TSHA3_512.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSHA3_512.Create;
begin
  Inherited Create(THashSize.hsHashSize512);
end;

{ TKeccak_224 }

function TKeccak_224.Clone(): IHash;
var
  LHashInstance: TKeccak_224;
begin
  LHashInstance := TKeccak_224.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_224.Create;
begin
  Inherited Create(THashSize.hsHashSize224);
end;

{ TKeccak_256 }

function TKeccak_256.Clone(): IHash;
var
  LHashInstance: TKeccak_256;
begin
  LHashInstance := TKeccak_256.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_256.Create;
begin
  Inherited Create(THashSize.hsHashSize256);
end;

{ TKeccak_288 }

function TKeccak_288.Clone(): IHash;
var
  LHashInstance: TKeccak_288;
begin
  LHashInstance := TKeccak_288.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_288.Create;
begin
  Inherited Create(THashSize.hsHashSize288);
end;

{ TKeccak_384 }

function TKeccak_384.Clone(): IHash;
var
  LHashInstance: TKeccak_384;
begin
  LHashInstance := TKeccak_384.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_384.Create;
begin
  Inherited Create(THashSize.hsHashSize384);
end;

{ TKeccak_512 }

function TKeccak_512.Clone(): IHash;
var
  LHashInstance: TKeccak_512;
begin
  LHashInstance := TKeccak_512.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_512.Create;
begin
  Inherited Create(THashSize.hsHashSize512);
end;

{ TShake }

function TShake.GetHashMode(): TSHA3.THashMode;
begin
  Result := TSHA3.THashMode.hmShake;
end;

function TShake.SetXOFSizeInBitsInternal(AXofSizeInBits: UInt64): IXOF;
var
  LXofSizeInBytes: UInt64;
begin
  LXofSizeInBytes := AXofSizeInBits shr 3;
  If (((AXofSizeInBits and $7) <> 0) or (LXofSizeInBytes < 1)) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes(@SInvalidXOFSize);
  end;
  FXOFSizeInBits := AXofSizeInBits;
  Result := Self;
end;

function TShake.GetXOFSizeInBits: UInt64;
begin
  Result := FXOFSizeInBits;
end;

procedure TShake.SetXOFSizeInBits(AXofSizeInBits: UInt64);
begin
  SetXOFSizeInBitsInternal(AXofSizeInBits);
end;

constructor TShake.Create(AHashSize: THashSize);
begin
  Inherited Create(AHashSize);
  FFinalized := False;
  System.SetLength(FShakeBuffer, 8);
end;

procedure TShake.DoOutput(const ADestination: THashLibByteArray;
  ADestinationOffset, AOutputLength: UInt64);
var
  LDiff, LCount, LBlockOffset: UInt64;
begin

  if (UInt64(System.Length(ADestination)) - ADestinationOffset) < AOutputLength
  then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateRes(@SOutputBufferTooShort);
  end;

  if ((FDigestPosition + AOutputLength) > (XOFSizeInBits shr 3)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateRes(@SOutputLengthInvalid);
  end;

  if not FFinalized then
  begin
    Finish();
    FFinalized := True;
  end;

  while AOutputLength > 0 do
  begin
    if (FDigestPosition and 7) = 0 then
    begin

      if (FBufferPosition * 8) >= UInt64(BlockSize) then
      begin
        KeccakF1600_StatePermute();
        FBufferPosition := 0;
      end;

      TConverters.ReadUInt64AsBytesLE(FState[FBufferPosition], FShakeBuffer, 0);
      System.Inc(FBufferPosition);
    end;

    LBlockOffset := FDigestPosition and 7;

    LDiff := UInt64(System.Length(FShakeBuffer)) - LBlockOffset;

    // Math.Min
    if AOutputLength < LDiff then
    begin
      LCount := AOutputLength
    end
    else
    begin
      LCount := LDiff;
    end;

    System.Move(FShakeBuffer[LBlockOffset],
      ADestination[ADestinationOffset], LCount);

    System.Dec(AOutputLength, LCount);
    System.Inc(ADestinationOffset, LCount);
    System.Inc(FDigestPosition, LCount);
  end;
end;

function TShake.GetResult: THashLibByteArray;
var
  LXofSizeInBytes: UInt64;
begin
  LXofSizeInBytes := FXOFSizeInBits shr 3;

  System.SetLength(Result, LXofSizeInBytes);

  DoOutput(Result, 0, LXofSizeInBytes);
end;

procedure TShake.Initialize;
begin
  inherited Initialize();
  FBufferPosition := 0;
  FDigestPosition := 0;
  FFinalized := False;
  TArrayUtils.ZeroFill(FShakeBuffer);
end;

procedure TShake.TransformBytes(const AData: THashLibByteArray;
  AIndex, ADataLength: Int32);
begin
  if FFinalized then
  begin
    raise EInvalidOperationHashLibException.CreateResFmt
      (@SWritetoXofAfterReadError, [Name]);
  end;
  inherited TransformBytes(AData, AIndex, ADataLength);
end;

function TShake.TransformFinal: IHashResult;
var
  LBuffer: THashLibByteArray;
begin
  LBuffer := GetResult();
{$IFDEF DEBUG}
  System.Assert(UInt64(System.Length(LBuffer)) = (XOFSizeInBits shr 3));
{$ENDIF DEBUG}
  Initialize();
  Result := THashResult.Create(LBuffer);
end;

{ TShake_128 }

function TShake_128.Clone(): IHash;
var
  LHashInstance: TShake_128;
  LXof: IXOF;
begin
  // Xof Cloning
  LXof := (TShake_128.Create() as IXOF);
  LXof.XOFSizeInBits := (Self as IXOF).XOFSizeInBits;

  // Shake_128 Cloning
  LHashInstance := LXof as TShake_128;
  LHashInstance.FBufferPosition := FBufferPosition;
  LHashInstance.FDigestPosition := FDigestPosition;
  LHashInstance.FFinalized := FFinalized;
  LHashInstance.FShakeBuffer := System.Copy(FShakeBuffer);

  // Internal Sha3 Cloning
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;

  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TShake_128.Create;
begin
  Inherited Create(THashSize.hsHashSize128);
end;

{ TShake_256 }

function TShake_256.Clone(): IHash;
var
  LHashInstance: TShake_256;
  LXof: IXOF;
begin
  // Xof Cloning
  LXof := (TShake_256.Create() as IXOF);
  LXof.XOFSizeInBits := (Self as IXOF).XOFSizeInBits;

  // Shake_256 Cloning
  LHashInstance := LXof as TShake_256;
  LHashInstance.FBufferPosition := FBufferPosition;
  LHashInstance.FDigestPosition := FDigestPosition;
  LHashInstance.FFinalized := FFinalized;
  LHashInstance.FShakeBuffer := System.Copy(FShakeBuffer);

  // Internal Sha3 Cloning
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;

  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TShake_256.Create;
begin
  Inherited Create(THashSize.hsHashSize256);
end;

{ TCShake }

function TCShake.GetHashMode(): TSHA3.THashMode;
begin
  if (System.Length(FN) = 0) and (System.Length(FS) = 0) then
  begin
    Result := TSHA3.THashMode.hmShake;
  end
  else
  begin
    Result := TSHA3.THashMode.hmCShake;
  end;
end;

class function TCShake.LeftEncode(AInput: UInt64): THashLibByteArray;
var
  LN: Byte;
  LV: UInt64;
  LIdx: Int32;
begin
  LN := 1;
  LV := AInput;
  LV := LV shr 8;

  while (LV <> 0) do
  begin
    System.Inc(LN);
    LV := LV shr 8;
  end;

  System.SetLength(Result, LN + 1);
  Result[0] := LN;
  for LIdx := 1 to LN do
  begin
    Result[LIdx] := Byte(AInput shr (8 * (LN - LIdx)));
  end;
end;

class function TCShake.RightEncode(AInput: UInt64): THashLibByteArray;
var
  LN: Byte;
  LV: UInt64;
  LIdx: Int32;
begin
  LN := 1;
  LV := AInput;
  LV := LV shr 8;

  while (LV <> 0) do
  begin
    System.Inc(LN);
    LV := LV shr 8;
  end;

  System.SetLength(Result, LN + 1);
  Result[LN] := LN;
  for LIdx := 1 to LN do
  begin
    Result[LIdx - 1] := Byte(AInput shr (8 * (LN - LIdx)));
  end;
end;

class function TCShake.BytePad(const AInput: THashLibByteArray; AW: Int32)
  : THashLibByteArray;
var
  LBuffer: THashLibByteArray;
  LPadLength: Int32;
begin
  LBuffer := TArrayUtils.Concatenate(LeftEncode(UInt64(AW)), AInput);
  LPadLength := AW - (System.Length(LBuffer) mod AW);
  System.SetLength(Result, LPadLength);
  Result := TArrayUtils.Concatenate(LBuffer, Result);
end;

class function TCShake.EncodeString(const AInput: THashLibByteArray)
  : THashLibByteArray;
begin
  if System.Length(AInput) = 0 then
  begin
    Result := LeftEncode(0);
    Exit;
  end;
  Result := TArrayUtils.Concatenate(LeftEncode(UInt64(System.Length(AInput) * 8)
    ), AInput);
end;

constructor TCShake.Create(AHashSize: THashSize; const N, S: THashLibByteArray);
begin
  Inherited Create(AHashSize);

  FN := N;
  FS := S;

  if (System.Length(FN) = 0) and (System.Length(FS) = 0) then
  begin
    FInitBlock := Nil;
  end
  else
  begin
    FInitBlock := TArrayUtils.Concatenate(EncodeString(N), EncodeString(S));
  end;
end;

procedure TCShake.Initialize;
begin
  Inherited Initialize();

  if FInitBlock <> Nil then
  begin
    TransformBytes(BytePad(FInitBlock, BlockSize));
  end;
end;

{ TCShake_128 }

function TCShake_128.Clone(): IHash;
var
  LHashInstance: TCShake_128;
  LXof: IXOF;
begin
  // Xof Cloning
  LXof := TCShake_128.Create(System.Copy(FN), System.Copy(FS)) as IXOF;
  LXof.XOFSizeInBits := (Self as IXOF).XOFSizeInBits;

  // CShake_128 Cloning
  LHashInstance := LXof as TCShake_128;
  LHashInstance.FInitBlock := System.Copy(FInitBlock);

  LHashInstance.FBufferPosition := FBufferPosition;
  LHashInstance.FDigestPosition := FDigestPosition;
  LHashInstance.FFinalized := FFinalized;
  LHashInstance.FShakeBuffer := System.Copy(FShakeBuffer);

  // Internal Sha3 Cloning
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;

  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TCShake_128.Create(const N, S: THashLibByteArray);
begin
  Inherited Create(THashSize.hsHashSize128, N, S);
end;

{ TCShake_256 }

function TCShake_256.Clone(): IHash;
var
  LHashInstance: TCShake_256;
  LXof: IXOF;
begin
  // Xof Cloning
  LXof := TCShake_256.Create(System.Copy(FN), System.Copy(FS)) as IXOF;
  LXof.XOFSizeInBits := (Self as IXOF).XOFSizeInBits;

  // CShake_256 Cloning
  LHashInstance := LXof as TCShake_256;
  LHashInstance.FInitBlock := System.Copy(FInitBlock);

  LHashInstance.FBufferPosition := FBufferPosition;
  LHashInstance.FDigestPosition := FDigestPosition;
  LHashInstance.FFinalized := FFinalized;
  LHashInstance.FShakeBuffer := System.Copy(FShakeBuffer);

  // Internal Sha3 Cloning
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;

  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TCShake_256.Create(const N, S: THashLibByteArray);
begin
  Inherited Create(THashSize.hsHashSize256, N, S);
end;

{ TKMACNotBuildInAdapter }

procedure TKMACNotBuildInAdapter.Clear();
begin
  TArrayUtils.ZeroFill(FKey);
end;

constructor TKMACNotBuildInAdapter.Create(AHashSize: Int32);
begin
  Inherited Create(AHashSize, 200 - (AHashSize * 2));
end;

destructor TKMACNotBuildInAdapter.Destroy;
begin
  Clear();
  inherited Destroy;
end;

procedure TKMACNotBuildInAdapter.DoOutput(const ADestination: THashLibByteArray;
  ADestinationOffset, AOutputLength: UInt64);
begin
  if Supports(Self, IXOF) then
  begin
    TransformBytes(TCShake.RightEncode(0));
  end
  else
  begin
    TransformBytes(TCShake.RightEncode((FHash as IXOF).XOFSizeInBits));
  end;

  (FHash as IXOF).DoOutput(ADestination, ADestinationOffset, AOutputLength);
end;

function TKMACNotBuildInAdapter.GetKey: THashLibByteArray;
begin
  Result := System.Copy(FKey);
end;

function TKMACNotBuildInAdapter.GetName: String;
begin
  Result := Self.ClassName;
end;

procedure TKMACNotBuildInAdapter.Initialize;
begin
  FHash.Initialize;
  TransformBytes(TCShake.BytePad(TCShake.EncodeString(FKey), BlockSize));
end;

procedure TKMACNotBuildInAdapter.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey := Nil;
  end
  else
  begin
    FKey := System.Copy(AValue);
  end;
end;

procedure TKMACNotBuildInAdapter.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
  FHash.TransformBytes(AData, AIndex, ALength);
end;

function TKMACNotBuildInAdapter.GetResult: THashLibByteArray;
var
  LXofSizeInBytes: UInt64;
begin
  LXofSizeInBytes := (FHash as IXOF).XOFSizeInBits shr 3;
  System.SetLength(Result, LXofSizeInBytes);
  DoOutput(Result, 0, LXofSizeInBytes);
end;

function TKMACNotBuildInAdapter.TransformFinal: IHashResult;
var
  LBuffer: THashLibByteArray;
begin
  LBuffer := GetResult();
{$IFDEF DEBUG}
  System.Assert(UInt64(System.Length(LBuffer))
    = ((FHash as IXOF).XOFSizeInBits shr 3));
{$ENDIF DEBUG}
  Initialize();
  Result := THashResult.Create(LBuffer);
end;

{ TKMAC128 }

function TKMAC128.Clone(): IHash;
var
  LHashInstance: TKMAC128;
begin
  LHashInstance := TKMAC128.Create(FHash.Clone(), FKey,
    (FHash as IXOF).XOFSizeInBits);
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKMAC128.Create(const AKMACKey, ACustomization: THashLibByteArray;
  AOutputLengthInBits: UInt64);
begin
  Create(TCShake_128.Create(TConverters.ConvertStringToBytes('KMAC',
    TEncoding.UTF8), ACustomization) as IHash, AKMACKey, AOutputLengthInBits);
end;

constructor TKMAC128.Create(const AHash: IHash;
  const AKMACKey: THashLibByteArray; AOutputLengthInBits: UInt64);
begin
  inherited Create(Int32(THashSize.hsHashSize128));
  SetKey(AKMACKey);
  FHash := AHash;
  (FHash as IXOF).XOFSizeInBits := AOutputLengthInBits;
end;

class function TKMAC128.CreateKMAC128(const AKMACKey, ACustomization
  : THashLibByteArray; AOutputLengthInBits: UInt64): IKMAC;
begin
  Result := TKMAC128.Create(AKMACKey, ACustomization,
    AOutputLengthInBits) as IKMAC;
end;

{ TKMAC128XOF }

function TKMAC128XOF.SetXOFSizeInBitsInternal(AXofSizeInBits: UInt64): IXOF;
var
  AXofSizeInBytes: UInt64;
begin
  AXofSizeInBytes := AXofSizeInBits shr 3;
  If (((AXofSizeInBytes and $7) <> 0) or (AXofSizeInBytes < 1)) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes(@SInvalidXOFSize);
  end;

  (FHash as IXOF).XOFSizeInBits := AXofSizeInBits;
  Result := Self;
end;

function TKMAC128XOF.GetXOFSizeInBits: UInt64;
begin
  Result := (FHash as IXOF).XOFSizeInBits;
end;

procedure TKMAC128XOF.SetXOFSizeInBits(AXofSizeInBits: UInt64);
begin
  SetXOFSizeInBitsInternal(AXofSizeInBits);
end;

function TKMAC128XOF.Clone(): IHash;
var
  LHashInstance: TKMAC128XOF;
  LXof: IXOF;
begin
  LHashInstance := TKMAC128XOF.Create(FHash.Clone(), FKey);
  LXof := LHashInstance as IXOF;
  LXof.XOFSizeInBits := XOFSizeInBits;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKMAC128XOF.Create(const AHash: IHash;
  const AKMACKey: THashLibByteArray);
begin
  inherited Create(Int32(THashSize.hsHashSize128));
  SetKey(AKMACKey);
  FHash := AHash;
end;

constructor TKMAC128XOF.Create(const AKMACKey, ACustomization
  : THashLibByteArray);
begin
  Create(TCShake_128.Create(TConverters.ConvertStringToBytes('KMAC',
    TEncoding.UTF8), ACustomization) as IHash, AKMACKey);
end;

class function TKMAC128XOF.CreateKMAC128XOF(const AKMACKey, ACustomization
  : THashLibByteArray; AXofSizeInBits: UInt64): IKMAC;
var
  LXof: IXOF;
begin
  LXof := (TKMAC128XOF.Create(AKMACKey, ACustomization) as IKMAC) as IXOF;
  LXof.XOFSizeInBits := AXofSizeInBits;
  Result := (LXof as IHash) as IKMAC;
end;

{ TKMAC256 }

function TKMAC256.Clone(): IHash;
var
  LHashInstance: TKMAC256;
begin
  LHashInstance := TKMAC256.Create(FHash.Clone(), FKey,
    (FHash as IXOF).XOFSizeInBits);
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKMAC256.Create(const AKMACKey, ACustomization: THashLibByteArray;
  AOutputLengthInBits: UInt64);
begin
  Create(TCShake_256.Create(TConverters.ConvertStringToBytes('KMAC',
    TEncoding.UTF8), ACustomization) as IHash, AKMACKey, AOutputLengthInBits);
end;

constructor TKMAC256.Create(const AHash: IHash;
  const AKMACKey: THashLibByteArray; AOutputLengthInBits: UInt64);
begin
  inherited Create(Int32(THashSize.hsHashSize256));
  SetKey(AKMACKey);
  FHash := AHash;
  (FHash as IXOF).XOFSizeInBits := AOutputLengthInBits;
end;

class function TKMAC256.CreateKMAC256(const AKMACKey, ACustomization
  : THashLibByteArray; AOutputLengthInBits: UInt64): IKMAC;
begin
  Result := TKMAC256.Create(AKMACKey, ACustomization,
    AOutputLengthInBits) as IKMAC;
end;

{ TKMAC256XOF }

function TKMAC256XOF.SetXOFSizeInBitsInternal(AXofSizeInBits: UInt64): IXOF;
var
  AXofSizeInBytes: UInt64;
begin
  AXofSizeInBytes := AXofSizeInBits shr 3;
  If (((AXofSizeInBytes and $7) <> 0) or (AXofSizeInBytes < 1)) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes(@SInvalidXOFSize);
  end;

  (FHash as IXOF).XOFSizeInBits := AXofSizeInBits;
  Result := Self;
end;

function TKMAC256XOF.GetXOFSizeInBits: UInt64;
begin
  Result := (FHash as IXOF).XOFSizeInBits;
end;

procedure TKMAC256XOF.SetXOFSizeInBits(AXofSizeInBits: UInt64);
begin
  SetXOFSizeInBitsInternal(AXofSizeInBits);
end;

function TKMAC256XOF.Clone(): IHash;
var
  LHashInstance: TKMAC256XOF;
  LXof: IXOF;
begin
  LHashInstance := TKMAC256XOF.Create(FHash.Clone(), FKey);
  LXof := LHashInstance as IXOF;
  LXof.XOFSizeInBits := XOFSizeInBits;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKMAC256XOF.Create(const AHash: IHash;
  const AKMACKey: THashLibByteArray);
begin
  inherited Create(Int32(THashSize.hsHashSize256));
  SetKey(AKMACKey);
  FHash := AHash;
end;

constructor TKMAC256XOF.Create(const AKMACKey, ACustomization
  : THashLibByteArray);
begin
  Create(TCShake_256.Create(TConverters.ConvertStringToBytes('KMAC',
    TEncoding.UTF8), ACustomization) as IHash, AKMACKey);
end;

class function TKMAC256XOF.CreateKMAC256XOF(const AKMACKey, ACustomization
  : THashLibByteArray; AXofSizeInBits: UInt64): IKMAC;
var
  LXof: IXOF;
begin
  LXof := (TKMAC256XOF.Create(AKMACKey, ACustomization) as IKMAC) as IXOF;
  LXof.XOFSizeInBits := AXofSizeInBits;
  Result := (LXof as IHash) as IKMAC;
end;

{ TKeccak }

function TKeccak.GetHashMode(): TSHA3.THashMode;
begin
  Result := TSHA3.THashMode.hmKeccak;
end;

end.
