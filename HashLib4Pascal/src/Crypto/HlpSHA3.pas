unit HlpSHA3;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF HAS_UNITSCOPE}
  HlpBits,
{$IFDEF DELPHI}
  HlpBitConverter,
  HlpHash,
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
  SInvalidHashMode = 'Only "[%s]" HashModes are Supported';
  SInvalidXOFSize = 'XOFSize in Bits must be Divisible by 8.';

type
  TSHA3 = class abstract(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  type
{$SCOPEDENUMS ON}
    THashMode = (hmKeccak = $1, hmSHA3 = $6, hmShake = $1F);
{$SCOPEDENUMS OFF}
  strict protected

    Fm_state: THashLibUInt64Array;
    FHashSize, FBlockSize: Int32;
    FHashMode: THashMode;

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
    constructor Create(a_hash_size: THashSize);

    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

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

  TKeccak_224 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type

  TKeccak_256 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type

  TKeccak_288 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type

  TKeccak_384 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type

  TKeccak_512 = class sealed(TSHA3)

  public

    constructor Create();
    function Clone(): IHash; override;
  end;

type
  TShake = class abstract(TSHA3, IXOF)
  strict private
  var
    FXOFSizeInBits: UInt32;
    function GetXOFSizeInBits: UInt32; inline;
    procedure SetXOFSizeInBits(a_xof_size_in_bits: UInt32); inline;
    function SetXOFSizeInBitsInternal(a_xof_size_in_bits: UInt32): IXOF;
  strict protected
    constructor Create(a_hash_size: THashSize);
    property XOFSizeInBits: UInt32 read GetXOFSizeInBits write SetXOFSizeInBits;

  public
    function GetResult(): THashLibByteArray; override;
    function TransformFinal(): IHashResult; override;
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

implementation

{ TSHA3 }

constructor TSHA3.Create(a_hash_size: THashSize);
begin
  Inherited Create(Int32(a_hash_size), 200 - (Int32(a_hash_size) * 2));

  FHashSize := HashSize;
  FBlockSize := BlockSize;

  System.SetLength(Fm_state, 25);

end;

procedure TSHA3.Finish;
var
  buffer_pos: Int32;
  block: THashLibByteArray;
begin
  buffer_pos := Fm_buffer.Pos;
  block := Fm_buffer.GetBytesZeroPadded();

  block[buffer_pos] := Int32(FHashMode);
  block[FBlockSize - 1] := block[FBlockSize - 1] xor $80;

  TransformBlock(PByte(block), System.Length(block), 0);
end;

function TSHA3.GetName: String;
begin
  case FHashMode of
    TSHA3.THashMode.hmKeccak:
      Result := Format('%s_%u', ['TKeccak', Self.HashSize * 8]);
    TSHA3.THashMode.hmSHA3:
      Result := Self.ClassName;
    TSHA3.THashMode.hmShake:
      Result := Format('%s_%s_%u', [Self.ClassName, 'XOFSizeInBits',
        (Self as IXOF).XOFSizeInBits]);
  else
    begin
      raise EArgumentInvalidHashLibException.CreateResFmt(@SInvalidHashMode,
        ['hmKeccak, hmSHA3, hmShake']);
    end;
  end;
end;

function TSHA3.GetResult: THashLibByteArray;
begin

  System.SetLength(Result, FHashSize);

  TConverters.le64_copy(PUInt64(Fm_state), 0, PByte(Result), 0,
    System.Length(Result));

end;

procedure TSHA3.Initialize;
begin
  TArrayUtils.ZeroFill(Fm_state);

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
  round: Int32;
{$ELSE}
  Ca, Ce, Ci, Co, Cu: UInt64;
  temp: array [0 .. 24] of UInt64;
  j: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
begin
{$IFDEF USE_UNROLLED_VARIANT}
  // copyFromState(A, state)
  Aba := Fm_state[0];
  Abe := Fm_state[1];
  Abi := Fm_state[2];
  Abo := Fm_state[3];
  Abu := Fm_state[4];
  Aga := Fm_state[5];
  Age := Fm_state[6];
  Agi := Fm_state[7];
  Ago := Fm_state[8];
  Agu := Fm_state[9];
  Aka := Fm_state[10];
  Ake := Fm_state[11];
  Aki := Fm_state[12];
  Ako := Fm_state[13];
  Aku := Fm_state[14];
  Ama := Fm_state[15];
  Ame := Fm_state[16];
  Ami := Fm_state[17];
  Amo := Fm_state[18];
  Amu := Fm_state[19];
  Asa := Fm_state[20];
  Ase := Fm_state[21];
  Asi := Fm_state[22];
  Aso := Fm_state[23];
  Asu := Fm_state[24];

  round := 0;
  while round < 24 do
  begin
    // prepareTheta
    BCa := Aba xor Aga xor Aka xor Ama xor Asa;
    BCe := Abe xor Age xor Ake xor Ame xor Ase;
    BCi := Abi xor Agi xor Aki xor Ami xor Asi;
    BCo := Abo xor Ago xor Ako xor Amo xor Aso;
    BCu := Abu xor Agu xor Aku xor Amu xor Asu;

    // thetaRhoPiChiIotaPrepareTheta(round  , A, E)
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
    Eba := Eba xor UInt64(RC[round]);
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

    // thetaRhoPiChiIotaPrepareTheta(round+1, E, A)
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
    Aba := Aba xor UInt64(RC[round + 1]);
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

    System.Inc(round, 2);
  end;

  // copyToState(state, A)
  Fm_state[0] := Aba;
  Fm_state[1] := Abe;
  Fm_state[2] := Abi;
  Fm_state[3] := Abo;
  Fm_state[4] := Abu;
  Fm_state[5] := Aga;
  Fm_state[6] := Age;
  Fm_state[7] := Agi;
  Fm_state[8] := Ago;
  Fm_state[9] := Agu;
  Fm_state[10] := Aka;
  Fm_state[11] := Ake;
  Fm_state[12] := Aki;
  Fm_state[13] := Ako;
  Fm_state[14] := Aku;
  Fm_state[15] := Ama;
  Fm_state[16] := Ame;
  Fm_state[17] := Ami;
  Fm_state[18] := Amo;
  Fm_state[19] := Amu;
  Fm_state[20] := Asa;
  Fm_state[21] := Ase;
  Fm_state[22] := Asi;
  Fm_state[23] := Aso;
  Fm_state[24] := Asu;

{$ELSE}
  for j := 0 to 23 do
  begin
    Ca := Fm_state[00] xor Fm_state[05] xor Fm_state[10] xor Fm_state[15]
      xor Fm_state[20];
    Ce := Fm_state[01] xor Fm_state[06] xor Fm_state[11] xor Fm_state[16]
      xor Fm_state[21];
    Ci := Fm_state[02] xor Fm_state[07] xor Fm_state[12] xor Fm_state[17]
      xor Fm_state[22];
    Co := Fm_state[03] xor Fm_state[08] xor Fm_state[13] xor Fm_state[18]
      xor Fm_state[23];
    Cu := Fm_state[04] xor Fm_state[09] xor Fm_state[14] xor Fm_state[19]
      xor Fm_state[24];
    Da := TBits.RotateLeft64(Ca, 1) xor Co;
    De := TBits.RotateLeft64(Ce, 1) xor Cu;
    Di := TBits.RotateLeft64(Ci, 1) xor Ca;
    &Do := TBits.RotateLeft64(Co, 1) xor Ce;
    Du := TBits.RotateLeft64(Cu, 1) xor Ci;
    temp[00] := Fm_state[00] xor De;
    temp[01] := TBits.RotateLeft64(Fm_state[06] xor Di, 44);
    temp[02] := TBits.RotateLeft64(Fm_state[12] xor &Do, 43);
    temp[03] := TBits.RotateLeft64(Fm_state[18] xor Du, 21);
    temp[04] := TBits.RotateLeft64(Fm_state[24] xor Da, 14);
    temp[05] := TBits.RotateLeft64(Fm_state[03] xor Du, 28);
    temp[06] := TBits.RotateLeft64(Fm_state[09] xor Da, 20);
    temp[07] := TBits.RotateLeft64(Fm_state[10] xor De, 3);
    temp[08] := TBits.RotateLeft64(Fm_state[16] xor Di, 45);
    temp[09] := TBits.RotateLeft64(Fm_state[22] xor &Do, 61);
    temp[10] := TBits.RotateLeft64(Fm_state[01] xor Di, 1);
    temp[11] := TBits.RotateLeft64(Fm_state[07] xor &Do, 6);
    temp[12] := TBits.RotateLeft64(Fm_state[13] xor Du, 25);
    temp[13] := TBits.RotateLeft64(Fm_state[19] xor Da, 8);
    temp[14] := TBits.RotateLeft64(Fm_state[20] xor De, 18);
    temp[15] := TBits.RotateLeft64(Fm_state[04] xor Da, 27);
    temp[16] := TBits.RotateLeft64(Fm_state[05] xor De, 36);
    temp[17] := TBits.RotateLeft64(Fm_state[11] xor Di, 10);
    temp[18] := TBits.RotateLeft64(Fm_state[17] xor &Do, 15);
    temp[19] := TBits.RotateLeft64(Fm_state[23] xor Du, 56);
    temp[20] := TBits.RotateLeft64(Fm_state[02] xor &Do, 62);
    temp[21] := TBits.RotateLeft64(Fm_state[08] xor Du, 55);
    temp[22] := TBits.RotateLeft64(Fm_state[14] xor Da, 39);
    temp[23] := TBits.RotateLeft64(Fm_state[15] xor De, 41);
    temp[24] := TBits.RotateLeft64(Fm_state[21] xor Di, 2);
    Fm_state[00] := temp[00] xor ((not temp[01]) and temp[02]);
    Fm_state[01] := temp[01] xor ((not temp[02]) and temp[03]);
    Fm_state[02] := temp[02] xor ((not temp[03]) and temp[04]);
    Fm_state[03] := temp[03] xor ((not temp[04]) and temp[00]);
    Fm_state[04] := temp[04] xor ((not temp[00]) and temp[01]);
    Fm_state[05] := temp[05] xor ((not temp[06]) and temp[07]);
    Fm_state[06] := temp[06] xor ((not temp[07]) and temp[08]);
    Fm_state[07] := temp[07] xor ((not temp[08]) and temp[09]);
    Fm_state[08] := temp[08] xor ((not temp[09]) and temp[05]);
    Fm_state[09] := temp[09] xor ((not temp[05]) and temp[06]);
    Fm_state[10] := temp[10] xor ((not temp[11]) and temp[12]);
    Fm_state[11] := temp[11] xor ((not temp[12]) and temp[13]);
    Fm_state[12] := temp[12] xor ((not temp[13]) and temp[14]);
    Fm_state[13] := temp[13] xor ((not temp[14]) and temp[10]);
    Fm_state[14] := temp[14] xor ((not temp[10]) and temp[11]);
    Fm_state[15] := temp[15] xor ((not temp[16]) and temp[17]);
    Fm_state[16] := temp[16] xor ((not temp[17]) and temp[18]);
    Fm_state[17] := temp[17] xor ((not temp[18]) and temp[19]);
    Fm_state[18] := temp[18] xor ((not temp[19]) and temp[15]);
    Fm_state[19] := temp[19] xor ((not temp[15]) and temp[16]);
    Fm_state[20] := temp[20] xor ((not temp[21]) and temp[22]);
    Fm_state[21] := temp[21] xor ((not temp[22]) and temp[23]);
    Fm_state[22] := temp[22] xor ((not temp[23]) and temp[24]);
    Fm_state[23] := temp[23] xor ((not temp[24]) and temp[20]);
    Fm_state[24] := temp[24] xor ((not temp[20]) and temp[21]);
    Fm_state[00] := Fm_state[00] xor RC[j];
  end;

  System.FillChar(temp, System.SizeOf(temp), UInt64(0));
{$ENDIF USE_UNROLLED_VARIANT}
end;

procedure TSHA3.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  data: array [0 .. 20] of UInt64;
  j, upcount: Int32;
begin
  TConverters.le64_copy(a_data, a_index, @(data[0]), 0, a_data_length);

  j := 0;
  upcount := FBlockSize shr 3;
  while j < upcount do
  begin
    Fm_state[j] := Fm_state[j] xor data[j];
    System.Inc(j);
  end;

  KeccakF1600_StatePermute();
  System.FillChar(data, System.SizeOf(data), UInt64(0));
end;

{ TSHA3_224 }

function TSHA3_224.Clone(): IHash;
var
  HashInstance: TSHA3_224;
begin
  HashInstance := TSHA3_224.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSHA3_224.Create;
begin
  Inherited Create(THashSize.hsHashSize224);
  FHashMode := THashMode.hmSHA3;
end;

{ TSHA3_256 }

function TSHA3_256.Clone(): IHash;
var
  HashInstance: TSHA3_256;
begin
  HashInstance := TSHA3_256.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSHA3_256.Create;
begin
  Inherited Create(THashSize.hsHashSize256);
  FHashMode := THashMode.hmSHA3;
end;

{ TSHA3_384 }

function TSHA3_384.Clone(): IHash;
var
  HashInstance: TSHA3_384;
begin
  HashInstance := TSHA3_384.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSHA3_384.Create;
begin
  Inherited Create(THashSize.hsHashSize384);
  FHashMode := THashMode.hmSHA3;
end;

{ TSHA3_512 }

function TSHA3_512.Clone(): IHash;
var
  HashInstance: TSHA3_512;
begin
  HashInstance := TSHA3_512.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSHA3_512.Create;
begin
  Inherited Create(THashSize.hsHashSize512);
  FHashMode := THashMode.hmSHA3;
end;

{ TKeccak_224 }

function TKeccak_224.Clone(): IHash;
var
  HashInstance: TKeccak_224;
begin
  HashInstance := TKeccak_224.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_224.Create;
begin
  Inherited Create(THashSize.hsHashSize224);
  FHashMode := THashMode.hmKeccak;
end;

{ TKeccak_256 }

function TKeccak_256.Clone(): IHash;
var
  HashInstance: TKeccak_256;
begin
  HashInstance := TKeccak_256.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_256.Create;
begin
  Inherited Create(THashSize.hsHashSize256);
  FHashMode := THashMode.hmKeccak;
end;

{ TKeccak_288 }

function TKeccak_288.Clone(): IHash;
var
  HashInstance: TKeccak_288;
begin
  HashInstance := TKeccak_288.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_288.Create;
begin
  Inherited Create(THashSize.hsHashSize288);
  FHashMode := THashMode.hmKeccak;
end;

{ TKeccak_384 }

function TKeccak_384.Clone(): IHash;
var
  HashInstance: TKeccak_384;
begin
  HashInstance := TKeccak_384.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_384.Create;
begin
  Inherited Create(THashSize.hsHashSize384);
  FHashMode := THashMode.hmKeccak;
end;

{ TKeccak_512 }

function TKeccak_512.Clone(): IHash;
var
  HashInstance: TKeccak_512;
begin
  HashInstance := TKeccak_512.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TKeccak_512.Create;
begin
  Inherited Create(THashSize.hsHashSize512);
  FHashMode := THashMode.hmKeccak;
end;

{ TShake }

function TShake.SetXOFSizeInBitsInternal(a_xof_size_in_bits: UInt32): IXOF;
begin
  If ((a_xof_size_in_bits and $7) <> 0) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes(@SInvalidXOFSize);
  end;
  FXOFSizeInBits := a_xof_size_in_bits;
  Result := Self;
end;

constructor TShake.Create(a_hash_size: THashSize);
begin
  Inherited Create(a_hash_size);
  FHashMode := THashMode.hmShake;
end;

function TShake.GetResult: THashLibByteArray;
var
  buffer_pos: Int32;
  Idx, LXofSizeInBytes: UInt32;
begin
  buffer_pos := Fm_buffer.Pos;

  LXofSizeInBytes := FXOFSizeInBits shr 3;
  Idx := 0;
  System.SetLength(Result, LXofSizeInBytes);

  while Idx < (LXofSizeInBytes shr 3) do
  begin

    if (buffer_pos * 8) >= FBlockSize then
    begin
      KeccakF1600_StatePermute();

      buffer_pos := 0;
    end;

    TConverters.ReadUInt64AsBytesLE(Fm_state[buffer_pos], Result, Idx * 8);

    System.Inc(buffer_pos);
    System.Inc(Idx);
  end;
end;

function TShake.GetXOFSizeInBits: UInt32;
begin
  Result := FXOFSizeInBits;
end;

function TShake.TransformFinal: IHashResult;
var
  tempresult: THashLibByteArray;
begin
  Finish();
{$IFDEF DEBUG}
  System.Assert(Fm_buffer.IsEmpty);
{$ENDIF DEBUG}
  tempresult := GetResult();
{$IFDEF DEBUG}
  System.Assert(UInt32(System.Length(tempresult)) = (XOFSizeInBits shr 3));
{$ENDIF DEBUG}
  Initialize();
  Result := THashResult.Create(tempresult);
end;

procedure TShake.SetXOFSizeInBits(a_xof_size_in_bits: UInt32);
begin
  SetXOFSizeInBitsInternal(a_xof_size_in_bits);
end;

{ TShake_128 }

function TShake_128.Clone(): IHash;
var
  HashInstance: TShake_128;
  LXof: IXOF;
begin
  LXof := (TShake_128.Create() as IXOF);
  LXof.XOFSizeInBits := (Self as IXOF).XOFSizeInBits;
  HashInstance := LXof as TShake_128;
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TShake_128.Create;
begin
  Inherited Create(THashSize.hsHashSize128);
end;

{ TShake_256 }

function TShake_256.Clone(): IHash;
var
  HashInstance: TShake_256;
  LXof: IXOF;
begin
  LXof := (TShake_256.Create() as IXOF);
  LXof.XOFSizeInBits := (Self as IXOF).XOFSizeInBits;
  HashInstance := LXof as TShake_256;
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TShake_256.Create;
begin
  Inherited Create(THashSize.hsHashSize256);
end;

end.
