unit HlpCRC64;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpICRC,
  HlpIHashResult,
  HlpIHashInfo,
  HlpCRC;

type

  TCRC64Polynomials = class sealed(TObject)

  private

    const

    ECMA_182 = UInt64($42F0E1EBA9EA3693);

  end;

  TCRC64 = class(THash, IChecksum, IHash64, ITransformBlock)

  strict private
  var
    FCRCAlgorithm: ICRC;

  public
    constructor Create(APolynomial, AInitial: UInt64;
      AIsInputReflected, AIsOutputReflected: Boolean;
      AOutputXor, ACheckValue: UInt64; const ANames: THashLibStringArray);

    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;

  end;

  TCRC64_ECMA_182 = class sealed(TCRC64)

  public
    constructor Create();

  end;

implementation

{ TCRC64 }

function TCRC64.Clone(): IHash;
begin
  result := FCRCAlgorithm.Clone();
end;

constructor TCRC64.Create(APolynomial, AInitial: UInt64;
  AIsInputReflected, AIsOutputReflected: Boolean;
  AOutputXor, ACheckValue: UInt64; const ANames: THashLibStringArray);
begin
  Inherited Create(8, 1);
  FCRCAlgorithm := TCRC.Create(64, APolynomial, AInitial, AIsInputReflected,
    AIsOutputReflected, AOutputXor, ACheckValue, ANames);
end;

procedure TCRC64.Initialize;
begin
  FCRCAlgorithm.Initialize;
end;

procedure TCRC64.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
  FCRCAlgorithm.TransformBytes(AData, AIndex, ALength);
end;

function TCRC64.TransformFinal: IHashResult;
begin
  result := FCRCAlgorithm.TransformFinal();
end;

{ TCRC64_ECMA_182 }

constructor TCRC64_ECMA_182.Create;
begin
  Inherited Create(TCRC64Polynomials.ECMA_182, $0000000000000000, false, false,
    $0000000000000000, $6C40DF5F0B497347, THashLibStringArray.Create('CRC-64',
    'CRC-64/ECMA-182'));
end;

end.
