unit HlpCRC16;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpICRC,
  HlpIHashResult,
  HlpIHashInfo,
  HlpCRC;

type

  TCRC16Polynomials = class sealed(TObject)

  private

    const

    BUYPASS = UInt16($8005);

  end;

  TCRC16 = class(THash, IChecksum, IHash16, ITransformBlock)

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

  end;

  TCRC16_BUYPASS = class sealed(TCRC16)

  public
    constructor Create();

  end;

implementation

{ TCRC16 }

constructor TCRC16.Create(APolynomial, AInitial: UInt64;
  AIsInputReflected, AIsOutputReflected: Boolean;
  AOutputXor, ACheckValue: UInt64; const ANames: THashLibStringArray);
begin
  Inherited Create(2, 1);
  FCRCAlgorithm := TCRC.Create(16, APolynomial, AInitial, AIsInputReflected,
    AIsOutputReflected, AOutputXor, ACheckValue, ANames);
end;

procedure TCRC16.Initialize;
begin
  FCRCAlgorithm.Initialize;
end;

procedure TCRC16.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
  FCRCAlgorithm.TransformBytes(AData, AIndex, ALength);
end;

function TCRC16.TransformFinal: IHashResult;
begin
  result := FCRCAlgorithm.TransformFinal();
end;

{ TCRC16_BUYPASS }

constructor TCRC16_BUYPASS.Create;
begin
  Inherited Create(TCRC16Polynomials.BUYPASS, $0000, false, false, $0000, $FEE8,
    THashLibStringArray.Create('CRC-16/BUYPASS', 'CRC-16/VERIFONE',
    'CRC-16/UMTS'));
end;

end.
