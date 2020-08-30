unit HlpCRC32;

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

  TCRC32Polynomials = class sealed(TObject)

  private

    const

    PKZIP = UInt32($04C11DB7);
    Castagnoli = UInt32($1EDC6F41);

  end;

  TCRC32 = class(THash, IChecksum, IHash32, ITransformBlock)

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

  TCRC32_PKZIP = class sealed(TCRC32)

  public
    constructor Create();

  end;

  TCRC32_CASTAGNOLI = class sealed(TCRC32)

  public
    constructor Create();

  end;

implementation

{ TCRC32 }

function TCRC32.Clone(): IHash;
begin
  Result := FCRCAlgorithm.Clone();
end;

constructor TCRC32.Create(APolynomial, AInitial: UInt64;
  AIsInputReflected, AIsOutputReflected: Boolean;
  AOutputXor, ACheckValue: UInt64; const ANames: THashLibStringArray);
begin
  Inherited Create(4, 1);
  FCRCAlgorithm := TCRC.Create(32, APolynomial, AInitial, AIsInputReflected,
    AIsOutputReflected, AOutputXor, ACheckValue, ANames);
end;

procedure TCRC32.Initialize;
begin
  FCRCAlgorithm.Initialize;
end;

procedure TCRC32.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
  FCRCAlgorithm.TransformBytes(AData, AIndex, ALength);
end;

function TCRC32.TransformFinal: IHashResult;
begin
  Result := FCRCAlgorithm.TransformFinal();
end;

{ TCRC32_PKZIP }

constructor TCRC32_PKZIP.Create;
begin
  Inherited Create(TCRC32Polynomials.PKZIP, $FFFFFFFF, true, true, $FFFFFFFF,
    $CBF43926, THashLibStringArray.Create('CRC-32', 'CRC-32/ADCCP',
    'CRC-32/V-42', 'CRC-32/XZ', 'PKZIP', 'CRC-32/ISO-HDLC'));

end;

{ TCRC32_CASTAGNOLI }

constructor TCRC32_CASTAGNOLI.Create;
begin
  Inherited Create(TCRC32Polynomials.Castagnoli, $FFFFFFFF, true, true,
    $FFFFFFFF, $E3069283, THashLibStringArray.Create('CRC-32C',
    'CRC-32/BASE91-C', 'CRC-32/CASTAGNOLI', 'CRC-32/INTERLAKEN',
    'CRC-32/ISCSI'));

end;

end.
