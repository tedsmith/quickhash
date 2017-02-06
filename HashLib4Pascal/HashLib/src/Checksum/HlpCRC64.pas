unit HlpCRC64;

interface

uses
  HlpHashLibTypes,
  HlpHash,
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

  TCRC64 = class(THash, IChecksum, IBlockHash, IHash64, ITransformBlock)

  strict private

    FCRCAlgorithm: ICRC;

  public
    constructor Create(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
      _XorOut, _check: UInt64; _Names: THashLibStringArray);

    procedure Initialize(); override;
    procedure TransformBytes(a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;

  end;

  TCRC64_ECMA = class sealed(TCRC64)

  public
    constructor Create();

  end;

implementation

{ TCRC64 }

constructor TCRC64.Create(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
  _XorOut, _check: UInt64; _Names: THashLibStringArray);
begin
  Inherited Create(8, 1);
  FCRCAlgorithm := TCRC.Create(64, _poly, _Init, _refIn, _refOut, _XorOut,
    _check, _Names);
end;

procedure TCRC64.Initialize;
begin
  FCRCAlgorithm.Initialize;
end;

procedure TCRC64.TransformBytes(a_data: THashLibByteArray;
  a_index, a_length: Int32);
begin
  FCRCAlgorithm.TransformBytes(a_data, a_index, a_length);
end;

function TCRC64.TransformFinal: IHashResult;
begin
  result := FCRCAlgorithm.TransformFinal();
end;

{ TCRC64_ECMA }

constructor TCRC64_ECMA.Create;
begin
  Inherited Create(TCRC64Polynomials.ECMA_182, $0000000000000000, false, false,
    $0000000000000000, $6C40DF5F0B497347,
    THashLibStringArray.Create('CRC-64/ECMA'));
end;

end.
