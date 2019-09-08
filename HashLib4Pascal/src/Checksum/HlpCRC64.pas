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

    FCRCAlgorithm: ICRC;

  public
    constructor Create(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
      _XorOut, _check: UInt64; const _Names: THashLibStringArray);

    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
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

constructor TCRC64.Create(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
  _XorOut, _check: UInt64; const _Names: THashLibStringArray);
begin
  Inherited Create(8, 1);
  FCRCAlgorithm := TCRC.Create(64, _poly, _Init, _refIn, _refOut, _XorOut,
    _check, _Names);
end;

procedure TCRC64.Initialize;
begin
  FCRCAlgorithm.Initialize;
end;

procedure TCRC64.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
begin
  FCRCAlgorithm.TransformBytes(a_data, a_index, a_length);
end;

function TCRC64.TransformFinal: IHashResult;
begin
  result := FCRCAlgorithm.TransformFinal();
end;

{ TCRC64_ECMA_182 }

constructor TCRC64_ECMA_182.Create;
begin
  Inherited Create(TCRC64Polynomials.ECMA_182, $0000000000000000, false, false,
    $0000000000000000, $6C40DF5F0B497347,
    THashLibStringArray.Create('CRC-64/ECMA'));
end;

end.
