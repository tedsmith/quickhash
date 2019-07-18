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

    FCRCAlgorithm: ICRC;

  public
    constructor Create(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
      _XorOut, _check: UInt64; const _Names: THashLibStringArray);

    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;

  end;

  TCRC16_BUYPASS = class sealed(TCRC16)

  public
    constructor Create();

  end;

implementation

{ TCRC16 }

constructor TCRC16.Create(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
  _XorOut, _check: UInt64; const _Names: THashLibStringArray);
begin
  Inherited Create(2, 1);
  FCRCAlgorithm := TCRC.Create(16, _poly, _Init, _refIn, _refOut, _XorOut,
    _check, _Names);
end;

procedure TCRC16.Initialize;
begin
  FCRCAlgorithm.Initialize;
end;

procedure TCRC16.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
begin
  FCRCAlgorithm.TransformBytes(a_data, a_index, a_length);
end;

function TCRC16.TransformFinal: IHashResult;
begin
  result := FCRCAlgorithm.TransformFinal();
end;

{ TCRC16_BUYPASS }

constructor TCRC16_BUYPASS.Create;
begin
  Inherited Create(TCRC16Polynomials.BUYPASS, $0000, false, false, $0000, $FEE8,
    THashLibStringArray.Create('CRC-16/BUYPASS', 'CRC-16/VERIFONE'));
end;

end.
