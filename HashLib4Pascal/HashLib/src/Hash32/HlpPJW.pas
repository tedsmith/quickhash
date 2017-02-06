unit HlpPJW;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TPJW = class sealed(THash, IHash32, IBlockHash, ITransformBlock)
  strict private

    Fm_hash: UInt32;

  const
    UInt32MaxValue = UInt32(4294967295);
    BitsInUnsignedInt = Int32(System.SizeOf(UInt32) * 8);
    ThreeQuarters = Int32(BitsInUnsignedInt * 3) shr 2;
    OneEighth = Int32(BitsInUnsignedInt shr 3);
    HighBits = UInt32(UInt32MaxValue shl (BitsInUnsignedInt - OneEighth));

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
  end;

implementation

{ TPJW }

constructor TPJW.Create;
begin
  Inherited Create(4, 1);
end;

procedure TPJW.Initialize;
begin
  Fm_hash := 0;
end;

procedure TPJW.TransformBytes(a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  i: Int32;
  test: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  i := a_index;
  while a_length > 0 do
  begin
    Fm_hash := (Fm_hash shl OneEighth) + a_data[i];
    test := Fm_hash and HighBits;
    if (test <> 0) then
      Fm_hash := ((Fm_hash xor (test shr ThreeQuarters)) and (not HighBits));
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TPJW.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
