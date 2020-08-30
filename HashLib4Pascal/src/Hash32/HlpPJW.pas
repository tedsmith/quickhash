unit HlpPJW;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TPJW = class sealed(THash, IHash32, ITransformBlock)
  strict private
  var
    FHash: UInt32;

  const
    UInt32MaxValue = UInt32(4294967295);
    BitsInUnsignedInt = Int32(System.SizeOf(UInt32) * 8);
    ThreeQuarters = Int32(BitsInUnsignedInt * 3) shr 2;
    OneEighth = Int32(BitsInUnsignedInt shr 3);
    HighBits = UInt32(UInt32MaxValue shl (BitsInUnsignedInt - OneEighth));

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TPJW }

function TPJW.Clone(): IHash;
var
  LHashInstance: TPJW;
begin
  LHashInstance := TPJW.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TPJW.Create;
begin
  Inherited Create(4, 1);
end;

procedure TPJW.Initialize;
begin
  FHash := 0;
end;

procedure TPJW.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LIdx: Int32;
  LTest: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  LIdx := AIndex;
  while ALength > 0 do
  begin
    FHash := (FHash shl OneEighth) + AData[LIdx];
    LTest := FHash and HighBits;
    if (LTest <> 0) then
    begin
      FHash := ((FHash xor (LTest shr ThreeQuarters)) and (not HighBits));
    end;
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TPJW.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
