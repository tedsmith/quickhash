unit HlpShiftAndXor;

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
  TShiftAndXor = class sealed(THash, IHash32, ITransformBlock)
  strict private
  var
    FHash: UInt32;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TShiftAndXor }

function TShiftAndXor.Clone(): IHash;
var
  LHashInstance: TShiftAndXor;
begin
  LHashInstance := TShiftAndXor.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TShiftAndXor.Create;
begin
  Inherited Create(4, 1);
end;

procedure TShiftAndXor.Initialize;
begin
  FHash := 0;
end;

procedure TShiftAndXor.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LIdx: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  LIdx := AIndex;
  while ALength > 0 do
  begin
    FHash := FHash xor ((FHash shl 5) + (FHash shr 2) + AData[LIdx]);
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TShiftAndXor.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
