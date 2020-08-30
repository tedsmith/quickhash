unit HlpOneAtTime;

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
  TOneAtTime = class sealed(THash, IHash32, ITransformBlock)
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

{ TOneAtTime }

function TOneAtTime.Clone(): IHash;
var
  LHashInstance: TOneAtTime;
begin
  LHashInstance := TOneAtTime.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TOneAtTime.Create;
begin
  Inherited Create(4, 1);
end;

procedure TOneAtTime.Initialize;
begin
  FHash := 0;
end;

procedure TOneAtTime.TransformBytes(const AData: THashLibByteArray;
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
    FHash := FHash + AData[LIdx];
    FHash := FHash + (FHash shl 10);
    FHash := FHash xor (FHash shr 6);
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TOneAtTime.TransformFinal: IHashResult;
begin
  FHash := FHash + (FHash shl 3);
  FHash := FHash xor (FHash shr 11);
  FHash := FHash + (FHash shl 15);

  result := THashResult.Create(FHash);
  Initialize();
end;

end.
