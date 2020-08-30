unit HlpAP;

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
  TAP = class sealed(THash, IHash32, ITransformBlock)
  strict private
  var
    FHash: UInt32;
    FIndex: Int32;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;

  end;

implementation

{ TAP }

function TAP.Clone(): IHash;
var
  LHashInstance: TAP;
begin
  LHashInstance := TAP.Create();
  LHashInstance.FHash := FHash;
  LHashInstance.FIndex := FIndex;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TAP.Create;
begin
  Inherited Create(4, 1);
end;

procedure TAP.Initialize;
begin
  FHash := $AAAAAAAA;
  FIndex := 0;
end;

procedure TAP.TransformBytes(const AData: THashLibByteArray;
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

    if (FIndex and 1) = 0 then
    begin
      FHash := FHash xor ((FHash shl 7) xor AData[LIdx] * (FHash shr 3))
    end
    else
    begin
      FHash := FHash xor
        (not((FHash shl 11) xor AData[LIdx] xor (FHash shr 5)));
    end;

    System.Inc(FIndex);
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TAP.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
