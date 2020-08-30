unit HlpDJB;

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
  TDJB = class sealed(THash, IHash32, ITransformBlock)
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

{ TDJB }

function TDJB.Clone(): IHash;
var
  LHashInstance: TDJB;
begin
  LHashInstance := TDJB.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TDJB.Create;
begin
  Inherited Create(4, 1);
end;

procedure TDJB.Initialize;
begin
  FHash := 5381;
end;

procedure TDJB.TransformBytes(const AData: THashLibByteArray;
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
    FHash := ((FHash shl 5) + FHash) + AData[LIdx];
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TDJB.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
