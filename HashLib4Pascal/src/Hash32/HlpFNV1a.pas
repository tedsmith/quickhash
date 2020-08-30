unit HlpFNV1a;

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
  TFNV1a = class sealed(THash, IHash32, ITransformBlock)
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

{ TFNV1a }

function TFNV1a.Clone(): IHash;
var
  LHashInstance: TFNV1a;
begin
  LHashInstance := TFNV1a.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TFNV1a.Create;
begin
  Inherited Create(4, 1);
end;

procedure TFNV1a.Initialize;
begin
  FHash := 2166136261;
end;

procedure TFNV1a.TransformBytes(const AData: THashLibByteArray;
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
    FHash := (FHash xor AData[LIdx]) * 16777619;
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TFNV1a.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
