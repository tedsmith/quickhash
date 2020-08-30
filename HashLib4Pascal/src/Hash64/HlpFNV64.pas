unit HlpFNV64;

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
  TFNV64 = class sealed(THash, IHash64, ITransformBlock)
  strict private
  var
    FHash: UInt64;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TFNV64 }

function TFNV64.Clone(): IHash;
var
  LHashInstance: TFNV64;
begin
  LHashInstance := TFNV64.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TFNV64.Create;
begin
  Inherited Create(8, 1);
end;

procedure TFNV64.Initialize;
begin
  FHash := 0;
end;

procedure TFNV64.TransformBytes(const AData: THashLibByteArray;
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
    FHash := UInt64(FHash * UInt64(1099511628211)) xor AData[LIdx];
    System.Inc(LIdx);
    System.Dec(ALength);
  end;

end;

function TFNV64.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
