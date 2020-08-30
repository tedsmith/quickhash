unit HlpSDBM;

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
  TSDBM = class sealed(THash, IHash32, ITransformBlock)
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

{ TSDBM }

function TSDBM.Clone(): IHash;
var
  LHashInstance: TSDBM;
begin
  LHashInstance := TSDBM.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSDBM.Create;
begin
  Inherited Create(4, 1);
end;

procedure TSDBM.Initialize;
begin
  FHash := 0;
end;

procedure TSDBM.TransformBytes(const AData: THashLibByteArray;
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
    FHash := UInt32(AData[LIdx] + Int64(FHash shl 6) +
      Int64(FHash shl 16) - FHash);
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TSDBM.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
