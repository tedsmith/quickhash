unit HlpBKDR;

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

  TBKDR = class sealed(THash, IHash32, ITransformBlock)
  strict private
  var
    FHash: UInt32;

  const
    SEED = Int32(131);

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TBKDR }

function TBKDR.Clone(): IHash;
var
  LHashInstance: TBKDR;
begin
  LHashInstance := TBKDR.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TBKDR.Create;
begin
  Inherited Create(4, 1);
end;

procedure TBKDR.Initialize;
begin
  FHash := 0;
end;

procedure TBKDR.TransformBytes(const AData: THashLibByteArray;
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
    FHash := (FHash * UInt32(SEED)) + AData[LIdx];
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TBKDR.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
