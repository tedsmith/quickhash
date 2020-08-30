unit HlpELF;

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
  TELF = class sealed(THash, IHash32, ITransformBlock)
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

{ TELF }

function TELF.Clone(): IHash;
var
  LHashInstance: TELF;
begin
  LHashInstance := TELF.Create();
  LHashInstance.FHash := FHash;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TELF.Create;
begin
  Inherited Create(4, 1);
end;

procedure TELF.Initialize;
begin
  FHash := 0;
end;

procedure TELF.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LIdx: Int32;
  LG: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  LIdx := AIndex;
  while ALength > 0 do
  begin
    FHash := (FHash shl 4) + AData[LIdx];
    LG := FHash and $F0000000;

    if (LG <> 0) then
    begin
      FHash := FHash xor (LG shr 24);
    end;

    FHash := FHash and (not LG);
    System.Inc(LIdx);
    System.Dec(ALength);
  end;
end;

function TELF.TransformFinal: IHashResult;
begin
  result := THashResult.Create(FHash);
  Initialize();
end;

end.
