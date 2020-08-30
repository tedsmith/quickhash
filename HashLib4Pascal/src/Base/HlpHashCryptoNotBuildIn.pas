unit HlpHashCryptoNotBuildIn;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpHashBuffer,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TBlockHash = class abstract(THash, IBlockHash)
  strict protected
  var
    FBuffer: THashBuffer;
    FProcessedBytesCount: UInt64;

    procedure TransformBuffer(); inline;
    procedure Finish(); virtual; abstract;
    procedure TransformBlock(AData: PByte; ADataLength: Int32; AIndex: Int32);
      virtual; abstract;
    function GetResult(): THashLibByteArray; virtual; abstract;

  public
    constructor Create(AHashSize, ABlockSize: Int32; ABufferSize: Int32 = -1);
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    procedure Initialize(); override;
    function TransformFinal(): IHashResult; override;
  end;

implementation

{ TBlockHash }

constructor TBlockHash.Create(AHashSize, ABlockSize, ABufferSize: Int32);
begin
  Inherited Create(AHashSize, ABlockSize);
  if (ABufferSize = -1) then
  begin
    ABufferSize := ABlockSize;
  end;
  FBuffer := THashBuffer.Create(ABufferSize);
end;

procedure TBlockHash.Initialize;
begin
  FBuffer.Initialize();
  FProcessedBytesCount := 0;
end;

procedure TBlockHash.TransformBuffer;
begin
{$IFDEF DEBUG}
  System.Assert(FBuffer.IsFull);
{$ENDIF DEBUG}
  TransformBlock(PByte(FBuffer.GetBytes()), FBuffer.Length, 0);
end;

procedure TBlockHash.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LPtrData: PByte;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  LPtrData := PByte(AData);

  if (not FBuffer.IsEmpty) then
  begin
    if (FBuffer.Feed(LPtrData, System.Length(AData), AIndex, ALength,
      FProcessedBytesCount)) then
    begin
      TransformBuffer();
    end;
  end;

  while (ALength >= (FBuffer.Length)) do
  begin
    FProcessedBytesCount := FProcessedBytesCount + UInt64(FBuffer.Length);
    TransformBlock(LPtrData, FBuffer.Length, AIndex);
    AIndex := AIndex + (FBuffer.Length);
    ALength := ALength - (FBuffer.Length);
  end;

  if (ALength > 0) then
  begin
    FBuffer.Feed(LPtrData, System.Length(AData), AIndex, ALength,
      FProcessedBytesCount);
  end;
end;

function TBlockHash.TransformFinal: IHashResult;
var
  LTempResult: THashLibByteArray;
begin
  Finish();

{$IFDEF DEBUG}
  System.Assert(FBuffer.IsEmpty);
{$ENDIF DEBUG}
  LTempResult := GetResult();
{$IFDEF DEBUG}
  System.Assert(System.Length(LTempResult) = HashSize);
{$ENDIF DEBUG}
  Initialize();

  result := THashResult.Create(LTempResult);
end;

end.
