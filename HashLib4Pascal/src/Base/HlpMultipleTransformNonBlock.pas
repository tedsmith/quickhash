unit HlpMultipleTransformNonBlock;

{$I ..\Include\HashLib.inc}

interface

uses
  Classes,
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpIHashResult;

type

  TMultipleTransformNonBlock = class abstract(THash, INonBlockHash)

  strict private

    function Aggregate(): THashLibByteArray;

  strict protected
  var
    FBuffer: TMemoryStream;

    function ComputeAggregatedBytes(const AData: THashLibByteArray)
      : IHashResult; virtual; abstract;

  public
    constructor Create(AHashSize, ABlockSize: Int32);
    destructor Destroy; override;
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function ComputeBytes(const AData: THashLibByteArray): IHashResult;
      override;

  end;

implementation

{ TMultipleTransformNonBlock }

function TMultipleTransformNonBlock.Aggregate: THashLibByteArray;
begin
  Result := Nil;
  if FBuffer.Size > 0 then
  begin
    FBuffer.Position := 0;
    System.SetLength(Result, FBuffer.Size);
    FBuffer.Read(Result[0], FBuffer.Size);
  end;
end;

constructor TMultipleTransformNonBlock.Create(AHashSize, ABlockSize: Int32);
begin
  Inherited Create(AHashSize, ABlockSize);
  FBuffer := TMemoryStream.Create();
end;

destructor TMultipleTransformNonBlock.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TMultipleTransformNonBlock.Initialize;
begin
  FBuffer.Clear;
  FBuffer.SetSize(Int64(0));
end;

procedure TMultipleTransformNonBlock.TransformBytes
  (const AData: THashLibByteArray; AIndex, ALength: Int32);
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  FBuffer.Write(AData[AIndex], ALength);
end;

function TMultipleTransformNonBlock.TransformFinal: IHashResult;
begin
  Result := ComputeAggregatedBytes(Aggregate());
  Initialize();
end;

function TMultipleTransformNonBlock.ComputeBytes(const AData: THashLibByteArray)
  : IHashResult;
begin
  Initialize();
  Result := ComputeAggregatedBytes(AData);
end;

end.
