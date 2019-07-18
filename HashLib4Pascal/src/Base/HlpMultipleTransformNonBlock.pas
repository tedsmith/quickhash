unit HlpMultipleTransformNonBlock;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF HAS_UNITSCOPE}
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpIHashResult;

type

  TMultipleTransformNonBlock = class abstract(THash, INonBlockHash)

  strict private

    function Aggregate(): THashLibByteArray;

  strict protected
    FBuffer: TMemoryStream;

    function ComputeAggregatedBytes(const a_data: THashLibByteArray)
      : IHashResult; virtual; abstract;

  public
    constructor Create(a_hash_size, a_block_size: Int32);
    destructor Destroy; override;
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function ComputeBytes(const a_data: THashLibByteArray)
      : IHashResult; override;

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

constructor TMultipleTransformNonBlock.Create(a_hash_size, a_block_size: Int32);
begin
  Inherited Create(a_hash_size, a_block_size);
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

procedure TMultipleTransformNonBlock.TransformBytes(const a_data
  : THashLibByteArray; a_index, a_length: Int32);
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  FBuffer.Write(a_data[a_index], a_length);
end;

function TMultipleTransformNonBlock.TransformFinal: IHashResult;
begin
  Result := ComputeAggregatedBytes(Aggregate());
  Initialize();
end;

function TMultipleTransformNonBlock.ComputeBytes(const a_data
  : THashLibByteArray): IHashResult;
begin
  Initialize();
  Result := ComputeAggregatedBytes(a_data);
end;

end.
