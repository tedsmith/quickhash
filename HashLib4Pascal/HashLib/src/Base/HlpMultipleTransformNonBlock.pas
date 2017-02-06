unit HlpMultipleTransformNonBlock;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI}
{$IFDEF HAS_UNITSCOPE}
  System.Generics.Collections,
{$ELSE}
  Generics.Collections,
{$ENDIF HAS_UNITSCOPE}
{$ELSE}
{$IFDEF FPC}
  fgl,
{$ENDIF FPC}
{$ENDIF DELPHI}
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpIHashResult;

type

  TMultipleTransformNonBlock = class abstract(THash, INonBlockHash)

  strict private

    Fm_list: {$IFDEF DELPHI} TList<THashLibByteArray>
{$ELSE} TFPGList<THashLibByteArray> {$ENDIF DELPHI};

    function Aggregate(): THashLibByteArray;

  strict protected
    function ComputeAggregatedBytes(a_data: THashLibByteArray): IHashResult;
      virtual; abstract;

  public
    constructor Create(a_hash_size, a_block_size: Int32);
    destructor Destroy; override;
    procedure Initialize(); override;
    procedure TransformBytes(a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function ComputeBytes(a_data: THashLibByteArray): IHashResult; override;

  end;

implementation

{ TMultipleTransformNonBlock }

function TMultipleTransformNonBlock.Aggregate: THashLibByteArray;
var
  sum, index: Int32;
  arr: THashLibByteArray;
begin
  sum := 0;
  for arr in Fm_list do
  begin
    sum := sum + System.Length(arr);
  end;

  System.SetLength(result, sum);
  index := 0;

  for arr in Fm_list do

  begin
    System.Move(arr[0], result[index], System.Length(arr) *
      System.SizeOf(Byte));
    index := index + System.Length(arr);
  end;

end;

constructor TMultipleTransformNonBlock.Create(a_hash_size, a_block_size: Int32);
begin
  Inherited Create(a_hash_size, a_block_size);
  Fm_list := {$IFDEF DELPHI} TList<THashLibByteArray>
{$ELSE} TFPGList<THashLibByteArray> {$ENDIF DELPHI}.Create();
end;

destructor TMultipleTransformNonBlock.Destroy;
begin
  Fm_list.Free;
  inherited Destroy;
end;

procedure TMultipleTransformNonBlock.Initialize;
begin
  Fm_list.Clear;
end;

procedure TMultipleTransformNonBlock.TransformBytes(a_data: THashLibByteArray;
  a_index, a_length: Int32);

begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  Fm_list.Add(System.Copy(a_data, a_index, a_length));

end;

function TMultipleTransformNonBlock.TransformFinal: IHashResult;
begin
  result := ComputeAggregatedBytes(Aggregate());
  Initialize();
end;

function TMultipleTransformNonBlock.ComputeBytes(a_data: THashLibByteArray)
  : IHashResult;
begin
  Initialize();
  result := ComputeAggregatedBytes(a_data);
end;

end.
