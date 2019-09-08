unit HlpDEK;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
{$ENDIF DELPHI}
  HlpBits,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpMultipleTransformNonBlock;

type

  TDEK = class sealed(TMultipleTransformNonBlock, IHash32, ITransformBlock)

  strict protected
    function ComputeAggregatedBytes(const a_data: THashLibByteArray)
      : IHashResult; override;
  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TDEK }

constructor TDEK.Create;
begin
  Inherited Create(4, 1);
end;

function TDEK.Clone(): IHash;
var
  HashInstance: TDEK;
begin
  HashInstance := TDEK.Create();
  FBuffer.Position := 0;
  HashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TDEK.ComputeAggregatedBytes(const a_data: THashLibByteArray)
  : IHashResult;
var
  hash: UInt32;
  i: Int32;
begin
  hash := UInt32(System.Length(a_data));
  for i := 0 to System.Length(a_data) - 1 do
  begin

    hash := TBits.RotateLeft32(hash, 5) xor a_data[i];
  end;

  result := THashResult.Create(hash);
end;

end.
