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
    function ComputeAggregatedBytes(const AData: THashLibByteArray)
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
  LHashInstance: TDEK;
begin
  LHashInstance := TDEK.Create();
  FBuffer.Position := 0;
  LHashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TDEK.ComputeAggregatedBytes(const AData: THashLibByteArray)
  : IHashResult;
var
  LHash: UInt32;
  LIdx: Int32;
begin
  LHash := UInt32(System.Length(AData));
  for LIdx := 0 to System.Length(AData) - 1 do
  begin
    LHash := TBits.RotateLeft32(LHash, 5) xor AData[LIdx];
  end;
  result := THashResult.Create(LHash);
end;

end.
