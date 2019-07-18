unit HlpSuperFast;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpMultipleTransformNonBlock;

type

  TSuperFast = class sealed(TMultipleTransformNonBlock, IHash32,
    ITransformBlock)

  strict protected
    function ComputeAggregatedBytes(const a_data: THashLibByteArray)
      : IHashResult; override;
  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TSuperFast }

constructor TSuperFast.Create;
begin
  Inherited Create(4, 4);
end;

function TSuperFast.Clone(): IHash;
var
  HashInstance: TSuperFast;
begin
  HashInstance := TSuperFast.Create();
  FBuffer.Position := 0;
  HashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TSuperFast.ComputeAggregatedBytes(const a_data: THashLibByteArray)
  : IHashResult;
var
  hash, tmp, u1: UInt32;
  &length, currentIndex, i1, i2: Int32;
begin
  length := System.length(a_data);

  if (length = 0) then
  begin
    result := THashResult.Create(Int32(0));
    Exit;
  end;

  hash := UInt32(length);

  currentIndex := 0;

  while (length >= 4) do
  begin
    i1 := a_data[currentIndex];
    System.Inc(currentIndex);
    i2 := a_data[currentIndex] shl 8;
    System.Inc(currentIndex);
    hash := UInt16(hash + UInt32(i1 or i2));
    u1 := UInt32(a_data[currentIndex]);
    System.Inc(currentIndex);
    tmp := UInt32((Byte(u1) or a_data[currentIndex] shl 8) shl 11) xor hash;
    System.Inc(currentIndex);
    hash := (hash shl 16) xor tmp;
    hash := hash + (hash shr 11);

    System.Dec(length, 4);
  end;

  case length of
    3:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex];
        System.Inc(currentIndex);
        hash := hash + UInt16(i1 or i2 shl 8);
        hash := hash xor (hash shl 16);
        hash := hash xor (UInt32(a_data[currentIndex]) shl 18);
        hash := hash + (hash shr 11);

      end;

    2:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex];
        hash := hash + UInt16(i1 or i2 shl 8);
        hash := hash xor (hash shl 11);
        hash := hash + (hash shr 17);

      end;

    1:
      begin
        i1 := a_data[currentIndex];

        hash := hash + UInt32(i1);
        hash := hash xor (hash shl 10);
        hash := hash + (hash shr 1);

      end;

  end;

  hash := hash xor (hash shl 3);
  hash := hash + (hash shr 5);
  hash := hash xor (hash shl 4);
  hash := hash + (hash shr 17);
  hash := hash xor (hash shl 25);
  hash := hash + (hash shr 6);

  result := THashResult.Create(hash);
end;

end.
