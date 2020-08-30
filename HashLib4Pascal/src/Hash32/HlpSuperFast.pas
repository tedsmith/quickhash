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
    function ComputeAggregatedBytes(const AData: THashLibByteArray)
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
  LHashInstance: TSuperFast;
begin
  LHashInstance := TSuperFast.Create();
  FBuffer.Position := 0;
  LHashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TSuperFast.ComputeAggregatedBytes(const AData: THashLibByteArray)
  : IHashResult;
var
  LHash, LTemp, U1: UInt32;
  LLength, LCurrentIndex, I1, I2: Int32;
begin
  LLength := System.length(AData);

  if (LLength = 0) then
  begin
    result := THashResult.Create(Int32(0));
    Exit;
  end;

  LHash := UInt32(LLength);

  LCurrentIndex := 0;

  while (LLength >= 4) do
  begin
    I1 := AData[LCurrentIndex];
    System.Inc(LCurrentIndex);
    I2 := AData[LCurrentIndex] shl 8;
    System.Inc(LCurrentIndex);
    LHash := UInt16(LHash + UInt32(I1 or I2));
    U1 := UInt32(AData[LCurrentIndex]);
    System.Inc(LCurrentIndex);
    LTemp := UInt32((Byte(U1) or AData[LCurrentIndex] shl 8) shl 11) xor LHash;
    System.Inc(LCurrentIndex);
    LHash := (LHash shl 16) xor LTemp;
    LHash := LHash + (LHash shr 11);

    System.Dec(LLength, 4);
  end;

  case LLength of
    3:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        LHash := LHash + UInt16(I1 or I2 shl 8);
        LHash := LHash xor (LHash shl 16);
        LHash := LHash xor (UInt32(AData[LCurrentIndex]) shl 18);
        LHash := LHash + (LHash shr 11);
      end;

    2:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex];
        LHash := LHash + UInt16(I1 or I2 shl 8);
        LHash := LHash xor (LHash shl 11);
        LHash := LHash + (LHash shr 17);
      end;

    1:
      begin
        I1 := AData[LCurrentIndex];

        LHash := LHash + UInt32(I1);
        LHash := LHash xor (LHash shl 10);
        LHash := LHash + (LHash shr 1);
      end;

  end;

  LHash := LHash xor (LHash shl 3);
  LHash := LHash + (LHash shr 5);
  LHash := LHash xor (LHash shl 4);
  LHash := LHash + (LHash shr 17);
  LHash := LHash xor (LHash shl 25);
  LHash := LHash + (LHash shr 6);

  result := THashResult.Create(LHash);
end;

end.
