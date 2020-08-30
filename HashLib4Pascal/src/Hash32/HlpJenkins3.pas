unit HlpJenkins3;

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

  TJenkins3 = class sealed(TMultipleTransformNonBlock, IHash32, ITransformBlock)
  strict private
  var
    FInitialValue: Int32;

  strict protected
    function ComputeAggregatedBytes(const AData: THashLibByteArray)
      : IHashResult; override;
  public
    constructor Create(AInitialValue: Int32 = 0);
    function Clone(): IHash; override;

  end;

implementation

{ TJenkins3 }

constructor TJenkins3.Create(AInitialValue: Int32);
begin
  Inherited Create(4, 12);
  FInitialValue := AInitialValue;
end;

function TJenkins3.Clone(): IHash;
var
  LHashInstance: TJenkins3;
begin
  LHashInstance := TJenkins3.Create();
  FBuffer.Position := 0;
  LHashInstance.FInitialValue := FInitialValue;
  LHashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TJenkins3.ComputeAggregatedBytes(const AData: THashLibByteArray)
  : IHashResult;
var
  LLength, LCurrentIndex, I1, I2, I3, I4: Int32;
  LA, LB, LC: UInt32;
begin
  LLength := System.length(AData);
  LA := UInt32($DEADBEEF) + UInt32(LLength) + UInt32(FInitialValue);
  LB := LA;
  LC := LB;
  if (LLength = 0) then
  begin
    result := THashResult.Create(LC);
    Exit;
  end;

  LCurrentIndex := 0;
  while (LLength > 12) do
  begin
    I1 := AData[LCurrentIndex];
    System.Inc(LCurrentIndex);
    I2 := AData[LCurrentIndex] shl 8;
    System.Inc(LCurrentIndex);
    I3 := AData[LCurrentIndex] shl 16;
    System.Inc(LCurrentIndex);
    I4 := AData[LCurrentIndex] shl 24;
    System.Inc(LCurrentIndex);

    LA := LA + UInt32(I1 or I2 or I3 or I4);

    I1 := AData[LCurrentIndex];
    System.Inc(LCurrentIndex);
    I2 := AData[LCurrentIndex] shl 8;
    System.Inc(LCurrentIndex);
    I3 := AData[LCurrentIndex] shl 16;
    System.Inc(LCurrentIndex);
    I4 := AData[LCurrentIndex] shl 24;
    System.Inc(LCurrentIndex);

    LB := LB + UInt32(I1 or I2 or I3 or I4);

    I1 := AData[LCurrentIndex];
    System.Inc(LCurrentIndex);
    I2 := AData[LCurrentIndex] shl 8;
    System.Inc(LCurrentIndex);
    I3 := AData[LCurrentIndex] shl 16;
    System.Inc(LCurrentIndex);
    I4 := AData[LCurrentIndex] shl 24;
    System.Inc(LCurrentIndex);

    LC := LC + UInt32(I1 or I2 or I3 or I4);

    LA := LA - LC;
    LA := LA xor TBits.RotateLeft32(LC, 4);
    LC := LC + LB;
    LB := LB - LA;
    LB := LB xor TBits.RotateLeft32(LA, 6);
    LA := LA + LC;
    LC := LC - LB;
    LC := LC xor TBits.RotateLeft32(LB, 8);
    LB := LB + LA;
    LA := LA - LC;
    LA := LA xor TBits.RotateLeft32(LC, 16);
    LC := LC + LB;
    LB := LB - LA;
    LB := LB xor TBits.RotateLeft32(LA, 19);
    LA := LA + LC;
    LC := LC - LB;
    LC := LC xor TBits.RotateLeft32(LB, 4);
    LB := LB + LA;

    System.Dec(LLength, 12);
  end;

  case LLength of
    12:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LA := LA + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LB := LB + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;

        LC := LC + UInt32(I1 or I2 or I3 or I4);
      end;

    11:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LA := LA + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LB := LB + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;

        LC := LC + UInt32(I1 or I2 or I3);
      end;

    10:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LA := LA + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LB := LB + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;

        LC := LC + UInt32(I1 or I2);
      end;

    9:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LA := LA + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LB := LB + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];

        LC := LC + UInt32(I1);
      end;

    8:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LA := LA + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;

        LB := LB + UInt32(I1 or I2 or I3 or I4);
      end;

    7:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LA := LA + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;

        LB := LB + UInt32(I1 or I2 or I3);
      end;

    6:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LA := LA + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;

        LB := LB + UInt32(I1 or I2);
      end;

    5:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;
        System.Inc(LCurrentIndex);

        LA := LA + UInt32(I1 or I2 or I3 or I4);

        I1 := AData[LCurrentIndex];

        LB := LB + UInt32(I1);
      end;

    4:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;
        System.Inc(LCurrentIndex);
        I4 := AData[LCurrentIndex] shl 24;

        LA := LA + UInt32(I1 or I2 or I3 or I4);
      end;

    3:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;
        System.Inc(LCurrentIndex);
        I3 := AData[LCurrentIndex] shl 16;

        LA := LA + UInt32(I1 or I2 or I3);
      end;

    2:
      begin
        I1 := AData[LCurrentIndex];
        System.Inc(LCurrentIndex);
        I2 := AData[LCurrentIndex] shl 8;

        LA := LA + UInt32(I1 or I2);
      end;

    1:
      begin
        I1 := AData[LCurrentIndex];

        LA := LA + UInt32(I1);
      end;
  end;

  LC := LC xor LB;
  LC := LC - TBits.RotateLeft32(LB, 14);
  LA := LA xor LC;
  LA := LA - TBits.RotateLeft32(LC, 11);
  LB := LB xor LA;
  LB := LB - TBits.RotateLeft32(LA, 25);
  LC := LC xor LB;
  LC := LC - TBits.RotateLeft32(LB, 16);
  LA := LA xor LC;
  LA := LA - TBits.RotateLeft32(LC, 4);
  LB := LB xor LA;
  LB := LB - TBits.RotateLeft32(LA, 14);
  LC := LC xor LB;
  LC := LC - TBits.RotateLeft32(LB, 24);

  result := THashResult.Create(LC);
end;

end.
