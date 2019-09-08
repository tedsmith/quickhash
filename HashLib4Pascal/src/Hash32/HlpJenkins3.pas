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
    function ComputeAggregatedBytes(const a_data: THashLibByteArray)
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
  HashInstance: TJenkins3;
begin
  HashInstance := TJenkins3.Create();
  FBuffer.Position := 0;
  HashInstance.FInitialValue := FInitialValue;
  HashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TJenkins3.ComputeAggregatedBytes(const a_data: THashLibByteArray)
  : IHashResult;
var
  &length, currentIndex, i1, i2, i3, i4: Int32;
  a, b, c: UInt32;
begin
  length := System.length(a_data);
  a := UInt32($DEADBEEF) + UInt32(length) + UInt32(FInitialValue);
  b := a;
  c := b;
  if (length = 0) then
  begin
    result := THashResult.Create(c);
    Exit;
  end;

  currentIndex := 0;
  while (length > 12) do
  begin

    i1 := a_data[currentIndex];
    System.Inc(currentIndex);
    i2 := a_data[currentIndex] shl 8;
    System.Inc(currentIndex);
    i3 := a_data[currentIndex] shl 16;
    System.Inc(currentIndex);
    i4 := a_data[currentIndex] shl 24;
    System.Inc(currentIndex);

    a := a + UInt32(i1 or i2 or i3 or i4);

    i1 := a_data[currentIndex];
    System.Inc(currentIndex);
    i2 := a_data[currentIndex] shl 8;
    System.Inc(currentIndex);
    i3 := a_data[currentIndex] shl 16;
    System.Inc(currentIndex);
    i4 := a_data[currentIndex] shl 24;
    System.Inc(currentIndex);

    b := b + UInt32(i1 or i2 or i3 or i4);

    i1 := a_data[currentIndex];
    System.Inc(currentIndex);
    i2 := a_data[currentIndex] shl 8;
    System.Inc(currentIndex);
    i3 := a_data[currentIndex] shl 16;
    System.Inc(currentIndex);
    i4 := a_data[currentIndex] shl 24;
    System.Inc(currentIndex);

    c := c + UInt32(i1 or i2 or i3 or i4);

    a := a - c;
    a := a xor TBits.RotateLeft32(c, 4);
    c := c + b;
    b := b - a;
    b := b xor TBits.RotateLeft32(a, 6);
    a := a + c;
    c := c - b;
    c := c xor TBits.RotateLeft32(b, 8);
    b := b + a;
    a := a - c;
    a := a xor TBits.RotateLeft32(c, 16);
    c := c + b;
    b := b - a;
    b := b xor TBits.RotateLeft32(a, 19);
    a := a + c;
    c := c - b;
    c := c xor TBits.RotateLeft32(b, 4);
    b := b + a;

    System.Dec(length, 12);
  end;

  case length of
    12:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        a := a + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        b := b + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;

        c := c + UInt32(i1 or i2 or i3 or i4);
      end;

    11:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        a := a + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        b := b + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;

        c := c + UInt32(i1 or i2 or i3);

      end;

    10:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        a := a + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        b := b + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;

        c := c + UInt32(i1 or i2);

      end;

    9:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        a := a + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        b := b + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];

        c := c + UInt32(i1);

      end;

    8:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        a := a + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;

        b := b + UInt32(i1 or i2 or i3 or i4);

      end;

    7:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        a := a + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;

        b := b + UInt32(i1 or i2 or i3);

      end;

    6:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        a := a + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;

        b := b + UInt32(i1 or i2);

      end;

    5:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;
        System.Inc(currentIndex);

        a := a + UInt32(i1 or i2 or i3 or i4);

        i1 := a_data[currentIndex];

        b := b + UInt32(i1);

      end;

    4:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;
        System.Inc(currentIndex);
        i4 := a_data[currentIndex] shl 24;

        a := a + UInt32(i1 or i2 or i3 or i4);

      end;

    3:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;
        System.Inc(currentIndex);
        i3 := a_data[currentIndex] shl 16;

        a := a + UInt32(i1 or i2 or i3);

      end;

    2:
      begin
        i1 := a_data[currentIndex];
        System.Inc(currentIndex);
        i2 := a_data[currentIndex] shl 8;

        a := a + UInt32(i1 or i2);

      end;

    1:
      begin
        i1 := a_data[currentIndex];

        a := a + UInt32(i1);

      end;

  end;

  c := c xor b;
  c := c - TBits.RotateLeft32(b, 14);
  a := a xor c;
  a := a - TBits.RotateLeft32(c, 11);
  b := b xor a;
  b := b - TBits.RotateLeft32(a, 25);
  c := c xor b;
  c := c - TBits.RotateLeft32(b, 16);
  a := a xor c;
  a := a - TBits.RotateLeft32(c, 4);
  b := b xor a;
  b := b - TBits.RotateLeft32(a, 14);
  c := c xor b;
  c := c - TBits.RotateLeft32(b, 24);

  result := THashResult.Create(c);

end;

end.
