unit HlpRadioGatun32;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpBitConverter,
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpBits,
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpArrayUtils,
  HlpHashCryptoNotBuildIn;

type
  TRadioGatun32 = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private

    Fm_mill: THashLibUInt32Array;

    Fm_belt: THashLibMatrixUInt32Array;

    procedure RoundFunction();

  strict protected
    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TRadioGatun32 }

function TRadioGatun32.Clone(): IHash;
var
  HashInstance: TRadioGatun32;
  Idx: Int32;
begin
  HashInstance := TRadioGatun32.Create();
  HashInstance.Fm_mill := System.Copy(Fm_mill);
  // since System.Copy() does not support jagged arrays (multidimensional dynamic arrays, we improvise)
  for Idx := System.Low(Fm_belt) to System.High(Fm_belt) do
  begin
    HashInstance.Fm_belt[Idx] := System.Copy(Fm_belt[Idx]);
  end;
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRadioGatun32.Create;
var
  i: Int32;
begin

  Inherited Create(32, 12);
  System.SetLength(Fm_mill, 19);

  System.SetLength(Fm_belt, 13);
  i := 0;
  while i < 13 do
  begin
    System.SetLength(Fm_belt[i], 3);

    System.Inc(i);
  end;

end;

procedure TRadioGatun32.Finish;
var
  padding_size, i: Int32;
  pad: THashLibByteArray;
begin
  padding_size := 12 - ((Int32(Fm_processed_bytes)) mod 12);

  System.SetLength(pad, padding_size);
  pad[0] := $01;
  TransformBytes(pad, 0, padding_size);
  i := 0;
  while i < 16 do
  begin
    RoundFunction();
    System.Inc(i);
  end;

end;

function TRadioGatun32.GetResult: THashLibByteArray;
var
  tempRes: THashLibUInt32Array;
  i: Int32;
begin
  System.SetLength(tempRes, 8);

  System.SetLength(result, System.Length(tempRes) * System.SizeOf(UInt32));

  i := 0;

  while i < 4 do
  begin
    RoundFunction();

    System.Move(Fm_mill[1], tempRes[i * 2], 2 * System.SizeOf(UInt32));
    System.Inc(i);
  end;

  TConverters.le32_copy(PCardinal(tempRes), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TRadioGatun32.Initialize;
var
  i: Int32;
begin

  TArrayUtils.ZeroFill(Fm_mill);

  i := 0;
  while i < 13 do
  begin

    TArrayUtils.ZeroFill(Fm_belt[i]);

    System.Inc(i);
  end;

  Inherited Initialize();

end;

procedure TRadioGatun32.RoundFunction;
var
  q: THashLibUInt32Array;
  a: array [0 .. 18] of UInt32;
  i: Int32;
begin

  q := Fm_belt[12];
  i := 12;
  while i > 0 do
  begin
    Fm_belt[i] := Fm_belt[i - 1];
    System.Dec(i);
  end;

  Fm_belt[0] := q;

  i := 0;
  while i < 12 do
  begin
    Fm_belt[i + 1][i mod 3] := Fm_belt[i + 1][i mod 3] xor Fm_mill[i + 1];
    System.Inc(i);
  end;

  i := 0;
  while i < 19 do
  begin
    a[i] := Fm_mill[i] xor (Fm_mill[(i + 1) mod 19] or
      not Fm_mill[(i + 2) mod 19]);
    System.Inc(i);
  end;

  i := 0;
  while i < 19 do
  begin
    Fm_mill[i] := TBits.RotateRight32(a[(7 * i) mod 19], (i * (i + 1)) shr 1);
    System.Inc(i);
  end;

  i := 0;
  while i < 19 do
  begin
    a[i] := Fm_mill[i] xor Fm_mill[(i + 1) mod 19] xor Fm_mill[(i + 4) mod 19];
    System.Inc(i);
  end;

  a[0] := a[0] xor 1;

  i := 0;
  while i < 19 do
  begin
    Fm_mill[i] := a[i];
    System.Inc(i);
  end;

  i := 0;
  while i < 3 do
  begin
    Fm_mill[i + 13] := Fm_mill[i + 13] xor q[i];
    System.Inc(i);
  end;

end;

procedure TRadioGatun32.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  data: array [0 .. 2] of UInt32;
  i: Int32;
begin
  TConverters.le32_copy(a_data, a_index, @(data[0]), 0, a_data_length);

  i := 0;
  while i < 3 do
  begin
    Fm_mill[i + 16] := Fm_mill[i + 16] xor data[i];
    Fm_belt[0][i] := Fm_belt[0][i] xor data[i];

    System.Inc(i);
  end;

  RoundFunction();

  System.FillChar(data, System.SizeOf(data), UInt32(0));
end;

end.
