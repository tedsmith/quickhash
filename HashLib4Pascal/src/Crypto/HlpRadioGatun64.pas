unit HlpRadioGatun64;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
{$IFDEF DELPHI}
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
  TRadioGatun64 = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private
  var
    FMill: THashLibUInt64Array;
    FBelt: THashLibMatrixUInt64Array;

    procedure RoundFunction();

  strict protected
    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TRadioGatun64 }

function TRadioGatun64.Clone(): IHash;
var
  LHashInstance: TRadioGatun64;
begin
  LHashInstance := TRadioGatun64.Create();
  LHashInstance.FMill := System.Copy(FMill);
  LHashInstance.FBelt := TArrayUtils.Clone(FBelt);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRadioGatun64.Create;
var
  LIdx: Int32;
begin
  Inherited Create(32, 24);
  System.SetLength(FMill, 19);

  System.SetLength(FBelt, 13);
  LIdx := 0;
  while LIdx < 13 do
  begin
    System.SetLength(FBelt[LIdx], 3);
    System.Inc(LIdx);
  end;
end;

procedure TRadioGatun64.Finish;
var
  LPaddingSize, LIdx: Int32;
  LPad: THashLibByteArray;
begin
  LPaddingSize := 24 - ((Int32(FProcessedBytesCount)) mod 24);

  System.SetLength(LPad, LPaddingSize);
  LPad[0] := $01;
  TransformBytes(LPad, 0, LPaddingSize);
  LIdx := 0;
  while LIdx < 16 do
  begin
    RoundFunction();
    System.Inc(LIdx);
  end;
end;

function TRadioGatun64.GetResult: THashLibByteArray;
var
  LBuffer: THashLibUInt64Array;
  LIdx: Int32;
begin
  System.SetLength(LBuffer, 4);

  System.SetLength(result, System.Length(LBuffer) * System.SizeOf(UInt64));
  LIdx := 0;

  while LIdx < 2 do
  begin
    RoundFunction();

    System.Move(FMill[1], LBuffer[LIdx * 2], 2 * System.SizeOf(UInt64));
    System.Inc(LIdx);
  end;

  TConverters.le64_copy(PCardinal(LBuffer), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TRadioGatun64.Initialize;
begin
  TArrayUtils.ZeroFill(FMill);
  TArrayUtils.ZeroFill(FBelt);
  Inherited Initialize();
end;

procedure TRadioGatun64.RoundFunction;
var
  LQ: THashLibUInt64Array;
  LA: array [0 .. 18] of UInt64;
  LIdx: Int32;
begin
  LQ := FBelt[12];
  LIdx := 12;
  while LIdx > 0 do
  begin
    FBelt[LIdx] := FBelt[LIdx - 1];
    System.Dec(LIdx);
  end;

  FBelt[0] := LQ;

  LIdx := 0;
  while LIdx < 12 do
  begin
    FBelt[LIdx + 1][LIdx mod 3] := FBelt[LIdx + 1][LIdx mod 3] xor FMill
      [LIdx + 1];
    System.Inc(LIdx);
  end;

  LIdx := 0;
  while LIdx < 19 do
  begin
    LA[LIdx] := FMill[LIdx] xor (FMill[(LIdx + 1) mod 19] or
      not FMill[(LIdx + 2) mod 19]);
    System.Inc(LIdx);
  end;

  LIdx := 0;
  while LIdx < 19 do
  begin
    FMill[LIdx] := TBits.RotateRight64(LA[(7 * LIdx) mod 19],
      (LIdx * (LIdx + 1)) shr 1);
    System.Inc(LIdx);
  end;

  LIdx := 0;
  while LIdx < 19 do
  begin
    LA[LIdx] := FMill[LIdx] xor FMill[(LIdx + 1) mod 19] xor FMill
      [(LIdx + 4) mod 19];
    System.Inc(LIdx);
  end;

  LA[0] := LA[0] xor 1;

  LIdx := 0;
  while LIdx < 19 do
  begin
    FMill[LIdx] := LA[LIdx];
    System.Inc(LIdx);
  end;

  LIdx := 0;
  while LIdx < 3 do
  begin
    FMill[LIdx + 13] := FMill[LIdx + 13] xor LQ[LIdx];
    System.Inc(LIdx);
  end;
end;

procedure TRadioGatun64.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  LData: array [0 .. 2] of UInt64;
  LIdx: Int32;
begin
  TConverters.le64_copy(AData, AIndex, @(LData[0]), 0, ADataLength);
  LIdx := 0;
  while LIdx < 3 do
  begin
    FMill[LIdx + 16] := FMill[LIdx + 16] xor LData[LIdx];
    FBelt[0][LIdx] := FBelt[0][LIdx] xor LData[LIdx];
    System.Inc(LIdx);
  end;

  RoundFunction();

  System.FillChar(LData, System.SizeOf(LData), UInt64(0));
end;

end.
