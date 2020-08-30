unit HlpPanama;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpBits,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpArrayUtils,
  HlpHashCryptoNotBuildIn;

type
  TPanama = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private
  var
    FState, FTheta, FGamma, FPi: THashLibUInt32Array;
    FStages: THashLibMatrixUInt32Array;
    FTap: Int32;

    procedure GPT(APtrTheta: PCardinal);

  const

  strict protected
    procedure Finish(); override;
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TPanama }

function TPanama.Clone(): IHash;
var
  LHashInstance: TPanama;
begin
  LHashInstance := TPanama.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FTheta := System.Copy(FTheta);
  LHashInstance.FGamma := System.Copy(FGamma);
  LHashInstance.FPi := System.Copy(FPi);
  LHashInstance.FStages := TArrayUtils.Clone(FStages);
  LHashInstance.FTap := FTap;
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TPanama.Create;
var
  LIdx: Int32;
begin
  Inherited Create(32, 32);
  System.SetLength(FState, 17);

  System.SetLength(FStages, 32);
  LIdx := 0;
  while LIdx <= System.High(FStages) do
  begin
    System.SetLength(FStages[LIdx], 8);
    System.Inc(LIdx);
  end;

  System.SetLength(FTheta, 17);

  System.SetLength(FGamma, 17);

  System.SetLength(FPi, 17);
end;

procedure TPanama.Finish;
var
  LPaddingSize, LIdx, LTap4, LTap16, LTap25: Int32;
  LPad: THashLibByteArray;
  LTheta: THashLibUInt32Array;
  LPtrTheta: PCardinal;
begin
  LPaddingSize := 32 - ((Int32(FProcessedBytesCount)) and 31);

  System.SetLength(LPad, LPaddingSize);

  LPad[0] := $01;
  TransformBytes(LPad, 0, LPaddingSize);

  System.SetLength(LTheta, 17);

  LPtrTheta := PCardinal(LTheta);

  LIdx := 0;
  while LIdx < 32 do
  begin
    LTap4 := (FTap + 4) and $1F;
    LTap16 := (FTap + 16) and $1F;

    FTap := (FTap - 1) and $1F;
    LTap25 := (FTap + 25) and $1F;

    GPT(LPtrTheta);

    FStages[LTap25, 0] := FStages[LTap25, 0] xor FStages[FTap, 2];
    FStages[LTap25, 1] := FStages[LTap25, 1] xor FStages[FTap, 3];
    FStages[LTap25, 2] := FStages[LTap25, 2] xor FStages[FTap, 4];
    FStages[LTap25, 3] := FStages[LTap25, 3] xor FStages[FTap, 5];
    FStages[LTap25, 4] := FStages[LTap25, 4] xor FStages[FTap, 6];
    FStages[LTap25, 5] := FStages[LTap25, 5] xor FStages[FTap, 7];
    FStages[LTap25, 6] := FStages[LTap25, 6] xor FStages[FTap, 0];
    FStages[LTap25, 7] := FStages[LTap25, 7] xor FStages[FTap, 1];
    FStages[FTap, 0] := FStages[FTap, 0] xor FState[1];
    FStages[FTap, 1] := FStages[FTap, 1] xor FState[2];
    FStages[FTap, 2] := FStages[FTap, 2] xor FState[3];
    FStages[FTap, 3] := FStages[FTap, 3] xor FState[4];
    FStages[FTap, 4] := FStages[FTap, 4] xor FState[5];
    FStages[FTap, 5] := FStages[FTap, 5] xor FState[6];
    FStages[FTap, 6] := FStages[FTap, 6] xor FState[7];
    FStages[FTap, 7] := FStages[FTap, 7] xor FState[8];

    FState[0] := LTheta[0] xor $01;
    FState[1] := LTheta[1] xor FStages[LTap4, 0];
    FState[2] := LTheta[2] xor FStages[LTap4, 1];
    FState[3] := LTheta[3] xor FStages[LTap4, 2];
    FState[4] := LTheta[4] xor FStages[LTap4, 3];
    FState[5] := LTheta[5] xor FStages[LTap4, 4];
    FState[6] := LTheta[6] xor FStages[LTap4, 5];
    FState[7] := LTheta[7] xor FStages[LTap4, 6];
    FState[8] := LTheta[8] xor FStages[LTap4, 7];
    FState[9] := LTheta[9] xor FStages[LTap16, 0];
    FState[10] := LTheta[10] xor FStages[LTap16, 1];
    FState[11] := LTheta[11] xor FStages[LTap16, 2];
    FState[12] := LTheta[12] xor FStages[LTap16, 3];
    FState[13] := LTheta[13] xor FStages[LTap16, 4];
    FState[14] := LTheta[14] xor FStages[LTap16, 5];
    FState[15] := LTheta[15] xor FStages[LTap16, 6];
    FState[16] := LTheta[16] xor FStages[LTap16, 7];

    System.Inc(LIdx);
  end;
end;

function TPanama.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 8 * System.SizeOf(UInt32));
  TConverters.le32_copy(PCardinal(FState), 9 * System.SizeOf(UInt32),
    PByte(result), 0, System.Length(result));
end;

procedure TPanama.GPT(APtrTheta: PCardinal);
begin
  FGamma[0] := FState[0] xor (FState[1] or not FState[2]);
  FGamma[1] := FState[1] xor (FState[2] or not FState[3]);
  FGamma[2] := FState[2] xor (FState[3] or not FState[4]);
  FGamma[3] := FState[3] xor (FState[4] or not FState[5]);
  FGamma[4] := FState[4] xor (FState[5] or not FState[6]);
  FGamma[5] := FState[5] xor (FState[6] or not FState[7]);
  FGamma[6] := FState[6] xor (FState[7] or not FState[8]);
  FGamma[7] := FState[7] xor (FState[8] or not FState[9]);
  FGamma[8] := FState[8] xor (FState[9] or not FState[10]);
  FGamma[9] := FState[9] xor (FState[10] or not FState[11]);
  FGamma[10] := FState[10] xor (FState[11] or not FState[12]);
  FGamma[11] := FState[11] xor (FState[12] or not FState[13]);
  FGamma[12] := FState[12] xor (FState[13] or not FState[14]);
  FGamma[13] := FState[13] xor (FState[14] or not FState[15]);
  FGamma[14] := FState[14] xor (FState[15] or not FState[16]);
  FGamma[15] := FState[15] xor (FState[16] or not FState[0]);
  FGamma[16] := FState[16] xor (FState[0] or not FState[1]);

  FPi[0] := FGamma[0];
  FPi[1] := TBits.RotateLeft32(FGamma[7], 1);
  FPi[2] := TBits.RotateLeft32(FGamma[14], 3);
  FPi[3] := TBits.RotateLeft32(FGamma[4], 6);
  FPi[4] := TBits.RotateLeft32(FGamma[11], 10);
  FPi[5] := TBits.RotateLeft32(FGamma[1], 15);
  FPi[6] := TBits.RotateLeft32(FGamma[8], 21);
  FPi[7] := TBits.RotateLeft32(FGamma[15], 28);
  FPi[8] := TBits.RotateLeft32(FGamma[5], 4);
  FPi[9] := TBits.RotateLeft32(FGamma[12], 13);
  FPi[10] := TBits.RotateLeft32(FGamma[2], 23);
  FPi[11] := TBits.RotateLeft32(FGamma[9], 2);
  FPi[12] := TBits.RotateLeft32(FGamma[16], 14);
  FPi[13] := TBits.RotateLeft32(FGamma[6], 27);
  FPi[14] := TBits.RotateLeft32(FGamma[13], 9);
  FPi[15] := TBits.RotateLeft32(FGamma[3], 24);
  FPi[16] := TBits.RotateLeft32(FGamma[10], 8);

  APtrTheta[0] := FPi[0] xor FPi[1] xor FPi[4];
  APtrTheta[1] := FPi[1] xor FPi[2] xor FPi[5];
  APtrTheta[2] := FPi[2] xor FPi[3] xor FPi[6];
  APtrTheta[3] := FPi[3] xor FPi[4] xor FPi[7];
  APtrTheta[4] := FPi[4] xor FPi[5] xor FPi[8];
  APtrTheta[5] := FPi[5] xor FPi[6] xor FPi[9];
  APtrTheta[6] := FPi[6] xor FPi[7] xor FPi[10];
  APtrTheta[7] := FPi[7] xor FPi[8] xor FPi[11];
  APtrTheta[8] := FPi[8] xor FPi[9] xor FPi[12];
  APtrTheta[9] := FPi[9] xor FPi[10] xor FPi[13];
  APtrTheta[10] := FPi[10] xor FPi[11] xor FPi[14];
  APtrTheta[11] := FPi[11] xor FPi[12] xor FPi[15];
  APtrTheta[12] := FPi[12] xor FPi[13] xor FPi[16];
  APtrTheta[13] := FPi[13] xor FPi[14] xor FPi[0];
  APtrTheta[14] := FPi[14] xor FPi[15] xor FPi[1];
  APtrTheta[15] := FPi[15] xor FPi[16] xor FPi[2];
  APtrTheta[16] := FPi[16] xor FPi[0] xor FPi[3];
end;

procedure TPanama.Initialize;
begin
  TArrayUtils.ZeroFill(FState);
  TArrayUtils.ZeroFill(FTheta);
  TArrayUtils.ZeroFill(FGamma);
  TArrayUtils.ZeroFill(FPi);

  FTap := 0;

  TArrayUtils.ZeroFill(FStages);

  Inherited Initialize();
end;

procedure TPanama.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  LWorkBuffer: array [0 .. 7] of UInt32;
  LTap16, LTap25: Int32;
begin
  TConverters.le32_copy(AData, AIndex, @(LWorkBuffer[0]), 0, ADataLength);

  LTap16 := (FTap + 16) and $1F;

  FTap := (FTap - 1) and $1F;
  LTap25 := (FTap + 25) and $1F;

  GPT(PCardinal(FTheta));

  FStages[LTap25, 0] := FStages[LTap25, 0] xor FStages[FTap, 2];
  FStages[LTap25, 1] := FStages[LTap25, 1] xor FStages[FTap, 3];
  FStages[LTap25, 2] := FStages[LTap25, 2] xor FStages[FTap, 4];
  FStages[LTap25, 3] := FStages[LTap25, 3] xor FStages[FTap, 5];
  FStages[LTap25, 4] := FStages[LTap25, 4] xor FStages[FTap, 6];
  FStages[LTap25, 5] := FStages[LTap25, 5] xor FStages[FTap, 7];
  FStages[LTap25, 6] := FStages[LTap25, 6] xor FStages[FTap, 0];
  FStages[LTap25, 7] := FStages[LTap25, 7] xor FStages[FTap, 1];
  FStages[FTap, 0] := FStages[FTap, 0] xor LWorkBuffer[0];
  FStages[FTap, 1] := FStages[FTap, 1] xor LWorkBuffer[1];
  FStages[FTap, 2] := FStages[FTap, 2] xor LWorkBuffer[2];
  FStages[FTap, 3] := FStages[FTap, 3] xor LWorkBuffer[3];
  FStages[FTap, 4] := FStages[FTap, 4] xor LWorkBuffer[4];
  FStages[FTap, 5] := FStages[FTap, 5] xor LWorkBuffer[5];
  FStages[FTap, 6] := FStages[FTap, 6] xor LWorkBuffer[6];
  FStages[FTap, 7] := FStages[FTap, 7] xor LWorkBuffer[7];

  FState[0] := FTheta[0] xor $01;
  FState[1] := FTheta[1] xor LWorkBuffer[0];
  FState[2] := FTheta[2] xor LWorkBuffer[1];
  FState[3] := FTheta[3] xor LWorkBuffer[2];
  FState[4] := FTheta[4] xor LWorkBuffer[3];
  FState[5] := FTheta[5] xor LWorkBuffer[4];
  FState[6] := FTheta[6] xor LWorkBuffer[5];
  FState[7] := FTheta[7] xor LWorkBuffer[6];
  FState[8] := FTheta[8] xor LWorkBuffer[7];
  FState[9] := FTheta[9] xor FStages[LTap16, 0];
  FState[10] := FTheta[10] xor FStages[LTap16, 1];
  FState[11] := FTheta[11] xor FStages[LTap16, 2];
  FState[12] := FTheta[12] xor FStages[LTap16, 3];
  FState[13] := FTheta[13] xor FStages[LTap16, 4];
  FState[14] := FTheta[14] xor FStages[LTap16, 5];
  FState[15] := FTheta[15] xor FStages[LTap16, 6];
  FState[16] := FTheta[16] xor FStages[LTap16, 7];

  System.FillChar(LWorkBuffer, System.SizeOf(LWorkBuffer), UInt32(0));
end;

end.
