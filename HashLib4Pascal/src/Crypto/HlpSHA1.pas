unit HlpSHA1;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpBits,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpSHA0,
  HlpIHash;

type
  TSHA1 = class sealed(TSHA0)

  strict protected
    procedure Expand(AData: PCardinal); override;

  public
    // Not really needed because there is an Intristic default constructor always
    // called for classes if none is defined by the developer but I just put it
    // for readability reasons.
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TSHA1 }

function TSHA1.Clone(): IHash;
var
  LHashInstance: TSHA1;
begin
  LHashInstance := TSHA1.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA1.Create;
begin
  Inherited Create();
end;

procedure TSHA1.Expand(AData: PCardinal);
var
{$IFNDEF USE_UNROLLED_VARIANT}
  LIdx: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
  LT: UInt32;
begin

{$IFDEF USE_UNROLLED_VARIANT}
  LT := AData[16 - 3] xor AData[16 - 8] xor AData[16 - 14] xor AData[0];
  AData[16] := TBits.RotateLeft32(LT, 1);
  LT := AData[17 - 3] xor AData[17 - 8] xor AData[17 - 14] xor AData[17 - 16];
  AData[17] := TBits.RotateLeft32(LT, 1);
  LT := AData[18 - 3] xor AData[18 - 8] xor AData[18 - 14] xor AData[18 - 16];
  AData[18] := TBits.RotateLeft32(LT, 1);
  LT := AData[19 - 3] xor AData[19 - 8] xor AData[19 - 14] xor AData[19 - 16];
  AData[19] := TBits.RotateLeft32(LT, 1);
  LT := AData[20 - 3] xor AData[20 - 8] xor AData[20 - 14] xor AData[20 - 16];
  AData[20] := TBits.RotateLeft32(LT, 1);
  LT := AData[21 - 3] xor AData[21 - 8] xor AData[21 - 14] xor AData[21 - 16];
  AData[21] := TBits.RotateLeft32(LT, 1);
  LT := AData[22 - 3] xor AData[22 - 8] xor AData[22 - 14] xor AData[22 - 16];
  AData[22] := TBits.RotateLeft32(LT, 1);
  LT := AData[23 - 3] xor AData[23 - 8] xor AData[23 - 14] xor AData[23 - 16];
  AData[23] := TBits.RotateLeft32(LT, 1);
  LT := AData[24 - 3] xor AData[24 - 8] xor AData[24 - 14] xor AData[24 - 16];
  AData[24] := TBits.RotateLeft32(LT, 1);
  LT := AData[25 - 3] xor AData[25 - 8] xor AData[25 - 14] xor AData[25 - 16];
  AData[25] := TBits.RotateLeft32(LT, 1);
  LT := AData[26 - 3] xor AData[26 - 8] xor AData[26 - 14] xor AData[26 - 16];
  AData[26] := TBits.RotateLeft32(LT, 1);
  LT := AData[27 - 3] xor AData[27 - 8] xor AData[27 - 14] xor AData[27 - 16];
  AData[27] := TBits.RotateLeft32(LT, 1);
  LT := AData[28 - 3] xor AData[28 - 8] xor AData[28 - 14] xor AData[28 - 16];
  AData[28] := TBits.RotateLeft32(LT, 1);
  LT := AData[29 - 3] xor AData[29 - 8] xor AData[29 - 14] xor AData[29 - 16];
  AData[29] := TBits.RotateLeft32(LT, 1);
  LT := AData[30 - 3] xor AData[30 - 8] xor AData[30 - 14] xor AData[30 - 16];
  AData[30] := TBits.RotateLeft32(LT, 1);
  LT := AData[31 - 3] xor AData[31 - 8] xor AData[31 - 14] xor AData[31 - 16];
  AData[31] := TBits.RotateLeft32(LT, 1);
  LT := AData[32 - 3] xor AData[32 - 8] xor AData[32 - 14] xor AData[32 - 16];
  AData[32] := TBits.RotateLeft32(LT, 1);
  LT := AData[33 - 3] xor AData[33 - 8] xor AData[33 - 14] xor AData[33 - 16];
  AData[33] := TBits.RotateLeft32(LT, 1);
  LT := AData[34 - 3] xor AData[34 - 8] xor AData[34 - 14] xor AData[34 - 16];
  AData[34] := TBits.RotateLeft32(LT, 1);
  LT := AData[35 - 3] xor AData[35 - 8] xor AData[35 - 14] xor AData[35 - 16];
  AData[35] := TBits.RotateLeft32(LT, 1);
  LT := AData[36 - 3] xor AData[36 - 8] xor AData[36 - 14] xor AData[36 - 16];
  AData[36] := TBits.RotateLeft32(LT, 1);
  LT := AData[37 - 3] xor AData[37 - 8] xor AData[37 - 14] xor AData[37 - 16];
  AData[37] := TBits.RotateLeft32(LT, 1);
  LT := AData[38 - 3] xor AData[38 - 8] xor AData[38 - 14] xor AData[38 - 16];
  AData[38] := TBits.RotateLeft32(LT, 1);
  LT := AData[39 - 3] xor AData[39 - 8] xor AData[39 - 14] xor AData[39 - 16];
  AData[39] := TBits.RotateLeft32(LT, 1);
  LT := AData[40 - 3] xor AData[40 - 8] xor AData[40 - 14] xor AData[40 - 16];
  AData[40] := TBits.RotateLeft32(LT, 1);
  LT := AData[41 - 3] xor AData[41 - 8] xor AData[41 - 14] xor AData[41 - 16];
  AData[41] := TBits.RotateLeft32(LT, 1);
  LT := AData[42 - 3] xor AData[42 - 8] xor AData[42 - 14] xor AData[42 - 16];
  AData[42] := TBits.RotateLeft32(LT, 1);
  LT := AData[43 - 3] xor AData[43 - 8] xor AData[43 - 14] xor AData[43 - 16];
  AData[43] := TBits.RotateLeft32(LT, 1);
  LT := AData[44 - 3] xor AData[44 - 8] xor AData[44 - 14] xor AData[44 - 16];
  AData[44] := TBits.RotateLeft32(LT, 1);
  LT := AData[45 - 3] xor AData[45 - 8] xor AData[45 - 14] xor AData[45 - 16];
  AData[45] := TBits.RotateLeft32(LT, 1);
  LT := AData[46 - 3] xor AData[46 - 8] xor AData[46 - 14] xor AData[46 - 16];
  AData[46] := TBits.RotateLeft32(LT, 1);
  LT := AData[47 - 3] xor AData[47 - 8] xor AData[47 - 14] xor AData[47 - 16];
  AData[47] := TBits.RotateLeft32(LT, 1);
  LT := AData[48 - 3] xor AData[48 - 8] xor AData[48 - 14] xor AData[48 - 16];
  AData[48] := TBits.RotateLeft32(LT, 1);
  LT := AData[49 - 3] xor AData[49 - 8] xor AData[49 - 14] xor AData[49 - 16];
  AData[49] := TBits.RotateLeft32(LT, 1);
  LT := AData[50 - 3] xor AData[50 - 8] xor AData[50 - 14] xor AData[50 - 16];
  AData[50] := TBits.RotateLeft32(LT, 1);
  LT := AData[51 - 3] xor AData[51 - 8] xor AData[51 - 14] xor AData[51 - 16];
  AData[51] := TBits.RotateLeft32(LT, 1);
  LT := AData[52 - 3] xor AData[52 - 8] xor AData[52 - 14] xor AData[52 - 16];
  AData[52] := TBits.RotateLeft32(LT, 1);
  LT := AData[53 - 3] xor AData[53 - 8] xor AData[53 - 14] xor AData[53 - 16];
  AData[53] := TBits.RotateLeft32(LT, 1);
  LT := AData[54 - 3] xor AData[54 - 8] xor AData[54 - 14] xor AData[54 - 16];
  AData[54] := TBits.RotateLeft32(LT, 1);
  LT := AData[55 - 3] xor AData[55 - 8] xor AData[55 - 14] xor AData[55 - 16];
  AData[55] := TBits.RotateLeft32(LT, 1);
  LT := AData[56 - 3] xor AData[56 - 8] xor AData[56 - 14] xor AData[56 - 16];
  AData[56] := TBits.RotateLeft32(LT, 1);
  LT := AData[57 - 3] xor AData[57 - 8] xor AData[57 - 14] xor AData[57 - 16];
  AData[57] := TBits.RotateLeft32(LT, 1);
  LT := AData[58 - 3] xor AData[58 - 8] xor AData[58 - 14] xor AData[58 - 16];
  AData[58] := TBits.RotateLeft32(LT, 1);
  LT := AData[59 - 3] xor AData[59 - 8] xor AData[59 - 14] xor AData[59 - 16];
  AData[59] := TBits.RotateLeft32(LT, 1);
  LT := AData[60 - 3] xor AData[60 - 8] xor AData[60 - 14] xor AData[60 - 16];
  AData[60] := TBits.RotateLeft32(LT, 1);
  LT := AData[61 - 3] xor AData[61 - 8] xor AData[61 - 14] xor AData[61 - 16];
  AData[61] := TBits.RotateLeft32(LT, 1);
  LT := AData[62 - 3] xor AData[62 - 8] xor AData[62 - 14] xor AData[62 - 16];
  AData[62] := TBits.RotateLeft32(LT, 1);
  LT := AData[63 - 3] xor AData[63 - 8] xor AData[63 - 14] xor AData[63 - 16];
  AData[63] := TBits.RotateLeft32(LT, 1);
  LT := AData[64 - 3] xor AData[64 - 8] xor AData[64 - 14] xor AData[64 - 16];
  AData[64] := TBits.RotateLeft32(LT, 1);
  LT := AData[65 - 3] xor AData[65 - 8] xor AData[65 - 14] xor AData[65 - 16];
  AData[65] := TBits.RotateLeft32(LT, 1);
  LT := AData[66 - 3] xor AData[66 - 8] xor AData[66 - 14] xor AData[66 - 16];
  AData[66] := TBits.RotateLeft32(LT, 1);
  LT := AData[67 - 3] xor AData[67 - 8] xor AData[67 - 14] xor AData[67 - 16];
  AData[67] := TBits.RotateLeft32(LT, 1);
  LT := AData[68 - 3] xor AData[68 - 8] xor AData[68 - 14] xor AData[68 - 16];
  AData[68] := TBits.RotateLeft32(LT, 1);
  LT := AData[69 - 3] xor AData[69 - 8] xor AData[69 - 14] xor AData[69 - 16];
  AData[69] := TBits.RotateLeft32(LT, 1);
  LT := AData[70 - 3] xor AData[70 - 8] xor AData[70 - 14] xor AData[70 - 16];
  AData[70] := TBits.RotateLeft32(LT, 1);
  LT := AData[71 - 3] xor AData[71 - 8] xor AData[71 - 14] xor AData[71 - 16];
  AData[71] := TBits.RotateLeft32(LT, 1);
  LT := AData[72 - 3] xor AData[72 - 8] xor AData[72 - 14] xor AData[72 - 16];
  AData[72] := TBits.RotateLeft32(LT, 1);
  LT := AData[73 - 3] xor AData[73 - 8] xor AData[73 - 14] xor AData[73 - 16];
  AData[73] := TBits.RotateLeft32(LT, 1);
  LT := AData[74 - 3] xor AData[74 - 8] xor AData[74 - 14] xor AData[74 - 16];
  AData[74] := TBits.RotateLeft32(LT, 1);
  LT := AData[75 - 3] xor AData[75 - 8] xor AData[75 - 14] xor AData[75 - 16];
  AData[75] := TBits.RotateLeft32(LT, 1);
  LT := AData[76 - 3] xor AData[76 - 8] xor AData[76 - 14] xor AData[76 - 16];
  AData[76] := TBits.RotateLeft32(LT, 1);
  LT := AData[77 - 3] xor AData[77 - 8] xor AData[77 - 14] xor AData[77 - 16];
  AData[77] := TBits.RotateLeft32(LT, 1);
  LT := AData[78 - 3] xor AData[78 - 8] xor AData[78 - 14] xor AData[78 - 16];
  AData[78] := TBits.RotateLeft32(LT, 1);
  LT := AData[79 - 3] xor AData[79 - 8] xor AData[79 - 14] xor AData[79 - 16];
  AData[79] := TBits.RotateLeft32(LT, 1);

{$ELSE}
  for LIdx := 16 to 79 do
  begin
    LT := AData[LIdx - 3] xor AData[LIdx - 8] xor AData[LIdx - 14] xor AData
      [LIdx - 16];
    AData[LIdx] := TBits.RotateLeft32(LT, 1);
  end;
{$ENDIF USE_UNROLLED_VARIANT}
end;

end.
