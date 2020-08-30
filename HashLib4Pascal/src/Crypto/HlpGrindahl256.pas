unit HlpGrindahl256;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
  HlpHashBuffer,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpArrayUtils,
  HlpHashCryptoNotBuildIn;

type
  TGrindahl256 = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private
  var
    FState, FTemp: THashLibUInt32Array;

    class var

      FSTable0, FSTable1, FSTable2, FSTable3: THashLibUInt32Array;

{$REGION 'Consts'}

  const
    SMasterTable: array [0 .. 255] of UInt32 = ($C66363A5, $F87C7C84, $EE777799,
      $F67B7B8D, $FFF2F20D, $D66B6BBD, $DE6F6FB1, $91C5C554, $60303050,
      $02010103, $CE6767A9, $562B2B7D, $E7FEFE19, $B5D7D762, $4DABABE6,
      $EC76769A, $8FCACA45, $1F82829D, $89C9C940, $FA7D7D87, $EFFAFA15,
      $B25959EB, $8E4747C9, $FBF0F00B, $41ADADEC, $B3D4D467, $5FA2A2FD,
      $45AFAFEA, $239C9CBF, $53A4A4F7, $E4727296, $9BC0C05B, $75B7B7C2,
      $E1FDFD1C, $3D9393AE, $4C26266A, $6C36365A, $7E3F3F41, $F5F7F702,
      $83CCCC4F, $6834345C, $51A5A5F4, $D1E5E534, $F9F1F108, $E2717193,
      $ABD8D873, $62313153, $2A15153F, $0804040C, $95C7C752, $46232365,
      $9DC3C35E, $30181828, $379696A1, $0A05050F, $2F9A9AB5, $0E070709,
      $24121236, $1B80809B, $DFE2E23D, $CDEBEB26, $4E272769, $7FB2B2CD,
      $EA75759F, $1209091B, $1D83839E, $582C2C74, $341A1A2E, $361B1B2D,
      $DC6E6EB2, $B45A5AEE, $5BA0A0FB, $A45252F6, $763B3B4D, $B7D6D661,
      $7DB3B3CE, $5229297B, $DDE3E33E, $5E2F2F71, $13848497, $A65353F5,
      $B9D1D168, $00000000, $C1EDED2C, $40202060, $E3FCFC1F, $79B1B1C8,
      $B65B5BED, $D46A6ABE, $8DCBCB46, $67BEBED9, $7239394B, $944A4ADE,
      $984C4CD4, $B05858E8, $85CFCF4A, $BBD0D06B, $C5EFEF2A, $4FAAAAE5,
      $EDFBFB16, $864343C5, $9A4D4DD7, $66333355, $11858594, $8A4545CF,
      $E9F9F910, $04020206, $FE7F7F81, $A05050F0, $783C3C44, $259F9FBA,
      $4BA8A8E3, $A25151F3, $5DA3A3FE, $804040C0, $058F8F8A, $3F9292AD,
      $219D9DBC, $70383848, $F1F5F504, $63BCBCDF, $77B6B6C1, $AFDADA75,
      $42212163, $20101030, $E5FFFF1A, $FDF3F30E, $BFD2D26D, $81CDCD4C,
      $180C0C14, $26131335, $C3ECEC2F, $BE5F5FE1, $359797A2, $884444CC,
      $2E171739, $93C4C457, $55A7A7F2, $FC7E7E82, $7A3D3D47, $C86464AC,
      $BA5D5DE7, $3219192B, $E6737395, $C06060A0, $19818198, $9E4F4FD1,
      $A3DCDC7F, $44222266, $542A2A7E, $3B9090AB, $0B888883, $8C4646CA,
      $C7EEEE29, $6BB8B8D3, $2814143C, $A7DEDE79, $BC5E5EE2, $160B0B1D,
      $ADDBDB76, $DBE0E03B, $64323256, $743A3A4E, $140A0A1E, $924949DB,
      $0C06060A, $4824246C, $B85C5CE4, $9FC2C25D, $BDD3D36E, $43ACACEF,
      $C46262A6, $399191A8, $319595A4, $D3E4E437, $F279798B, $D5E7E732,
      $8BC8C843, $6E373759, $DA6D6DB7, $018D8D8C, $B1D5D564, $9C4E4ED2,
      $49A9A9E0, $D86C6CB4, $AC5656FA, $F3F4F407, $CFEAEA25, $CA6565AF,
      $F47A7A8E, $47AEAEE9, $10080818, $6FBABAD5, $F0787888, $4A25256F,
      $5C2E2E72, $381C1C24, $57A6A6F1, $73B4B4C7, $97C6C651, $CBE8E823,
      $A1DDDD7C, $E874749C, $3E1F1F21, $964B4BDD, $61BDBDDC, $0D8B8B86,
      $0F8A8A85, $E0707090, $7C3E3E42, $71B5B5C4, $CC6666AA, $904848D8,
      $06030305, $F7F6F601, $1C0E0E12, $C26161A3, $6A35355F, $AE5757F9,
      $69B9B9D0, $17868691, $99C1C158, $3A1D1D27, $279E9EB9, $D9E1E138,
      $EBF8F813, $2B9898B3, $22111133, $D26969BB, $A9D9D970, $078E8E89,
      $339494A7, $2D9B9BB6, $3C1E1E22, $15878792, $C9E9E920, $87CECE49,
      $AA5555FF, $50282878, $A5DFDF7A, $038C8C8F, $59A1A1F8, $09898980,
      $1A0D0D17, $65BFBFDA, $D7E6E631, $844242C6, $D06868B8, $824141C3,
      $299999B0, $5A2D2D77, $1E0F0F11, $7BB0B0CB, $A85454FC, $6DBBBBD6,
      $2C16163A);

{$ENDREGION}
    class function CalcTable(AI: Int32): THashLibUInt32Array;

    procedure InjectMsg(AFullProcess: Boolean);

    class constructor Grindahl256();

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

{ TGrindahl256 }

class function TGrindahl256.CalcTable(AI: Int32): THashLibUInt32Array;
var
  Jdx: Int32;
begin
  System.SetLength(result, 256);
  Jdx := 0;
  while Jdx < 256 do
  begin
    result[Jdx] := UInt32((SMasterTable[Jdx] shr (AI * 8)) or
      (SMasterTable[Jdx] shl (32 - AI * 8)));
    System.Inc(Jdx);
  end;
end;

function TGrindahl256.Clone(): IHash;
var
  LHashInstance: TGrindahl256;
begin
  LHashInstance := TGrindahl256.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FTemp := System.Copy(FTemp);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TGrindahl256.Create;
begin
  Inherited Create(32, 4);
  System.SetLength(FState, 13);
  System.SetLength(FTemp, 13);
end;

procedure TGrindahl256.Finish;
var
  LPaddingSize, LIdx: Int32;
  LMessageLength: UInt64;
  LPad: THashLibByteArray;
begin
  LPaddingSize := 12 - Int32(FProcessedBytesCount and UInt32(3));
  LMessageLength := (FProcessedBytesCount shr UInt64(2)) + 1;

  System.SetLength(LPad, LPaddingSize);

  LPad[0] := $80;

  LMessageLength := TConverters.be2me_64(LMessageLength);

  TConverters.ReadUInt64AsBytesLE(LMessageLength, LPad, LPaddingSize - 8);

  TransformBytes(LPad, 0, LPaddingSize - 4);

  FState[0] := TConverters.ReadBytesAsUInt32LE(PByte(LPad), LPaddingSize - 4);

  FState[0] := TConverters.be2me_32(FState[0]);

  InjectMsg(true);

  LIdx := 0;

  while LIdx < 8 do
  begin
    InjectMsg(true);
    System.Inc(LIdx);
  end;
end;

function TGrindahl256.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 8 * System.SizeOf(UInt32));
  TConverters.be32_copy(PCardinal(FState), 5 * System.SizeOf(UInt32),
    PByte(result), 0, System.Length(result));
end;

class constructor TGrindahl256.Grindahl256;
var
  LLowIndex1, LLowIndex2: Int32;
begin
  System.SetLength(FSTable0, System.Length(SMasterTable));

  LLowIndex1 := System.Low(SMasterTable);
  LLowIndex2 := System.Low(FSTable0);

  System.Move(SMasterTable[LLowIndex1], FSTable0[LLowIndex2],
    System.SizeOf(SMasterTable));

  FSTable1 := CalcTable(1);
  FSTable2 := CalcTable(2);
  FSTable3 := CalcTable(3);
end;

procedure TGrindahl256.Initialize;
begin
  TArrayUtils.ZeroFill(FState);
  TArrayUtils.ZeroFill(FTemp);
  Inherited Initialize();
end;

procedure TGrindahl256.InjectMsg(AFullProcess: Boolean);
var
  LU: THashLibUInt32Array;
begin
  FState[12] := FState[12] xor $01;

  if (AFullProcess) then
  begin
    FTemp[0] := FSTable0[Byte(FState[12] shr 24)] xor FSTable1
      [Byte(FState[11] shr 16)] xor FSTable2[Byte(FState[9] shr 8)] xor FSTable3
      [Byte(FState[3])];
  end;

  FTemp[1] := FSTable0[Byte(FState[0] shr 24)] xor FSTable1
    [Byte(FState[12] shr 16)] xor FSTable2[Byte(FState[10] shr 8)] xor FSTable3
    [Byte(FState[4])];

  FTemp[2] := FSTable0[Byte(FState[1] shr 24)] xor FSTable1
    [Byte(FState[0] shr 16)] xor FSTable2[Byte(FState[11] shr 8)] xor FSTable3
    [Byte(FState[5])];

  FTemp[3] := FSTable0[Byte(FState[2] shr 24)] xor FSTable1
    [Byte(FState[1] shr 16)] xor FSTable2[Byte(FState[12] shr 8)] xor FSTable3
    [Byte(FState[6])];

  FTemp[4] := FSTable0[Byte(FState[3] shr 24)] xor FSTable1
    [Byte(FState[2] shr 16)] xor FSTable2[Byte(FState[0] shr 8)] xor FSTable3
    [Byte(FState[7])];

  FTemp[5] := FSTable0[Byte(FState[4] shr 24)] xor FSTable1
    [Byte(FState[3] shr 16)] xor FSTable2[Byte(FState[1] shr 8)] xor FSTable3
    [Byte(FState[8])];

  FTemp[6] := FSTable0[Byte(FState[5] shr 24)] xor FSTable1
    [Byte(FState[4] shr 16)] xor FSTable2[Byte(FState[2] shr 8)] xor FSTable3
    [Byte(FState[9])];

  FTemp[7] := FSTable0[Byte(FState[6] shr 24)] xor FSTable1
    [Byte(FState[5] shr 16)] xor FSTable2[Byte(FState[3] shr 8)] xor FSTable3
    [Byte(FState[10])];

  FTemp[8] := FSTable0[Byte(FState[7] shr 24)] xor FSTable1
    [Byte(FState[6] shr 16)] xor FSTable2[Byte(FState[4] shr 8)] xor FSTable3
    [Byte(FState[11])];

  FTemp[9] := FSTable0[Byte(FState[8] shr 24)] xor FSTable1
    [Byte(FState[7] shr 16)] xor FSTable2[Byte(FState[5] shr 8)] xor FSTable3
    [Byte(FState[12])];

  FTemp[10] := FSTable0[Byte(FState[9] shr 24)] xor FSTable1
    [Byte(FState[8] shr 16)] xor FSTable2[Byte(FState[6] shr 8)] xor FSTable3
    [Byte(FState[0])];

  FTemp[11] := FSTable0[Byte(FState[10] shr 24)] xor FSTable1
    [Byte(FState[9] shr 16)] xor FSTable2[Byte(FState[7] shr 8)] xor FSTable3
    [Byte(FState[1])];

  FTemp[12] := FSTable0[Byte(FState[11] shr 24)] xor FSTable1
    [Byte(FState[10] shr 16)] xor FSTable2[Byte(FState[8] shr 8)] xor FSTable3
    [Byte(FState[2])];

  LU := FTemp;
  FTemp := FState;
  FState := LU;
end;

procedure TGrindahl256.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
begin
  FState[0] := TConverters.ReadBytesAsUInt32LE(AData, AIndex);
  FState[0] := TConverters.be2me_32(FState[0]);
  InjectMsg(false);
end;

end.
