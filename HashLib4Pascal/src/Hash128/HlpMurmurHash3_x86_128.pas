unit HlpMurmurHash3_x86_128;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
  HlpConverters,
  HlpIHashInfo,
  HlpNullable,
  HlpHash,
  HlpIHash,
  HlpHashResult,
  HlpIHashResult,
  HlpBits;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type
  TMurmurHash3_x86_128 = class sealed(THash, IHash128, IHashWithKey,
    ITransformBlock)

  strict private
  var
    FKey, FH1, FH2, FH3, FH4, FTotalLength: UInt32;
    FIdx: Int32;
    FBuffer: THashLibByteArray;

    procedure ByteUpdate(AByte: Byte); inline;
    procedure Finish();
    procedure ProcessPendings();

{$REGION 'Consts'}

  const
    CKEY = UInt32($0);

    C1 = UInt32($239B961B);
    C2 = UInt32($AB0E9789);
    C3 = UInt32($38B34AE5);
    C4 = UInt32($A1E38B93);
    C5 = UInt32($85EBCA6B);
    C6 = UInt32($C2B2AE35);

    C7 = UInt32($561CCD1B);
    C8 = UInt32($0BCAA747);
    C9 = UInt32($96CD1C35);
    C10 = UInt32($32AC3B17);

{$ENDREGION}
    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const AValue: THashLibByteArray); inline;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;
  end;

implementation

{ TMurmurHash3_x86_128 }

procedure TMurmurHash3_x86_128.ProcessPendings;
var
  LK1, LK2, LK3, LK4: UInt32;
  LPtrBuffer: PByte;
begin
  if FIdx >= 16 then
  begin
    LPtrBuffer := PByte(FBuffer);
    LK1 := TConverters.ReadBytesAsUInt32LE(LPtrBuffer, 0);
    LK2 := TConverters.ReadBytesAsUInt32LE(LPtrBuffer, 4);
    LK3 := TConverters.ReadBytesAsUInt32LE(LPtrBuffer, 8);
    LK4 := TConverters.ReadBytesAsUInt32LE(LPtrBuffer, 12);

    LK1 := LK1 * C1;
    LK1 := TBits.RotateLeft32(LK1, 15);
    LK1 := LK1 * C2;
    FH1 := FH1 xor LK1;

    FH1 := TBits.RotateLeft32(FH1, 19);

    FH1 := FH1 + FH2;
    FH1 := FH1 * 5 + C7;

    LK2 := LK2 * C2;
    LK2 := TBits.RotateLeft32(LK2, 16);
    LK2 := LK2 * C3;
    FH2 := FH2 xor LK2;

    FH2 := TBits.RotateLeft32(FH2, 17);

    FH2 := FH2 + FH3;
    FH2 := FH2 * 5 + C8;

    LK3 := LK3 * C3;
    LK3 := TBits.RotateLeft32(LK3, 17);
    LK3 := LK3 * C4;
    FH3 := FH3 xor LK3;

    FH3 := TBits.RotateLeft32(FH3, 15);

    FH3 := FH3 + FH4;
    FH3 := FH3 * 5 + C9;

    LK4 := LK4 * C4;
    LK4 := TBits.RotateLeft32(LK4, 18);
    LK4 := LK4 * C1;
    FH4 := FH4 xor LK4;

    FH4 := TBits.RotateLeft32(FH4, 13);

    FH4 := FH4 + FH1;
    FH4 := FH4 * 5 + C10;

    FIdx := 0;
  end;
end;

procedure TMurmurHash3_x86_128.ByteUpdate(AByte: Byte);
begin
  FBuffer[FIdx] := AByte;
  System.Inc(FIdx);
  ProcessPendings();
end;

function TMurmurHash3_x86_128.Clone(): IHash;
var
  LHashInstance: TMurmurHash3_x86_128;
begin
  LHashInstance := TMurmurHash3_x86_128.Create();
  LHashInstance.FKey := FKey;
  LHashInstance.FH1 := FH1;
  LHashInstance.FH2 := FH2;
  LHashInstance.FH3 := FH3;
  LHashInstance.FH4 := FH4;
  LHashInstance.FTotalLength := FTotalLength;
  LHashInstance.FIdx := FIdx;
  LHashInstance.FBuffer := System.Copy(FBuffer);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TMurmurHash3_x86_128.Create;
begin
  Inherited Create(16, 16);
  FKey := CKEY;
  System.SetLength(FBuffer, 16);
end;

procedure TMurmurHash3_x86_128.Finish;
var
  LK1, LK2, LK3, LK4: UInt32;
  LLength: Int32;
begin

  // tail

  LK1 := 0;
  LK2 := 0;
  LK3 := 0;
  LK4 := 0;

  LLength := FIdx;
  if (LLength <> 0) then
  begin
    case (LLength) of
      15:
        begin
          LK4 := LK4 xor (FBuffer[14] shl 16);
          LK4 := LK4 xor (FBuffer[13] shl 8);
          LK4 := LK4 xor (FBuffer[12] shl 0);

          LK4 := LK4 * C4;
          LK4 := TBits.RotateLeft32(LK4, 18);
          LK4 := LK4 * C1;
          FH4 := FH4 xor LK4;
        end;

      14:
        begin
          LK4 := LK4 xor (FBuffer[13] shl 8);
          LK4 := LK4 xor (FBuffer[12] shl 0);
          LK4 := LK4 * C4;
          LK4 := TBits.RotateLeft32(LK4, 18);
          LK4 := LK4 * C1;
          FH4 := FH4 xor LK4;
        end;

      13:
        begin
          LK4 := LK4 xor (FBuffer[12] shl 0);
          LK4 := LK4 * C4;
          LK4 := TBits.RotateLeft32(LK4, 18);
          LK4 := LK4 * C1;
          FH4 := FH4 xor LK4;
        end;

    end;

    if (LLength > 12) then
    begin
      LLength := 12;
    end;

    case LLength of

      12:
        begin
          LK3 := LK3 xor (FBuffer[11] shl 24);
          LK3 := LK3 xor (FBuffer[10] shl 16);
          LK3 := LK3 xor (FBuffer[9] shl 8);
          LK3 := LK3 xor (FBuffer[8] shl 0);

          LK3 := LK3 * C3;
          LK3 := TBits.RotateLeft32(LK3, 17);
          LK3 := LK3 * C4;
          FH3 := FH3 xor LK3;
        end;

      11:
        begin
          LK3 := LK3 xor (FBuffer[10] shl 16);
          LK3 := LK3 xor (FBuffer[9] shl 8);
          LK3 := LK3 xor (FBuffer[8] shl 0);

          LK3 := LK3 * C3;
          LK3 := TBits.RotateLeft32(LK3, 17);
          LK3 := LK3 * C4;
          FH3 := FH3 xor LK3;
        end;

      10:
        begin
          LK3 := LK3 xor (FBuffer[9] shl 8);
          LK3 := LK3 xor (FBuffer[8] shl 0);

          LK3 := LK3 * C3;
          LK3 := TBits.RotateLeft32(LK3, 17);
          LK3 := LK3 * C4;
          FH3 := FH3 xor LK3;
        end;

      9:
        begin
          LK3 := LK3 xor (FBuffer[8] shl 0);

          LK3 := LK3 * C3;
          LK3 := TBits.RotateLeft32(LK3, 17);
          LK3 := LK3 * C4;
          FH3 := FH3 xor LK3;
        end;

    end;

    if (LLength > 8) then
    begin
      LLength := 8;
    end;

    case LLength of

      8:
        begin
          LK2 := LK2 xor (FBuffer[7] shl 24);
          LK2 := LK2 xor (FBuffer[6] shl 16);
          LK2 := LK2 xor (FBuffer[5] shl 8);
          LK2 := LK2 xor (FBuffer[4] shl 0);

          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft32(LK2, 16);
          LK2 := LK2 * C3;
          FH2 := FH2 xor LK2;
        end;

      7:
        begin
          LK2 := LK2 xor (FBuffer[6] shl 16);
          LK2 := LK2 xor (FBuffer[5] shl 8);
          LK2 := LK2 xor (FBuffer[4] shl 0);

          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft32(LK2, 16);
          LK2 := LK2 * C3;
          FH2 := FH2 xor LK2;
        end;

      6:
        begin
          LK2 := LK2 xor (FBuffer[5] shl 8);
          LK2 := LK2 xor (FBuffer[4] shl 0);

          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft32(LK2, 16);
          LK2 := LK2 * C3;
          FH2 := FH2 xor LK2;
        end;

      5:
        begin
          LK2 := LK2 xor (FBuffer[4] shl 0);

          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft32(LK2, 16);
          LK2 := LK2 * C3;
          FH2 := FH2 xor LK2;
        end;

    end;

    if (LLength > 4) then
    begin
      LLength := 4;
    end;

    case LLength of

      4:
        begin
          LK1 := LK1 xor (FBuffer[3] shl 24);
          LK1 := LK1 xor (FBuffer[2] shl 16);
          LK1 := LK1 xor (FBuffer[1] shl 8);
          LK1 := LK1 xor (FBuffer[0] shl 0);

          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft32(LK1, 15);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      3:
        begin
          LK1 := LK1 xor (FBuffer[2] shl 16);
          LK1 := LK1 xor (FBuffer[1] shl 8);
          LK1 := LK1 xor (FBuffer[0] shl 0);

          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft32(LK1, 15);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      2:
        begin

          LK1 := LK1 xor (FBuffer[1] shl 8);
          LK1 := LK1 xor (FBuffer[0] shl 0);

          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft32(LK1, 15);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      1:
        begin
          LK1 := LK1 xor (FBuffer[0] shl 0);

          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft32(LK1, 15);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

    end;
  end;

  // finalization

  FH1 := FH1 xor FTotalLength;
  FH2 := FH2 xor FTotalLength;
  FH3 := FH3 xor FTotalLength;
  FH4 := FH4 xor FTotalLength;

  FH1 := FH1 + FH2;
  FH1 := FH1 + FH3;
  FH1 := FH1 + FH4;
  FH2 := FH2 + FH1;
  FH3 := FH3 + FH1;
  FH4 := FH4 + FH1;

  FH1 := FH1 xor (FH1 shr 16);
  FH1 := FH1 * C5;
  FH1 := FH1 xor (FH1 shr 13);
  FH1 := FH1 * C6;
  FH1 := FH1 xor (FH1 shr 16);

  FH2 := FH2 xor (FH2 shr 16);
  FH2 := FH2 * C5;
  FH2 := FH2 xor (FH2 shr 13);
  FH2 := FH2 * C6;
  FH2 := FH2 xor (FH2 shr 16);

  FH3 := FH3 xor (FH3 shr 16);
  FH3 := FH3 * C5;
  FH3 := FH3 xor (FH3 shr 13);
  FH3 := FH3 * C6;
  FH3 := FH3 xor (FH3 shr 16);

  FH4 := FH4 xor (FH4 shr 16);
  FH4 := FH4 * C5;
  FH4 := FH4 xor (FH4 shr 13);
  FH4 := FH4 * C6;
  FH4 := FH4 xor (FH4 shr 16);

  FH1 := FH1 + FH2;
  FH1 := FH1 + FH3;
  FH1 := FH1 + FH4;
  FH2 := FH2 + FH1;
  FH3 := FH3 + FH1;
  FH4 := FH4 + FH1;
end;

function TMurmurHash3_x86_128.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(FKey);
end;

function TMurmurHash3_x86_128.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmurHash3_x86_128.Initialize;
begin
  FH1 := FKey;
  FH2 := FKey;
  FH3 := FKey;
  FH4 := FKey;

  FTotalLength := 0;
  FIdx := 0;
end;

procedure TMurmurHash3_x86_128.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey := CKEY;
  end
  else
  begin
    if System.length(AValue) <> KeyLength.value then
    begin
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    end;
    FKey := TConverters.ReadBytesAsUInt32LE(PByte(AValue), 0);
  end;
end;

procedure TMurmurHash3_x86_128.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LLength, LNBlocks, LIndex, LOffset, LIdx: Int32;
  LK1, LK2, LK3, LK4, LH1, LH2, LH3, LH4: UInt32;
  LPtrData: PByte;
  LPtrDataCardinal: PCardinal;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.length(AData));
{$ENDIF DEBUG}
  LLength := ALength;
  LIndex := AIndex;
  LIdx := 0;
  System.Inc(FTotalLength, LLength);
  LPtrData := PByte(AData);

  // consume last pending bytes

  if ((FIdx <> 0) and (ALength <> 0)) then
  begin
{$IFDEF DEBUG}
    System.Assert(AIndex = 0); // nothing would work anyways if AIndex <> 0
{$ENDIF DEBUG}
    while ((FIdx < 16) and (LLength <> 0)) do
    begin
      FBuffer[FIdx] := (LPtrData + AIndex)^;
      System.Inc(FIdx);
      System.Inc(AIndex);
      System.Dec(LLength);
    end;
    if (FIdx = 16) then
    begin
      ProcessPendings;
    end;
  end
  else
  begin
    LIndex := 0;
  end;

  LNBlocks := LLength shr 4;

  // body

  LH1 := FH1;
  LH2 := FH2;
  LH3 := FH3;
  LH4 := FH4;
  LPtrDataCardinal := PCardinal(LPtrData + AIndex);
  while LIndex < LNBlocks do
  begin

    LK1 := TConverters.ReadPCardinalAsUInt32LE(LPtrDataCardinal + LIdx);
    System.Inc(LIdx);
    LK2 := TConverters.ReadPCardinalAsUInt32LE(LPtrDataCardinal + LIdx);
    System.Inc(LIdx);
    LK3 := TConverters.ReadPCardinalAsUInt32LE(LPtrDataCardinal + LIdx);
    System.Inc(LIdx);
    LK4 := TConverters.ReadPCardinalAsUInt32LE(LPtrDataCardinal + LIdx);
    System.Inc(LIdx);

    LK1 := LK1 * C1;
    LK1 := TBits.RotateLeft32(LK1, 15);
    LK1 := LK1 * C2;
    LH1 := LH1 xor LK1;

    LH1 := TBits.RotateLeft32(LH1, 19);

    LH1 := LH1 + LH2;
    LH1 := LH1 * 5 + C7;

    LK2 := LK2 * C2;
    LK2 := TBits.RotateLeft32(LK2, 16);
    LK2 := LK2 * C3;
    LH2 := LH2 xor LK2;

    LH2 := TBits.RotateLeft32(LH2, 17);

    LH2 := LH2 + LH3;
    LH2 := LH2 * 5 + C8;

    LK3 := LK3 * C3;
    LK3 := TBits.RotateLeft32(LK3, 17);
    LK3 := LK3 * C4;
    LH3 := LH3 xor LK3;

    LH3 := TBits.RotateLeft32(LH3, 15);

    LH3 := LH3 + LH4;
    LH3 := LH3 * 5 + C9;

    LK4 := LK4 * C4;
    LK4 := TBits.RotateLeft32(LK4, 18);
    LK4 := LK4 * C1;
    LH4 := LH4 xor LK4;

    LH4 := TBits.RotateLeft32(LH4, 13);

    LH4 := LH4 + LH1;
    LH4 := LH4 * 5 + C10;

    System.Inc(LIndex);
  end;

  FH1 := LH1;
  FH2 := LH2;
  FH3 := LH3;
  FH4 := LH4;

  LOffset := AIndex + (LIndex * 16);

  while LOffset < (AIndex + LLength) do
  begin
    ByteUpdate(AData[LOffset]);
    System.Inc(LOffset);
  end;

end;

function TMurmurHash3_x86_128.TransformFinal: IHashResult;
var
  LBufferBytes: THashLibByteArray;
begin
  Finish();

  System.SetLength(LBufferBytes, HashSize);
  TConverters.ReadUInt32AsBytesBE(FH1, LBufferBytes, 0);
  TConverters.ReadUInt32AsBytesBE(FH2, LBufferBytes, 4);
  TConverters.ReadUInt32AsBytesBE(FH3, LBufferBytes, 8);
  TConverters.ReadUInt32AsBytesBE(FH4, LBufferBytes, 12);

  result := THashResult.Create(LBufferBytes);
  Initialize();
end;

end.
