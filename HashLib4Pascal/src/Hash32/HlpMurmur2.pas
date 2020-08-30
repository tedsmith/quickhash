unit HlpMurmur2;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpConverters,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpMultipleTransformNonBlock,
  HlpNullable;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type
  // The original MurmurHash2 32-bit algorithm by Austin Appleby.
  TMurmur2 = class sealed(TMultipleTransformNonBlock, IHash32, IHashWithKey,
    ITransformBlock)

  strict private
  var
    FKey, FWorkingKey: UInt32;

  const
    CKEY = UInt32($0);
    M = UInt32($5BD1E995);
    R = Int32(24);

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const AValue: THashLibByteArray); inline;

  strict protected
    function ComputeAggregatedBytes(const AData: THashLibByteArray)
      : IHashResult; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

implementation

{ TMurmur2 }

constructor TMurmur2.Create;
begin
  Inherited Create(4, 4);
  FKey := CKEY;
end;

function TMurmur2.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(FKey);
end;

procedure TMurmur2.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey := CKEY;
  end
  else
  begin
    if System.Length(AValue) <> KeyLength.value then
    begin
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    end;
    FKey := TConverters.ReadBytesAsUInt32LE(PByte(AValue), 0);
  end;
end;

function TMurmur2.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmur2.Initialize;
begin
  FWorkingKey := FKey;
  inherited Initialize();
end;

function TMurmur2.Clone(): IHash;
var
  LHashInstance: TMurmur2;
begin
  LHashInstance := TMurmur2.Create();
  LHashInstance.FKey := FKey;
  LHashInstance.FWorkingKey := FWorkingKey;
  FBuffer.Position := 0;
  LHashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TMurmur2.ComputeAggregatedBytes(const AData: THashLibByteArray)
  : IHashResult;
var
  LLength, LCurrentIndex, LNBlocks, LIdx: Int32;
  LBlock, LH: UInt32;
  LPtrData: PByte;
  LPtrDataCardinal: PCardinal;
begin
  LLength := System.Length(AData);
  LPtrData := PByte(AData);

  if (LLength = 0) then
  begin
    result := THashResult.Create(Int32(0));
    Exit;
  end;

  LH := FWorkingKey xor UInt32(LLength);

  LCurrentIndex := 0;
  LIdx := 0;
  LPtrDataCardinal := PCardinal(LPtrData);
  LNBlocks := LLength shr 2;

  while LIdx < LNBlocks do
  begin
    LBlock := TConverters.ReadPCardinalAsUInt32LE(LPtrDataCardinal + LIdx);

    LBlock := LBlock * M;
    LBlock := LBlock xor (LBlock shr R);
    LBlock := LBlock * M;

    LH := LH * M;
    LH := LH xor LBlock;

    System.Inc(LIdx);
    System.Inc(LCurrentIndex, 4);
    System.Dec(LLength, 4);
  end;

  case LLength of
    3:
      begin
        LH := LH xor (AData[LCurrentIndex + 2] shl 16);

        LH := LH xor (AData[LCurrentIndex + 1] shl 8);

        LH := LH xor (AData[LCurrentIndex]);

        LH := LH * M;
      end;

    2:
      begin
        LH := LH xor (AData[LCurrentIndex + 1] shl 8);

        LH := LH xor (AData[LCurrentIndex]);

        LH := LH * M;
      end;

    1:
      begin
        LH := LH xor (AData[LCurrentIndex]);

        LH := LH * M;
      end;
  end;

  LH := LH xor (LH shr 13);

  LH := LH * M;
  LH := LH xor (LH shr 15);

  result := THashResult.Create(Int32(LH));
end;

end.
