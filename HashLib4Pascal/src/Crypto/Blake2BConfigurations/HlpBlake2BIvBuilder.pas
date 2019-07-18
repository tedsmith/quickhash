unit HlpBlake2BIvBuilder;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpConverters,
  HlpBlake2BTreeConfig,
  HlpIBlake2BConfig,
  HlpIBlake2BTreeConfig,
  HlpHashLibTypes;

resourcestring
  SInvalidHashSize =
    '"HashSize" Must Be Greater Than 0 And Less Than or Equal To 64';
  SInvalidKeyLength = '"Key" Length Must Not Be Greater Than 64';
  SInvalidPersonalisationLength =
    '"Personalisation" Length Must Be Equal To 16';
  SInvalidSaltLength = '"Salt" Length Must Be Equal To 16';
  STreeIncorrectInnerHashSize =
    'Tree Inner Hash Size Must Not Be Greater Than 64';

type
  TBlake2BIvBuilder = class sealed(TObject)

  strict private
    class var

      FSequentialTreeConfig: IBlake2BTreeConfig;

    class procedure VerifyConfigB(const config: IBlake2BConfig;
      const treeConfig: IBlake2BTreeConfig; isSequential: Boolean); static;

    class constructor Blake2BIvBuilder();

  public
    class function ConfigB(const config: IBlake2BConfig;
      var treeConfig: IBlake2BTreeConfig): THashLibUInt64Array; static;

  end;

implementation

{ TBlake2BIvBuilder }

class procedure TBlake2BIvBuilder.VerifyConfigB(const config: IBlake2BConfig;
  const treeConfig: IBlake2BTreeConfig; isSequential: Boolean);
begin

  // digest length
  if ((config.HashSize <= 0) or (config.HashSize > 64)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidHashSize);
  end;

  // Key length
  if (config.Key <> Nil) then
  begin
    if (System.Length(config.Key) > 64) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidKeyLength);
    end;
  end;

  // Salt length
  if (config.Salt <> Nil) then
  begin
    if (System.Length(config.Salt) <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidSaltLength);
    end;
  end;

  // Personalisation length
  if (config.Personalisation <> Nil) then
  begin
    if (System.Length(config.Personalisation) <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes
        (@SInvalidPersonalisationLength);
    end;
  end;

  // Tree InnerHashSize
  if (treeConfig <> Nil) then
  begin

    if ((not isSequential) and ((treeConfig.InnerHashSize <= 0))) then
    begin
      raise EArgumentOutOfRangeHashLibException.Create
        ('treeConfig.TreeIntermediateHashSize');
    end;

    if (treeConfig.InnerHashSize > 64) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes
        (@STreeIncorrectInnerHashSize);
    end;
  end;

end;

class constructor TBlake2BIvBuilder.Blake2BIvBuilder;
begin
  FSequentialTreeConfig := TBlake2BTreeConfig.Create();
  FSequentialTreeConfig.FanOut := 1;
  FSequentialTreeConfig.MaxDepth := 1;
  FSequentialTreeConfig.LeafSize := 0;
  FSequentialTreeConfig.NodeOffset := 0;
  FSequentialTreeConfig.NodeDepth := 0;
  FSequentialTreeConfig.InnerHashSize := 0;
  FSequentialTreeConfig.IsLastNode := False;
end;

class function TBlake2BIvBuilder.ConfigB(const config: IBlake2BConfig;
  var treeConfig: IBlake2BTreeConfig): THashLibUInt64Array;
var
  isSequential: Boolean;
  tempBuffer: THashLibByteArray;
begin
  isSequential := treeConfig = Nil;
  if (isSequential) then
  begin
    treeConfig := FSequentialTreeConfig;
  end;

  VerifyConfigB(config, treeConfig, isSequential);

  System.SetLength(tempBuffer, 64);

  tempBuffer[0] := config.HashSize;
  tempBuffer[1] := System.Length(config.Key);

  if treeConfig <> Nil then
  begin
    tempBuffer[2] := treeConfig.FanOut;
    tempBuffer[3] := treeConfig.MaxDepth;
    TConverters.ReadUInt32AsBytesLE(treeConfig.LeafSize, tempBuffer, 4);
    TConverters.ReadUInt64AsBytesLE(treeConfig.NodeOffset, tempBuffer, 8);
    tempBuffer[16] := treeConfig.NodeDepth;
    tempBuffer[17] := treeConfig.InnerHashSize;
  end;

  if config.Salt <> Nil then
  begin
    System.Move(config.Salt[0], tempBuffer[32], 16 * System.SizeOf(Byte));
  end;

  if config.Personalisation <> Nil then
  begin
    System.Move(config.Personalisation[0], tempBuffer[48],
      16 * System.SizeOf(Byte));
  end;

  System.SetLength(Result, 8);
  TConverters.le64_copy(PByte(tempBuffer), 0, PUInt64(Result), 0,
    System.Length(tempBuffer) * System.SizeOf(Byte));
end;

end.
