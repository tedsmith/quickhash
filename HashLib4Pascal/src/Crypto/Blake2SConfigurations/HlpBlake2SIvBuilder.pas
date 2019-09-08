unit HlpBlake2SIvBuilder;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpConverters,
  HlpBlake2STreeConfig,
  HlpIBlake2SConfig,
  HlpIBlake2STreeConfig,
  HlpHashLibTypes;

resourcestring
  SInvalidHashSize =
    '"HashSize" Must Be Greater Than 0 And Less Than or Equal To 32';
  SInvalidKeyLength = '"Key" Length Must Not Be Greater Than 32';
  SInvalidPersonalisationLength = '"Personalisation" Length Must Be Equal To 8';
  SInvalidSaltLength = '"Salt" Length Must Be Equal To 8';
  STreeIncorrectInnerHashSize =
    'Tree Inner Hash Size Must Not Be Greater Than 32';

type
  TBlake2SIvBuilder = class sealed(TObject)

  strict private
    class var

      FSequentialTreeConfig: IBlake2STreeConfig;

    class procedure VerifyConfigS(const config: IBlake2SConfig;
      const treeConfig: IBlake2STreeConfig; isSequential: Boolean); static;

    class constructor Blake2SIvBuilder();

  public
    class function ConfigS(const config: IBlake2SConfig;
      var treeConfig: IBlake2STreeConfig): THashLibUInt32Array; static;

  end;

implementation

{ TBlake2SIvBuilder }

class procedure TBlake2SIvBuilder.VerifyConfigS(const config: IBlake2SConfig;
  const treeConfig: IBlake2STreeConfig; isSequential: Boolean);
begin

  // digest length
  if ((config.HashSize <= 0) or (config.HashSize > 32)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidHashSize);
  end;

  // Key length
  if (config.Key <> Nil) then
  begin
    if (System.Length(config.Key) > 32) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidKeyLength);
    end;
  end;

  // Salt length
  if (config.Salt <> Nil) then
  begin
    if (System.Length(config.Salt) <> 8) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidSaltLength);
    end;
  end;

  // Personalisation length
  if (config.Personalisation <> Nil) then
  begin
    if (System.Length(config.Personalisation) <> 8) then
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

    if (treeConfig.InnerHashSize > 32) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes
        (@STreeIncorrectInnerHashSize);
    end;
  end;

end;

class constructor TBlake2SIvBuilder.Blake2SIvBuilder;
begin
  FSequentialTreeConfig := TBlake2STreeConfig.Create();
  FSequentialTreeConfig.FanOut := 1;
  FSequentialTreeConfig.MaxDepth := 1;
  FSequentialTreeConfig.LeafSize := 0;
  FSequentialTreeConfig.NodeOffset := 0;
  FSequentialTreeConfig.NodeDepth := 0;
  FSequentialTreeConfig.InnerHashSize := 0;
  FSequentialTreeConfig.IsLastNode := False;
end;

class function TBlake2SIvBuilder.ConfigS(const config: IBlake2SConfig;
  var treeConfig: IBlake2STreeConfig): THashLibUInt32Array;
var
  isSequential: Boolean;
  tempBuffer: THashLibByteArray;
begin
  isSequential := treeConfig = Nil;
  if (isSequential) then
  begin
    treeConfig := FSequentialTreeConfig;
  end;

  VerifyConfigS(config, treeConfig, isSequential);

  System.SetLength(tempBuffer, 32);

  tempBuffer[0] := config.HashSize;
  tempBuffer[1] := System.Length(config.Key);

  if treeConfig <> Nil then
  begin
    tempBuffer[2] := treeConfig.FanOut;
    tempBuffer[3] := treeConfig.MaxDepth;
    TConverters.ReadUInt32AsBytesLE(treeConfig.LeafSize, tempBuffer, 4);
    tempBuffer[8] := Byte(treeConfig.NodeOffset);
    tempBuffer[9] := Byte(treeConfig.NodeOffset shr 8);
    tempBuffer[10] := Byte(treeConfig.NodeOffset shr 16);
    tempBuffer[11] := Byte(treeConfig.NodeOffset shr 24);
    tempBuffer[12] := Byte(treeConfig.NodeOffset shr 32);
    tempBuffer[13] := Byte(treeConfig.NodeOffset shr 40);
    tempBuffer[14] := treeConfig.NodeDepth;
    tempBuffer[15] := treeConfig.InnerHashSize;
  end;

  if config.Salt <> Nil then
  begin
    System.Move(config.Salt[0], tempBuffer[16], 8 * System.SizeOf(Byte));
  end;

  if config.Personalisation <> Nil then
  begin
    System.Move(config.Personalisation[0], tempBuffer[24],
      8 * System.SizeOf(Byte));
  end;

  System.SetLength(Result, 8);
  TConverters.le32_copy(PByte(tempBuffer), 0, PCardinal(Result), 0,
    System.Length(tempBuffer) * System.SizeOf(Byte));
end;

end.
