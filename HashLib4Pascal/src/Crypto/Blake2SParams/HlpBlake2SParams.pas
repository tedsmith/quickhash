unit HlpBlake2SParams;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpIBlake2SParams,
  HlpHashSize,
  HlpArrayUtils,
  HlpHashLibTypes,
  HlpConverters;

resourcestring
  SInvalidHashSize =
    'BLAKE2S HashSize must be restricted to one of the following [1 .. 32], "%d"';
  SInvalidKeyLength = '"Key" Length Must Not Be Greater Than 32, "%d"';
  SInvalidPersonalisationLength =
    '"Personalisation" Length Must Be Equal To 8, "%d"';
  SInvalidSaltLength = '"Salt" Length Must Be Equal To 8, "%d"';

  SInvalidFanOutParameter =
    'FanOut Value Should be Between [0 .. 255] for Blake2S';
  SInvalidMaxDepthParameter =
    'MaxDepth Value Should be Between [1 .. 255] for Blake2S';
  SInvalidNodeDepthParameter =
    'NodeDepth Value Should be Between [0 .. 255] for Blake2S';
  SInvalidInnerHashSizeParameter =
    'InnerHashSize Value Should be Between [0 .. 32] for Blake2S';
  SInvalidNodeOffsetParameter =
    'NodeOffset Value Should be Between [0 .. (2^48-1)] for Blake2S';

  STreeIncorrectInnerHashSize =
    'Tree Inner Hash Size Must Not Be Greater Than 32, "%d"';

type
  TBlake2SConfig = class sealed(TInterfacedObject, IBlake2SConfig)

  strict private

  var

    FHashSize: Int32;
    FPersonalisation, FSalt, FKey: THashLibByteArray;

    class function GetDefaultConfig: IBlake2SConfig; static;

    procedure ValidateHashSize(AHashSize: Int32); inline;
    procedure ValidateKeyLength(const AKey: THashLibByteArray); inline;
    procedure ValidatePersonalisationLength(const APersonalisation
      : THashLibByteArray); inline;
    procedure ValidateSaltLength(const ASalt: THashLibByteArray); inline;

    function GetPersonalisation: THashLibByteArray; inline;
    procedure SetPersonalisation(const AValue: THashLibByteArray); inline;

    function GetSalt: THashLibByteArray; inline;
    procedure SetSalt(const AValue: THashLibByteArray); inline;

    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const AValue: THashLibByteArray); inline;

    function GetHashSize: Int32; inline;
    procedure SetHashSize(AValue: Int32); inline;

  public
    constructor Create(AHashSize: THashSize = THashSize.hsHashSize256);
      overload;
    constructor Create(AHashSize: Int32); overload;
    destructor Destroy; override;
    property Personalisation: THashLibByteArray read GetPersonalisation
      write SetPersonalisation;
    property Salt: THashLibByteArray read GetSalt write SetSalt;
    property Key: THashLibByteArray read GetKey write SetKey;
    property HashSize: Int32 read GetHashSize write SetHashSize;

    class property DefaultConfig: IBlake2SConfig read GetDefaultConfig;

    function Clone(): IBlake2SConfig;

    procedure Clear();

  end;

type
  TBlake2STreeConfig = class sealed(TInterfacedObject, IBlake2STreeConfig)

  strict private
  var
    FFanOut, FMaxDepth, FNodeDepth, FInnerHashSize: Byte;
    FLeafSize: UInt32;
    FNodeOffset: UInt64;
    FIsLastNode: Boolean;

    procedure ValidateFanOut(AFanOut: Byte); inline;
    procedure ValidateInnerHashSize(AInnerHashSize: Byte); inline;
    procedure ValidateMaxDepth(AMaxDepth: Byte); inline;
    procedure ValidateNodeDepth(ANodeDepth: Byte); inline;
    procedure ValidateNodeOffset(ANodeOffset: UInt64); inline;

    function GetFanOut: Byte; inline;
    procedure SetFanOut(AValue: Byte); inline;

    function GetMaxDepth: Byte; inline;
    procedure SetMaxDepth(AValue: Byte); inline;

    function GetNodeDepth: Byte; inline;
    procedure SetNodeDepth(AValue: Byte); inline;

    function GetInnerHashSize: Byte; inline;
    procedure SetInnerHashSize(AValue: Byte); inline;

    function GetLeafSize: UInt32; inline;
    procedure SetLeafSize(AValue: UInt32); inline;

    function GetNodeOffset: UInt64; inline;
    procedure SetNodeOffset(AValue: UInt64); inline;

    function GetIsLastNode: Boolean; inline;
    procedure SetIsLastNode(AValue: Boolean); inline;

    class function GetSequentialTreeConfig: IBlake2STreeConfig; static;

  public
    constructor Create();

    property FanOut: Byte read GetFanOut write SetFanOut;

    property MaxDepth: Byte read GetMaxDepth write SetMaxDepth;

    property NodeDepth: Byte read GetNodeDepth write SetNodeDepth;

    property InnerHashSize: Byte read GetInnerHashSize write SetInnerHashSize;

    property LeafSize: UInt32 read GetLeafSize write SetLeafSize;

    property NodeOffset: UInt64 read GetNodeOffset write SetNodeOffset;

    property IsLastNode: Boolean read GetIsLastNode write SetIsLastNode;

    function Clone(): IBlake2STreeConfig;

    class property SequentialTreeConfig: IBlake2STreeConfig
      read GetSequentialTreeConfig;

  end;

type
  TBlake2SIvBuilder = class sealed(TObject)

  strict private

    class procedure VerifyConfigS(const AConfig: IBlake2SConfig;
      const ATreeConfig: IBlake2STreeConfig; AIsSequential: Boolean); static;

  public
    class function ConfigS(const AConfig: IBlake2SConfig;
      var ATreeConfig: IBlake2STreeConfig): THashLibUInt32Array; static;

  end;

implementation

{ TBlake2SConfig }

class function TBlake2SConfig.GetDefaultConfig: IBlake2SConfig;
begin
  Result := TBlake2SConfig.Create();
end;

procedure TBlake2SConfig.ValidateHashSize(AHashSize: Int32);
begin
  if not((AHashSize) in [1 .. 32]) or (((AHashSize * 8) and 7) <> 0) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SInvalidHashSize,
      [AHashSize]);
  end;
end;

procedure TBlake2SConfig.ValidateKeyLength(const AKey: THashLibByteArray);
var
  KeyLength: Int32;
begin
  if (AKey <> Nil) then
  begin
    KeyLength := System.Length(AKey);
    if (KeyLength > 32) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength]);
    end;
  end;
end;

procedure TBlake2SConfig.ValidatePersonalisationLength(const APersonalisation
  : THashLibByteArray);
var
  PersonalisationLength: Int32;
begin
  if (APersonalisation <> Nil) then
  begin
    PersonalisationLength := System.Length(APersonalisation);
    if (PersonalisationLength <> 8) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@SInvalidPersonalisationLength, [PersonalisationLength]);
    end;
  end;
end;

procedure TBlake2SConfig.ValidateSaltLength(const ASalt: THashLibByteArray);
var
  SaltLength: Int32;
begin
  if (ASalt <> Nil) then
  begin
    SaltLength := System.Length(ASalt);
    if (SaltLength <> 8) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@SInvalidSaltLength, [SaltLength]);
    end;
  end;
end;

function TBlake2SConfig.GetHashSize: Int32;
begin
  Result := FHashSize;
end;

function TBlake2SConfig.GetKey: THashLibByteArray;
begin
  Result := FKey;
end;

function TBlake2SConfig.GetPersonalisation: THashLibByteArray;
begin
  Result := FPersonalisation;
end;

function TBlake2SConfig.GetSalt: THashLibByteArray;
begin
  Result := FSalt;
end;

procedure TBlake2SConfig.SetHashSize(AValue: Int32);
begin
  ValidateHashSize(AValue);
  FHashSize := AValue;
end;

procedure TBlake2SConfig.SetKey(const AValue: THashLibByteArray);
begin
  ValidateKeyLength(AValue);
  FKey := System.Copy(AValue);
end;

procedure TBlake2SConfig.SetPersonalisation(const AValue: THashLibByteArray);
begin
  ValidatePersonalisationLength(AValue);
  FPersonalisation := System.Copy(AValue);
end;

procedure TBlake2SConfig.SetSalt(const AValue: THashLibByteArray);
begin
  ValidateSaltLength(AValue);
  FSalt := System.Copy(AValue);
end;

constructor TBlake2SConfig.Create(AHashSize: THashSize);
var
  LHashSize: Int32;
begin
  Inherited Create();
  LHashSize := Int32(AHashSize);
  ValidateHashSize(LHashSize);
  FHashSize := LHashSize;
end;

constructor TBlake2SConfig.Create(AHashSize: Int32);
begin
  Inherited Create();
  ValidateHashSize(AHashSize);
  FHashSize := AHashSize;
end;

procedure TBlake2SConfig.Clear;
begin
  TArrayUtils.ZeroFill(FKey);
end;

destructor TBlake2SConfig.Destroy;
begin
  Clear();
  inherited Destroy;
end;

function TBlake2SConfig.Clone(): IBlake2SConfig;
begin
  Result := TBlake2SConfig.Create(FHashSize);
  Result.Key := System.Copy(FKey);
  Result.Personalisation := System.Copy(FPersonalisation);
  Result.Salt := System.Copy(FSalt);
end;

{ TBlake2STreeConfig }

procedure TBlake2STreeConfig.ValidateFanOut(AFanOut: Byte);
begin
  if not(AFanOut in [0 .. 255]) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes(@SInvalidFanOutParameter);
  end;
end;

procedure TBlake2STreeConfig.ValidateInnerHashSize(AInnerHashSize: Byte);
begin
  if not(AInnerHashSize in [0 .. 32]) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes
      (@SInvalidInnerHashSizeParameter);
  end;
end;

procedure TBlake2STreeConfig.ValidateMaxDepth(AMaxDepth: Byte);
begin
  if not(AMaxDepth in [1 .. 255]) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes
      (@SInvalidMaxDepthParameter);
  end;
end;

procedure TBlake2STreeConfig.ValidateNodeDepth(ANodeDepth: Byte);
begin
  if not(ANodeDepth in [0 .. 255]) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes
      (@SInvalidNodeDepthParameter);
  end;
end;

procedure TBlake2STreeConfig.ValidateNodeOffset(ANodeOffset: UInt64);
begin
  if ANodeOffset > UInt64((UInt64(1) shl 48) - 1) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes
      (@SInvalidNodeOffsetParameter);
  end;
end;

function TBlake2STreeConfig.GetFanOut: Byte;
begin
  Result := FFanOut;
end;

function TBlake2STreeConfig.GetInnerHashSize: Byte;
begin
  Result := FInnerHashSize;
end;

function TBlake2STreeConfig.GetIsLastNode: Boolean;
begin
  Result := FIsLastNode;
end;

function TBlake2STreeConfig.GetLeafSize: UInt32;
begin
  Result := FLeafSize;
end;

function TBlake2STreeConfig.GetMaxDepth: Byte;
begin
  Result := FMaxDepth;
end;

function TBlake2STreeConfig.GetNodeDepth: Byte;
begin
  Result := FNodeDepth;
end;

function TBlake2STreeConfig.GetNodeOffset: UInt64;
begin
  Result := FNodeOffset;
end;

procedure TBlake2STreeConfig.SetFanOut(AValue: Byte);
begin
  ValidateFanOut(AValue);
  FFanOut := AValue;
end;

procedure TBlake2STreeConfig.SetInnerHashSize(AValue: Byte);
begin
  ValidateInnerHashSize(AValue);
  FInnerHashSize := AValue;
end;

procedure TBlake2STreeConfig.SetIsLastNode(AValue: Boolean);
begin
  FIsLastNode := AValue;
end;

procedure TBlake2STreeConfig.SetLeafSize(AValue: UInt32);
begin
  FLeafSize := AValue;
end;

procedure TBlake2STreeConfig.SetMaxDepth(AValue: Byte);
begin
  ValidateMaxDepth(AValue);
  FMaxDepth := AValue;
end;

procedure TBlake2STreeConfig.SetNodeDepth(AValue: Byte);
begin
  ValidateNodeDepth(AValue);
  FNodeDepth := AValue;
end;

procedure TBlake2STreeConfig.SetNodeOffset(AValue: UInt64);
begin
  ValidateNodeOffset(AValue);
  FNodeOffset := AValue;
end;

constructor TBlake2STreeConfig.Create;
begin
  Inherited Create();
  FFanOut := 0;
  FMaxDepth := 0;
  FLeafSize := 32;
  FNodeOffset := 0;
  FNodeDepth := 0;
  FInnerHashSize := 32;
  FIsLastNode := False;
end;

function TBlake2STreeConfig.Clone(): IBlake2STreeConfig;
var
  LResult: TBlake2STreeConfig;
begin
  LResult := TBlake2STreeConfig.Create();
  LResult.FFanOut := FFanOut;
  LResult.FInnerHashSize := FInnerHashSize;
  LResult.FMaxDepth := FMaxDepth;
  LResult.FNodeDepth := FNodeDepth;
  LResult.FLeafSize := FLeafSize;
  LResult.FNodeOffset := FNodeOffset;
  LResult.FIsLastNode := FIsLastNode;
  Result := LResult as IBlake2STreeConfig;
end;

class function TBlake2STreeConfig.GetSequentialTreeConfig: IBlake2STreeConfig;
begin
  Result := TBlake2STreeConfig.Create();
  Result.FanOut := 1;
  Result.MaxDepth := 1;
  Result.LeafSize := 0;
  Result.NodeOffset := 0;
  Result.NodeDepth := 0;
  Result.InnerHashSize := 0;
  Result.IsLastNode := False;
end;

{ TBlake2SIvBuilder }

class procedure TBlake2SIvBuilder.VerifyConfigS(const AConfig: IBlake2SConfig;
  const ATreeConfig: IBlake2STreeConfig; AIsSequential: Boolean);
begin

  // digest length
  if ((AConfig.HashSize <= 0) or (AConfig.HashSize > 32)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SInvalidHashSize,
      [AConfig.HashSize]);
  end;

  // Key length
  if (AConfig.Key <> Nil) then
  begin
    if (System.Length(AConfig.Key) > 32) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SInvalidKeyLength,
        [System.Length(AConfig.Key)]);
    end;
  end;

  // Personalisation length
  if (AConfig.Personalisation <> Nil) then
  begin
    if (System.Length(AConfig.Personalisation) <> 8) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@SInvalidPersonalisationLength,
        [System.Length(AConfig.Personalisation)]);
    end;
  end;

  // Salt length
  if (AConfig.Salt <> Nil) then
  begin
    if (System.Length(AConfig.Salt) <> 8) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@SInvalidSaltLength, [System.Length(AConfig.Salt)]);
    end;
  end;

  // Tree InnerHashSize
  if (ATreeConfig <> Nil) then
  begin

    if ((AIsSequential) and ((ATreeConfig.InnerHashSize <> 0))) then
    begin
      raise EArgumentOutOfRangeHashLibException.Create
        ('treeConfig.TreeIntermediateHashSize');
    end;

    if (ATreeConfig.InnerHashSize > 32) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@STreeIncorrectInnerHashSize, [ATreeConfig.InnerHashSize]);
    end;
  end;

end;

class function TBlake2SIvBuilder.ConfigS(const AConfig: IBlake2SConfig;
  var ATreeConfig: IBlake2STreeConfig): THashLibUInt32Array;
var
  LIsSequential: Boolean;
  LBuffer: THashLibByteArray;
begin
  LIsSequential := ATreeConfig = Nil;
  if (LIsSequential) then
  begin
    ATreeConfig := TBlake2STreeConfig.SequentialTreeConfig;
  end;

  VerifyConfigS(AConfig, ATreeConfig, LIsSequential);

  System.SetLength(LBuffer, 32);

  LBuffer[0] := AConfig.HashSize;
  LBuffer[1] := System.Length(AConfig.Key);

  if ATreeConfig <> Nil then
  begin
    LBuffer[2] := ATreeConfig.FanOut;
    LBuffer[3] := ATreeConfig.MaxDepth;
    TConverters.ReadUInt32AsBytesLE(ATreeConfig.LeafSize, LBuffer, 4);
    LBuffer[8] := Byte(ATreeConfig.NodeOffset);
    LBuffer[9] := Byte(ATreeConfig.NodeOffset shr 8);
    LBuffer[10] := Byte(ATreeConfig.NodeOffset shr 16);
    LBuffer[11] := Byte(ATreeConfig.NodeOffset shr 24);
    LBuffer[12] := Byte(ATreeConfig.NodeOffset shr 32);
    LBuffer[13] := Byte(ATreeConfig.NodeOffset shr 40);
    LBuffer[14] := ATreeConfig.NodeDepth;
    LBuffer[15] := ATreeConfig.InnerHashSize;
  end;

  if AConfig.Salt <> Nil then
  begin
    System.Move(AConfig.Salt[0], LBuffer[16], 8 * System.SizeOf(Byte));
  end;

  if AConfig.Personalisation <> Nil then
  begin
    System.Move(AConfig.Personalisation[0], LBuffer[24],
      8 * System.SizeOf(Byte));
  end;

  System.SetLength(Result, 8);
  TConverters.le32_copy(PByte(LBuffer), 0, PCardinal(Result), 0,
    System.Length(LBuffer) * System.SizeOf(Byte));
end;

end.
