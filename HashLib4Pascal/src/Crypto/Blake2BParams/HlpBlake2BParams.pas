unit HlpBlake2BParams;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpIBlake2BParams,
  HlpHashSize,
  HlpArrayUtils,
  HlpHashLibTypes,
  HlpConverters;

resourcestring
  SInvalidHashSize =
    'BLAKE2B HashSize must be restricted to one of the following [1 .. 64], "%d"';
  SInvalidKeyLength = '"Key" Length Must Not Be Greater Than 64, "%d"';
  SInvalidPersonalisationLength =
    '"Personalisation" Length Must Be Equal To 16, "%d"';
  SInvalidSaltLength = '"Salt" Length Must Be Equal To 16, "%d"';

  SInvalidFanOutParameter =
    'FanOut Value Should be Between [0 .. 255] for Blake2B';
  SInvalidMaxDepthParameter =
    'MaxDepth Value Should be Between [1 .. 255] for Blake2B';
  SInvalidNodeDepthParameter =
    'NodeDepth Value Should be Between [0 .. 255] for Blake2B';
  SInvalidInnerHashSizeParameter =
    'InnerHashSize Value Should be Between [0 .. 64] for Blake2B';
  SInvalidNodeOffsetParameter =
    'NodeOffset Value Should be Between [0 .. (2^64-1)] for Blake2B';

  STreeIncorrectInnerHashSize =
    'Tree Inner Hash Size Must Not Be Greater Than 64, "%d"';

type
  TBlake2BConfig = class sealed(TInterfacedObject, IBlake2BConfig)

  strict private

  var

    FHashSize: Int32;
    FPersonalisation, FSalt, FKey: THashLibByteArray;

    class function GetDefaultConfig: IBlake2BConfig; static;

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
    constructor Create(AHashSize: THashSize = THashSize.hsHashSize512);
      overload;
    constructor Create(AHashSize: Int32); overload;
    destructor Destroy; override;
    property Personalisation: THashLibByteArray read GetPersonalisation
      write SetPersonalisation;
    property Salt: THashLibByteArray read GetSalt write SetSalt;
    property Key: THashLibByteArray read GetKey write SetKey;
    property HashSize: Int32 read GetHashSize write SetHashSize;

    class property DefaultConfig: IBlake2BConfig read GetDefaultConfig;

    function Clone(): IBlake2BConfig;

    procedure Clear();

  end;

type
  TBlake2BTreeConfig = class sealed(TInterfacedObject, IBlake2BTreeConfig)

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

    class function GetSequentialTreeConfig: IBlake2BTreeConfig; static;

  public
    constructor Create();

    property FanOut: Byte read GetFanOut write SetFanOut;

    property MaxDepth: Byte read GetMaxDepth write SetMaxDepth;

    property NodeDepth: Byte read GetNodeDepth write SetNodeDepth;

    property InnerHashSize: Byte read GetInnerHashSize write SetInnerHashSize;

    property LeafSize: UInt32 read GetLeafSize write SetLeafSize;

    property NodeOffset: UInt64 read GetNodeOffset write SetNodeOffset;

    property IsLastNode: Boolean read GetIsLastNode write SetIsLastNode;

    function Clone(): IBlake2BTreeConfig;

    class property SequentialTreeConfig: IBlake2BTreeConfig
      read GetSequentialTreeConfig;

  end;

type
  TBlake2BIvBuilder = class sealed(TObject)

  strict private

    class procedure VerifyConfigB(const AConfig: IBlake2BConfig;
      const ATreeConfig: IBlake2BTreeConfig; AIsSequential: Boolean); static;

  public
    class function ConfigB(const AConfig: IBlake2BConfig;
      var ATreeConfig: IBlake2BTreeConfig): THashLibUInt64Array; static;

  end;

implementation

{ TBlake2BConfig }

class function TBlake2BConfig.GetDefaultConfig: IBlake2BConfig;
begin
  Result := TBlake2BConfig.Create();
end;

procedure TBlake2BConfig.ValidateHashSize(AHashSize: Int32);
begin
  if not((AHashSize) in [1 .. 64]) or (((AHashSize * 8) and 7) <> 0) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SInvalidHashSize,
      [AHashSize]);
  end;
end;

procedure TBlake2BConfig.ValidateKeyLength(const AKey: THashLibByteArray);
var
  KeyLength: Int32;
begin
  if (AKey <> Nil) then
  begin
    KeyLength := System.Length(AKey);
    if (KeyLength > 64) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength]);
    end;
  end;
end;

procedure TBlake2BConfig.ValidatePersonalisationLength(const APersonalisation
  : THashLibByteArray);
var
  PersonalisationLength: Int32;
begin
  if (APersonalisation <> Nil) then
  begin
    PersonalisationLength := System.Length(APersonalisation);
    if (PersonalisationLength <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@SInvalidPersonalisationLength, [PersonalisationLength]);
    end;
  end;
end;

procedure TBlake2BConfig.ValidateSaltLength(const ASalt: THashLibByteArray);
var
  SaltLength: Int32;
begin
  if (ASalt <> Nil) then
  begin
    SaltLength := System.Length(ASalt);
    if (SaltLength <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@SInvalidSaltLength, [SaltLength]);
    end;
  end;
end;

function TBlake2BConfig.GetHashSize: Int32;
begin
  Result := FHashSize;
end;

function TBlake2BConfig.GetKey: THashLibByteArray;
begin
  Result := FKey;
end;

function TBlake2BConfig.GetPersonalisation: THashLibByteArray;
begin
  Result := FPersonalisation;
end;

function TBlake2BConfig.GetSalt: THashLibByteArray;
begin
  Result := FSalt;
end;

procedure TBlake2BConfig.SetHashSize(AValue: Int32);
begin
  ValidateHashSize(AValue);
  FHashSize := AValue;
end;

procedure TBlake2BConfig.SetKey(const AValue: THashLibByteArray);
begin
  ValidateKeyLength(AValue);
  FKey := System.Copy(AValue);
end;

procedure TBlake2BConfig.SetPersonalisation(const AValue: THashLibByteArray);
begin
  ValidatePersonalisationLength(AValue);
  FPersonalisation := System.Copy(AValue);
end;

procedure TBlake2BConfig.SetSalt(const AValue: THashLibByteArray);
begin
  ValidateSaltLength(AValue);
  FSalt := System.Copy(AValue);
end;

constructor TBlake2BConfig.Create(AHashSize: THashSize);
var
  LHashSize: Int32;
begin
  Inherited Create();
  LHashSize := Int32(AHashSize);
  ValidateHashSize(LHashSize);
  FHashSize := LHashSize;
end;

constructor TBlake2BConfig.Create(AHashSize: Int32);
begin
  Inherited Create();
  ValidateHashSize(AHashSize);
  FHashSize := AHashSize;
end;

procedure TBlake2BConfig.Clear;
begin
  TArrayUtils.ZeroFill(FKey);
end;

destructor TBlake2BConfig.Destroy;
begin
  Clear();
  inherited Destroy;
end;

function TBlake2BConfig.Clone(): IBlake2BConfig;
begin
  Result := TBlake2BConfig.Create(FHashSize);
  Result.Key := System.Copy(FKey);
  Result.Personalisation := System.Copy(FPersonalisation);
  Result.Salt := System.Copy(FSalt);
end;

{ TBlake2BTreeConfig }

procedure TBlake2BTreeConfig.ValidateFanOut(AFanOut: Byte);
begin
  if not(AFanOut in [0 .. 255]) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes(@SInvalidFanOutParameter);
  end;
end;

procedure TBlake2BTreeConfig.ValidateInnerHashSize(AInnerHashSize: Byte);
begin
  if not(AInnerHashSize in [0 .. 64]) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes
      (@SInvalidInnerHashSizeParameter);
  end;
end;

procedure TBlake2BTreeConfig.ValidateMaxDepth(AMaxDepth: Byte);
begin
  if not(AMaxDepth in [1 .. 255]) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes
      (@SInvalidMaxDepthParameter);
  end;
end;

procedure TBlake2BTreeConfig.ValidateNodeDepth(ANodeDepth: Byte);
begin
  if not(ANodeDepth in [0 .. 255]) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes
      (@SInvalidNodeDepthParameter);
  end;
end;

procedure TBlake2BTreeConfig.ValidateNodeOffset(ANodeOffset: UInt64);
begin
  // ANodeOffset > ((2^64) - 1)
  if ANodeOffset > System.High(UInt64) then
  begin
    raise EArgumentInvalidHashLibException.CreateRes
      (@SInvalidNodeOffsetParameter);
  end;
end;

function TBlake2BTreeConfig.GetFanOut: Byte;
begin
  Result := FFanOut;
end;

function TBlake2BTreeConfig.GetInnerHashSize: Byte;
begin
  Result := FInnerHashSize;
end;

function TBlake2BTreeConfig.GetIsLastNode: Boolean;
begin
  Result := FIsLastNode;
end;

function TBlake2BTreeConfig.GetLeafSize: UInt32;
begin
  Result := FLeafSize;
end;

function TBlake2BTreeConfig.GetMaxDepth: Byte;
begin
  Result := FMaxDepth;
end;

function TBlake2BTreeConfig.GetNodeDepth: Byte;
begin
  Result := FNodeDepth;
end;

function TBlake2BTreeConfig.GetNodeOffset: UInt64;
begin
  Result := FNodeOffset;
end;

procedure TBlake2BTreeConfig.SetFanOut(AValue: Byte);
begin
  ValidateFanOut(AValue);
  FFanOut := AValue;
end;

procedure TBlake2BTreeConfig.SetInnerHashSize(AValue: Byte);
begin
  ValidateInnerHashSize(AValue);
  FInnerHashSize := AValue;
end;

procedure TBlake2BTreeConfig.SetIsLastNode(AValue: Boolean);
begin
  FIsLastNode := AValue;
end;

procedure TBlake2BTreeConfig.SetLeafSize(AValue: UInt32);
begin
  FLeafSize := AValue;
end;

procedure TBlake2BTreeConfig.SetMaxDepth(AValue: Byte);
begin
  ValidateMaxDepth(AValue);
  FMaxDepth := AValue;
end;

procedure TBlake2BTreeConfig.SetNodeDepth(AValue: Byte);
begin
  ValidateNodeDepth(AValue);
  FNodeDepth := AValue;
end;

procedure TBlake2BTreeConfig.SetNodeOffset(AValue: UInt64);
begin
  ValidateNodeOffset(AValue);
  FNodeOffset := AValue;
end;

constructor TBlake2BTreeConfig.Create;
begin
  Inherited Create();
  FFanOut := 0;
  FMaxDepth := 0;
  FLeafSize := 64;
  FNodeOffset := 0;
  FNodeDepth := 0;
  FInnerHashSize := 64;
  FIsLastNode := False;
end;

function TBlake2BTreeConfig.Clone(): IBlake2BTreeConfig;
var
  LResult: TBlake2BTreeConfig;
begin
  LResult := TBlake2BTreeConfig.Create();
  LResult.FFanOut := FFanOut;
  LResult.FInnerHashSize := FInnerHashSize;
  LResult.FMaxDepth := FMaxDepth;
  LResult.FNodeDepth := FNodeDepth;
  LResult.FLeafSize := FLeafSize;
  LResult.FNodeOffset := FNodeOffset;
  LResult.FIsLastNode := FIsLastNode;
  Result := LResult as IBlake2BTreeConfig;
end;

class function TBlake2BTreeConfig.GetSequentialTreeConfig: IBlake2BTreeConfig;
begin
  Result := TBlake2BTreeConfig.Create();
  Result.FanOut := 1;
  Result.MaxDepth := 1;
  Result.LeafSize := 0;
  Result.NodeOffset := 0;
  Result.NodeDepth := 0;
  Result.InnerHashSize := 0;
  Result.IsLastNode := False;
end;

{ TBlake2BIvBuilder }

class procedure TBlake2BIvBuilder.VerifyConfigB(const AConfig: IBlake2BConfig;
  const ATreeConfig: IBlake2BTreeConfig; AIsSequential: Boolean);
begin

  // digest length
  if ((AConfig.HashSize <= 0) or (AConfig.HashSize > 64)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SInvalidHashSize,
      [AConfig.HashSize]);
  end;

  // Key length
  if (AConfig.Key <> Nil) then
  begin
    if (System.Length(AConfig.Key) > 64) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SInvalidKeyLength,
        [System.Length(AConfig.Key)]);
    end;
  end;

  // Personalisation length
  if (AConfig.Personalisation <> Nil) then
  begin
    if (System.Length(AConfig.Personalisation) <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@SInvalidPersonalisationLength,
        [System.Length(AConfig.Personalisation)]);
    end;
  end;

  // Salt length
  if (AConfig.Salt <> Nil) then
  begin
    if (System.Length(AConfig.Salt) <> 16) then
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

    if (ATreeConfig.InnerHashSize > 64) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateResFmt
        (@STreeIncorrectInnerHashSize, [ATreeConfig.InnerHashSize]);
    end;
  end;

end;

class function TBlake2BIvBuilder.ConfigB(const AConfig: IBlake2BConfig;
  var ATreeConfig: IBlake2BTreeConfig): THashLibUInt64Array;
var
  LIsSequential: Boolean;
  LBuffer: THashLibByteArray;
begin
  LIsSequential := ATreeConfig = Nil;
  if (LIsSequential) then
  begin
    ATreeConfig := TBlake2BTreeConfig.SequentialTreeConfig;
  end;

  VerifyConfigB(AConfig, ATreeConfig, LIsSequential);

  System.SetLength(LBuffer, 64);

  LBuffer[0] := AConfig.HashSize;
  LBuffer[1] := System.Length(AConfig.Key);

  if ATreeConfig <> Nil then
  begin
    LBuffer[2] := ATreeConfig.FanOut;
    LBuffer[3] := ATreeConfig.MaxDepth;
    TConverters.ReadUInt32AsBytesLE(ATreeConfig.LeafSize, LBuffer, 4);
    TConverters.ReadUInt64AsBytesLE(ATreeConfig.NodeOffset, LBuffer, 8);
    LBuffer[16] := ATreeConfig.NodeDepth;
    LBuffer[17] := ATreeConfig.InnerHashSize;
  end;

  if AConfig.Salt <> Nil then
  begin
    System.Move(AConfig.Salt[0], LBuffer[32], 16 * System.SizeOf(Byte));
  end;

  if AConfig.Personalisation <> Nil then
  begin
    System.Move(AConfig.Personalisation[0], LBuffer[48],
      16 * System.SizeOf(Byte));
  end;

  System.SetLength(Result, 8);
  TConverters.le64_copy(PByte(LBuffer), 0, PUInt64(Result), 0,
    System.Length(LBuffer) * System.SizeOf(Byte));
end;

end.
