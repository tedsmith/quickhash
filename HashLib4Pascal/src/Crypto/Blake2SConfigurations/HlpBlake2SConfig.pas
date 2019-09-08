unit HlpBlake2SConfig;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpIBlake2SConfig,
  HlpHashSize,
  HlpHashLibTypes;

resourcestring
  SInvalidHashSize =
    'BLAKE2S HashSize must be restricted to one of the following [1 .. 32], "%d"';
  SInvalidKeyLength = '"Key" Length Must Not Be Greater Than 32, "%d"';
  SInvalidPersonalisationLength =
    '"Personalisation" Length Must Be Equal To 8, "%d"';
  SInvalidSaltLength = '"Salt" Length Must Be Equal To 8, "%d"';

type

  TBlake2SConfig = class sealed(TInterfacedObject, IBlake2SConfig)

  strict private

  var

    FHashSize: Int32;
    FPersonalisation, FSalt, FKey: THashLibByteArray;

    procedure ValidateHashSize(AHashSize: Int32); inline;
    procedure ValidateKeyLength(const AKey: THashLibByteArray); inline;
    procedure ValidatePersonalisationLength(const APersonalisation
      : THashLibByteArray); inline;
    procedure ValidateSaltLength(const ASalt: THashLibByteArray); inline;

    function GetPersonalisation: THashLibByteArray; inline;
    procedure SetPersonalisation(const value: THashLibByteArray); inline;

    function GetSalt: THashLibByteArray; inline;
    procedure SetSalt(const value: THashLibByteArray); inline;

    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const value: THashLibByteArray); inline;

    function GetHashSize: Int32; inline;
    procedure SetHashSize(value: Int32); inline;

  public
    constructor Create(AHashSize: THashSize = THashSize.hsHashSize256);
      overload;
    constructor Create(AHashSize: Int32); overload;
    property Personalisation: THashLibByteArray read GetPersonalisation
      write SetPersonalisation;
    property Salt: THashLibByteArray read GetSalt write SetSalt;
    property Key: THashLibByteArray read GetKey write SetKey;
    property HashSize: Int32 read GetHashSize write SetHashSize;

  end;

implementation

{ TBlake2SConfig }

procedure TBlake2SConfig.ValidateHashSize(AHashSize: Int32);
begin
  if not((AHashSize) in [1 .. 32]) or (((AHashSize * 8) and 7) <> 0) then
  begin
    raise EArgumentHashLibException.CreateResFmt(@SInvalidHashSize,
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
  result := FHashSize;
end;

function TBlake2SConfig.GetKey: THashLibByteArray;
begin
  result := FKey;
end;

function TBlake2SConfig.GetPersonalisation: THashLibByteArray;
begin
  result := FPersonalisation;
end;

function TBlake2SConfig.GetSalt: THashLibByteArray;
begin
  result := FSalt;
end;

procedure TBlake2SConfig.SetHashSize(value: Int32);
begin
  ValidateHashSize(value);
  FHashSize := value;
end;

procedure TBlake2SConfig.SetKey(const value: THashLibByteArray);
begin
  ValidateKeyLength(value);
  FKey := value;
end;

procedure TBlake2SConfig.SetPersonalisation(const value: THashLibByteArray);
begin
  ValidatePersonalisationLength(value);
  FPersonalisation := value;
end;

procedure TBlake2SConfig.SetSalt(const value: THashLibByteArray);
begin
  ValidateSaltLength(value);
  FSalt := value;
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

end.
