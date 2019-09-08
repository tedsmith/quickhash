unit HlpBlake2BConfig;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpIBlake2BConfig,
  HlpHashSize,
  HlpHashLibTypes;

resourcestring
  SInvalidHashSize =
    'BLAKE2B HashSize must be restricted to one of the following [1 .. 64], "%d"';
  SInvalidKeyLength = '"Key" Length Must Not Be Greater Than 64, "%d"';
  SInvalidPersonalisationLength =
    '"Personalisation" Length Must Be Equal To 16, "%d"';
  SInvalidSaltLength = '"Salt" Length Must Be Equal To 16, "%d"';

type

  TBlake2BConfig = class sealed(TInterfacedObject, IBlake2BConfig)

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
    constructor Create(AHashSize: THashSize = THashSize.hsHashSize512);
      overload;
    constructor Create(AHashSize: Int32); overload;
    property Personalisation: THashLibByteArray read GetPersonalisation
      write SetPersonalisation;
    property Salt: THashLibByteArray read GetSalt write SetSalt;
    property Key: THashLibByteArray read GetKey write SetKey;
    property HashSize: Int32 read GetHashSize write SetHashSize;

  end;

implementation

{ TBlake2BConfig }

procedure TBlake2BConfig.ValidateHashSize(AHashSize: Int32);
begin
  if not((AHashSize) in [1 .. 64]) or (((AHashSize * 8) and 7) <> 0) then
  begin
    raise EArgumentHashLibException.CreateResFmt(@SInvalidHashSize,
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
  result := FHashSize;
end;

function TBlake2BConfig.GetKey: THashLibByteArray;
begin
  result := FKey;
end;

function TBlake2BConfig.GetPersonalisation: THashLibByteArray;
begin
  result := FPersonalisation;
end;

function TBlake2BConfig.GetSalt: THashLibByteArray;
begin
  result := FSalt;
end;

procedure TBlake2BConfig.SetHashSize(value: Int32);
begin
  ValidateHashSize(value);
  FHashSize := value;
end;

procedure TBlake2BConfig.SetKey(const value: THashLibByteArray);
begin
  ValidateKeyLength(value);
  FKey := value;
end;

procedure TBlake2BConfig.SetPersonalisation(const value: THashLibByteArray);
begin
  ValidatePersonalisationLength(value);
  FPersonalisation := value;
end;

procedure TBlake2BConfig.SetSalt(const value: THashLibByteArray);
begin
  ValidateSaltLength(value);
  FSalt := value;
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

end.
