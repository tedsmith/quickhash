unit HlpNullable;

{$I ..\Include\HashLib.inc}
{ /*  The "Nullable Types" found in this Unit were extracted from
  https://github.com/jpluimers/Conferences/blob/master/2009/DelphiLive.2009/Nullable-types-in-Delphi-Win32/Delphi-generic/src/NullableTypes.pas
  with some little modifications by me. */ }

interface

uses
  HlpHashLibTypes,
{$IFDEF HAS_UNITSCOPE}
  System.TypInfo
{$ELSE}
  TypInfo
{$ENDIF HAS_UNITSCOPE};

resourcestring
  SCannotAssignPointerToNullable =
    'Cannot assign non-null pointer to nullable type.';
  SUnsupportedType =
    'Unsupported Type: Only supports Integers, Int64, Floats and Strings.';
  SGetNullValue = 'Attempted to get a null value.';

type

  Nullable<T> = record
  private
  var
    fValue: T;
    class function CastBack(const aValue): T; static;
    class function AddFloat(const aFloat, bFloat): T; static;
    class function AddString(const aString, bString): T; static;
    class function AddInt64(const aInt64, bInt64): T; static;
    class function NewAddInt(const aInt, bInt): T; static;

  var
    fInitValue: string;

  var
    fDefault: T;

  var
    fInitDefault: string;
    procedure SetValue(const aValue: T); inline;
    procedure CheckValue; inline;
    procedure CheckType; inline;
    function GetValue: T; inline;
    function GetIsNull: Boolean; inline;
    function GetHasValue: Boolean; inline;
    function GetHasDefault: Boolean; inline;
  public
    property Value: T read GetValue write SetValue;
    property IsNull: Boolean read GetIsNull;
    property HasValue: Boolean read GetHasValue;
    property HasDefault: Boolean read GetHasDefault;
    procedure ClearValue;
    procedure SetDefault(const aDefault: T);

    constructor Create(const aValue: T); overload;
    constructor Create(const aValue: T; const aDefault: T); overload;

    class operator Implicit(a: T): Nullable<T>;
    class operator Implicit(a: Nullable<T>): T;
    class operator Implicit(a: Pointer): Nullable<T>;
    class operator Explicit(aValue: Nullable<T>): T;

    class operator Add(a, b: Nullable<T>): Nullable<T>;
  end;

  /// <summary>
  /// Represents a Nullable Integer.
  /// </summary>
  TNullableInteger = Nullable<Int32>;

implementation

{ Nullable<T> }

function Nullable<T>.GetHasDefault: Boolean;
begin
  Result := fInitDefault = 'I';
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := not IsNull;
end;

function Nullable<T>.GetIsNull: Boolean;
begin
  Result := fInitValue <> 'I';
end;

procedure Nullable<T>.CheckType;
var
  info: PTypeInfo;
begin
  info := TypeInfo(T);
  case info^.Kind of
    tkInteger:
      ;
    tkFloat:
      ;
    tkString:
      ;
    tkInt64:
      ;
    tkUString:
      ;
  else
    Raise EUnsupportedTypeHashLibException.CreateRes(@SUnsupportedType);
  end;
end;

procedure Nullable<T>.CheckValue;
begin
  if IsNull then
    if HasDefault then
      fValue := fDefault
    else
      raise ENullReferenceHashLibException.CreateRes(@SGetNullValue);
end;

function Nullable<T>.GetValue: T;
begin
  CheckType;
  CheckValue;
  Result := fValue;
end;

procedure Nullable<T>.SetDefault(const aDefault: T);
begin
  fDefault := aDefault;
  fInitDefault := 'I';
  if IsNull then
    fValue := aDefault;
end;

procedure Nullable<T>.SetValue(const aValue: T);
begin
  fInitValue := 'I';
  fValue := aValue;
end;

class operator Nullable<T>.Implicit(a: Nullable<T>): T;
begin
  Result := a.Value;
end;

class operator Nullable<T>.Implicit(a: T): Nullable<T>;
begin
  Result.Value := a;
end;

class operator Nullable<T>.Implicit(a: Pointer): Nullable<T>;
begin
  if not System.Assigned(a) then
    Result.ClearValue
  else
    raise EInvalidOperationHashLibException.CreateRes
      (@SCannotAssignPointerToNullable);
end;

// got the idea from Andreas Hausladen
class function Nullable<T>.CastBack(const aValue): T;
begin
  Result := T(aValue);
end;

class function Nullable<T>.AddInt64(const aInt64, bInt64): T;
var
  Value: Int64;
begin
  Value := Int64(aInt64) + Int64(bInt64);
  Result := CastBack(Value);
end;

class function Nullable<T>.AddFloat(const aFloat, bFloat): T;
var
  Value: Double;
begin
  Value := Double(aFloat) + Double(bFloat);
  Result := CastBack(Value);
end;

class function Nullable<T>.AddString(const aString, bString): T;
var
  Value: String;
begin
  Value := String(aString) + String(bString);
  Result := CastBack(Value);
end;

class function Nullable<T>.NewAddInt(const aInt, bInt): T;
var
  Value: Int32;
begin
  Value := Int32(aInt) + Int32(bInt);
  Result := CastBack(Value);
end;

class operator Nullable<T>.Add(a, b: Nullable<T>): Nullable<T>;
var
  info: PTypeInfo;
begin
  if a.IsNull or b.IsNull then
    Result.ClearValue
  else
  begin
    info := TypeInfo(T);
    case info^.Kind of
      tkInteger:
        Result.Value := NewAddInt(a.fValue, b.fValue);
      tkFloat:
        Result.Value := AddFloat(a.fValue, b.fValue);
      tkString:
        Result.Value := AddString(a.fValue, b.fValue);
      tkInt64:
        Result.Value := AddInt64(a.fValue, b.fValue);
    end;
  end;
end;

procedure Nullable<T>.ClearValue;
begin
  fInitValue := '';
end;

constructor Nullable<T>.Create(const aValue: T);
begin
  SetValue(aValue);
end;

constructor Nullable<T>.Create(const aValue, aDefault: T);
begin
  SetValue(aValue);
  SetDefault(aDefault);
end;

class operator Nullable<T>.Explicit(aValue: Nullable<T>): T;
begin
  Result := aValue.Value;
end;

end.
