unit HlpBitConverter;

{$I ..\Include\HashLib.inc}
{ /*  Some code here were translated from Microsoft .NET Framework source on Github */ }

interface

uses
  HlpHashLibTypes;

type

  TBitConverter = class sealed(TObject)

  strict private

    class var

      FIsLittleEndian: Boolean;

    class function GetHexValue(AValue: Int32): Char; static; inline;
    class function GetIsLittleEndian(): Boolean; static; inline;
    class constructor BitConverter();

  public

    class property IsLittleEndian: Boolean read GetIsLittleEndian;

    { ==================================================================== }

    class function GetBytes(AValue: Boolean): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: Char): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: Double): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: Int16): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: Int32): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: Int64): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: Single): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: UInt8): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: UInt16): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: UInt32): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(AValue: UInt64): THashLibByteArray; overload;
      static; inline;

    { ==================================================================== }

    class function ToBoolean(const AValue: THashLibByteArray;
      AStartIndex: Int32): Boolean; static; inline;
    class function ToChar(const AValue: THashLibByteArray; AStartIndex: Int32)
      : Char; static; inline;
    class function ToDouble(const AValue: THashLibByteArray; AStartIndex: Int32)
      : Double; static; inline;
    class function ToInt16(const AValue: THashLibByteArray; AStartIndex: Int32)
      : Int16; static; inline;
    class function ToInt32(const AValue: THashLibByteArray; AStartIndex: Int32)
      : Int32; static; inline;
    class function ToInt64(const AValue: THashLibByteArray; AStartIndex: Int32)
      : Int64; static; inline;
    class function ToSingle(const AValue: THashLibByteArray; AStartIndex: Int32)
      : Single; static; inline;
    class function ToString(const AValue: THashLibByteArray): String;
      reintroduce; overload; static;
    class function ToString(const AValue: THashLibByteArray; AStartIndex: Int32)
      : String; reintroduce; overload; static;
    class function ToString(const AValue: THashLibByteArray;
      AStartIndex, ALength: Int32): String; reintroduce; overload; static;
    class function ToUInt8(const AValue: THashLibByteArray; AStartIndex: Int32)
      : UInt8; static; inline;
    class function ToUInt16(const AValue: THashLibByteArray; AStartIndex: Int32)
      : UInt16; static; inline;
    class function ToUInt32(const AValue: THashLibByteArray; AStartIndex: Int32)
      : UInt32; static; inline;
    class function ToUInt64(const AValue: THashLibByteArray; AStartIndex: Int32)
      : UInt64; static; inline;

  end;

implementation

{ TBitConverter }

class constructor TBitConverter.BitConverter;
var
  LInt32Value: Int32;
begin
  LInt32Value := 1;
  FIsLittleEndian := (PByte((@LInt32Value))^) = 1;
end;

{ ==================================================================== }

class function TBitConverter.GetBytes(AValue: Int16): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PSmallInt(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: Int32): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PInteger(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: Double): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PDouble(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: Boolean): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PBoolean(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: Char): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PChar(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: UInt8): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PByte(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: UInt16): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PWord(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: Int64): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PInt64(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: Single): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PSingle(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: UInt32): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PCardinal(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

class function TBitConverter.GetBytes(AValue: UInt64): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(AValue));
  // PUInt64(@result[0])^ := AValue;
  System.SetLength(result, System.SizeOf(AValue));
  System.Move(AValue, result[0], System.SizeOf(AValue));
end;

{ ==================================================================== }

class function TBitConverter.GetHexValue(AValue: Int32): Char;
begin
  if AValue < 10 then
  begin
    result := Char(AValue + System.Ord('0'))
  end
  else
  begin
    result := Char((AValue - 10) + System.Ord('A'));
  end;
end;

class function TBitConverter.GetIsLittleEndian: Boolean;
begin
  result := FIsLittleEndian;
end;

{ ==================================================================== }

class function TBitConverter.ToBoolean(const AValue: THashLibByteArray;
  AStartIndex: Int32): Boolean;
begin
  // result := PBoolean(@AValue[AStartIndex])^;
  System.Move(AValue[AStartIndex], result, System.SizeOf(result));
end;

class function TBitConverter.ToChar(const AValue: THashLibByteArray;
  AStartIndex: Int32): Char;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    result := Char(AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8));
  end
  else
  begin
    result := Char((AValue[AStartIndex] shl 8) or AValue[AStartIndex + 1]);
  end;
end;

class function TBitConverter.ToDouble(const AValue: THashLibByteArray;
  AStartIndex: Int32): Double;
var
  i1, i2: Int32;
  LValue: Int64;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    i1 := AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8) or
      (AValue[AStartIndex + 2] shl 16) or (AValue[AStartIndex + 3] shl 24);
    i2 := (AValue[AStartIndex + 4]) or (AValue[AStartIndex + 5] shl 8) or
      (AValue[AStartIndex + 6] shl 16) or (AValue[AStartIndex + 7] shl 24);
    LValue := UInt32(i1) or (Int64(i2) shl 32);
    result := PDouble(@LValue)^;
  end
  else
  begin
    i1 := (AValue[AStartIndex] shl 24) or (AValue[AStartIndex + 1] shl 16) or
      (AValue[AStartIndex + 2] shl 8) or (AValue[AStartIndex + 3]);
    i2 := (AValue[AStartIndex + 4] shl 24) or (AValue[AStartIndex + 5] shl 16)
      or (AValue[AStartIndex + 6] shl 8) or (AValue[AStartIndex + 7]);
    LValue := UInt32(i2) or (Int64(i1) shl 32);
    result := PDouble(@LValue)^;
  end;
end;

class function TBitConverter.ToInt16(const AValue: THashLibByteArray;
  AStartIndex: Int32): Int16;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    result := SmallInt(AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8));
  end
  else
  begin
    result := SmallInt((AValue[AStartIndex] shl 8) or AValue[AStartIndex + 1]);
  end;
end;

class function TBitConverter.ToInt32(const AValue: THashLibByteArray;
  AStartIndex: Int32): Int32;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    result := AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8) or
      (AValue[AStartIndex + 2] shl 16) or (AValue[AStartIndex + 3] shl 24);
  end
  else
  begin
    result := (AValue[AStartIndex] shl 24) or (AValue[AStartIndex + 1] shl 16)
      or (AValue[AStartIndex + 2] shl 8) or (AValue[AStartIndex + 3]);
  end;
end;

class function TBitConverter.ToInt64(const AValue: THashLibByteArray;
  AStartIndex: Int32): Int64;
var
  i1, i2: Int32;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    i1 := AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8) or
      (AValue[AStartIndex + 2] shl 16) or (AValue[AStartIndex + 3] shl 24);
    i2 := (AValue[AStartIndex + 4]) or (AValue[AStartIndex + 5] shl 8) or
      (AValue[AStartIndex + 6] shl 16) or (AValue[AStartIndex + 7] shl 24);
    result := UInt32(i1) or (Int64(i2) shl 32);
  end
  else
  begin
    i1 := (AValue[AStartIndex] shl 24) or (AValue[AStartIndex + 1] shl 16) or
      (AValue[AStartIndex + 2] shl 8) or (AValue[AStartIndex + 3]);
    i2 := (AValue[AStartIndex + 4] shl 24) or (AValue[AStartIndex + 5] shl 16)
      or (AValue[AStartIndex + 6] shl 8) or (AValue[AStartIndex + 7]);
    result := UInt32(i2) or (Int64(i1) shl 32);
  end;
end;

class function TBitConverter.ToSingle(const AValue: THashLibByteArray;
  AStartIndex: Int32): Single;
var
  LValue: Int32;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    LValue := (AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8) or
      (AValue[AStartIndex + 2] shl 16) or (AValue[AStartIndex + 3] shl 24));
    result := PSingle(@LValue)^;
  end
  else
  begin
    LValue := (AValue[AStartIndex] shl 24) or (AValue[AStartIndex + 1] shl 16)
      or (AValue[AStartIndex + 2] shl 8) or (AValue[AStartIndex + 3]);
    result := PSingle(@LValue)^;
  end;
end;

class function TBitConverter.ToUInt8(const AValue: THashLibByteArray;
  AStartIndex: Int32): UInt8;
begin
  // result := PByte(@AValue[AStartIndex])^;
  System.Move(AValue[AStartIndex], result, System.SizeOf(result));
end;

class function TBitConverter.ToUInt16(const AValue: THashLibByteArray;
  AStartIndex: Int32): UInt16;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    result := Word(AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8));
  end
  else
  begin
    result := Word((AValue[AStartIndex] shl 8) or AValue[AStartIndex + 1]);
  end;
end;

class function TBitConverter.ToUInt32(const AValue: THashLibByteArray;
  AStartIndex: Int32): UInt32;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    result := UInt32(AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8) or
      (AValue[AStartIndex + 2] shl 16) or (AValue[AStartIndex + 3] shl 24));
  end
  else
  begin
    result := UInt32((AValue[AStartIndex] shl 24) or
      (AValue[AStartIndex + 1] shl 16) or (AValue[AStartIndex + 2] shl 8) or
      (AValue[AStartIndex + 3]));
  end;
end;

class function TBitConverter.ToUInt64(const AValue: THashLibByteArray;
  AStartIndex: Int32): UInt64;
var
  i1, i2: Int32;
begin
  // System.Move(AValue[AStartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    i1 := AValue[AStartIndex] or (AValue[AStartIndex + 1] shl 8) or
      (AValue[AStartIndex + 2] shl 16) or (AValue[AStartIndex + 3] shl 24);
    i2 := (AValue[AStartIndex + 4]) or (AValue[AStartIndex + 5] shl 8) or
      (AValue[AStartIndex + 6] shl 16) or (AValue[AStartIndex + 7] shl 24);
    result := UInt64(UInt32(i1) or (Int64(i2) shl 32));
  end
  else
  begin
    i1 := (AValue[AStartIndex] shl 24) or (AValue[AStartIndex + 1] shl 16) or
      (AValue[AStartIndex + 2] shl 8) or (AValue[AStartIndex + 3]);
    i2 := (AValue[AStartIndex + 4] shl 24) or (AValue[AStartIndex + 5] shl 16)
      or (AValue[AStartIndex + 6] shl 8) or (AValue[AStartIndex + 7]);
    result := UInt64(UInt32(i2) or (Int64(i1) shl 32));
  end;
end;

class function TBitConverter.ToString(const AValue: THashLibByteArray): String;
begin
  result := ToString(AValue, System.Low(AValue));
end;

class function TBitConverter.ToString(const AValue: THashLibByteArray;
  AStartIndex: Int32): String;
begin
  result := ToString(AValue, AStartIndex, System.Length(AValue) - AStartIndex);
end;

class function TBitConverter.ToString(const AValue: THashLibByteArray;
  AStartIndex, ALength: Int32): String;
var
  LIdx, LIndex, LCharArrayLength: Int32;
  LCharArray: THashLibCharArray;
  LByte: Byte;
begin
  result := '';

  LCharArrayLength := ALength * 3;

  System.SetLength(LCharArray, LCharArrayLength);
  LIdx := 0;
  LIndex := AStartIndex;
  while LIdx < LCharArrayLength do
  begin
    LByte := AValue[LIndex];
    System.Inc(LIndex);

    LCharArray[LIdx] := GetHexValue(LByte shr 4);
    LCharArray[LIdx + 1] := GetHexValue(LByte and 15);
    LCharArray[LIdx + 2] := '-';

    System.Inc(LIdx, 3);
  end;
  System.SetString(result, PChar(@LCharArray[System.Low(LCharArray)]),
    System.Length(LCharArray) - 1);
end;

end.
