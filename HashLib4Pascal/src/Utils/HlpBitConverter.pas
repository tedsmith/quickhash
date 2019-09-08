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

    class function GetHexValue(i: Int32): Char; static; inline;
    class function GetIsLittleEndian(): Boolean; static; inline;
    class constructor BitConverter();

  public

    class property IsLittleEndian: Boolean read GetIsLittleEndian;

    { ==================================================================== }

    class function GetBytes(value: Boolean): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: Char): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: Double): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: Int16): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: Int32): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: Int64): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: Single): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: UInt8): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: UInt16): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: UInt32): THashLibByteArray; overload;
      static; inline;
    class function GetBytes(value: UInt64): THashLibByteArray; overload;
      static; inline;

    { ==================================================================== }

    class function ToBoolean(const value: THashLibByteArray; StartIndex: Int32)
      : Boolean; static; inline;
    class function ToChar(const value: THashLibByteArray; StartIndex: Int32)
      : Char; static; inline;
    class function ToDouble(const value: THashLibByteArray; StartIndex: Int32)
      : Double; static; inline;
    class function ToInt16(const value: THashLibByteArray; StartIndex: Int32)
      : Int16; static; inline;
    class function ToInt32(const value: THashLibByteArray; StartIndex: Int32)
      : Int32; static; inline;
    class function ToInt64(const value: THashLibByteArray; StartIndex: Int32)
      : Int64; static; inline;
    class function ToSingle(const value: THashLibByteArray; StartIndex: Int32)
      : Single; static; inline;
    class function ToString(const value: THashLibByteArray): String;
      reintroduce; overload; static;
    class function ToString(const value: THashLibByteArray; StartIndex: Int32)
      : String; reintroduce; overload; static;
    class function ToString(const value: THashLibByteArray;
      StartIndex, &Length: Int32): String; reintroduce; overload; static;
    class function ToUInt8(const value: THashLibByteArray; StartIndex: Int32)
      : UInt8; static; inline;
    class function ToUInt16(const value: THashLibByteArray; StartIndex: Int32)
      : UInt16; static; inline;
    class function ToUInt32(const value: THashLibByteArray; StartIndex: Int32)
      : UInt32; static; inline;
    class function ToUInt64(const value: THashLibByteArray; StartIndex: Int32)
      : UInt64; static; inline;

  end;

implementation

{ TBitConverter }

class constructor TBitConverter.BitConverter;
var
  IntValue: Int32;
  PIIntValueAddress: PInteger;
  PBIntValueAddress: PByte;
  ByteValue: Byte;
begin
  IntValue := 1;
  PIIntValueAddress := @IntValue;
  PBIntValueAddress := PByte(PIIntValueAddress);
  ByteValue := PBIntValueAddress^;
  FIsLittleEndian := ByteValue = 1;
end;

{ ==================================================================== }

class function TBitConverter.GetBytes(value: Int16): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PSmallInt(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: Int32): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PInteger(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));

end;

class function TBitConverter.GetBytes(value: Double): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PDouble(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: Boolean): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PBoolean(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: Char): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PChar(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: UInt8): THashLibByteArray;
begin
  // System.SetLength(result, System.SizeOf(value));
  // PByte(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: UInt16): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PWord(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: Int64): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PInt64(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: Single): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PSingle(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: UInt32): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PCardinal(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

class function TBitConverter.GetBytes(value: UInt64): THashLibByteArray;
begin

  // System.SetLength(result, System.SizeOf(value));
  // PUInt64(@result[0])^ := value;
  System.SetLength(result, System.SizeOf(value));
  System.Move(value, result[0], System.SizeOf(value));
end;

{ ==================================================================== }

class function TBitConverter.GetHexValue(i: Int32): Char;
begin
  if i < 10 then
    result := Char(i + System.Ord('0'))
  else
    result := Char((i - 10) + System.Ord('A'));
end;

class function TBitConverter.GetIsLittleEndian: Boolean;
begin
  result := FIsLittleEndian;
end;

{ ==================================================================== }

class function TBitConverter.ToBoolean(const value: THashLibByteArray;
  StartIndex: Int32): Boolean;
begin
  // result := PBoolean(@value[StartIndex])^;
  System.Move(value[StartIndex], result, System.SizeOf(result));

end;

class function TBitConverter.ToChar(const value: THashLibByteArray;
  StartIndex: Int32): Char;
begin
  // System.Move(value[StartIndex], result, System.SizeOf(result));

  if (IsLittleEndian) then
  begin
    result := Char(value[StartIndex] or (value[StartIndex + 1] shl 8));
    Exit;
  end
  else
  begin

    result := Char((value[StartIndex] shl 8) or value[StartIndex + 1]);
    Exit;
  end;

end;

class function TBitConverter.ToDouble(const value: THashLibByteArray;
  StartIndex: Int32): Double;
var
  i1, i2: Int32;
  val: Int64;
begin
  // System.Move(value[StartIndex], result, System.SizeOf(result));

  if (IsLittleEndian) then
  begin
    i1 := value[StartIndex] or (value[StartIndex + 1] shl 8) or
      (value[StartIndex + 2] shl 16) or (value[StartIndex + 3] shl 24);
    i2 := (value[StartIndex + 4]) or (value[StartIndex + 5] shl 8) or
      (value[StartIndex + 6] shl 16) or (value[StartIndex + 7] shl 24);
    val := UInt32(i1) or (Int64(i2) shl 32);
    result := PDouble(@val)^;
    Exit;
  end
  else
  begin

    i1 := (value[StartIndex] shl 24) or (value[StartIndex + 1] shl 16) or
      (value[StartIndex + 2] shl 8) or (value[StartIndex + 3]);
    i2 := (value[StartIndex + 4] shl 24) or (value[StartIndex + 5] shl 16) or
      (value[StartIndex + 6] shl 8) or (value[StartIndex + 7]);
    val := UInt32(i2) or (Int64(i1) shl 32);
    result := PDouble(@val)^;
    Exit;
  end;

end;

class function TBitConverter.ToInt16(const value: THashLibByteArray;
  StartIndex: Int32): Int16;
begin

  // System.Move(value[StartIndex], result, System.SizeOf(result));

  if (IsLittleEndian) then
  begin
    result := SmallInt(value[StartIndex] or (value[StartIndex + 1] shl 8));
    Exit;
  end
  else
  begin

    result := SmallInt((value[StartIndex] shl 8) or value[StartIndex + 1]);
    Exit;
  end;

end;

class function TBitConverter.ToInt32(const value: THashLibByteArray;
  StartIndex: Int32): Int32;
begin
  // System.Move(value[StartIndex], result, System.SizeOf(result));

  if (IsLittleEndian) then
  begin
    result := value[StartIndex] or (value[StartIndex + 1] shl 8) or
      (value[StartIndex + 2] shl 16) or (value[StartIndex + 3] shl 24);
    Exit;
  end
  else
  begin

    result := (value[StartIndex] shl 24) or (value[StartIndex + 1] shl 16) or
      (value[StartIndex + 2] shl 8) or (value[StartIndex + 3]);
    Exit;
  end;

end;

class function TBitConverter.ToInt64(const value: THashLibByteArray;
  StartIndex: Int32): Int64;
var
  i1, i2: Int32;
begin
  // System.Move(value[StartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    i1 := value[StartIndex] or (value[StartIndex + 1] shl 8) or
      (value[StartIndex + 2] shl 16) or (value[StartIndex + 3] shl 24);
    i2 := (value[StartIndex + 4]) or (value[StartIndex + 5] shl 8) or
      (value[StartIndex + 6] shl 16) or (value[StartIndex + 7] shl 24);
    result := UInt32(i1) or (Int64(i2) shl 32);
    Exit;
  end
  else
  begin

    i1 := (value[StartIndex] shl 24) or (value[StartIndex + 1] shl 16) or
      (value[StartIndex + 2] shl 8) or (value[StartIndex + 3]);
    i2 := (value[StartIndex + 4] shl 24) or (value[StartIndex + 5] shl 16) or
      (value[StartIndex + 6] shl 8) or (value[StartIndex + 7]);
    result := UInt32(i2) or (Int64(i1) shl 32);
    Exit;
  end;
end;

class function TBitConverter.ToSingle(const value: THashLibByteArray;
  StartIndex: Int32): Single;
var
  val: Int32;
begin
  // System.Move(value[StartIndex], result, System.SizeOf(result));
  if (IsLittleEndian) then
  begin
    val := (value[StartIndex] or (value[StartIndex + 1] shl 8) or
      (value[StartIndex + 2] shl 16) or (value[StartIndex + 3] shl 24));
    result := PSingle(@val)^;
    Exit;
  end
  else
  begin
    val := (value[StartIndex] shl 24) or (value[StartIndex + 1] shl 16) or
      (value[StartIndex + 2] shl 8) or (value[StartIndex + 3]);
    result := PSingle(@val)^;
    Exit;
  end;

end;

class function TBitConverter.ToString(const value: THashLibByteArray): String;
var
  LowVal: Int32;
begin
  LowVal := System.Low(value);
  result := ToString(value, LowVal);
end;

class function TBitConverter.ToString(const value: THashLibByteArray;
  StartIndex: Int32): String;
begin
  result := ToString(value, StartIndex, System.Length(value) - StartIndex);
end;

class function TBitConverter.ToString(const value: THashLibByteArray;
  StartIndex, &Length: Int32): String;

var
  Idx, Index, chArrayLength, LowVal: Int32;
  chArray: THashLibCharArray;
  b: Byte;

begin
  result := '';

  chArrayLength := &Length * 3;

  System.SetLength(chArray, chArrayLength);
  Idx := 0;
  Index := StartIndex;
  while Idx < chArrayLength do
  begin
    b := value[Index];
    System.Inc(Index);

    chArray[Idx] := GetHexValue(b shr 4);
    chArray[Idx + 1] := GetHexValue(b and 15);
    chArray[Idx + 2] := '-';

    System.Inc(Idx, 3);
  end;

  LowVal := System.Low(chArray);

  System.SetString(result, PChar(@chArray[LowVal]), System.Length(chArray) - 1);

end;

class function TBitConverter.ToUInt8(const value: THashLibByteArray;
  StartIndex: Int32): UInt8;
begin
  // result := PByte(@value[StartIndex])^;
  System.Move(value[StartIndex], result, System.SizeOf(result));
end;

class function TBitConverter.ToUInt16(const value: THashLibByteArray;
  StartIndex: Int32): UInt16;
begin
  // System.Move(value[StartIndex], result, System.SizeOf(result));

  if (IsLittleEndian) then
  begin
    result := Word(value[StartIndex] or (value[StartIndex + 1] shl 8));
    Exit;
  end
  else
  begin

    result := Word((value[StartIndex] shl 8) or value[StartIndex + 1]);
    Exit;
  end;
end;

class function TBitConverter.ToUInt32(const value: THashLibByteArray;
  StartIndex: Int32): UInt32;
begin
  // System.Move(value[StartIndex], result, System.SizeOf(result));

  if (IsLittleEndian) then
  begin
    result := UInt32(value[StartIndex] or (value[StartIndex + 1] shl 8) or
      (value[StartIndex + 2] shl 16) or (value[StartIndex + 3] shl 24));
    Exit;
  end
  else
  begin

    result := UInt32((value[StartIndex] shl 24) or
      (value[StartIndex + 1] shl 16) or (value[StartIndex + 2] shl 8) or
      (value[StartIndex + 3]));
    Exit;
  end;
end;

class function TBitConverter.ToUInt64(const value: THashLibByteArray;
  StartIndex: Int32): UInt64;
var
  i1, i2: Int32;
begin

  // System.Move(value[StartIndex], result, System.SizeOf(result));

  if (IsLittleEndian) then
  begin
    i1 := value[StartIndex] or (value[StartIndex + 1] shl 8) or
      (value[StartIndex + 2] shl 16) or (value[StartIndex + 3] shl 24);
    i2 := (value[StartIndex + 4]) or (value[StartIndex + 5] shl 8) or
      (value[StartIndex + 6] shl 16) or (value[StartIndex + 7] shl 24);
    result := UInt64(UInt32(i1) or (Int64(i2) shl 32));
    Exit;
  end
  else
  begin

    i1 := (value[StartIndex] shl 24) or (value[StartIndex + 1] shl 16) or
      (value[StartIndex + 2] shl 8) or (value[StartIndex + 3]);
    i2 := (value[StartIndex + 4] shl 24) or (value[StartIndex + 5] shl 16) or
      (value[StartIndex + 6] shl 8) or (value[StartIndex + 7]);
    result := UInt64(UInt32(i2) or (Int64(i1) shl 32));
    Exit;
  end;

end;

end.
