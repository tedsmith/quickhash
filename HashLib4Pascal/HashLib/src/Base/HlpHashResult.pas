unit HlpHashResult;

{$I ..\Include\HashLib.inc}

interface

uses

{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
{$IFDEF DELPHIXE7_UP}
  System.NetEncoding,
{$ELSE}
  System.Classes,
  Soap.EncdDecd,
{$ENDIF DELPHIXE7_UP}
{$ELSE}
  SysUtils,
{$IFDEF DELPHI}
  Classes,
  EncdDecd,
{$ENDIF DELPHI}
{$IFDEF FPC}
  base64,
{$ENDIF FPC}
{$ENDIF HAS_UNITSCOPE}
  HlpBits,
  HlpHashLibTypes,
  HlpIHashResult,
  HlpConverters;

resourcestring
  SImpossibleRepresentationInt32 =
    'Current Data Structure cannot be Represented as an "Int32" Type.';
  SImpossibleRepresentationUInt8 =
    'Current Data Structure cannot be Represented as an "UInt8" Type.';
  SImpossibleRepresentationUInt16 =
    'Current Data Structure cannot be Represented as an "UInt16" Type.';
  SImpossibleRepresentationUInt32 =
    'Current Data Structure cannot be Represented as an "UInt32" Type.';
  SImpossibleRepresentationUInt64 =
    'Current Data Structure cannot be Represented as an "UInt64" Type.';

type
  THashResult = class sealed(TInterfacedObject, IHashResult)

  strict private

    Fm_hash: THashLibByteArray;

    class function SlowEquals(a_ar1, a_ar2: THashLibByteArray): Boolean;

  public

    constructor Create(a_hash: Int32); overload;
    constructor Create(a_hash: UInt8); overload;
    constructor Create(a_hash: UInt16); overload;
    constructor Create(a_hash: UInt32); overload;
    constructor Create(a_hash: UInt64); overload;
    constructor Create(a_hash: THashLibByteArray); overload;

    function GetBytes(): THashLibByteArray;
    function GetUInt8(): UInt8;
    function GetUInt16(): UInt16;
    function GetUInt32(): UInt32;
    function GetInt32(): Int32;
    function GetUInt64(): UInt64;
    function ToString(a_group: Boolean = false): String; reintroduce;
    function Equals(a_hashResult: IHashResult): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ THashResult }

constructor THashResult.Create(a_hash: UInt64);
begin

  Fm_hash := THashLibByteArray.Create(Byte(a_hash shr 56), Byte(a_hash shr 48),
    Byte(a_hash shr 40), Byte(a_hash shr 32), Byte(a_hash shr 24),
    Byte(a_hash shr 16), Byte(a_hash shr 8), Byte(a_hash));
end;

constructor THashResult.Create(a_hash: THashLibByteArray);
begin
  Fm_hash := a_hash;
end;

constructor THashResult.Create(a_hash: UInt32);
begin
  Fm_hash := THashLibByteArray.Create(Byte(a_hash shr 24), Byte(a_hash shr 16),
    Byte(a_hash shr 8), Byte(a_hash));
end;

constructor THashResult.Create(a_hash: UInt8);
begin
  Fm_hash := THashLibByteArray.Create(a_hash);
end;

constructor THashResult.Create(a_hash: UInt16);
begin
  Fm_hash := THashLibByteArray.Create(Byte(a_hash shr 8), Byte(a_hash));
end;

constructor THashResult.Create(a_hash: Int32);
begin
  Fm_hash := THashLibByteArray.Create(Byte(TBits.Asr32(a_hash, 24)),
    Byte(TBits.Asr32(a_hash, 16)), Byte(TBits.Asr32(a_hash, 8)), Byte(a_hash));
end;

function THashResult.Equals(a_hashResult: IHashResult): Boolean;

begin
  result := THashResult.SlowEquals(a_hashResult.GetBytes(), Fm_hash);
end;

function THashResult.GetBytes: THashLibByteArray;
begin
  result := Fm_hash;
end;

function THashResult.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}

var
  LResult: UInt32;
  I, Top: Int32;
  Temp: string;
{$IFDEF DELPHIXE7_UP}
  TempHolder: THashLibByteArray;
{$ELSE}
{$IFDEF DELPHI}
  TempHolder: TBytesStream;
{$ENDIF DELPHI}
{$ENDIF DELPHIXE7_UP}
{$IFDEF FPC}
  TempHolder: String;
{$ENDIF FPC}
begin

{$IFDEF DELPHIXE7_UP}
  TempHolder := Self.Fm_hash;
{$ELSE}
{$IFDEF DELPHI}
  TempHolder := TBytesStream.Create(Self.Fm_hash);
{$ENDIF DELPHI}
{$ENDIF DELPHIXE7_UP}
{$IFDEF FPC}
  TempHolder := EncodeStringBase64
    (String(TEncoding.UTF8.GetString(Self.Fm_hash)));
{$ENDIF FPC}
{$IFDEF DELPHIXE7_UP}
  Temp := StringReplace(TNetEncoding.base64.EncodeBytesToString(TempHolder),
    sLineBreak, '', [rfReplaceAll]);
{$ELSE}
{$IFDEF DELPHI}
  try
    Temp := StringReplace(String(EncodeBase64(TempHolder.Memory,
      TempHolder.Size)), sLineBreak, '', [rfReplaceAll]);
  finally
    TempHolder.Free;
  end;
{$ENDIF DELPHI}
{$ENDIF DELPHIXE7_UP}
{$IFDEF FPC}
  Temp := TempHolder;
{$ENDIF FPC}
  Temp := AnsiUpperCase(Temp);

  LResult := 0;
{$IFDEF DELPHIXE3_UP}
  I := System.Low(Temp);
  Top := System.High(Temp);
{$ELSE}
  I := 1;
  Top := System.Length(Temp);
{$ENDIF DELPHIXE3_UP}
  while I <= Top do
  begin

    LResult := TBits.RotateLeft32(LResult, 5);
    LResult := LResult xor UInt32(Temp[I]);
    System.Inc(I);
  end;

  result := LResult;
end;

function THashResult.GetInt32: Int32;
begin
  if (System.Length(Fm_hash) <> 4) then
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationInt32);

  result := Int32((Int32(Fm_hash[0]) shl 24) or (Int32(Fm_hash[1]) shl 16) or
    (Int32(Fm_hash[2]) shl 8) or (Int32(Fm_hash[3])));

end;

function THashResult.GetUInt8: UInt8;
begin
  if (System.Length(Fm_hash) <> 1) then
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationUInt8);

  result := (UInt8(Fm_hash[0]));
end;

function THashResult.GetUInt16: UInt16;
begin
  if (System.Length(Fm_hash) <> 2) then
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationUInt16);

  result := (UInt16(Fm_hash[0]) shl 8) or (UInt16(Fm_hash[1]));

end;

function THashResult.GetUInt32: UInt32;
begin
  if (System.Length(Fm_hash) <> 4) then
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationUInt32);

  result := (UInt32(Fm_hash[0]) shl 24) or (UInt32(Fm_hash[1]) shl 16) or
    (UInt32(Fm_hash[2]) shl 8) or (UInt32(Fm_hash[3]));

end;

function THashResult.GetUInt64: UInt64;
begin
  if (System.Length(Fm_hash) <> 8) then
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationUInt64);

  result := (UInt64(Fm_hash[0]) shl 56) or (UInt64(Fm_hash[1]) shl 48) or
    (UInt64(Fm_hash[2]) shl 40) or (UInt64(Fm_hash[3]) shl 32) or
    (UInt64(Fm_hash[4]) shl 24) or (UInt64(Fm_hash[5]) shl 16) or
    (UInt64(Fm_hash[6]) shl 8) or (UInt64(Fm_hash[7]));

end;

{$B+}

class function THashResult.SlowEquals(a_ar1, a_ar2: THashLibByteArray): Boolean;
var
  I: Int32;
  diff: UInt32;

begin
  diff := UInt32(System.Length(a_ar1)) xor UInt32(System.Length(a_ar2));

  I := 0;

  while (I <= System.High(a_ar1)) and (I <= System.High(a_ar2)) do
  begin
    diff := diff or (UInt32(a_ar1[I] xor a_ar2[I]));
    System.Inc(I);
  end;

  result := diff = 0;
end;

{$B-}

function THashResult.ToString(a_group: Boolean): String;
begin
  result := TConverters.ConvertBytesToHexString(Fm_hash, a_group);
end;

end.
