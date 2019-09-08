unit HlpArrayUtils;

{$I ..\Include\HashLib.inc}

interface

uses
  SysUtils,
  HlpHashLibTypes;

type
  TArrayUtils = class sealed(TObject)

  public

    class function AreEqual(const A, B: THashLibByteArray): Boolean;
      overload; static;

    class function ConstantTimeAreEqual(const a_ar1, a_ar2: THashLibByteArray)
      : Boolean; static;

    class procedure Fill(const buf: THashLibByteArray; from, &to: Int32;
      filler: Byte); overload; static;

    class procedure Fill(const buf: THashLibUInt32Array; from, &to: Int32;
      filler: UInt32); overload; static;

    class procedure Fill(const buf: THashLibUInt64Array; from, &to: Int32;
      filler: UInt64); overload; static;

    class procedure ZeroFill(const buf: THashLibByteArray); overload; static;

    class procedure ZeroFill(const buf: THashLibUInt32Array); overload; static;

    class procedure ZeroFill(const buf: THashLibUInt64Array); overload; static;

  end;

implementation

{ TArrayUtils }

class function TArrayUtils.AreEqual(const A, B: THashLibByteArray): Boolean;
begin
  if System.Length(A) <> System.Length(B) then
  begin
    Result := false;
    Exit;
  end;

  Result := CompareMem(A, B, System.Length(A) * System.SizeOf(Byte));
end;

{$B+}

class function TArrayUtils.ConstantTimeAreEqual(const a_ar1,
  a_ar2: THashLibByteArray): Boolean;
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

  Result := diff = 0;
end;

{$B-}

class procedure TArrayUtils.Fill(const buf: THashLibByteArray; from, &to: Int32;
  filler: Byte);
begin
  if buf <> Nil then
  begin
    System.FillChar(buf[from], (&to - from) * System.SizeOf(Byte), filler);
  end;
end;

class procedure TArrayUtils.Fill(const buf: THashLibUInt32Array;
  from, &to: Int32; filler: UInt32);
begin
  if buf <> Nil then
  begin
{$IFDEF FPC}
    System.FillDWord(buf[from], (&to - from), filler);
{$ELSE}
    while from < &to do
    begin
      buf[from] := filler;
      System.Inc(from);
    end;
{$ENDIF}
  end;
end;

class procedure TArrayUtils.Fill(const buf: THashLibUInt64Array;
  from, &to: Int32; filler: UInt64);
begin
  if buf <> Nil then
  begin
{$IFDEF FPC}
    System.FillQWord(buf[from], (&to - from), filler);
{$ELSE}
    while from < &to do
    begin
      buf[from] := filler;
      System.Inc(from);
    end;
{$ENDIF}
  end;
end;

class procedure TArrayUtils.ZeroFill(const buf: THashLibByteArray);
begin
  TArrayUtils.Fill(buf, 0, System.Length(buf), Byte(0));
end;

class procedure TArrayUtils.ZeroFill(const buf: THashLibUInt32Array);
begin
  TArrayUtils.Fill(buf, 0, System.Length(buf), UInt32(0));
end;

class procedure TArrayUtils.ZeroFill(const buf: THashLibUInt64Array);
begin
  TArrayUtils.Fill(buf, 0, System.Length(buf), UInt64(0));
end;

end.
