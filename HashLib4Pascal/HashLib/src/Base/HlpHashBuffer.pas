unit HlpHashBuffer;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF HAS_UNITSCOPE}
  HlpHashLibTypes;

type
  THashBuffer = record

  strict private

    Fm_data: THashLibByteArray;
    Fm_pos: Int32;

    function GetIsEmpty: Boolean; inline;
    function GetIsFull: Boolean; inline;
    function GetPos: Int32; inline;
    function GetLength: Int32; inline;

  public
    constructor Create(a_length: Int32);
    procedure Initialize();
    function GetBytes(): THashLibByteArray; inline;
    function GetBytesZeroPadded(): THashLibByteArray; inline;
    function Feed(a_data: PByte; a_length_a_data: Int32;
      var a_start_index: Int32; var a_length: Int32;
      var a_processed_bytes: UInt64): Boolean; overload;
    function Feed(a_data: PByte; a_length_a_data: Int32; a_length: Int32)
      : Boolean; overload;
    function ToString(): String;

    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;
    property Pos: Int32 read GetPos;
    property Length: Int32 read GetLength;
  end;

implementation

{ THashBuffer }

constructor THashBuffer.Create(a_length: Int32);
begin
{$IFDEF DEBUG}
  System.Assert(a_length > 0);
{$ENDIF DEBUG}
  System.SetLength(Fm_data, a_length);
  Initialize();

end;

function THashBuffer.Feed(a_data: PByte; a_length_a_data: Int32;
  a_length: Int32): Boolean;
var
  &Length: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(a_length >= 0);
  System.Assert(a_length <= a_length_a_data);
  System.Assert(not IsFull);
{$ENDIF DEBUG}
  if (a_length_a_data = 0) then
  begin
    result := false;
    Exit;
  end;

  if (a_length = 0) then
  begin
    result := false;
    Exit;
  end;
  Length := System.Length(Fm_data) - Fm_pos;
  if (Length > a_length) then
  begin
    Length := a_length;
  end;

  System.Move(a_data[0], Fm_data[Fm_pos], Length * System.SizeOf(Byte));

  Fm_pos := Fm_pos + Length;

  result := IsFull;
end;

function THashBuffer.Feed(a_data: PByte; a_length_a_data: Int32;
  var a_start_index, a_length: Int32; var a_processed_bytes: UInt64): Boolean;
var
  &Length: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(a_start_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert((a_start_index + a_length) <= a_length_a_data);
  System.Assert(not IsFull);
{$ENDIF DEBUG}
  if (a_length_a_data = 0) then
  begin
    result := false;
    Exit;
  end;

  if (a_length = 0) then
  begin
    result := false;
    Exit;
  end;

  Length := System.Length(Fm_data) - Fm_pos;
  if (Length > a_length) then
  begin
    Length := a_length;
  end;

  System.Move(a_data[a_start_index], Fm_data[Fm_pos],
    Length * System.SizeOf(Byte));

  Fm_pos := Fm_pos + Length;
  a_start_index := a_start_index + Length;
  a_length := a_length - Length;
  a_processed_bytes := a_processed_bytes + UInt64(Length);

  result := IsFull;
end;

function THashBuffer.GetBytes: THashLibByteArray;
begin
{$IFDEF DEBUG}
  System.Assert(IsFull);
{$ENDIF DEBUG}
  Fm_pos := 0;
  result := Fm_data;
end;

function THashBuffer.GetBytesZeroPadded: THashLibByteArray;
begin
  System.FillChar(Fm_data[Fm_pos], (System.Length(Fm_data) - Fm_pos) *
    System.SizeOf(Byte), 0);
  Fm_pos := 0;
  result := Fm_data;
end;

function THashBuffer.GetIsEmpty: Boolean;
begin
  result := Fm_pos = 0;
end;

function THashBuffer.GetIsFull: Boolean;
begin
  result := Fm_pos = System.Length(Fm_data);
end;

function THashBuffer.GetLength: Int32;
begin
  result := System.Length(Fm_data);
end;

function THashBuffer.GetPos: Int32;
begin
  result := Fm_pos;
end;

procedure THashBuffer.Initialize;
begin
  Fm_pos := 0;
  System.FillChar(Fm_data[0], System.Length(Fm_data) * System.SizeOf(Byte), 0);
end;

function THashBuffer.ToString: String;
begin
  result := Format('HashBuffer, Length: %d, Pos: %d, IsEmpty: %s',
    [Self.Length, Self.Pos, BoolToStr(Self.IsEmpty, True)]);
end;

end.
