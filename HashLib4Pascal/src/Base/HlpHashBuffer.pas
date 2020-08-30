unit HlpHashBuffer;

{$I ..\Include\HashLib.inc}

interface

uses
  SysUtils,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SHashBufferMessage = 'HashBuffer, Length: %d, Position: %d, IsEmpty: %s';

type
  THashBuffer = record

  private
  var
    FData: THashLibByteArray;
    FPosition: Int32;

    function GetIsEmpty: Boolean; inline;
    function GetIsFull: Boolean; inline;
    function GetPosition: Int32; inline;
    function GetLength: Int32; inline;

  public
    constructor Create(ALength: Int32);

    procedure Initialize();
    function GetBytes(): THashLibByteArray; inline;
    function GetBytesZeroPadded(): THashLibByteArray; inline;

    function Feed(AData: PByte; ADataLength: Int32; var AStartIndex: Int32;
      var ALength: Int32; var AProcessedBytesCount: UInt64): Boolean; overload;

    function Feed(AData: PByte; ADataLength: Int32; ALength: Int32)
      : Boolean; overload;

    function ToString(): String;
    function Clone(): THashBuffer; inline;

    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;
    property Position: Int32 read GetPosition;
    property Length: Int32 read GetLength;
  end;

implementation

{ THashBuffer }

function THashBuffer.Clone(): THashBuffer;
begin
  result := Default (THashBuffer);
  result.FData := System.Copy(FData);
  result.FPosition := FPosition;
end;

constructor THashBuffer.Create(ALength: Int32);
begin
{$IFDEF DEBUG}
  System.Assert(ALength > 0);
{$ENDIF DEBUG}
  System.SetLength(FData, ALength);
  Initialize();
end;

function THashBuffer.GetIsFull: Boolean;
begin
  result := FPosition = System.Length(FData);
end;

function THashBuffer.Feed(AData: PByte; ADataLength: Int32;
  ALength: Int32): Boolean;
var
  LLength: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(ALength >= 0);
  System.Assert(ALength <= ADataLength);
  System.Assert(not IsFull);
{$ENDIF DEBUG}
  if (ADataLength = 0) then
  begin
    result := false;
    Exit;
  end;

  if (ALength = 0) then
  begin
    result := false;
    Exit;
  end;
  LLength := System.Length(FData) - FPosition;
  if (LLength > ALength) then
  begin
    LLength := ALength;
  end;

  System.Move(AData[0], FData[FPosition], LLength * System.SizeOf(Byte));

  FPosition := FPosition + LLength;

  result := IsFull;
end;

function THashBuffer.Feed(AData: PByte; ADataLength: Int32;
  var AStartIndex, ALength: Int32; var AProcessedBytesCount: UInt64): Boolean;
var
  LLength: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(AStartIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert((AStartIndex + ALength) <= ADataLength);
  System.Assert(not IsFull);
{$ENDIF DEBUG}
  if (ADataLength = 0) then
  begin
    result := false;
    Exit;
  end;

  if (ALength = 0) then
  begin
    result := false;
    Exit;
  end;

  LLength := System.Length(FData) - FPosition;
  if (LLength > ALength) then
  begin
    LLength := ALength;
  end;

  System.Move(AData[AStartIndex], FData[FPosition],
    LLength * System.SizeOf(Byte));

  FPosition := FPosition + LLength;
  AStartIndex := AStartIndex + LLength;
  ALength := ALength - LLength;
  AProcessedBytesCount := AProcessedBytesCount + UInt64(LLength);

  result := IsFull;
end;

function THashBuffer.GetBytes: THashLibByteArray;
begin
{$IFDEF DEBUG}
  System.Assert(IsFull);
{$ENDIF DEBUG}
  FPosition := 0;
  result := FData;
end;

function THashBuffer.GetBytesZeroPadded: THashLibByteArray;
begin
  TArrayUtils.Fill(FData, FPosition, (System.Length(FData) - FPosition) +
    FPosition, Byte(0));
  FPosition := 0;
  result := FData;
end;

function THashBuffer.GetIsEmpty: Boolean;
begin
  result := FPosition = 0;
end;

function THashBuffer.GetLength: Int32;
begin
  result := System.Length(FData);
end;

function THashBuffer.GetPosition: Int32;
begin
  result := FPosition;
end;

procedure THashBuffer.Initialize;
begin
  FPosition := 0;
  TArrayUtils.ZeroFill(FData);
end;

function THashBuffer.ToString: String;
begin
  result := Format(SHashBufferMessage, [Self.Length, Self.Position,
    BoolToStr(Self.IsEmpty, True)]);
end;

end.
