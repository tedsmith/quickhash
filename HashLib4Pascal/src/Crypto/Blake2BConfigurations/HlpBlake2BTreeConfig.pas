unit HlpBlake2BTreeConfig;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpIBlake2BTreeConfig,
  HlpHashLibTypes;

resourcestring
  SInvalidFanOutParameter =
    'FanOut Value Should be Between [0 .. 255] for Blake2B';
  SInvalidMaxDepthParameter =
    'FanOut Value Should be Between [1 .. 255] for Blake2B';
  SInvalidNodeDepthParameter =
    'NodeDepth Value Should be Between [0 .. 255] for Blake2B';
  SInvalidInnerHashSizeParameter =
    'InnerHashSize Value Should be Between [0 .. 64] for Blake2B';

type

  TBlake2BTreeConfig = class sealed(TInterfacedObject, IBlake2BTreeConfig)

  strict private

    FFanOut, FMaxDepth, FNodeDepth, FInnerHashSize: Byte;
    FLeafSize: UInt32;
    FNodeOffset: UInt64;
    FIsLastNode: Boolean;

    procedure ValidateFanOut(AFanOut: Byte); inline;
    procedure ValidateInnerHashSize(AInnerHashSize: Byte); inline;
    procedure ValidateMaxDepth(AMaxDepth: Byte); inline;
    procedure ValidateNodeDepth(ANodeDepth: Byte); inline;

    function GetFanOut: Byte; inline;
    procedure SetFanOut(value: Byte); inline;

    function GetMaxDepth: Byte; inline;
    procedure SetMaxDepth(value: Byte); inline;

    function GetNodeDepth: Byte; inline;
    procedure SetNodeDepth(value: Byte); inline;

    function GetInnerHashSize: Byte; inline;
    procedure SetInnerHashSize(value: Byte); inline;

    function GetLeafSize: UInt32; inline;
    procedure SetLeafSize(value: UInt32); inline;

    function GetNodeOffset: UInt64; inline;
    procedure SetNodeOffset(value: UInt64); inline;

    function GetIsLastNode: Boolean; inline;
    procedure SetIsLastNode(value: Boolean); inline;

  public
    constructor Create();

    property FanOut: Byte read GetFanOut write SetFanOut;

    property MaxDepth: Byte read GetMaxDepth write SetMaxDepth;

    property NodeDepth: Byte read GetNodeDepth write SetNodeDepth;

    property InnerHashSize: Byte read GetInnerHashSize write SetInnerHashSize;

    property LeafSize: UInt32 read GetLeafSize write SetLeafSize;

    property NodeOffset: UInt64 read GetNodeOffset write SetNodeOffset;

    property IsLastNode: Boolean read GetIsLastNode write SetIsLastNode;

  end;

implementation

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

function TBlake2BTreeConfig.GetFanOut: Byte;
begin
  result := FFanOut;
end;

function TBlake2BTreeConfig.GetInnerHashSize: Byte;
begin
  result := FInnerHashSize;
end;

function TBlake2BTreeConfig.GetIsLastNode: Boolean;
begin
  result := FIsLastNode;
end;

function TBlake2BTreeConfig.GetLeafSize: UInt32;
begin
  result := FLeafSize;
end;

function TBlake2BTreeConfig.GetMaxDepth: Byte;
begin
  result := FMaxDepth;
end;

function TBlake2BTreeConfig.GetNodeDepth: Byte;
begin
  result := FNodeDepth;
end;

function TBlake2BTreeConfig.GetNodeOffset: UInt64;
begin
  result := FNodeOffset;
end;

procedure TBlake2BTreeConfig.SetFanOut(value: Byte);
begin
  ValidateFanOut(value);
  FFanOut := value;
end;

procedure TBlake2BTreeConfig.SetInnerHashSize(value: Byte);
begin
  ValidateInnerHashSize(value);
  FInnerHashSize := value;
end;

procedure TBlake2BTreeConfig.SetIsLastNode(value: Boolean);
begin
  FIsLastNode := value;
end;

procedure TBlake2BTreeConfig.SetLeafSize(value: UInt32);
begin
  FLeafSize := value;
end;

procedure TBlake2BTreeConfig.SetMaxDepth(value: Byte);
begin
  ValidateMaxDepth(value);
  FMaxDepth := value;
end;

procedure TBlake2BTreeConfig.SetNodeDepth(value: Byte);
begin
  ValidateNodeDepth(value);
  FNodeDepth := value;
end;

procedure TBlake2BTreeConfig.SetNodeOffset(value: UInt64);
begin
  FNodeOffset := value;
end;

constructor TBlake2BTreeConfig.Create;
begin
  Inherited Create();
  ValidateInnerHashSize(64);
  FInnerHashSize := 64;
end;

end.
