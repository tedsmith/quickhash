unit HlpIBlake2SParams;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes;

type
  IBlake2SConfig = interface(IInterface)
    ['{C78DE94A-0290-467D-BE26-D0AD1639076C}']
    function GetPersonalisation: THashLibByteArray;
    procedure SetPersonalisation(const AValue: THashLibByteArray);
    property Personalisation: THashLibByteArray read GetPersonalisation
      write SetPersonalisation;
    function GetSalt: THashLibByteArray;
    procedure SetSalt(const AValue: THashLibByteArray);
    property Salt: THashLibByteArray read GetSalt write SetSalt;
    function GetKey: THashLibByteArray;
    procedure SetKey(const AValue: THashLibByteArray);
    property Key: THashLibByteArray read GetKey write SetKey;
    function GetHashSize: Int32;
    procedure SetHashSize(AValue: Int32);
    property HashSize: Int32 read GetHashSize write SetHashSize;

    function Clone(): IBlake2SConfig;

    procedure Clear();

  end;

type
  IBlake2STreeConfig = interface(IInterface)
    ['{93635D4F-7104-4E4A-BE9B-C608606F620F}']

    function GetFanOut: Byte;
    procedure SetFanOut(AValue: Byte);
    property FanOut: Byte read GetFanOut write SetFanOut;

    function GetMaxDepth: Byte;
    procedure SetMaxDepth(AValue: Byte);
    property MaxDepth: Byte read GetMaxDepth write SetMaxDepth;

    function GetNodeDepth: Byte;
    procedure SetNodeDepth(AValue: Byte);
    property NodeDepth: Byte read GetNodeDepth write SetNodeDepth;

    function GetInnerHashSize: Byte;
    procedure SetInnerHashSize(AValue: Byte);
    property InnerHashSize: Byte read GetInnerHashSize write SetInnerHashSize;

    function GetLeafSize: UInt32;
    procedure SetLeafSize(AValue: UInt32);
    property LeafSize: UInt32 read GetLeafSize write SetLeafSize;

    function GetNodeOffset: UInt64;
    procedure SetNodeOffset(AValue: UInt64);
    property NodeOffset: UInt64 read GetNodeOffset write SetNodeOffset;

    function GetIsLastNode: Boolean;
    procedure SetIsLastNode(AValue: Boolean);
    property IsLastNode: Boolean read GetIsLastNode write SetIsLastNode;

    function Clone(): IBlake2STreeConfig;

  end;

implementation

end.
