unit HlpICRC;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpIHashInfo,
  HlpHashLibTypes;

type

  ICRC = Interface(IChecksum)
    ['{44A105E5-6716-43C0-BE69-AE80F87FDC39}']

    function GetNames: THashLibStringArray;
    property Names: THashLibStringArray read GetNames;
    function GetWidth: Int32;
    property Width: Int32 read GetWidth;
    function GetPolynomial: UInt64;
    property Polynomial: UInt64 read GetPolynomial;
    function GetInitial: UInt64;
    property Initial: UInt64 read GetInitial;
    function GetIsInputReflected: Boolean;
    property IsInputReflected: Boolean read GetIsInputReflected;
    function GetIsOutputReflected: Boolean;
    property IsOutputReflected: Boolean read GetIsOutputReflected;
    function GetOutputXor: UInt64;
    property OutputXor: UInt64 read GetOutputXor;
    function GetCheckValue: UInt64;
    property CheckValue: UInt64 read GetCheckValue;

  end;

implementation

end.
