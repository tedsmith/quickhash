unit HlpICRC;

interface

uses
  HlpIHash,
  HlpHashLibTypes;

type

  ICRC = Interface(IHash)
    ['{44A105E5-6716-43C0-BE69-AE80F87FDC39}']

    function GetNames: THashLibStringArray;
    property Names: THashLibStringArray read GetNames;
    function GetWidth: Int32;
    property Width: Int32 read GetWidth;
    function GetPolynomial: UInt64;
    property Polynomial: UInt64 read GetPolynomial;
    function GetInit: UInt64;
    property Init: UInt64 read GetInit;
    function GetReflectIn: Boolean;
    property ReflectIn: Boolean read GetReflectIn;
    function GetReflectOut: Boolean;
    property ReflectOut: Boolean read GetReflectOut;
    function GetXOROut: UInt64;
    property XOROut: UInt64 read GetXOROut;
    function GetCheckValue: UInt64;
    property CheckValue: UInt64 read GetCheckValue;

  end;

implementation

end.
