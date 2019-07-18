unit HlpIBlake2BConfig;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes;

type
  IBlake2BConfig = interface(IInterface)
    ['{176861A3-B06E-4CA3-A1BB-DDEAFF40BFE1}']
    function GetPersonalisation: THashLibByteArray;
    procedure SetPersonalisation(const value: THashLibByteArray);
    property Personalisation: THashLibByteArray read GetPersonalisation
      write SetPersonalisation;
    function GetSalt: THashLibByteArray;
    procedure SetSalt(const value: THashLibByteArray);
    property Salt: THashLibByteArray read GetSalt write SetSalt;
    function GetKey: THashLibByteArray;
    procedure SetKey(const value: THashLibByteArray);
    property Key: THashLibByteArray read GetKey write SetKey;
    function GetHashSize: Int32;
    procedure SetHashSize(value: Int32);
    property HashSize: Int32 read GetHashSize write SetHashSize;

  end;

implementation

end.
