unit HlpIHashResult;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes;

type
  IHashResult = interface(IInterface)
    ['{A467E23D-DBC4-41CC-848D-7267476430C1}']

    function GetBytes(): THashLibByteArray;
    function GetUInt8(): UInt8;
    function GetUInt16(): UInt16;
    function GetUInt32(): UInt32;
    function GetInt32(): Int32;
    function GetUInt64(): UInt64;
    function ToString(a_group: Boolean = false): String;
    function Equals(const a_hashResult: IHashResult): Boolean; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
  end;

implementation

end.
