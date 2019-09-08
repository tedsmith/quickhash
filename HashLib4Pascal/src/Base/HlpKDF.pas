unit HlpKDF;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpIKDF,
  HlpHashLibTypes;

type
  TKDF = class abstract(TInterfacedObject, IKDF)

  strict protected

    // Not really needed because there is an Intristic default constructor always
    // called for classes if none is defined by the developer but I just put it
    // for readability reasons.
    constructor Create();

  public

    /// <summary>
    /// Clear sensitive materials from memory
    /// </summary>
    procedure Clear(); virtual; abstract;

    /// <summary>
    /// Returns the pseudo-random bytes for this object.
    /// </summary>
    /// <param name="bc">The number of pseudo-random key bytes to generate.</param>
    /// <returns>A byte array filled with pseudo-random key bytes.</returns>
    /// <exception cref="EArgumentOutOfRangeHashLibException">bc must be greater than zero.</exception>
    /// <exception cref="EArgumentHashLibException">invalid start index or end index of internal buffer.</exception>
    function GetBytes(bc: Int32): THashLibByteArray; virtual; abstract;

  end;

implementation

{ TKDF }

constructor TKDF.Create;
begin
  Inherited Create();
end;

end.
