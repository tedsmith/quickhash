unit HlpSHA2_512_256;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpBitConverter,
{$ENDIF DELPHI}
  HlpSHA2_512Base,
  HlpConverters;

type
  TSHA2_512_256 = class sealed(TSHA2_512Base)

  strict protected
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;

  end;

implementation

{ TSHA2_512_256 }

constructor TSHA2_512_256.Create;
begin
  Inherited Create(32);
end;

function TSHA2_512_256.GetResult: THashLibByteArray;
begin

  System.SetLength(result, 4 * System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(Fm_state), 0, PByte(result), 0,
    System.Length(result));

end;

procedure TSHA2_512_256.Initialize;
begin

  Fm_state[0] := $22312194FC2BF72C;
  Fm_state[1] := UInt64($9F555FA3C84C64C2);
  Fm_state[2] := $2393B86B6F53B151;
  Fm_state[3] := UInt64($963877195940EABD);
  Fm_state[4] := UInt64($96283EE2A88EFFE3);
  Fm_state[5] := UInt64($BE5E1E2553863992);
  Fm_state[6] := $2B0199FC2C85B8AA;
  Fm_state[7] := $0EB72DDC81C52CA2;

  Inherited Initialize();

end;

end.
