unit HlpSHA2_512_224;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpBitConverter,
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpSHA2_512Base,
  HlpConverters;

type
  TSHA2_512_224 = class sealed(TSHA2_512Base)

  strict protected
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TSHA2_512_224 }

function TSHA2_512_224.Clone(): IHash;
var
  HashInstance: TSHA2_512_224;
begin
  HashInstance := TSHA2_512_224.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA2_512_224.Create;
begin
  Inherited Create(28);
end;

function TSHA2_512_224.GetResult: THashLibByteArray;
begin

  System.SetLength(result, 4 * System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(Fm_state), 0, PByte(result), 0,
    System.Length(result));
  System.SetLength(result, HashSize * System.SizeOf(Byte));

end;

procedure TSHA2_512_224.Initialize;
begin

  Fm_state[0] := UInt64($8C3D37C819544DA2);
  Fm_state[1] := $73E1996689DCD4D6;
  Fm_state[2] := $1DFAB7AE32FF9C82;
  Fm_state[3] := $679DD514582F9FCF;
  Fm_state[4] := $0F6D2B697BD44DA8;
  Fm_state[5] := $77E36F7304C48942;
  Fm_state[6] := $3F9D85A86A1D36C8;
  Fm_state[7] := $1112E6AD91D692A1;

  Inherited Initialize();

end;

end.
