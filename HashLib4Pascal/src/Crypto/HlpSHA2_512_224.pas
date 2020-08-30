unit HlpSHA2_512_224;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
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
  LHashInstance: TSHA2_512_224;
begin
  LHashInstance := TSHA2_512_224.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA2_512_224.Create;
begin
  Inherited Create(28);
end;

function TSHA2_512_224.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 4 * System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(FState), 0, PByte(result), 0,
    System.Length(result));
  System.SetLength(result, HashSize * System.SizeOf(Byte));
end;

procedure TSHA2_512_224.Initialize;
begin
  FState[0] := UInt64($8C3D37C819544DA2);
  FState[1] := $73E1996689DCD4D6;
  FState[2] := $1DFAB7AE32FF9C82;
  FState[3] := $679DD514582F9FCF;
  FState[4] := $0F6D2B697BD44DA8;
  FState[5] := $77E36F7304C48942;
  FState[6] := $3F9D85A86A1D36C8;
  FState[7] := $1112E6AD91D692A1;
  Inherited Initialize();
end;

end.
