unit HlpSHA2_224;

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
  HlpSHA2_256Base,
  HlpIHash,
  HlpConverters;

type
  TSHA2_224 = class sealed(TSHA2_256Base)

  strict protected
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TSHA2_224 }

function TSHA2_224.Clone(): IHash;
var
  LHashInstance: TSHA2_224;
begin
  LHashInstance := TSHA2_224.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA2_224.Create;
begin
  Inherited Create(28);
end;

function TSHA2_224.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 7 * System.SizeOf(UInt32));
  TConverters.be32_copy(PCardinal(FState), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TSHA2_224.Initialize;
begin
  FState[0] := $C1059ED8;
  FState[1] := $367CD507;
  FState[2] := $3070DD17;
  FState[3] := $F70E5939;
  FState[4] := $FFC00B31;
  FState[5] := $68581511;
  FState[6] := $64F98FA7;
  FState[7] := $BEFA4FA4;
  Inherited Initialize();
end;

end.
