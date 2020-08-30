unit HlpSHA2_256;

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
  HlpSHA2_256Base,
  HlpConverters;

type
  TSHA2_256 = class sealed(TSHA2_256Base)

  strict protected
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TSHA2_256 }

function TSHA2_256.Clone(): IHash;
var
  LHashInstance: TSHA2_256;
begin
  LHashInstance := TSHA2_256.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA2_256.Create;
begin
  Inherited Create(32);
end;

function TSHA2_256.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 8 * System.SizeOf(UInt32));
  TConverters.be32_copy(PCardinal(FState), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TSHA2_256.Initialize;
begin
  FState[0] := $6A09E667;
  FState[1] := $BB67AE85;
  FState[2] := $3C6EF372;
  FState[3] := $A54FF53A;
  FState[4] := $510E527F;
  FState[5] := $9B05688C;
  FState[6] := $1F83D9AB;
  FState[7] := $5BE0CD19;
  Inherited Initialize();
end;

end.
