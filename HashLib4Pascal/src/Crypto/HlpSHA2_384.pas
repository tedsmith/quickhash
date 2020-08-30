unit HlpSHA2_384;

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
  TSHA2_384 = class sealed(TSHA2_512Base)

  strict protected
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TSHA2_384 }

function TSHA2_384.Clone(): IHash;
var
  LHashInstance: TSHA2_384;
begin
  LHashInstance := TSHA2_384.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA2_384.Create;
begin
  Inherited Create(48);
end;

function TSHA2_384.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 6 * System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(FState), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TSHA2_384.Initialize;
begin
  FState[0] := UInt64($CBBB9D5DC1059ED8);
  FState[1] := $629A292A367CD507;
  FState[2] := UInt64($9159015A3070DD17);
  FState[3] := $152FECD8F70E5939;
  FState[4] := $67332667FFC00B31;
  FState[5] := UInt64($8EB44A8768581511);
  FState[6] := UInt64($DB0C2E0D64F98FA7);
  FState[7] := $47B5481DBEFA4FA4;
  Inherited Initialize();
end;

end.
