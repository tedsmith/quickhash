unit HlpMDBase;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHashBuffer,
{$ENDIF DELPHI}
  HlpIHashInfo,
  HlpHashCryptoNotBuildIn,
  HlpConverters;

type
  TMDBase = class abstract(TBlockHash, ICryptoNotBuildIn)

  strict protected
  var
    FState: THashLibUInt32Array;

  const

    C1 = UInt32($50A28BE6);
    C2 = UInt32($5A827999);
    C3 = UInt32($5C4DD124);
    C4 = UInt32($6ED9EBA1);
    C5 = UInt32($6D703EF3);
    C6 = UInt32($8F1BBCDC);
    C7 = UInt32($7A6D76E9);
    C8 = UInt32($A953FD4E);

    constructor Create(AStateLength, AHashSize: Int32);

    function GetResult(): THashLibByteArray; override;
    procedure Finish(); override;

  public
    procedure Initialize(); override;

  end;

implementation

{ TMDBase }

constructor TMDBase.Create(AStateLength, AHashSize: Int32);
begin
  Inherited Create(AHashSize, 64);
  System.SetLength(FState, AStateLength);
end;

procedure TMDBase.Finish;
var
  LBits: UInt64;
  LPadIndex: Int32;
  LPad: THashLibByteArray;
begin
  LBits := FProcessedBytesCount * 8;
  if (FBuffer.Position < 56) then
  begin
    LPadIndex := 56 - FBuffer.Position
  end
  else
  begin
    LPadIndex := 120 - FBuffer.Position;
  end;
  System.SetLength(LPad, LPadIndex + 8);

  LPad[0] := $80;

  LBits := TConverters.le2me_64(LBits);

  TConverters.ReadUInt64AsBytesLE(LBits, LPad, LPadIndex);

  LPadIndex := LPadIndex + 8;

  TransformBytes(LPad, 0, LPadIndex);
end;

function TMDBase.GetResult: THashLibByteArray;
begin
  System.SetLength(result, System.Length(FState) * System.SizeOf(UInt32));
  TConverters.le32_copy(PCardinal(FState), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TMDBase.Initialize;
begin
  FState[0] := $67452301;
  FState[1] := $EFCDAB89;
  FState[2] := $98BADCFE;
  FState[3] := $10325476;
  inherited Initialize();
end;

end.
