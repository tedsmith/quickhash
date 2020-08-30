unit HlpSipHash128;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI}
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpSipHash;

type
  /// <summary>
  /// SipHash128 2 - 4 algorithm.
  /// <summary>
  TSipHash128_2_4 = class sealed(TSipHash)

  strict protected
    function GetMagicXor(): Byte; override;

  public

    constructor Create(ACompressionRounds: Int32 = 2;
      AFinalizationRounds: Int32 = 4);
    function Clone(): IHash; override;

  end;

implementation

{ TSipHash128_2_4 }

function TSipHash128_2_4.GetMagicXor: Byte;
begin
  Result := $EE;
end;

function TSipHash128_2_4.Clone: IHash;
var
  LHashInstance: TSipHash128_2_4;
begin
  LHashInstance := TSipHash128_2_4.Create();
  LHashInstance.FV0 := FV0;
  LHashInstance.FV1 := FV1;
  LHashInstance.FV2 := FV2;
  LHashInstance.FV3 := FV3;
  LHashInstance.FKey0 := FKey0;
  LHashInstance.FKey1 := FKey1;
  LHashInstance.FPartA := FPartA;
  LHashInstance.FPartB := FPartB;
  LHashInstance.FTotalLength := FTotalLength;
  LHashInstance.FCompressionRounds := FCompressionRounds;
  LHashInstance.FFinalizationRounds := FFinalizationRounds;
  LHashInstance.FIdx := FIdx;
  LHashInstance.FBuffer := System.Copy(FBuffer);
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSipHash128_2_4.Create(ACompressionRounds,
  AFinalizationRounds: Int32);
begin
  Inherited Create(16, 8);
  FCompressionRounds := ACompressionRounds;
  FFinalizationRounds := AFinalizationRounds;
end;

end.
