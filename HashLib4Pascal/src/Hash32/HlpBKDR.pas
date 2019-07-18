unit HlpBKDR;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type

  TBKDR = class sealed(THash, IHash32, ITransformBlock)
  strict private

    Fm_hash: UInt32;

  const
    SEED = Int32(131);

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TBKDR }

function TBKDR.Clone(): IHash;
var
  HashInstance: TBKDR;
begin
  HashInstance := TBKDR.Create();
  HashInstance.Fm_hash := Fm_hash;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TBKDR.Create;
begin
  Inherited Create(4, 1);
end;

procedure TBKDR.Initialize;
begin
  Fm_hash := 0;
end;

procedure TBKDR.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  i: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  i := a_index;
  while a_length > 0 do
  begin
    Fm_hash := (Fm_hash * UInt32(SEED)) + a_data[i];
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TBKDR.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
