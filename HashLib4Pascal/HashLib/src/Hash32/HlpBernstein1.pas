unit HlpBernstein1;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TBernstein1 = class sealed(THash, IHash32, IBlockHash, ITransformBlock)
  strict private

    Fm_hash: UInt32;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
  end;

implementation

{ TBernstein1 }

constructor TBernstein1.Create;
begin
  Inherited Create(4, 1);

end;

procedure TBernstein1.Initialize;
begin
  Fm_hash := 5381;
end;

procedure TBernstein1.TransformBytes(a_data: THashLibByteArray;
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
    Fm_hash := (Fm_hash * 33) xor a_data[i];
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TBernstein1.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
