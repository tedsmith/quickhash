unit HlpFNV1a;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TFNV1a = class sealed(THash, IHash32, IBlockHash, ITransformBlock)
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

{ TFNV1a }

constructor TFNV1a.Create;
begin
  Inherited Create(4, 1);

end;

procedure TFNV1a.Initialize;
begin
  Fm_hash := 2166136261;
end;

procedure TFNV1a.TransformBytes(a_data: THashLibByteArray;
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
    Fm_hash := (Fm_hash xor a_data[i]) * 16777619;
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TFNV1a.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
