unit HlpFNV;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TFNV = class sealed(THash, IHash32, IBlockHash, ITransformBlock)
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

{ TFNV }

constructor TFNV.Create;
begin
  Inherited Create(4, 1);

end;

procedure TFNV.Initialize;
begin
  Fm_hash := 0;
end;

procedure TFNV.TransformBytes(a_data: THashLibByteArray;
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
    Fm_hash := (Fm_hash * 16777619) xor a_data[i];
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TFNV.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
