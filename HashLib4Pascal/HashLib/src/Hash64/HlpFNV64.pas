unit HlpFNV64;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TFNV64 = class sealed(THash, IHash64, IBlockHash, ITransformBlock)
  strict private

    Fm_hash: UInt64;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
  end;

implementation

{ TFNV64 }

constructor TFNV64.Create;
begin
  Inherited Create(8, 1);

end;

procedure TFNV64.Initialize;
begin
  Fm_hash := 0;
end;

procedure TFNV64.TransformBytes(a_data: THashLibByteArray;
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
    Fm_hash := (Fm_hash * 1099511628211) xor a_data[i];
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TFNV64.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
