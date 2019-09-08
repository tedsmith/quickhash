unit HlpRotating;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpBits,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TRotating = class sealed(THash, IHash32, ITransformBlock)
  strict private

    Fm_hash: UInt32;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TRotating }

function TRotating.Clone(): IHash;
var
  HashInstance: TRotating;
begin
  HashInstance := TRotating.Create();
  HashInstance.Fm_hash := Fm_hash;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRotating.Create;
begin
  Inherited Create(4, 1);
end;

procedure TRotating.Initialize;
begin
  Fm_hash := 0;
end;

procedure TRotating.TransformBytes(const a_data: THashLibByteArray;
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

    Fm_hash := TBits.RotateLeft32(Fm_hash, 4) xor a_data[i];
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TRotating.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
