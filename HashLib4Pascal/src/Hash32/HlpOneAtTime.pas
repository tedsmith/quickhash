unit HlpOneAtTime;

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
  TOneAtTime = class sealed(THash, IHash32, ITransformBlock)
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

{ TOneAtTime }

function TOneAtTime.Clone(): IHash;
var
  HashInstance: TOneAtTime;
begin
  HashInstance := TOneAtTime.Create();
  HashInstance.Fm_hash := Fm_hash;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TOneAtTime.Create;
begin
  Inherited Create(4, 1);
end;

procedure TOneAtTime.Initialize;
begin
  Fm_hash := 0;
end;

procedure TOneAtTime.TransformBytes(const a_data: THashLibByteArray;
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
    Fm_hash := Fm_hash + a_data[i];
    Fm_hash := Fm_hash + (Fm_hash shl 10);
    Fm_hash := Fm_hash xor (Fm_hash shr 6);
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TOneAtTime.TransformFinal: IHashResult;
begin
  Fm_hash := Fm_hash + (Fm_hash shl 3);
  Fm_hash := Fm_hash xor (Fm_hash shr 11);
  Fm_hash := Fm_hash + (Fm_hash shl 15);

  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
