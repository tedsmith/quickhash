unit HlpSDBM;

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
  TSDBM = class sealed(THash, IHash32, ITransformBlock)
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

{ TSDBM }

function TSDBM.Clone(): IHash;
var
  HashInstance: TSDBM;
begin
  HashInstance := TSDBM.Create();
  HashInstance.Fm_hash := Fm_hash;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSDBM.Create;
begin
  Inherited Create(4, 1);
end;

procedure TSDBM.Initialize;
begin
  Fm_hash := 0;
end;

procedure TSDBM.TransformBytes(const a_data: THashLibByteArray;
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
    Fm_hash := UInt32(a_data[i] + Int64(Fm_hash shl 6) + Int64(Fm_hash shl 16)
      - Fm_hash);
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TSDBM.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
