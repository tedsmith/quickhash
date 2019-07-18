unit HlpIHash;

{$I ..\Include\HashLib.inc}

interface

uses

{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  System.Classes,
{$ELSE}
  SysUtils,
  Classes,
{$ENDIF HAS_UNITSCOPE}
  HlpHashLibTypes,
  HlpIHashResult;

type
  IHash = interface(IInterface)
    ['{E91711E1-FD37-4AFD-875A-DD122470D269}']
    function GetName: String;
    property Name: String read GetName;
    function GetBlockSize: Int32;
    property BlockSize: Int32 read GetBlockSize;
    function GetHashSize: Int32;
    property HashSize: Int32 read GetHashSize;
    function GetBufferSize: Int32;
    procedure SetBufferSize(value: Int32);
    property BufferSize: Int32 read GetBufferSize write SetBufferSize;

    function ComputeString(const a_data: String; a_encoding: TEncoding)
      : IHashResult;
    function ComputeBytes(const a_data: THashLibByteArray): IHashResult;
    function ComputeUntyped(const a_data; a_length: Int64): IHashResult;
    function ComputeStream(const a_stream: TStream; a_length: Int64 = -1)
      : IHashResult;
    function ComputeFile(const a_file_name: String; a_from: Int64 = 0;
      a_length: Int64 = -1): IHashResult;

    procedure Initialize();

    procedure TransformBytes(const a_data: THashLibByteArray); overload;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index: Int32); overload;
    procedure TransformBytes(const a_data: THashLibByteArray; a_index: Int32;
      a_length: Int32); overload;
    procedure TransformUntyped(const a_data; a_length: Int64);

    function TransformFinal(): IHashResult;

    function Clone(): IHash;

    procedure TransformString(const a_data: String;
      const a_encoding: TEncoding);
    procedure TransformStream(const a_stream: TStream; a_length: Int64 = -1);
    procedure TransformFile(const a_file_name: String; a_from: Int64 = 0;
      a_length: Int64 = -1);

  end;

implementation

end.
