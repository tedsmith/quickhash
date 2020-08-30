unit HlpIHash;

{$I ..\Include\HashLib.inc}

interface

uses
  SysUtils,
  Classes,
  HlpHashLibTypes,
  HlpIHashResult;

type
  IHash = interface(IInterface)
    ['{E91711E1-FD37-4AFD-875A-DD122470D269}']

    function GetBlockSize: Int32;
    property BlockSize: Int32 read GetBlockSize;

    function GetHashSize: Int32;
    property HashSize: Int32 read GetHashSize;

    function GetBufferSize: Int32;
    procedure SetBufferSize(AValue: Int32);
    property BufferSize: Int32 read GetBufferSize write SetBufferSize;

    function GetName: String;
    property Name: String read GetName;

    procedure Initialize();

    procedure TransformString(const AData: String; const AEncoding: TEncoding);
    procedure TransformBytes(const AData: THashLibByteArray); overload;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex: Int32); overload;
    procedure TransformBytes(const AData: THashLibByteArray; AIndex: Int32;
      ALength: Int32); overload;
    procedure TransformUntyped(const AData; ALength: Int64);
    procedure TransformStream(const AStream: TStream; ALength: Int64 = -1);
    procedure TransformFile(const AFileName: String; AFrom: Int64 = 0;
      ALength: Int64 = -1);

    function TransformFinal(): IHashResult;

    function ComputeString(const AData: String; AEncoding: TEncoding)
      : IHashResult;
    function ComputeBytes(const AData: THashLibByteArray): IHashResult;
    function ComputeUntyped(const AData; ALength: Int64): IHashResult;
    function ComputeStream(const AStream: TStream; ALength: Int64 = -1)
      : IHashResult;
    function ComputeFile(const AFileName: String; AFrom: Int64 = 0;
      ALength: Int64 = -1): IHashResult;

    function Clone(): IHash;

  end;

implementation

end.
