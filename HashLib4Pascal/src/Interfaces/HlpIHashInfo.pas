unit HlpIHashInfo;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpIKDF,
  HlpIHash,
  HlpArgon2TypeAndVersion,
  HlpNullable;

type

  ITransformBlock = Interface(IInterface)
    ['{0C375CFF-B379-41B8-955F-A32E22991651}']
  end;

  IBlockHash = Interface(IInterface)
    ['{3B9A2D29-AC4E-44E4-92B1-6AF9A64DFF0A}']
  end;

  INonBlockHash = Interface(IInterface)
    ['{7C7E8B14-DBC7-44A3-BB7C-B24E0BFAA09C}']
  end;

  IChecksum = Interface(IHash)
    ['{EF0885C5-D331-44D8-89CA-05409E20F76E}']
  end;

  ICrypto = Interface(IHash)
    ['{5C669048-644C-4E96-B411-9FEA603D7086}']
  end;

  ICryptoNotBuildIn = Interface(ICrypto)
    ['{391E62CE-219D-4D33-A753-C32D63353685}']
  end;

  IWithKey = Interface(IHash)
    ['{DD5E0FE4-3573-4051-B7CF-F23BABE982D8}']

    function GetKey(): THashLibByteArray;
    procedure SetKey(const AValue: THashLibByteArray);
    property Key: THashLibByteArray read GetKey write SetKey;
    function GetKeyLength(): TNullableInteger;
    property KeyLength: TNullableInteger read GetKeyLength;

  end;

  IMAC = Interface(IHash)
    ['{C75C99A1-B7D3-475F-AC39-03386EECC095}']
    procedure Clear();
    function GetKey(): THashLibByteArray;
    procedure SetKey(const AValue: THashLibByteArray);
    property Key: THashLibByteArray read GetKey write SetKey;
  end;

  IHMAC = Interface(IMAC)
    ['{A6D4DCC6-F6C3-4110-8CA2-FBE85227676E}']
  end;

  IHMACNotBuildIn = Interface(IHMAC)
    ['{A44E01D3-164E-4E3F-9551-3EFFDE95A36C}']
  end;

  IKMAC = Interface(IMAC)
    ['{49309B2F-20C3-4631-BFDD-06373D14CCE0}']
  end;

  IKMACNotBuildIn = Interface(IKMAC)
    ['{FC7AF5A9-BD6A-4DBD-B1DD-B6E110B44A20}']
  end;

  IBlake2BMAC = Interface(IMAC)
    ['{F6E0B1CA-1497-43C6-9CD9-2628F70E8451}']
  end;

  IBlake2BMACNotBuildIn = Interface(IBlake2BMAC)
    ['{20B33EE5-48B4-4F7E-B1B8-1FD7B45E256E}']
  end;

  IBlake2SMAC = Interface(IMAC)
    ['{7354FC5C-775C-42E9-9A25-274F62BF2CCE}']
  end;

  IBlake2SMACNotBuildIn = Interface(IBlake2SMAC)
    ['{FFB17B7A-86A1-40D7-A5E7-60366FF8513C}']
  end;

  IPBKDF2_HMAC = Interface(IKDF)
    ['{0D409BA8-7F98-4417-858F-3C1EBA11B7E1}']
  end;

  IPBKDF2_HMACNotBuildIn = Interface(IPBKDF2_HMAC)
    ['{D7E23DFB-036D-44AD-AA0C-FB83C9970565}']
  end;

  IPBKDF_Argon2 = Interface(IKDF)
    ['{A2BF19D2-8CEE-45B7-93A1-110A63A0A5A7}']
  end;

  IPBKDF_Argon2NotBuildIn = Interface(IPBKDF_Argon2)
    ['{666D652C-E4E5-4C72-B09F-145495D1A95D}']
  end;

  IPBKDF_Scrypt = Interface(IKDF)
    ['{D1AD2681-FBDB-41EF-B8F5-72E3F5872D27}']
  end;

  IPBKDF_ScryptNotBuildIn = Interface(IPBKDF_Scrypt)
    ['{7DD70C4D-FBF6-4629-B587-C6A7CC047D35}']
  end;

  IHash16 = Interface(IHash)
    ['{C15AF648-C9F7-460D-9F74-B68CA593C2F8}']
  end;

  IHash32 = Interface(IHash)
    ['{004BBFDB-71B6-4C74-ABE8-88EC1777263D}']
  end;

  IHash64 = Interface(IHash)
    ['{F0354E86-3BEC-4EBC-B17D-ABFC91C02997}']
  end;

  IHash128 = Interface(IHash)
    ['{8DD14E37-DDD6-455C-A795-21A15C9E5376}']
  end;

  IHashWithKey = Interface(IWithKey)
    ['{D38AE885-651F-4F15-BF90-5B64A0F24E49}']
  end;

  IXOF = Interface(IHash)
    ['{944ED7F0-D033-4489-A5DD-9C83353F23F0}']
    function GetXOFSizeInBits: UInt64;
    procedure SetXOFSizeInBits(AXofSizeInBits: UInt64);
    property XOFSizeInBits: UInt64 read GetXOFSizeInBits write SetXOFSizeInBits;
    procedure DoOutput(const ADestination: THashLibByteArray;
      ADestinationOffset, AOutputLength: UInt64);
  end;

type
  IArgon2Parameters = interface(IInterface)
    ['{566D3381-57F1-4EE0-81EC-3DB21FF49FBC}']
    procedure Clear();

    function GetSalt(): THashLibByteArray;
    property Salt: THashLibByteArray read GetSalt;
    function GetSecret(): THashLibByteArray;
    property Secret: THashLibByteArray read GetSecret;
    function GetAdditional(): THashLibByteArray;
    property Additional: THashLibByteArray read GetAdditional;
    function GetIterations(): Int32;
    property Iterations: Int32 read GetIterations;
    function GetMemory(): Int32;
    property Memory: Int32 read GetMemory;
    function GetLanes(): Int32;
    property Lanes: Int32 read GetLanes;
    function GetType(): TArgon2Type;
    property &Type: TArgon2Type read GetType;
    function GetVersion(): TArgon2Version;
    property Version: TArgon2Version read GetVersion;
  end;

type
  IArgon2ParametersBuilder = interface(IInterface)
    ['{DD0EF0C0-BAB8-4587-95FD-B9A266E67BC1}']

    function WithParallelism(AParallelism: Int32): IArgon2ParametersBuilder;

    function WithSalt(const ASalt: THashLibByteArray): IArgon2ParametersBuilder;

    function WithSecret(const ASecret: THashLibByteArray)
      : IArgon2ParametersBuilder;

    function WithAdditional(const AAdditional: THashLibByteArray)
      : IArgon2ParametersBuilder;

    function WithIterations(AIterations: Int32): IArgon2ParametersBuilder;

    function WithMemoryAsKB(AMemory: Int32): IArgon2ParametersBuilder;

    function WithMemoryPowOfTwo(AMemory: Int32): IArgon2ParametersBuilder;

    function WithVersion(AVersion: TArgon2Version): IArgon2ParametersBuilder;

    procedure Clear();

    function Build(): IArgon2Parameters;

  end;

implementation

end.
