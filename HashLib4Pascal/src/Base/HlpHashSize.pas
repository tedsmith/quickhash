unit HlpHashSize;

{$I ..\Include\HashLib.inc}

interface

type
{$SCOPEDENUMS ON}
  THashSize = (hsHashSize128 = 16, hsHashSize160 = 20, hsHashSize192 = 24,
    hsHashSize224 = 28, hsHashSize256 = 32, hsHashSize288 = 36,
    hsHashSize384 = 48, hsHashSize512 = 64);
{$SCOPEDENUMS OFF}

implementation

end.
