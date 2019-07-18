unit HlpArgon2TypeAndVersion;

{$I ..\Include\HashLib.inc}

interface

type
{$SCOPEDENUMS ON}
  TArgon2Type = (a2tARGON2_d = $00, a2tARGON2_i = $01, a2tARGON2_id = $02);
  TArgon2Version = (a2vARGON2_VERSION_10 = $10, a2vARGON2_VERSION_13 = $13);
{$SCOPEDENUMS OFF}

implementation

end.
