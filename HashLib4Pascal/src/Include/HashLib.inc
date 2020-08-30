{ *********************************************************************************** }
{ *                                 HashLib Library                                 * }
{ *                  Copyright (c) 2016 - 2020 Ugochukwu Mmaduekwe                  * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)


{$DEFINE DELPHI}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)
{$IFDEF FPC}
{$I HashLibHelper.inc} // Had to Include this Since Delphi Does not allow "FPC_FULLVERSION" to Compile.
{$UNDEF DELPHI}
{$MODE delphi}

{$IFDEF FPC_LITTLE_ENDIAN}
   {$DEFINE HASHLIB_LITTLE_ENDIAN}
{$ENDIF}

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
   {$DEFINE HASHLIB_REQUIRES_PROPER_ALIGNMENT}
{$ENDIF}

{$DEFINE USE_UNROLLED_VARIANT}

// Disable Overflow and RangeChecks.
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

// Enable Pointer Math
{$POINTERMATH ON}

// Disable Warnings and Hints.
{$WARNINGS OFF}
{$HINTS OFF}
{$NOTES OFF}

// Optimizations
{$OPTIMIZATION LEVEL3}
// disable "USELOADMODIFYSTORE" because it produces incorrect result
// when used in combination with -CpCOREAVX2 and -OpCOREAVX2 in FPC 3.2.0 beta
{$IFDEF FPC_GREATER_THAN_3.0.4}
   {$OPTIMIZATION NOUSELOADMODIFYSTORE}
{$ENDIF}
// level 4 optimizations
{$OPTIMIZATION ORDERFIELDS}
{$OPTIMIZATION DEADVALUES}

{$IFDEF CPUI386}
   {$OPTIMIZATION USEEBP}
{$ENDIF}

{$IFDEF CPUX86_64}
   {$OPTIMIZATION USERBP}
{$ENDIF}

{.$DEFINE USE_MTPROCS}
{.$DEFINE USE_PASMP}

{$IF DEFINED(USE_MTPROCS) AND DEFINED(USE_PASMP)}
   {$MESSAGE ERROR 'Only One Threading Library can be used at a time.'}
{$IFEND}

{$ENDIF FPC}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

{$IFDEF DELPHI}

{$DEFINE HASHLIB_LITTLE_ENDIAN}

{$IFDEF CPUARM}
   {$DEFINE HASHLIB_REQUIRES_PROPER_ALIGNMENT}
{$ENDIF}

  // XE3 and Above
{$IF CompilerVersion >= 24.0}
   {$DEFINE DELPHIXE3_UP}
   {$LEGACYIFEND ON}
   {$ZEROBASEDSTRINGS OFF}
{$IFEND}

 // 2010 only
{$IF CompilerVersion = 21.0}
   {$DEFINE DELPHI2010}
{$IFEND}

  // 2010 and Above
{$IF CompilerVersion >= 21.0}
   {$DEFINE DELPHI2010_UP}
{$IFEND}

  // XE and Above
{$IF CompilerVersion >= 22.0}
   {$DEFINE DELPHIXE_UP}
{$IFEND}

  // XE2 and Above
{$IF CompilerVersion >= 23.0}
   {$DEFINE DELPHIXE2_UP}
{$IFEND}

 // XE4 and Above
{$IF CompilerVersion >= 25.0}
   {$DEFINE DELPHIXE4_UP}
   {$DEFINE SHIFT_OVERFLOW_BUG_FIXED}
{$IFEND}

  // XE7 and Above
{$IF CompilerVersion >= 28.0}
   {$DEFINE DELPHIXE7_UP}
   {.$DEFINE USE_DELPHI_PPL} // Use Delphi Parallel Programming Library
   {$DEFINE HAS_DELPHI_NET_ENCODING}
{$IFEND}

  // 2010 and Above
{$IFNDEF DELPHI2010_UP}
   {$MESSAGE ERROR 'This Library requires Delphi 2010 or higher.'}
{$ENDIF}

{$DEFINE USE_UNROLLED_VARIANT}

{.$DEFINE USE_PASMP}

{$IF DEFINED(USE_DELPHI_PPL) AND DEFINED(USE_PASMP)}
   {$MESSAGE ERROR 'Only One Threading Library can be used at a time.'}
{$IFEND}

// This option is needed to enable code browsing (aka Ctrl+Click)
// It does not affect the binary size or generated code
{$DEFINITIONINFO ON}

// Disable Overflow and RangeChecks.
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

 // Enable Pointer Math
{$POINTERMATH ON}

// Disable String Checks
{$STRINGCHECKS OFF}

// Disable Duplicate Constructor Warnings
{$WARN DUPLICATE_CTOR_DTOR OFF}

{$ENDIF DELPHI}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)
