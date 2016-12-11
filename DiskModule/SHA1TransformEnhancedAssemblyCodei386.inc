procedure SHA1Transform(var ctx: TSHA1Context; const Buf: Pointer);assembler;
var
  pctx:^TSHA1Context;
  pbuf:pointer;
  A, B, C, D, E, T: Cardinal;
  Data: array[0..15] of Cardinal;

{$asmmode intel}
asm
  push EAX
  push EBX
  push ECX
  push EDX
  push ESI
  push EDI
  push EBP

  mov pctx, eax
  mov pbuf, edx

//  Invert(Buf, @Data, 64);
//Beginning of Invert
  mov eax, edx//[pbuf]
  mov ebx, [eax+0]
  bswap ebx
  mov [data+0], ebx
  mov ebx, [eax+4]
  bswap ebx
  mov [data+4], ebx
  mov ebx, [eax+8]
  bswap ebx
  mov [data+8], ebx
  mov ebx, [eax+12]
  bswap ebx
  mov [data+12], ebx
  mov ebx, [eax+16]
  bswap ebx
  mov [data+16], ebx
  mov ebx, [eax+20]
  bswap ebx
  mov [data+20], ebx
  mov ebx, [eax+24]
  bswap ebx
  mov [data+24], ebx
  mov ebx, [eax+28]
  bswap ebx
  mov [data+28], ebx
  mov ebx, [eax+32]
  bswap ebx
  mov [data+32], ebx
  mov ebx, [eax+36]
  bswap ebx
  mov [data+36], ebx
  mov ebx, [eax+40]
  bswap ebx
  mov [data+40], ebx
  mov ebx, [eax+44]
  bswap ebx
  mov [data+44], ebx
  mov ebx, [eax+48]
  bswap ebx
  mov [data+48], ebx
  mov ebx, [eax+52]
  bswap ebx
  mov [data+52], ebx
  mov ebx, [eax+56]
  bswap ebx
  mov [data+56], ebx
  mov ebx, [eax+60]
  bswap ebx
  mov [data+60], ebx
//End of Invert

// A := ctx.State[0];
  mov EDI, pctx{.State}
  mov EAX, [EDI]
// B := ctx.State[1];
  mov EBX, [EDI+4]
// C := ctx.State[2];
  mov ECX, [EDI+8]
// D := ctx.State[3];
  mov EDX, [EDI+12]
// E := ctx.State[4];
  mov ESI, [EDI+16]
//i=0
//*    T := (EBX and ECX) or (not EBX and EDX) + K20 + ESI;
  mov EDI, ECX
  xor EDI, EDX
  and EDI, EBX
  xor EDI, EDX
  add EDI, K20
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[0];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*0]
//*    Data[0] := roldword(Data[0] xor Data[2] xor Data[8] xor Data[13], 1);
  mov EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*13]
  rol EDI, 1
  mov [esp+96-96+4*7+4*0], EDI

//i=1
//*    T := (EAX and EBX) or (not EAX and ECX) + K20 + EDX;
  mov EDI, EBX
  xor EDI, ECX
  and EDI, EAX
  xor EDI, ECX
  add EDI, K20
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[1];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*1]
//*    Data[1] := roldword(Data[1] xor Data[3] xor Data[9] xor Data[14], 1);
  mov EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*14]
  rol EDI, 1
  mov [esp+96-96+4*7+4*1], EDI

//i=2
//*    T := (ESI and EAX) or (not ESI and EBX) + K20 + ECX;
  mov EDI, EAX
  xor EDI, EBX
  and EDI, ESI
  xor EDI, EBX
  add EDI, K20
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[2];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*2]
//*    Data[2] := roldword(Data[2] xor Data[4] xor Data[10] xor Data[15], 1);
  mov EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*15]
  rol EDI, 1
  mov [esp+96-96+4*7+4*2], EDI

//i=3
//*    T := (EDX and ESI) or (not EDX and EAX) + K20 + EBX;
  mov EDI, ESI
  xor EDI, EAX
  and EDI, EDX
  xor EDI, EAX
  add EDI, K20
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[3];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*3]
//*    Data[3] := roldword(Data[3] xor Data[5] xor Data[11] xor Data[0], 1);
  mov EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*0]
  rol EDI, 1
  mov [esp+96-96+4*7+4*3], EDI

//i=4
//*    T := (ECX and EDX) or (not ECX and ESI) + K20 + EAX;
  mov EDI, EDX
  xor EDI, ESI
  and EDI, ECX
  xor EDI, ESI
  add EDI, K20
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[4];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*4]
//*    Data[4] := roldword(Data[4] xor Data[6] xor Data[12] xor Data[1], 1);
  mov EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*1]
  rol EDI, 1
  mov [esp+96-96+4*7+4*4], EDI

//i=5
//*    T := (EBX and ECX) or (not EBX and EDX) + K20 + ESI;
  mov EDI, ECX
  xor EDI, EDX
  and EDI, EBX
  xor EDI, EDX
  add EDI, K20
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[5];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*5]
//*    Data[5] := roldword(Data[5] xor Data[7] xor Data[13] xor Data[2], 1);
  mov EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*2]
  rol EDI, 1
  mov [esp+96-96+4*7+4*5], EDI

//i=6
//*    T := (EAX and EBX) or (not EAX and ECX) + K20 + EDX;
  mov EDI, EBX
  xor EDI, ECX
  and EDI, EAX
  xor EDI, ECX
  add EDI, K20
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[6];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*6]
//*    Data[6] := roldword(Data[6] xor Data[8] xor Data[14] xor Data[3], 1);
  mov EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*3]
  rol EDI, 1
  mov [esp+96-96+4*7+4*6], EDI

//i=7
//*    T := (ESI and EAX) or (not ESI and EBX) + K20 + ECX;
  mov EDI, EAX
  xor EDI, EBX
  and EDI, ESI
  xor EDI, EBX
  add EDI, K20
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[7];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*7]
//*    Data[7] := roldword(Data[7] xor Data[9] xor Data[15] xor Data[4], 1);
  mov EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*4]
  rol EDI, 1
  mov [esp+96-96+4*7+4*7], EDI

//i=8
//*    T := (EDX and ESI) or (not EDX and EAX) + K20 + EBX;
  mov EDI, ESI
  xor EDI, EAX
  and EDI, EDX
  xor EDI, EAX
  add EDI, K20
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[8];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*8]
//*    Data[8] := roldword(Data[8] xor Data[10] xor Data[0] xor Data[5], 1);
  mov EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*5]
  rol EDI, 1
  mov [esp+96-96+4*7+4*8], EDI

//i=9
//*    T := (ECX and EDX) or (not ECX and ESI) + K20 + EAX;
  mov EDI, EDX
  xor EDI, ESI
  and EDI, ECX
  xor EDI, ESI
  add EDI, K20
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[9];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*9]
//*    Data[9] := roldword(Data[9] xor Data[11] xor Data[1] xor Data[6], 1);
  mov EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*6]
  rol EDI, 1
  mov [esp+96-96+4*7+4*9], EDI

//i=10
//*    T := (EBX and ECX) or (not EBX and EDX) + K20 + ESI;
  mov EDI, ECX
  xor EDI, EDX
  and EDI, EBX
  xor EDI, EDX
  add EDI, K20
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[10];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*10]
//*    Data[10] := roldword(Data[10] xor Data[12] xor Data[2] xor Data[7], 1);
  mov EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*7]
  rol EDI, 1
  mov [esp+96-96+4*7+4*10], EDI

//i=11
//*    T := (EAX and EBX) or (not EAX and ECX) + K20 + EDX;
  mov EDI, EBX
  xor EDI, ECX
  and EDI, EAX
  xor EDI, ECX
  add EDI, K20
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[11];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*11]
//*    Data[11] := roldword(Data[11] xor Data[13] xor Data[3] xor Data[8], 1);
  mov EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*8]
  rol EDI, 1
  mov [esp+96-96+4*7+4*11], EDI

//i=12
//*    T := (ESI and EAX) or (not ESI and EBX) + K20 + ECX;
  mov EDI, EAX
  xor EDI, EBX
  and EDI, ESI
  xor EDI, EBX
  add EDI, K20
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[12];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*12]
//*    Data[12] := roldword(Data[12] xor Data[14] xor Data[4] xor Data[9], 1);
  mov EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*9]
  rol EDI, 1
  mov [esp+96-96+4*7+4*12], EDI

//i=13
//*    T := (EDX and ESI) or (not EDX and EAX) + K20 + EBX;
  mov EDI, ESI
  xor EDI, EAX
  and EDI, EDX
  xor EDI, EAX
  add EDI, K20
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[13];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*13]
//*    Data[13] := roldword(Data[13] xor Data[15] xor Data[5] xor Data[10], 1);
  mov EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*10]
  rol EDI, 1
  mov [esp+96-96+4*7+4*13], EDI

//i=14
//*    T := (ECX and EDX) or (not ECX and ESI) + K20 + EAX;
  mov EDI, EDX
  xor EDI, ESI
  and EDI, ECX
  xor EDI, ESI
  add EDI, K20
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[14];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*14]
//*    Data[14] := roldword(Data[14] xor Data[0] xor Data[6] xor Data[11], 1);
  mov EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*11]
  rol EDI, 1
  mov [esp+96-96+4*7+4*14], EDI

//i=15
//*    T := (EBX and ECX) or (not EBX and EDX) + K20 + ESI;
  mov EDI, ECX
  xor EDI, EDX
  and EDI, EBX
  xor EDI, EDX
  add EDI, K20
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[15];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*15]
//*    Data[15] := roldword(Data[15] xor Data[1] xor Data[7] xor Data[12], 1);
  mov EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*12]
  rol EDI, 1
  mov [esp+96-96+4*7+4*15], EDI

//i=16
//*    T := (EAX and EBX) or (not EAX and ECX) + K20 + EDX;
  mov EDI, EBX
  xor EDI, ECX
  and EDI, EAX
  xor EDI, ECX
  add EDI, K20
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[0];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*0]
//*    Data[0] := roldword(Data[0] xor Data[2] xor Data[8] xor Data[13], 1);
  mov EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*13]
  rol EDI, 1
  mov [esp+96-96+4*7+4*0], EDI

//i=17
//*    T := (ESI and EAX) or (not ESI and EBX) + K20 + ECX;
  mov EDI, EAX
  xor EDI, EBX
  and EDI, ESI
  xor EDI, EBX
  add EDI, K20
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[1];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*1]
//*    Data[1] := roldword(Data[1] xor Data[3] xor Data[9] xor Data[14], 1);
  mov EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*14]
  rol EDI, 1
  mov [esp+96-96+4*7+4*1], EDI

//i=18
//*    T := (EDX and ESI) or (not EDX and EAX) + K20 + EBX;
  mov EDI, ESI
  xor EDI, EAX
  and EDI, EDX
  xor EDI, EAX
  add EDI, K20
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[2];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*2]
//*    Data[2] := roldword(Data[2] xor Data[4] xor Data[10] xor Data[15], 1);
  mov EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*15]
  rol EDI, 1
  mov [esp+96-96+4*7+4*2], EDI

//i=19
//*    T := (ECX and EDX) or (not ECX and ESI) + K20 + EAX;
  mov EDI, EDX
  xor EDI, ESI
  and EDI, ECX
  xor EDI, ESI
  add EDI, K20
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[3];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*3]
//*    Data[3] := roldword(Data[3] xor Data[5] xor Data[11] xor Data[0], 1);
  mov EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*0]
  rol EDI, 1
  mov [esp+96-96+4*7+4*3], EDI

//i=20
//*    T := (EBX xor ECX xor EDX) + K40 + ESI;
  mov EDI, EBX
  xor EDI, ECX
  xor EDI, EDX
  add EDI, K40
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[4];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*4]
//*    Data[4] := roldword(Data[4] xor Data[6] xor Data[12] xor Data[1], 1);
  mov EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*1]
  rol EDI, 1
  mov [esp+96-96+4*7+4*4], EDI

//i=21
//*    T := (EAX xor EBX xor ECX) + K40 + EDX;
  mov EDI, EAX
  xor EDI, EBX
  xor EDI, ECX
  add EDI, K40
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[5];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*5]
//*    Data[5] := roldword(Data[5] xor Data[7] xor Data[13] xor Data[2], 1);
  mov EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*2]
  rol EDI, 1
  mov [esp+96-96+4*7+4*5], EDI

//i=22
//*    T := (ESI xor EAX xor EBX) + K40 + ECX;
  mov EDI, ESI
  xor EDI, EAX
  xor EDI, EBX
  add EDI, K40
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[6];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*6]
//*    Data[6] := roldword(Data[6] xor Data[8] xor Data[14] xor Data[3], 1);
  mov EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*3]
  rol EDI, 1
  mov [esp+96-96+4*7+4*6], EDI

//i=23
//*    T := (EDX xor ESI xor EAX) + K40 + EBX;
  mov EDI, EDX
  xor EDI, ESI
  xor EDI, EAX
  add EDI, K40
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[7];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*7]
//*    Data[7] := roldword(Data[7] xor Data[9] xor Data[15] xor Data[4], 1);
  mov EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*4]
  rol EDI, 1
  mov [esp+96-96+4*7+4*7], EDI

//i=24
//*    T := (ECX xor EDX xor ESI) + K40 + EAX;
  mov EDI, ECX
  xor EDI, EDX
  xor EDI, ESI
  add EDI, K40
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[8];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*8]
//*    Data[8] := roldword(Data[8] xor Data[10] xor Data[0] xor Data[5], 1);
  mov EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*5]
  rol EDI, 1
  mov [esp+96-96+4*7+4*8], EDI

//i=25
//*    T := (EBX xor ECX xor EDX) + K40 + ESI;
  mov EDI, EBX
  xor EDI, ECX
  xor EDI, EDX
  add EDI, K40
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[9];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*9]
//*    Data[9] := roldword(Data[9] xor Data[11] xor Data[1] xor Data[6], 1);
  mov EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*6]
  rol EDI, 1
  mov [esp+96-96+4*7+4*9], EDI

//i=26
//*    T := (EAX xor EBX xor ECX) + K40 + EDX;
  mov EDI, EAX
  xor EDI, EBX
  xor EDI, ECX
  add EDI, K40
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[10];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*10]
//*    Data[10] := roldword(Data[10] xor Data[12] xor Data[2] xor Data[7], 1);
  mov EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*7]
  rol EDI, 1
  mov [esp+96-96+4*7+4*10], EDI

//i=27
//*    T := (ESI xor EAX xor EBX) + K40 + ECX;
  mov EDI, ESI
  xor EDI, EAX
  xor EDI, EBX
  add EDI, K40
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[11];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*11]
//*    Data[11] := roldword(Data[11] xor Data[13] xor Data[3] xor Data[8], 1);
  mov EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*8]
  rol EDI, 1
  mov [esp+96-96+4*7+4*11], EDI

//i=28
//*    T := (EDX xor ESI xor EAX) + K40 + EBX;
  mov EDI, EDX
  xor EDI, ESI
  xor EDI, EAX
  add EDI, K40
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[12];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*12]
//*    Data[12] := roldword(Data[12] xor Data[14] xor Data[4] xor Data[9], 1);
  mov EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*9]
  rol EDI, 1
  mov [esp+96-96+4*7+4*12], EDI

//i=29
//*    T := (ECX xor EDX xor ESI) + K40 + EAX;
  mov EDI, ECX
  xor EDI, EDX
  xor EDI, ESI
  add EDI, K40
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[13];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*13]
//*    Data[13] := roldword(Data[13] xor Data[15] xor Data[5] xor Data[10], 1);
  mov EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*10]
  rol EDI, 1
  mov [esp+96-96+4*7+4*13], EDI

//i=30
//*    T := (EBX xor ECX xor EDX) + K40 + ESI;
  mov EDI, EBX
  xor EDI, ECX
  xor EDI, EDX
  add EDI, K40
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[14];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*14]
//*    Data[14] := roldword(Data[14] xor Data[0] xor Data[6] xor Data[11], 1);
  mov EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*11]
  rol EDI, 1
  mov [esp+96-96+4*7+4*14], EDI

//i=31
//*    T := (EAX xor EBX xor ECX) + K40 + EDX;
  mov EDI, EAX
  xor EDI, EBX
  xor EDI, ECX
  add EDI, K40
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[15];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*15]
//*    Data[15] := roldword(Data[15] xor Data[1] xor Data[7] xor Data[12], 1);
  mov EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*12]
  rol EDI, 1
  mov [esp+96-96+4*7+4*15], EDI

//i=32
//*    T := (ESI xor EAX xor EBX) + K40 + ECX;
  mov EDI, ESI
  xor EDI, EAX
  xor EDI, EBX
  add EDI, K40
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[0];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*0]
//*    Data[0] := roldword(Data[0] xor Data[2] xor Data[8] xor Data[13], 1);
  mov EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*13]
  rol EDI, 1
  mov [esp+96-96+4*7+4*0], EDI

//i=33
//*    T := (EDX xor ESI xor EAX) + K40 + EBX;
  mov EDI, EDX
  xor EDI, ESI
  xor EDI, EAX
  add EDI, K40
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[1];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*1]
//*    Data[1] := roldword(Data[1] xor Data[3] xor Data[9] xor Data[14], 1);
  mov EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*14]
  rol EDI, 1
  mov [esp+96-96+4*7+4*1], EDI

//i=34
//*    T := (ECX xor EDX xor ESI) + K40 + EAX;
  mov EDI, ECX
  xor EDI, EDX
  xor EDI, ESI
  add EDI, K40
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[2];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*2]
//*    Data[2] := roldword(Data[2] xor Data[4] xor Data[10] xor Data[15], 1);
  mov EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*15]
  rol EDI, 1
  mov [esp+96-96+4*7+4*2], EDI

//i=35
//*    T := (EBX xor ECX xor EDX) + K40 + ESI;
  mov EDI, EBX
  xor EDI, ECX
  xor EDI, EDX
  add EDI, K40
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[3];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*3]
//*    Data[3] := roldword(Data[3] xor Data[5] xor Data[11] xor Data[0], 1);
  mov EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*0]
  rol EDI, 1
  mov [esp+96-96+4*7+4*3], EDI

//i=36
//*    T := (EAX xor EBX xor ECX) + K40 + EDX;
  mov EDI, EAX
  xor EDI, EBX
  xor EDI, ECX
  add EDI, K40
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[4];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*4]
//*    Data[4] := roldword(Data[4] xor Data[6] xor Data[12] xor Data[1], 1);
  mov EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*1]
  rol EDI, 1
  mov [esp+96-96+4*7+4*4], EDI

//i=37
//*    T := (ESI xor EAX xor EBX) + K40 + ECX;
  mov EDI, ESI
  xor EDI, EAX
  xor EDI, EBX
  add EDI, K40
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[5];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*5]
//*    Data[5] := roldword(Data[5] xor Data[7] xor Data[13] xor Data[2], 1);
  mov EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*2]
  rol EDI, 1
  mov [esp+96-96+4*7+4*5], EDI

//i=38
//*    T := (EDX xor ESI xor EAX) + K40 + EBX;
  mov EDI, EDX
  xor EDI, ESI
  xor EDI, EAX
  add EDI, K40
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[6];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*6]
//*    Data[6] := roldword(Data[6] xor Data[8] xor Data[14] xor Data[3], 1);
  mov EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*3]
  rol EDI, 1
  mov [esp+96-96+4*7+4*6], EDI

//i=39
//*    T := (ECX xor EDX xor ESI) + K40 + EAX;
  mov EDI, ECX
  xor EDI, EDX
  xor EDI, ESI
  add EDI, K40
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[7];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*7]
//*    Data[7] := roldword(Data[7] xor Data[9] xor Data[15] xor Data[4], 1);
  mov EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*4]
  rol EDI, 1
  mov [esp+96-96+4*7+4*7], EDI

//i=40
//*    T := (EBX and ECX) or (EBX and EDX) or (ECX and EDX) + K60 + ESI;
  mov EDI, ECX
  mov EBP, ECX
  or EDI, EDX
  and EBP, EDX
  and EDI, EBX
  or EDI, EBP
  add EDI, K60
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[8];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*8]
//*    Data[8] := roldword(Data[8] xor Data[10] xor Data[0] xor Data[5], 1);
  mov EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*5]
  rol EDI, 1
  mov [esp+96-96+4*7+4*8], EDI

//i=41
//*    T := (EAX and EBX) or (EAX and ECX) or (EBX and ECX) + K60 + EDX;
  mov EDI, EBX
  mov EBP, EBX
  or EDI, ECX
  and EBP, ECX
  and EDI, EAX
  or EDI, EBP
  add EDI, K60
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[9];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*9]
//*    Data[9] := roldword(Data[9] xor Data[11] xor Data[1] xor Data[6], 1);
  mov EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*6]
  rol EDI, 1
  mov [esp+96-96+4*7+4*9], EDI

//i=42
//*    T := (ESI and EAX) or (ESI and EBX) or (EAX and EBX) + K60 + ECX;
  mov EDI, EAX
  mov EBP, EAX
  or EDI, EBX
  and EBP, EBX
  and EDI, ESI
  or EDI, EBP
  add EDI, K60
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[10];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*10]
//*    Data[10] := roldword(Data[10] xor Data[12] xor Data[2] xor Data[7], 1);
  mov EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*7]
  rol EDI, 1
  mov [esp+96-96+4*7+4*10], EDI

//i=43
//*    T := (EDX and ESI) or (EDX and EAX) or (ESI and EAX) + K60 + EBX;
  mov EDI, ESI
  mov EBP, ESI
  or EDI, EAX
  and EBP, EAX
  and EDI, EDX
  or EDI, EBP
  add EDI, K60
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[11];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*11]
//*    Data[11] := roldword(Data[11] xor Data[13] xor Data[3] xor Data[8], 1);
  mov EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*8]
  rol EDI, 1
  mov [esp+96-96+4*7+4*11], EDI

//i=44
//*    T := (ECX and EDX) or (ECX and ESI) or (EDX and ESI) + K60 + EAX;
  mov EDI, EDX
  mov EBP, EDX
  or EDI, ESI
  and EBP, ESI
  and EDI, ECX
  or EDI, EBP
  add EDI, K60
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[12];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*12]
//*    Data[12] := roldword(Data[12] xor Data[14] xor Data[4] xor Data[9], 1);
  mov EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*9]
  rol EDI, 1
  mov [esp+96-96+4*7+4*12], EDI

//i=45
//*    T := (EBX and ECX) or (EBX and EDX) or (ECX and EDX) + K60 + ESI;
  mov EDI, ECX
  mov EBP, ECX
  or EDI, EDX
  and EBP, EDX
  and EDI, EBX
  or EDI, EBP
  add EDI, K60
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[13];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*13]
//*    Data[13] := roldword(Data[13] xor Data[15] xor Data[5] xor Data[10], 1);
  mov EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*10]
  rol EDI, 1
  mov [esp+96-96+4*7+4*13], EDI

//i=46
//*    T := (EAX and EBX) or (EAX and ECX) or (EBX and ECX) + K60 + EDX;
  mov EDI, EBX
  mov EBP, EBX
  or EDI, ECX
  and EBP, ECX
  and EDI, EAX
  or EDI, EBP
  add EDI, K60
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[14];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*14]
//*    Data[14] := roldword(Data[14] xor Data[0] xor Data[6] xor Data[11], 1);
  mov EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*11]
  rol EDI, 1
  mov [esp+96-96+4*7+4*14], EDI

//i=47
//*    T := (ESI and EAX) or (ESI and EBX) or (EAX and EBX) + K60 + ECX;
  mov EDI, EAX
  mov EBP, EAX
  or EDI, EBX
  and EBP, EBX
  and EDI, ESI
  or EDI, EBP
  add EDI, K60
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[15];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*15]
//*    Data[15] := roldword(Data[15] xor Data[1] xor Data[7] xor Data[12], 1);
  mov EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*12]
  rol EDI, 1
  mov [esp+96-96+4*7+4*15], EDI

//i=48
//*    T := (EDX and ESI) or (EDX and EAX) or (ESI and EAX) + K60 + EBX;
  mov EDI, ESI
  mov EBP, ESI
  or EDI, EAX
  and EBP, EAX
  and EDI, EDX
  or EDI, EBP
  add EDI, K60
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[0];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*0]
//*    Data[0] := roldword(Data[0] xor Data[2] xor Data[8] xor Data[13], 1);
  mov EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*13]
  rol EDI, 1
  mov [esp+96-96+4*7+4*0], EDI

//i=49
//*    T := (ECX and EDX) or (ECX and ESI) or (EDX and ESI) + K60 + EAX;
  mov EDI, EDX
  mov EBP, EDX
  or EDI, ESI
  and EBP, ESI
  and EDI, ECX
  or EDI, EBP
  add EDI, K60
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[1];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*1]
//*    Data[1] := roldword(Data[1] xor Data[3] xor Data[9] xor Data[14], 1);
  mov EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*14]
  rol EDI, 1
  mov [esp+96-96+4*7+4*1], EDI

//i=50
//*    T := (EBX and ECX) or (EBX and EDX) or (ECX and EDX) + K60 + ESI;
  mov EDI, ECX
  mov EBP, ECX
  or EDI, EDX
  and EBP, EDX
  and EDI, EBX
  or EDI, EBP
  add EDI, K60
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[2];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*2]
//*    Data[2] := roldword(Data[2] xor Data[4] xor Data[10] xor Data[15], 1);
  mov EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*15]
  rol EDI, 1
  mov [esp+96-96+4*7+4*2], EDI

//i=51
//*    T := (EAX and EBX) or (EAX and ECX) or (EBX and ECX) + K60 + EDX;
  mov EDI, EBX
  mov EBP, EBX
  or EDI, ECX
  and EBP, ECX
  and EDI, EAX
  or EDI, EBP
  add EDI, K60
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[3];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*3]
//*    Data[3] := roldword(Data[3] xor Data[5] xor Data[11] xor Data[0], 1);
  mov EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*0]
  rol EDI, 1
  mov [esp+96-96+4*7+4*3], EDI

//i=52
//*    T := (ESI and EAX) or (ESI and EBX) or (EAX and EBX) + K60 + ECX;
  mov EDI, EAX
  mov EBP, EAX
  or EDI, EBX
  and EBP, EBX
  and EDI, ESI
  or EDI, EBP
  add EDI, K60
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[4];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*4]
//*    Data[4] := roldword(Data[4] xor Data[6] xor Data[12] xor Data[1], 1);
  mov EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*1]
  rol EDI, 1
  mov [esp+96-96+4*7+4*4], EDI

//i=53
//*    T := (EDX and ESI) or (EDX and EAX) or (ESI and EAX) + K60 + EBX;
  mov EDI, ESI
  mov EBP, ESI
  or EDI, EAX
  and EBP, EAX
  and EDI, EDX
  or EDI, EBP
  add EDI, K60
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[5];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*5]
//*    Data[5] := roldword(Data[5] xor Data[7] xor Data[13] xor Data[2], 1);
  mov EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*2]
  rol EDI, 1
  mov [esp+96-96+4*7+4*5], EDI

//i=54
//*    T := (ECX and EDX) or (ECX and ESI) or (EDX and ESI) + K60 + EAX;
  mov EDI, EDX
  mov EBP, EDX
  or EDI, ESI
  and EBP, ESI
  and EDI, ECX
  or EDI, EBP
  add EDI, K60
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[6];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*6]
//*    Data[6] := roldword(Data[6] xor Data[8] xor Data[14] xor Data[3], 1);
  mov EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*3]
  rol EDI, 1
  mov [esp+96-96+4*7+4*6], EDI

//i=55
//*    T := (EBX and ECX) or (EBX and EDX) or (ECX and EDX) + K60 + ESI;
  mov EDI, ECX
  mov EBP, ECX
  or EDI, EDX
  and EBP, EDX
  and EDI, EBX
  or EDI, EBP
  add EDI, K60
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[7];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*7]
//*    Data[7] := roldword(Data[7] xor Data[9] xor Data[15] xor Data[4], 1);
  mov EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*4]
  rol EDI, 1
  mov [esp+96-96+4*7+4*7], EDI

//i=56
//*    T := (EAX and EBX) or (EAX and ECX) or (EBX and ECX) + K60 + EDX;
  mov EDI, EBX
  mov EBP, EBX
  or EDI, ECX
  and EBP, ECX
  and EDI, EAX
  or EDI, EBP
  add EDI, K60
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[8];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*8]
//*    Data[8] := roldword(Data[8] xor Data[10] xor Data[0] xor Data[5], 1);
  mov EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*5]
  rol EDI, 1
  mov [esp+96-96+4*7+4*8], EDI

//i=57
//*    T := (ESI and EAX) or (ESI and EBX) or (EAX and EBX) + K60 + ECX;
  mov EDI, EAX
  mov EBP, EAX
  or EDI, EBX
  and EBP, EBX
  and EDI, ESI
  or EDI, EBP
  add EDI, K60
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[9];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*9]
//*    Data[9] := roldword(Data[9] xor Data[11] xor Data[1] xor Data[6], 1);
  mov EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*6]
  rol EDI, 1
  mov [esp+96-96+4*7+4*9], EDI

//i=58
//*    T := (EDX and ESI) or (EDX and EAX) or (ESI and EAX) + K60 + EBX;
  mov EDI, ESI
  mov EBP, ESI
  or EDI, EAX
  and EBP, EAX
  and EDI, EDX
  or EDI, EBP
  add EDI, K60
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[10];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*10]
//*    Data[10] := roldword(Data[10] xor Data[12] xor Data[2] xor Data[7], 1);
  mov EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*7]
  rol EDI, 1
  mov [esp+96-96+4*7+4*10], EDI

//i=59
//*    T := (ECX and EDX) or (ECX and ESI) or (EDX and ESI) + K60 + EAX;
  mov EDI, EDX
  mov EBP, EDX
  or EDI, ESI
  and EBP, ESI
  and EDI, ECX
  or EDI, EBP
  add EDI, K60
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[11];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*11]
//*    Data[11] := roldword(Data[11] xor Data[13] xor Data[3] xor Data[8], 1);
  mov EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*8]
  rol EDI, 1
  mov [esp+96-96+4*7+4*11], EDI

//i=60
//*    T := (EBX xor ECX xor EDX) + K80 + ESI;
  mov EDI, EBX
  xor EDI, ECX
  xor EDI, EDX
  add EDI, K80
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[12];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*12]
//*    Data[12] := roldword(Data[12] xor Data[14] xor Data[4] xor Data[9], 1);
  mov EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*9]
  rol EDI, 1
  mov [esp+96-96+4*7+4*12], EDI

//i=61
//*    T := (EAX xor EBX xor ECX) + K80 + EDX;
  mov EDI, EAX
  xor EDI, EBX
  xor EDI, ECX
  add EDI, K80
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[13];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*13]
//*    Data[13] := roldword(Data[13] xor Data[15] xor Data[5] xor Data[10], 1);
  mov EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*10]
  rol EDI, 1
  mov [esp+96-96+4*7+4*13], EDI

//i=62
//*    T := (ESI xor EAX xor EBX) + K80 + ECX;
  mov EDI, ESI
  xor EDI, EAX
  xor EDI, EBX
  add EDI, K80
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[14];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*14]
//*    Data[14] := roldword(Data[14] xor Data[0] xor Data[6] xor Data[11], 1);
  mov EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*11]
  rol EDI, 1
  mov [esp+96-96+4*7+4*14], EDI

//i=63
//*    T := (EDX xor ESI xor EAX) + K80 + EBX;
  mov EDI, EDX
  xor EDI, ESI
  xor EDI, EAX
  add EDI, K80
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[15];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*15]
//*    Data[15] := roldword(Data[15] xor Data[1] xor Data[7] xor Data[12], 1);
  mov EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*12]
  rol EDI, 1
  mov [esp+96-96+4*7+4*15], EDI

//i=64
//*    T := (ECX xor EDX xor ESI) + K80 + EAX;
  mov EDI, ECX
  xor EDI, EDX
  xor EDI, ESI
  add EDI, K80
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[0];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*0]
//*    Data[0] := roldword(Data[0] xor Data[2] xor Data[8] xor Data[13], 1);
  mov EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*13]
  rol EDI, 1
  mov [esp+96-96+4*7+4*0], EDI

//i=65
//*    T := (EBX xor ECX xor EDX) + K80 + ESI;
  mov EDI, EBX
  xor EDI, ECX
  xor EDI, EDX
  add EDI, K80
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[1];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*1]
//*    Data[1] := roldword(Data[1] xor Data[3] xor Data[9] xor Data[14], 1);
  mov EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*14]
  rol EDI, 1
  mov [esp+96-96+4*7+4*1], EDI

//i=66
//*    T := (EAX xor EBX xor ECX) + K80 + EDX;
  mov EDI, EAX
  xor EDI, EBX
  xor EDI, ECX
  add EDI, K80
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[2];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*2]
//*    Data[2] := roldword(Data[2] xor Data[4] xor Data[10] xor Data[15], 1);
  mov EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*15]
  rol EDI, 1
  mov [esp+96-96+4*7+4*2], EDI

//i=67
//*    T := (ESI xor EAX xor EBX) + K80 + ECX;
  mov EDI, ESI
  xor EDI, EAX
  xor EDI, EBX
  add EDI, K80
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[3];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*3]
//*    Data[3] := roldword(Data[3] xor Data[5] xor Data[11] xor Data[0], 1);
  mov EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*0]
  rol EDI, 1
  mov [esp+96-96+4*7+4*3], EDI

//i=68
//*    T := (EDX xor ESI xor EAX) + K80 + EBX;
  mov EDI, EDX
  xor EDI, ESI
  xor EDI, EAX
  add EDI, K80
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[4];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*4]
//*    Data[4] := roldword(Data[4] xor Data[6] xor Data[12] xor Data[1], 1);
  mov EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*1]
  rol EDI, 1
  mov [esp+96-96+4*7+4*4], EDI

//i=69
//*    T := (ECX xor EDX xor ESI) + K80 + EAX;
  mov EDI, ECX
  xor EDI, EDX
  xor EDI, ESI
  add EDI, K80
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[5];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*5]
//*    Data[5] := roldword(Data[5] xor Data[7] xor Data[13] xor Data[2], 1);
  mov EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*2]
  rol EDI, 1
  mov [esp+96-96+4*7+4*5], EDI

//i=70
//*    T := (EBX xor ECX xor EDX) + K80 + ESI;
  mov EDI, EBX
  xor EDI, ECX
  xor EDI, EDX
  add EDI, K80
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[6];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*6]
//*    Data[6] := roldword(Data[6] xor Data[8] xor Data[14] xor Data[3], 1);
  mov EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*3]
  rol EDI, 1
  mov [esp+96-96+4*7+4*6], EDI

//i=71
//*    T := (EAX xor EBX xor ECX) + K80 + EDX;
  mov EDI, EAX
  xor EDI, EBX
  xor EDI, ECX
  add EDI, K80
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[7];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*7]
//*    Data[7] := roldword(Data[7] xor Data[9] xor Data[15] xor Data[4], 1);
  mov EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*4]
  rol EDI, 1
  mov [esp+96-96+4*7+4*7], EDI

//i=72
//*    T := (ESI xor EAX xor EBX) + K80 + ECX;
  mov EDI, ESI
  xor EDI, EAX
  xor EDI, EBX
  add EDI, K80
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[8];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*8]
//*    Data[8] := roldword(Data[8] xor Data[10] xor Data[0] xor Data[5], 1);
  mov EDI, [esp+96-96+4*7+4*8]
  xor EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*5]
  rol EDI, 1
  mov [esp+96-96+4*7+4*8], EDI

//i=73
//*    T := (EDX xor ESI xor EAX) + K80 + EBX;
  mov EDI, EDX
  xor EDI, ESI
  xor EDI, EAX
  add EDI, K80
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[9];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*9]
//*    Data[9] := roldword(Data[9] xor Data[11] xor Data[1] xor Data[6], 1);
  mov EDI, [esp+96-96+4*7+4*9]
  xor EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*6]
  rol EDI, 1
  mov [esp+96-96+4*7+4*9], EDI

//i=74
//*    T := (ECX xor EDX xor ESI) + K80 + EAX;
  mov EDI, ECX
  xor EDI, EDX
  xor EDI, ESI
  add EDI, K80
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[10];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*10]
//*    Data[10] := roldword(Data[10] xor Data[12] xor Data[2] xor Data[7], 1);
  mov EDI, [esp+96-96+4*7+4*10]
  xor EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*2]
  xor EDI, [esp+96-96+4*7+4*7]
  rol EDI, 1
  mov [esp+96-96+4*7+4*10], EDI

//i=75
//*    T := (EBX xor ECX xor EDX) + K80 + ESI;
  mov EDI, EBX
  xor EDI, ECX
  xor EDI, EDX
  add EDI, K80
  add EDI, ESI
//*    EBX := rordword(EBX, 2);
  ror EBX, 2
//*    ESI := T + roldword(EAX, 5) + Data[11];
  mov ESI, EAX
  rol ESI, 5
  add ESI, EDI
  add ESI, [esp+96-96+4*7+4*11]
//*    Data[11] := roldword(Data[11] xor Data[13] xor Data[3] xor Data[8], 1);
  mov EDI, [esp+96-96+4*7+4*11]
  xor EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*3]
  xor EDI, [esp+96-96+4*7+4*8]
  rol EDI, 1
  mov [esp+96-96+4*7+4*11], EDI

//i=76
//*    T := (EAX xor EBX xor ECX) + K80 + EDX;
  mov EDI, EAX
  xor EDI, EBX
  xor EDI, ECX
  add EDI, K80
  add EDI, EDX
//*    EAX := rordword(EAX, 2);
  ror EAX, 2
//*    EDX := T + roldword(ESI, 5) + Data[12];
  mov EDX, ESI
  rol EDX, 5
  add EDX, EDI
  add EDX, [esp+96-96+4*7+4*12]
//*    Data[12] := roldword(Data[12] xor Data[14] xor Data[4] xor Data[9], 1);
  mov EDI, [esp+96-96+4*7+4*12]
  xor EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*4]
  xor EDI, [esp+96-96+4*7+4*9]
  rol EDI, 1
  mov [esp+96-96+4*7+4*12], EDI

//i=77
//*    T := (ESI xor EAX xor EBX) + K80 + ECX;
  mov EDI, ESI
  xor EDI, EAX
  xor EDI, EBX
  add EDI, K80
  add EDI, ECX
//*    ESI := rordword(ESI, 2);
  ror ESI, 2
//*    ECX := T + roldword(EDX, 5) + Data[13];
  mov ECX, EDX
  rol ECX, 5
  add ECX, EDI
  add ECX, [esp+96-96+4*7+4*13]
//*    Data[13] := roldword(Data[13] xor Data[15] xor Data[5] xor Data[10], 1);
  mov EDI, [esp+96-96+4*7+4*13]
  xor EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*5]
  xor EDI, [esp+96-96+4*7+4*10]
  rol EDI, 1
  mov [esp+96-96+4*7+4*13], EDI

//i=78
//*    T := (EDX xor ESI xor EAX) + K80 + EBX;
  mov EDI, EDX
  xor EDI, ESI
  xor EDI, EAX
  add EDI, K80
  add EDI, EBX
//*    EDX := rordword(EDX, 2);
  ror EDX, 2
//*    EBX := T + roldword(ECX, 5) + Data[14];
  mov EBX, ECX
  rol EBX, 5
  add EBX, EDI
  add EBX, [esp+96-96+4*7+4*14]
//*    Data[14] := roldword(Data[14] xor Data[0] xor Data[6] xor Data[11], 1);
  mov EDI, [esp+96-96+4*7+4*14]
  xor EDI, [esp+96-96+4*7+4*0]
  xor EDI, [esp+96-96+4*7+4*6]
  xor EDI, [esp+96-96+4*7+4*11]
  rol EDI, 1
  mov [esp+96-96+4*7+4*14], EDI

//i=79
//*    T := (ECX xor EDX xor ESI) + K80 + EAX;
  mov EDI, ECX
  xor EDI, EDX
  xor EDI, ESI
  add EDI, K80
  add EDI, EAX
//*    ECX := rordword(ECX, 2);
  ror ECX, 2
//*    EAX := T + roldword(EBX, 5) + Data[15];
  mov EAX, EBX
  rol EAX, 5
  add EAX, EDI
  add EAX, [esp+96-96+4*7+4*15]
//*    Data[15] := roldword(Data[15] xor Data[1] xor Data[7] xor Data[12], 1);
  mov EDI, [esp+96-96+4*7+4*15]
  xor EDI, [esp+96-96+4*7+4*1]
  xor EDI, [esp+96-96+4*7+4*7]
  xor EDI, [esp+96-96+4*7+4*12]
  rol EDI, 1
  mov [esp+96-96+4*7+4*15], EDI

  pop EBP

  mov EDI, [pctx{.State}]
//  Inc(ctx.State[0], A);
  add [EDI+4*0], EAX
//  Inc(ctx.State[1], B);
  add [EDI+4*1], EBX
//  Inc(ctx.State[2], C);
  add [EDI+4*2], ECX
//  Inc(ctx.State[3], D);
  add [EDI+4*3], EDX
//  Inc(ctx.State[4], E);
  add [EDI+4*4], ESI

//Inc(ctx.Length,64);
  mov eax, [ebp-4]
  add	dword ptr [eax+88],64
  adc	dword ptr [eax+92],0

  pop EDI
  pop ESI
  pop EDX
  pop ECX
  pop EBX
  pop EAX
end;

