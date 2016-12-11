// i386 assembler optimized version
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer);assembler;
var
  pContext: ^TMDContext;
  pBuffer: Pointer;
  a, b, c, d: Cardinal;
  //Block: array[0..15] of Cardinal;
{$asmmode intel}
asm
    push EAX
    push EBX
    push ECX
    push EDX
    push ESI
    push EDI
    push EBP

    mov pContext, eax
    mov pBuffer, edx

    mov ESI, pContext
    mov ebp, edx

// A := Context.State[0];
    mov EAX, [ESI+12+4*0]
// B := Context.State[1];
    mov EBX, [ESI+12+4*1]
// C := Context.State[2];
    mov ECX, [ESI+12+4*2]
// D := Context.State[3];
    mov EDX, [ESI+12+4*3]
// Round 1
//EAX := EBX + roldword(dword(EAX + ((EBX and ECX) or ((not EBX) and EDX)) + Data[0] + $d76aa478),  7);
    mov ESI, ECX
    add EAX, $d76aa478
    xor ESI, EDX
    add EAX, [ebp + 4*0]
    and ESI, EBX
    xor ESI, EDX
    add EAX, ESI
    rol EAX,  7
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + ((EAX and EBX) or ((not EAX) and ECX)) + Data[1] + $e8c7b756), 12);
    mov ESI, EBX
    add EDX, $e8c7b756
    xor ESI, ECX
    add EDX, [ebp + 4*1]
    and ESI, EAX
    xor ESI, ECX
    add EDX, ESI
    rol EDX, 12
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + ((EDX and EAX) or ((not EDX) and EBX)) + Data[2] + $242070db), 17);
    mov ESI, EAX
    add ECX, $242070db
    xor ESI, EBX
    add ECX, [ebp + 4*2]
    and ESI, EDX
    xor ESI, EBX
    add ECX, ESI
    rol ECX, 17
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + ((ECX and EDX) or ((not ECX) and EAX)) + Data[3] + $c1bdceee), 22);
    mov ESI, EDX
    add EBX, $c1bdceee
    xor ESI, EAX
    add EBX, [ebp + 4*3]
    and ESI, ECX
    xor ESI, EAX
    add EBX, ESI
    rol EBX, 22
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + ((EBX and ECX) or ((not EBX) and EDX)) + Data[4] + $f57c0faf),  7);
    mov ESI, ECX
    add EAX, $f57c0faf
    xor ESI, EDX
    add EAX, [ebp + 4*4]
    and ESI, EBX
    xor ESI, EDX
    add EAX, ESI
    rol EAX,  7
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + ((EAX and EBX) or ((not EAX) and ECX)) + Data[5] + $4787c62a), 12);
    mov ESI, EBX
    add EDX, $4787c62a
    xor ESI, ECX
    add EDX, [ebp + 4*5]
    and ESI, EAX
    xor ESI, ECX
    add EDX, ESI
    rol EDX, 12
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + ((EDX and EAX) or ((not EDX) and EBX)) + Data[6] + $a8304613), 17);
    mov ESI, EAX
    add ECX, $a8304613
    xor ESI, EBX
    add ECX, [ebp + 4*6]
    and ESI, EDX
    xor ESI, EBX
    add ECX, ESI
    rol ECX, 17
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + ((ECX and EDX) or ((not ECX) and EAX)) + Data[7] + $fd469501), 22);
    mov ESI, EDX
    add EBX, $fd469501
    xor ESI, EAX
    add EBX, [ebp + 4*7]
    and ESI, ECX
    xor ESI, EAX
    add EBX, ESI
    rol EBX, 22
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + ((EBX and ECX) or ((not EBX) and EDX)) + Data[8] + $698098d8),  7);
    mov ESI, ECX
    add EAX, $698098d8
    xor ESI, EDX
    add EAX, [ebp + 4*8]
    and ESI, EBX
    xor ESI, EDX
    add EAX, ESI
    rol EAX,  7
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + ((EAX and EBX) or ((not EAX) and ECX)) + Data[9] + $8b44f7af), 12);
    mov ESI, EBX
    add EDX, $8b44f7af
    xor ESI, ECX
    add EDX, [ebp + 4*9]
    and ESI, EAX
    xor ESI, ECX
    add EDX, ESI
    rol EDX, 12
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + ((EDX and EAX) or ((not EDX) and EBX)) + Data[10] + $ffff5bb1), 17);
    mov ESI, EAX
    add ECX, $ffff5bb1
    xor ESI, EBX
    add ECX, [ebp + 4*10]
    and ESI, EDX
    xor ESI, EBX
    add ECX, ESI
    rol ECX, 17
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + ((ECX and EDX) or ((not ECX) and EAX)) + Data[11] + $895cd7be), 22);
    mov ESI, EDX
    add EBX, $895cd7be
    xor ESI, EAX
    add EBX, [ebp + 4*11]
    and ESI, ECX
    xor ESI, EAX
    add EBX, ESI
    rol EBX, 22
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + ((EBX and ECX) or ((not EBX) and EDX)) + Data[12] + $6b901122),  7);
    mov ESI, ECX
    add EAX, $6b901122
    xor ESI, EDX
    add EAX, [ebp + 4*12]
    and ESI, EBX
    xor ESI, EDX
    add EAX, ESI
    rol EAX,  7
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + ((EAX and EBX) or ((not EAX) and ECX)) + Data[13] + $fd987193), 12);
    mov ESI, EBX
    add EDX, $fd987193
    xor ESI, ECX
    add EDX, [ebp + 4*13]
    and ESI, EAX
    xor ESI, ECX
    add EDX, ESI
    rol EDX, 12
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + ((EDX and EAX) or ((not EDX) and EBX)) + Data[14] + $a679438e), 17);
    mov ESI, EAX
    add ECX, $a679438e
    xor ESI, EBX
    add ECX, [ebp + 4*14]
    and ESI, EDX
    xor ESI, EBX
    add ECX, ESI
    rol ECX, 17
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + ((ECX and EDX) or ((not ECX) and EAX)) + Data[15] + $49b40821), 22);
    mov ESI, EDX
    add EBX, $49b40821
    xor ESI, EAX
    add EBX, [ebp + 4*15]
    and ESI, ECX
    xor ESI, EAX
    add EBX, ESI
    rol EBX, 22
    add EBX, ECX

// Round 2
//EAX := EBX + roldword(dword(EAX + ((EBX and EDX) or (ECX and (not EDX))) + Data[1] + $f61e2562),  5);
    mov ESI, EBX
    add EAX, $f61e2562
    xor ESI, ECX
    add EAX, [ebp + 4*1]
    and ESI, EDX
    xor ESI, ECX
    add EAX, ESI
    rol EAX,  5
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + ((EAX and ECX) or (EBX and (not ECX))) + Data[6] + $c040b340),  9);
    mov ESI, EAX
    add EDX, $c040b340
    xor ESI, EBX
    add EDX, [ebp + 4*6]
    and ESI, ECX
    xor ESI, EBX
    add EDX, ESI
    rol EDX,  9
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + ((EDX and EBX) or (EAX and (not EBX))) + Data[11] + $265e5a51), 14);
    mov ESI, EDX
    add ECX, $265e5a51
    xor ESI, EAX
    add ECX, [ebp + 4*11]
    and ESI, EBX
    xor ESI, EAX
    add ECX, ESI
    rol ECX, 14
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + ((ECX and EAX) or (EDX and (not EAX))) + Data[0] + $e9b6c7aa), 20);
    mov ESI, ECX
    add EBX, $e9b6c7aa
    xor ESI, EDX
    add EBX, [ebp + 4*0]
    and ESI, EAX
    xor ESI, EDX
    add EBX, ESI
    rol EBX, 20
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + ((EBX and EDX) or (ECX and (not EDX))) + Data[5] + $d62f105d),  5);
    mov ESI, EBX
    add EAX, $d62f105d
    xor ESI, ECX
    add EAX, [ebp + 4*5]
    and ESI, EDX
    xor ESI, ECX
    add EAX, ESI
    rol EAX,  5
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + ((EAX and ECX) or (EBX and (not ECX))) + Data[10] + $02441453),  9);
    mov ESI, EAX
    add EDX, $02441453
    xor ESI, EBX
    add EDX, [ebp + 4*10]
    and ESI, ECX
    xor ESI, EBX
    add EDX, ESI
    rol EDX,  9
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + ((EDX and EBX) or (EAX and (not EBX))) + Data[15] + $d8a1e681), 14);
    mov ESI, EDX
    add ECX, $d8a1e681
    xor ESI, EAX
    add ECX, [ebp + 4*15]
    and ESI, EBX
    xor ESI, EAX
    add ECX, ESI
    rol ECX, 14
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + ((ECX and EAX) or (EDX and (not EAX))) + Data[4] + $e7d3fbc8), 20);
    mov ESI, ECX
    add EBX, $e7d3fbc8
    xor ESI, EDX
    add EBX, [ebp + 4*4]
    and ESI, EAX
    xor ESI, EDX
    add EBX, ESI
    rol EBX, 20
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + ((EBX and EDX) or (ECX and (not EDX))) + Data[9] + $21e1cde6),  5);
    mov ESI, EBX
    add EAX, $21e1cde6
    xor ESI, ECX
    add EAX, [ebp + 4*9]
    and ESI, EDX
    xor ESI, ECX
    add EAX, ESI
    rol EAX,  5
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + ((EAX and ECX) or (EBX and (not ECX))) + Data[14] + $c33707d6),  9);
    mov ESI, EAX
    add EDX, $c33707d6
    xor ESI, EBX
    add EDX, [ebp + 4*14]
    and ESI, ECX
    xor ESI, EBX
    add EDX, ESI
    rol EDX,  9
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + ((EDX and EBX) or (EAX and (not EBX))) + Data[3] + $f4d50d87), 14);
    mov ESI, EDX
    add ECX, $f4d50d87
    xor ESI, EAX
    add ECX, [ebp + 4*3]
    and ESI, EBX
    xor ESI, EAX
    add ECX, ESI
    rol ECX, 14
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + ((ECX and EAX) or (EDX and (not EAX))) + Data[8] + $455a14ed), 20);
    mov ESI, ECX
    add EBX, $455a14ed
    xor ESI, EDX
    add EBX, [ebp + 4*8]
    and ESI, EAX
    xor ESI, EDX
    add EBX, ESI
    rol EBX, 20
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + ((EBX and EDX) or (ECX and (not EDX))) + Data[13] + $a9e3e905),  5);
    mov ESI, EBX
    add EAX, $a9e3e905
    xor ESI, ECX
    add EAX, [ebp + 4*13]
    and ESI, EDX
    xor ESI, ECX
    add EAX, ESI
    rol EAX,  5
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + ((EAX and ECX) or (EBX and (not ECX))) + Data[2] + $fcefa3f8),  9);
    mov ESI, EAX
    add EDX, $fcefa3f8
    xor ESI, EBX
    add EDX, [ebp + 4*2]
    and ESI, ECX
    xor ESI, EBX
    add EDX, ESI
    rol EDX,  9
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + ((EDX and EBX) or (EAX and (not EBX))) + Data[7] + $676f02d9), 14);
    mov ESI, EDX
    add ECX, $676f02d9
    xor ESI, EAX
    add ECX, [ebp + 4*7]
    and ESI, EBX
    xor ESI, EAX
    add ECX, ESI
    rol ECX, 14
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + ((ECX and EAX) or (EDX and (not EAX))) + Data[12] + $8d2a4c8a), 20);
    mov ESI, ECX
    add EBX, $8d2a4c8a
    xor ESI, EDX
    add EBX, [ebp + 4*12]
    and ESI, EAX
    xor ESI, EDX
    add EBX, ESI
    rol EBX, 20
    add EBX, ECX

// Round 3
//EAX := EBX + roldword(dword(EAX + (EBX xor ECX xor EDX) + Data[5] + $fffa3942),  4);
    mov ESI, EBX
    add EAX, $fffa3942
    xor ESI, ECX
    add EAX, [ebp + 4*5]
    xor ESI, EDX
    add EAX, ESI
    rol EAX,  4
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + (EAX xor EBX xor ECX) + Data[8] + $8771f681), 11);
    mov ESI, EAX
    add EDX, $8771f681
    xor ESI, EBX
    add EDX, [ebp + 4*8]
    xor ESI, ECX
    add EDX, ESI
    rol EDX, 11
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + (EDX xor EAX xor EBX) + Data[11] + $6d9d6122), 16);
    mov ESI, EDX
    add ECX, $6d9d6122
    xor ESI, EAX
    add ECX, [ebp + 4*11]
    xor ESI, EBX
    add ECX, ESI
    rol ECX, 16
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + (ECX xor EDX xor EAX) + Data[14] + $fde5380c), 23);
    mov ESI, ECX
    add EBX, $fde5380c
    xor ESI, EDX
    add EBX, [ebp + 4*14]
    xor ESI, EAX
    add EBX, ESI
    rol EBX, 23
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + (EBX xor ECX xor EDX) + Data[1] + $a4beea44),  4);
    mov ESI, EBX
    add EAX, $a4beea44
    xor ESI, ECX
    add EAX, [ebp + 4*1]
    xor ESI, EDX
    add EAX, ESI
    rol EAX,  4
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + (EAX xor EBX xor ECX) + Data[4] + $4bdecfa9), 11);
    mov ESI, EAX
    add EDX, $4bdecfa9
    xor ESI, EBX
    add EDX, [ebp + 4*4]
    xor ESI, ECX
    add EDX, ESI
    rol EDX, 11
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + (EDX xor EAX xor EBX) + Data[7] + $f6bb4b60), 16);
    mov ESI, EDX
    add ECX, $f6bb4b60
    xor ESI, EAX
    add ECX, [ebp + 4*7]
    xor ESI, EBX
    add ECX, ESI
    rol ECX, 16
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + (ECX xor EDX xor EAX) + Data[10] + $bebfbc70), 23);
    mov ESI, ECX
    add EBX, $bebfbc70
    xor ESI, EDX
    add EBX, [ebp + 4*10]
    xor ESI, EAX
    add EBX, ESI
    rol EBX, 23
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + (EBX xor ECX xor EDX) + Data[13] + $289b7ec6),  4);
    mov ESI, EBX
    add EAX, $289b7ec6
    xor ESI, ECX
    add EAX, [ebp + 4*13]
    xor ESI, EDX
    add EAX, ESI
    rol EAX,  4
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + (EAX xor EBX xor ECX) + Data[0] + $eaa127fa), 11);
    mov ESI, EAX
    add EDX, $eaa127fa
    xor ESI, EBX
    add EDX, [ebp + 4*0]
    xor ESI, ECX
    add EDX, ESI
    rol EDX, 11
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + (EDX xor EAX xor EBX) + Data[3] + $d4ef3085), 16);
    mov ESI, EDX
    add ECX, $d4ef3085
    xor ESI, EAX
    add ECX, [ebp + 4*3]
    xor ESI, EBX
    add ECX, ESI
    rol ECX, 16
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + (ECX xor EDX xor EAX) + Data[6] + $04881d05), 23);
    mov ESI, ECX
    add EBX, $04881d05
    xor ESI, EDX
    add EBX, [ebp + 4*6]
    xor ESI, EAX
    add EBX, ESI
    rol EBX, 23
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + (EBX xor ECX xor EDX) + Data[9] + $d9d4d039),  4);
    mov ESI, EBX
    add EAX, $d9d4d039
    xor ESI, ECX
    add EAX, [ebp + 4*9]
    xor ESI, EDX
    add EAX, ESI
    rol EAX,  4
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + (EAX xor EBX xor ECX) + Data[12] + $e6db99e5), 11);
    mov ESI, EAX
    add EDX, $e6db99e5
    xor ESI, EBX
    add EDX, [ebp + 4*12]
    xor ESI, ECX
    add EDX, ESI
    rol EDX, 11
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + (EDX xor EAX xor EBX) + Data[15] + $1fa27cf8), 16);
    mov ESI, EDX
    add ECX, $1fa27cf8
    xor ESI, EAX
    add ECX, [ebp + 4*15]
    xor ESI, EBX
    add ECX, ESI
    rol ECX, 16
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + (ECX xor EDX xor EAX) + Data[2] + $c4ac5665), 23);
    mov ESI, ECX
    add EBX, $c4ac5665
    xor ESI, EDX
    add EBX, [ebp + 4*2]
    xor ESI, EAX
    add EBX, ESI
    rol EBX, 23
    add EBX, ECX

// Round 4
//EAX := EBX + roldword(dword(EAX + (ECX xor (EBX or (not EDX))) + Data[0] + $f4292244),  6);
    mov ESI, EDX
    add EAX, $f4292244
    not ESI
    add EAX, [ebp + 4*0]
    or ESI, EBX
    xor ESI, ECX
    add EAX, ESI
    rol EAX,  6
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + (EBX xor (EAX or (not ECX))) + Data[7] + $432aff97), 10);
    mov ESI, ECX
    add EDX, $432aff97
    not ESI
    add EDX, [ebp + 4*7]
    or ESI, EAX
    xor ESI, EBX
    add EDX, ESI
    rol EDX, 10
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + (EAX xor (EDX or (not EBX))) + Data[14] + $ab9423a7), 15);
    mov ESI, EBX
    add ECX, $ab9423a7
    not ESI
    add ECX, [ebp + 4*14]
    or ESI, EDX
    xor ESI, EAX
    add ECX, ESI
    rol ECX, 15
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + (EDX xor (ECX or (not EAX))) + Data[5] + $fc93a039), 21);
    mov ESI, EAX
    add EBX, $fc93a039
    not ESI
    add EBX, [ebp + 4*5]
    or ESI, ECX
    xor ESI, EDX
    add EBX, ESI
    rol EBX, 21
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + (ECX xor (EBX or (not EDX))) + Data[12] + $655b59c3),  6);
    mov ESI, EDX
    add EAX, $655b59c3
    not ESI
    add EAX, [ebp + 4*12]
    or ESI, EBX
    xor ESI, ECX
    add EAX, ESI
    rol EAX,  6
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + (EBX xor (EAX or (not ECX))) + Data[3] + $8f0ccc92), 10);
    mov ESI, ECX
    add EDX, $8f0ccc92
    not ESI
    add EDX, [ebp + 4*3]
    or ESI, EAX
    xor ESI, EBX
    add EDX, ESI
    rol EDX, 10
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + (EAX xor (EDX or (not EBX))) + Data[10] + $ffeff47d), 15);
    mov ESI, EBX
    add ECX, $ffeff47d
    not ESI
    add ECX, [ebp + 4*10]
    or ESI, EDX
    xor ESI, EAX
    add ECX, ESI
    rol ECX, 15
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + (EDX xor (ECX or (not EAX))) + Data[1] + $85845dd1), 21);
    mov ESI, EAX
    add EBX, $85845dd1
    not ESI
    add EBX, [ebp + 4*1]
    or ESI, ECX
    xor ESI, EDX
    add EBX, ESI
    rol EBX, 21
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + (ECX xor (EBX or (not EDX))) + Data[8] + $6fa87e4f),  6);
    mov ESI, EDX
    add EAX, $6fa87e4f
    not ESI
    add EAX, [ebp + 4*8]
    or ESI, EBX
    xor ESI, ECX
    add EAX, ESI
    rol EAX,  6
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + (EBX xor (EAX or (not ECX))) + Data[15] + $fe2ce6e0), 10);
    mov ESI, ECX
    add EDX, $fe2ce6e0
    not ESI
    add EDX, [ebp + 4*15]
    or ESI, EAX
    xor ESI, EBX
    add EDX, ESI
    rol EDX, 10
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + (EAX xor (EDX or (not EBX))) + Data[6] + $a3014314), 15);
    mov ESI, EBX
    add ECX, $a3014314
    not ESI
    add ECX, [ebp + 4*6]
    or ESI, EDX
    xor ESI, EAX
    add ECX, ESI
    rol ECX, 15
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + (EDX xor (ECX or (not EAX))) + Data[13] + $4e0811a1), 21);
    mov ESI, EAX
    add EBX, $4e0811a1
    not ESI
    add EBX, [ebp + 4*13]
    or ESI, ECX
    xor ESI, EDX
    add EBX, ESI
    rol EBX, 21
    add EBX, ECX

//EAX := EBX + roldword(dword(EAX + (ECX xor (EBX or (not EDX))) + Data[4] + $f7537e82),  6);
    mov ESI, EDX
    add EAX, $f7537e82
    not ESI
    add EAX, [ebp + 4*4]
    or ESI, EBX
    xor ESI, ECX
    add EAX, ESI
    rol EAX,  6
    add EAX, EBX

//EDX := EAX + roldword(dword(EDX + (EBX xor (EAX or (not ECX))) + Data[11] + $bd3af235), 10);
    mov ESI, ECX
    add EDX, $bd3af235
    not ESI
    add EDX, [ebp + 4*11]
    or ESI, EAX
    xor ESI, EBX
    add EDX, ESI
    rol EDX, 10
    add EDX, EAX

//ECX := EDX + roldword(dword(ECX + (EAX xor (EDX or (not EBX))) + Data[2] + $2ad7d2bb), 15);
    mov ESI, EBX
    add ECX, $2ad7d2bb
    not ESI
    add ECX, [ebp + 4*2]
    or ESI, EDX
    xor ESI, EAX
    add ECX, ESI
    rol ECX, 15
    add ECX, EDX

//EBX := ECX + roldword(dword(EBX + (EDX xor (ECX or (not EAX))) + Data[9] + $eb86d391), 21);
    mov ESI, EAX
    add EBX, $eb86d391
    not ESI
    add EBX, [ebp + 4*9]
    or ESI, ECX
    xor ESI, EDX
    add EBX, ESI
    rol EBX, 21
    add EBX, ECX


    pop EBP

    mov EDI, [pContext{.State}]
//  Inc(Context.State[0], A);
    add [EDI+12+4*0], EAX
//  Inc(Context.State[1], B);
    add [EDI+12+4*1], EBX
//  Inc(Context.State[2], C);
    add [EDI+12+4*2], ECX
//  Inc(Context.State[3], D);
    add [EDI+12+4*3], EDX

//Inc(Context.Length,64);
    add	dword ptr [EDI+104],64
    adc	dword ptr [EDI+108],0

    pop EDI
    pop ESI
    pop EDX
    pop ECX
    pop EBX
    pop EAX
end;
