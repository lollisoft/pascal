(* http://www.delphigroups.info/2/ac/3804.html *)

{$A-,B-,D+,E-,F-,G+,I-,L+,N-,O-,R-,S-,V-,X+}
{$M 4096,0,0}
Unit IdleKey;

Interface

Uses dos;

Procedure InitIdleKey;

Procedure UninitIdleKey;

Implementation

{***********************************************************************
Version 0.91

This little program was designed to save CPU time in Windows 95/98/NT
running Borland Pascal.

It also works brilliantly in programs (run in the IDE) that do a
"repeat until keypressed".

Copy the executable into the same directory as BP.EXE.

The program grabs interrupt 16 (keyboard interrupt) and performs a task
switch whenever bp queries the keyboard buffer (if it's empty).
***********************************************************************}

VAR oldint16:pointer;

Procedure newint16;far;assembler;asm
    jmp @start
@test : db 0                  {flag that is set if function 01 is called}
@start:
    mov  byte ptr @test,0
    push ds                   {save DS}
    push ax
    mov  ax,seg @data
    mov  ds,ax                {Make DS = our DS}
    pop  ax
    cmp  ah, $11
    je @SetTest
    cmp  ah,1                 {Set flag if Keyboard buffer query}
    jne  @cont
@SetTest:
    mov  byte ptr @test,1
@cont:
    Pushf                     {push dummy on stack to simulate intxx call}
    call dword ptr (oldint16) {call farr old interrupt routine}
    pop DS                    {Restore data segment}
    pushf                     {flags are vital-save them}

    cmp byte ptr @test,1      {Did we do a keyboard buffer query?}
    jnz @exit                 {if no-exit}
    popf
    pushf                     {Was the buffer empty?}
    jnz @exit

    push ax                   {Yes, so program is idle. Save AX,}
    mov  ax,1680h             {Do a taskswitch now,}
    int  2fh
    pop  ax                   {and restore AX again}

@exit:
    POPF                      {these are the flags from old int16}
    RETF 2                    {and ready!}
end;


Procedure InitIdleKey;
Begin
  Swapvectors;
  getintvec($16,oldint16);
  setintvec($16,@newint16);
End;

Procedure UninitIdleKey;
Begin
  setintvec($16,oldint16);
  Swapvectors;
End;

end. 