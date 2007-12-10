Unit EMSAr;

{-------------------------------------------------------------------}
{ This program allows you to define any array in the Expand Memory  }
{ and access the array directly. You may freely distribute it or    }
{ use it for any purpose, provided this notice is attached with it. }
{ I would be very happy if you intend to use it in your program.    }
{ Please let me know if you like it.                                }
{                                                                   }
{ InterNet:                                                         }
{          jialong@neuro.informatik.uni-ulm.de                      }
{                                                                   }
{ Home address:                                                     }
{          Hof Str. 22                                              }
{          7906 Blaustein                                           }
{          Germany                                                  }
{                                                                   }
{-------------------------------------------------------------------}


Interface

Const

  PAGESIZE                  = 16384;  {bytes}

Type

  {--------------------------------------------------------------}
  {   You can modify BaseType to meet your need                  }
  {--------------------------------------------------------------}
  ByteArrayType =  Array [1..(65535 div Sizeof(Byte))] of Byte;
  IntArrayType  =  Array [1..(65535 div Sizeof(Integer))] of Integer;
  RealArrayType =  Array [1..(65535 div Sizeof(Real))] of Real;
  {--------------------------------------------------------------}
  {   The following define a basic EMS object                    }
  {--------------------------------------------------------------}
  EMSArray=Object
     Ems_Handle,
     Page_Frame_Address,                   
     Pages_Needed  : Word;
     NormalPtr     :  Pointer;
     ByteArry      : ^ByteArrayType;
     IntArry       : ^IntArrayType;
     RealArry      : ^RealArrayType;
  {--------------------------------------------------------------}
  {   The followings are all methods you can call                }
  {--------------------------------------------------------------}
     Function  Ems_Installed              : Boolean;
     Function  Pages_Available            : Word;
     Procedure Alloc(Page_Requested: Word);
     Procedure MapIn(Logical_Page_No, Physical_Page_No: Word);
     Procedure Release;
  End;

Var
  {--------------------------------------------------------------}
  {   Global varible, You can check it after calling a function  }
  {--------------------------------------------------------------}

   EmsError              : Integer;



Implementation


Uses  Dos;

Const
  EMM_INT                   = $67;
  DOS_Int                   = $21;
  GET_PAGE_FRAME            = $41;
  GET_UNALLOCATED_PAGE_COUNT= $42;
  ALLOCATE_PAGES            = $43;
  MAP_PAGES                 = $44;
  DEALLOCATE_PAGES          = $45;
  GET_VERSION               = $46;
  STATUS_OK                 = 0;

  {--------------------------------------------------------------}
  {   The function Emm_Installed checks to see if the Expanded   }
  {   Memory Manager (EMM) is loaded in memory. It does this by  }
  {   looking for the string 'EMMXXXX0', which should be located }
  {   at 10 bytes from the beginning of the code segment pointed }
  {   to by the EMM interrupt, 67h.                              }
  {--------------------------------------------------------------}
  Function EMSArray.Ems_installed : Boolean;
    Var
      Emm_Device_Name       : string[8];
      Int_67_Device_Name    : string[8];
      Position              : integer;
      Regs                  : registers;
    Begin
        Int_67_Device_Name := '';
        Emm_Device_Name    := 'EMMXXXX0';
        with Regs do
         Begin
          {------------------------------------------------------}
          {   Get the code segment pointed to by interrupt 67h,  }
          {   the EMM interrupt by using DOS function 35h.       }
          {   (get interrupt vector)                             }
          {------------------------------------------------------}
          AH := $35;
          AL := EMM_INT;
          Intr (DOS_int,Regs);
          For Position := 0 to 7 do
            Int_67_Device_Name :=
                    Int_67_Device_Name+Chr (mem[ES:Position+$0A]);
          {------------------------------------------------------}
          {   If the string is the EMM manager signature,        }
          {   'EMMXXXX0', then EMM is installed and ready for    }
          {   use.  If not, then EMM is not present.             }
          {------------------------------------------------------}
          If Int_67_Device_Name = Emm_Device_Name
            then Ems_Installed := True
          Else   Ems_Installed := False
        end; { with Regs do }
    End;
  {--------------------------------------------------------------}
  { This function returns the pages number that are available    }
  { for use. After calling Intr, BX contains currrently          }
  { unallocated pages, DS contains total pages in the system     }
  {--------------------------------------------------------------}
  Function  EMSArray.Pages_Available : Word;

    Var
      Regs: Registers;

      Begin
       With Regs Do
        Begin
          AH := Get_Unallocated_Page_Count;
          intr (EMM_INT, Regs);
          Pages_Available := BX;
          EmsError := AH;
        End;
      End;

  {--------------------------------------------------------------}
  {--------------------------------------------------------------}
  Procedure EMSArray.Alloc(Page_Requested: Word);
    Var
      Regs: Registers;

      Begin
      With Regs do
        Begin
          {------------------------------------------------------}
          {   Get the page frame segment address from EMM.       }
          {      AH = get page frame segment function            }
          {------------------------------------------------------}
          AH := Get_Page_Frame;
          intr (EMM_INT,Regs);
          {------------------------------------------------------}
          {      BX = page frame segment address                 }
          {      AH = status                                     }
          {------------------------------------------------------}
          Page_Frame_Address := BX;
          EmsError := AH;

          {------------------------------------------------------}
          {   Allocate the specified number of pages from EMM.   }
          {      AH = allocate pages function.                   }
          {      BX = number of pages to allocate.               }
          {------------------------------------------------------}
          Pages_Needed:= Page_Requested;
          AH := Allocate_Pages;
          BX := Pages_Needed;
          intr (EMM_INT,Regs);
          Ems_Handle  := DX;
          EmsError    := AH;
        End; {With Regs Do}

      End;

  {--------------------------------------------------------------}
  {   This function maps a logical page allocated by the         }
  {   Allocate_Expanded_Memory_Pages function into one of the    }
  {   four physical pages.                                       }
  {--------------------------------------------------------------}
  Procedure EMSArray.MapIn(Logical_Page_No, Physical_Page_No : Word);
    Var
       Regs: Registers;

    Begin

      With Regs do
        Begin
          {------------------------------------------------------}
          {   Map a logical page at physical_page_No             }
          {      AH = map page function                          }
          {      DX = handle                                     }
          {      BX = logical page number                        }
          {      AL = physical page number                       }
          {------------------------------------------------------}
          AH := Map_Pages;
          DX := Ems_Handle;
          BX := Logical_Page_No;
          AL := Physical_Page_No;
          Intr (EMM_INT, Regs);
          EmsError := AH;
        end; { with Regs do }
          {------------------------------------------------------}
          {   Making array pointer point to this physical pages  }
          {     For convience, Different pointer points to same  }
          {     address. User can add his own pointer type and   }
          {     add a line here.                                 }
          {------------------------------------------------------}
        NormalPtr:= Ptr(Page_Frame_Address, Physical_Page_No * PAGESIZE);
        ByteArry := Ptr(Page_Frame_Address, Physical_Page_No * PAGESIZE);
        IntArry  := Ptr(Page_Frame_Address, Physical_Page_No * PAGESIZE);
        RealArry := Ptr(Page_Frame_Address, Physical_Page_No * PAGESIZE);
        { ****  add other pointer here *****}

    end; { Function Map_Expanded_Memory_Pages }




  {--------------------------------------------------------------}
  {   This function releases the EMM memory pages allocated to   }
  {   us, back to the EMM memory pool.                           }
  {--------------------------------------------------------------}
  Procedure EMSArray.Release;
    Var
      Regs: Registers;

    Begin
      with Regs do
        Begin
          {------------------------------------------------------}
          {   Deallocate the pages allocated to an EMM handle.   }
          {      AH = deallocate pages function                  }
          {      DX = EMM handle                                 }
          {------------------------------------------------------}
          AH := DEALLOCATE_PAGES;
          DX := Ems_Handle;
          Intr (EMM_INT,Regs);
          EmsError := AH;
        end; { with Regs do }
    end;  { Function Deallocate_Expanded_Memory_Pages }
End.
