! **********************************************************************
module Types
integer,parameter :: I1B = selected_int_kind(2)
integer,parameter :: I2B = selected_int_kind(4)
integer,parameter :: I4B = selected_int_kind(9)
integer,parameter :: I8B = selected_int_kind(18)
!integer,parameter :: ILong = I4B
integer,parameter :: ILong = I8B
integer,parameter :: SPT = kind(1.0e0)
integer,parameter :: DPT = kind(1.0d0)
integer,parameter :: LGT = kind(.true.)
integer,parameter :: LongStrLen = 255
end module Types
