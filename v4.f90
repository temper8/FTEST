module Test
  TYPE Vector4  
    Complex,  Dimension(4) :: value
  ENDTYPE Vector4
  INTERFACE OPERATOR ( + )
    MODULE PROCEDURE V_ADD
  END INTERFACE


Contains

  TYPE(Vector4) FUNCTION V_ADD(X,Y)
  TYPE(Vector4), intent(IN) :: X, Y
  V_ADD%value = X%value + Y%value
  END FUNCTION V_ADD

  SUBROUTINE  FillVector(v)
  TYPE(Vector4) v
    do i=1,4
      v%value(i) = (1.,1.)
    end do
  end  SUBROUTINE  FillVector

end module Test

program v4
! This is a comment line, it is ignored by the compiler
  use Test
TYPE(Vector4) v1, v2, v3
  print *, 'Hello, V4!'
  call FillVector(v1)
  call FillVector(v2)
  print *,v1
  v3 = v1 + v2
  print *, v3
end program v4