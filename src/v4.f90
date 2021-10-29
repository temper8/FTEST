module Test

  type Vector4  
    Complex,  Dimension(4) :: value
  endtype Vector4

  interface operator ( + )
    module procedure V_ADD
  end interface


Contains

  type(Vector4) function V_ADD(X,Y)
  type(Vector4), intent(IN) :: X, Y
    V_ADD%value = X%value + Y%value
  end function V_ADD

  subroutine  FillVector(v)
    type(Vector4) v
    do i=1,4
      v%value(i) = (1.,1.)
    end do
  end  subroutine  FillVector

end module Test

program vt
! Vector4 tests
  use Test
  type(Vector4) v1, v2, v3
  print *, 'Hello, V4!'
  call FillVector(v1)
  call FillVector(v2)
  print *,v1
  v3 = v1 + v2
  print *, v3
end program vt