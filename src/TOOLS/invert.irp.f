subroutine invert(a,LDA,na,det_l)
 implicit none
 double precision, intent(inout) :: a (LDA,na)
 integer, intent(in)             :: LDA
 integer, intent(in)             :: na
 double precision, intent(inout) :: det_l

 integer :: i,j
 select case (na)
  case default
!DIR$ forceinline
    call invert_general(a,LDA,na,det_l)
  case (5)
!DIR$ forceinline
    call invert5(a,LDA,na,det_l)
  case (4)
!DIR$ forceinline
    call invert4(a,LDA,na,det_l)
  case (3)
!DIR$ forceinline
    call invert3(a,LDA,na,det_l)
  case (2)
!DIR$ forceinline
    call invert2(a,LDA,na,det_l)
  case (1)
!DIR$ forceinline
    call invert1(a,LDA,na,det_l)
  case (0)
    det_l=1.d0
 end select
end

subroutine invert_general(a,LDA,na,det_l)
 implicit none
 double precision, intent(inout) :: a (LDA,na)
 integer, intent(in)             :: LDA
 integer, intent(in)             :: na
 double precision, intent(inout) :: det_l

 double precision :: work(LDA*max(na,64))
!DIR$ ATTRIBUTES ALIGN: $IRP_ALIGN :: work
 integer          :: inf
 integer          :: ipiv(LDA)
!DIR$ ATTRIBUTES ALIGN: $IRP_ALIGN :: ipiv
  integer          :: lwork
  double precision :: f
  integer          :: i, j
  call dgetrf(na, na, a, LDA, ipiv, inf )
  det_l = 1.d0
  j=0
  !DIR$ VECTOR ALIGNED
  do i=1,na
   j = j+min(abs(ipiv(i)-i),1)
   det_l = det_l*a(i,i)
  enddo
  if (iand(j,1) /= 0)  then
    det_l = -det_l
  endif
  lwork = SIZE(work)
  call dgetri(na, a, LDA, ipiv, work, lwork, inf )
  a = a*det_l
end

subroutine sinvert(a,LDA,na,det_l)
 implicit none
 real             :: a (LDA,na)
 integer          :: LDA
 integer          :: na
 real             :: det_l

 real             :: work(LDA*max(na,64))
!DIR$ ATTRIBUTES ALIGN: $IRP_ALIGN :: work
 integer          :: inf
 integer          :: ipiv(LDA)
!DIR$ ATTRIBUTES ALIGN: $IRP_ALIGN :: ipiv
  integer          :: lwork
  real             :: f
  integer          :: i, j
  call sgetrf(na, na, a, LDA, ipiv, inf )
  det_l = 1.d0
  j=0
  !DIR$ VECTOR ALIGNED
  do i=1,na
   if (ipiv(i) /= i) then
     j = j+1
   endif
   det_l = det_l*a(i,i)
  enddo
  if (iand(j,1) /= 0)  then
    det_l = -det_l
  endif
  lwork = SIZE(work)
  call sgetri(na, a, LDA, ipiv, work, lwork, inf )
  a = a*det_l
end

subroutine invert1(a,LDA,na,det_l)
 implicit none
 double precision, intent(inout) :: a (LDA,na)
 integer, intent(in)             :: LDA
 integer, intent(in)             :: na
 double precision, intent(inout) :: det_l

 det_l = a(1,1)
 a(1,1) = 1.d0
end

subroutine invert2(a,LDA,na,det_l)
 implicit none
 double precision :: a (LDA,na)
 integer          :: LDA
 integer          :: na
 double precision :: det_l
 double precision :: b(2,2)

 double precision :: f
 b(1,1) = a(1,1)
 b(2,1) = a(2,1)
 b(1,2) = a(1,2)
 b(2,2) = a(2,2)
 det_l = a(1,1)*a(2,2) - a(1,2)*a(2,1)
 a(1,1) = b(2,2)
 a(2,1) = -b(2,1)
 a(1,2) = -b(1,2)
 a(2,2) = b(1,1)
end

subroutine invert3(a,LDA,na,det_l)
 implicit none
 double precision, intent(inout) :: a (LDA,na)
 integer, intent(in)             :: LDA
 integer, intent(in)             :: na
 double precision, intent(inout) :: det_l
 double precision :: b(4,3)
 !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: b
 integer :: i
 double precision :: f
 det_l = a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2)) &
        -a(1,2)*(a(2,1)*a(3,3)-a(2,3)*a(3,1)) &
        +a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1))
 do i=1,4
  b(i,1) = a(i,1)
  b(i,2) = a(i,2)
  b(i,3) = a(i,3)
 enddo
 a(1,1) =  b(2,2)*b(3,3) - b(2,3)*b(3,2)
 a(2,1) =  b(2,3)*b(3,1) - b(2,1)*b(3,3)
 a(3,1) =  b(2,1)*b(3,2) - b(2,2)*b(3,1)

 a(1,2) =  b(1,3)*b(3,2) - b(1,2)*b(3,3)
 a(2,2) =  b(1,1)*b(3,3) - b(1,3)*b(3,1)
 a(3,2) =  b(1,2)*b(3,1) - b(1,1)*b(3,2)

 a(1,3) =  b(1,2)*b(2,3) - b(1,3)*b(2,2)
 a(2,3) =  b(1,3)*b(2,1) - b(1,1)*b(2,3)
 a(3,3) =  b(1,1)*b(2,2) - b(1,2)*b(2,1)

end

subroutine invert4(a,LDA,na,det_l)
 implicit none
 double precision, intent(inout) :: a (LDA,na)
 integer, intent(in)             :: LDA
 integer, intent(in)             :: na
 double precision, intent(inout) :: det_l
 double precision :: b(4,4)
 !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: b
 integer :: i,j
 double precision :: f
 det_l =  a(1,1)*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))  &
                 -a(2,3)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))  &
                 +a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))) &
         -a(1,2)*(a(2,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))  &
                 -a(2,3)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))  &
                 +a(2,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1))) &
         +a(1,3)*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))  &
                 -a(2,2)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))  &
                 +a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1))) &
         -a(1,4)*(a(2,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))  &
                 -a(2,2)*(a(3,1)*a(4,3)-a(3,3)*a(4,1))  &
                 +a(2,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
 do i=1,4
  b(1,i) = a(1,i)
  b(2,i) = a(2,i)
  b(3,i) = a(3,i)
  b(4,i) = a(4,i)
 enddo
 
 a(1,1) =  b(2,2)*(b(3,3)*b(4,4)-b(3,4)*b(4,3))-b(2,3)*(b(3,2)*b(4,4)-b(3,4)*b(4,2))+b(2,4)*(b(3,2)*b(4,3)-b(3,3)*b(4,2))
 a(2,1) = -b(2,1)*(b(3,3)*b(4,4)-b(3,4)*b(4,3))+b(2,3)*(b(3,1)*b(4,4)-b(3,4)*b(4,1))-b(2,4)*(b(3,1)*b(4,3)-b(3,3)*b(4,1))
 a(3,1) =  b(2,1)*(b(3,2)*b(4,4)-b(3,4)*b(4,2))-b(2,2)*(b(3,1)*b(4,4)-b(3,4)*b(4,1))+b(2,4)*(b(3,1)*b(4,2)-b(3,2)*b(4,1))
 a(4,1) = -b(2,1)*(b(3,2)*b(4,3)-b(3,3)*b(4,2))+b(2,2)*(b(3,1)*b(4,3)-b(3,3)*b(4,1))-b(2,3)*(b(3,1)*b(4,2)-b(3,2)*b(4,1))

 a(1,2) = -b(1,2)*(b(3,3)*b(4,4)-b(3,4)*b(4,3))+b(1,3)*(b(3,2)*b(4,4)-b(3,4)*b(4,2))-b(1,4)*(b(3,2)*b(4,3)-b(3,3)*b(4,2))
 a(2,2) =  b(1,1)*(b(3,3)*b(4,4)-b(3,4)*b(4,3))-b(1,3)*(b(3,1)*b(4,4)-b(3,4)*b(4,1))+b(1,4)*(b(3,1)*b(4,3)-b(3,3)*b(4,1))
 a(3,2) = -b(1,1)*(b(3,2)*b(4,4)-b(3,4)*b(4,2))+b(1,2)*(b(3,1)*b(4,4)-b(3,4)*b(4,1))-b(1,4)*(b(3,1)*b(4,2)-b(3,2)*b(4,1))
 a(4,2) =  b(1,1)*(b(3,2)*b(4,3)-b(3,3)*b(4,2))-b(1,2)*(b(3,1)*b(4,3)-b(3,3)*b(4,1))+b(1,3)*(b(3,1)*b(4,2)-b(3,2)*b(4,1))

 a(1,3) =  b(1,2)*(b(2,3)*b(4,4)-b(2,4)*b(4,3))-b(1,3)*(b(2,2)*b(4,4)-b(2,4)*b(4,2))+b(1,4)*(b(2,2)*b(4,3)-b(2,3)*b(4,2))
 a(2,3) = -b(1,1)*(b(2,3)*b(4,4)-b(2,4)*b(4,3))+b(1,3)*(b(2,1)*b(4,4)-b(2,4)*b(4,1))-b(1,4)*(b(2,1)*b(4,3)-b(2,3)*b(4,1))
 a(3,3) =  b(1,1)*(b(2,2)*b(4,4)-b(2,4)*b(4,2))-b(1,2)*(b(2,1)*b(4,4)-b(2,4)*b(4,1))+b(1,4)*(b(2,1)*b(4,2)-b(2,2)*b(4,1))
 a(4,3) = -b(1,1)*(b(2,2)*b(4,3)-b(2,3)*b(4,2))+b(1,2)*(b(2,1)*b(4,3)-b(2,3)*b(4,1))-b(1,3)*(b(2,1)*b(4,2)-b(2,2)*b(4,1))

 a(1,4) = -b(1,2)*(b(2,3)*b(3,4)-b(2,4)*b(3,3))+b(1,3)*(b(2,2)*b(3,4)-b(2,4)*b(3,2))-b(1,4)*(b(2,2)*b(3,3)-b(2,3)*b(3,2))
 a(2,4) =  b(1,1)*(b(2,3)*b(3,4)-b(2,4)*b(3,3))-b(1,3)*(b(2,1)*b(3,4)-b(2,4)*b(3,1))+b(1,4)*(b(2,1)*b(3,3)-b(2,3)*b(3,1))
 a(3,4) = -b(1,1)*(b(2,2)*b(3,4)-b(2,4)*b(3,2))+b(1,2)*(b(2,1)*b(3,4)-b(2,4)*b(3,1))-b(1,4)*(b(2,1)*b(3,2)-b(2,2)*b(3,1))
 a(4,4) =  b(1,1)*(b(2,2)*b(3,3)-b(2,3)*b(3,2))-b(1,2)*(b(2,1)*b(3,3)-b(2,3)*b(3,1))+b(1,3)*(b(2,1)*b(3,2)-b(2,2)*b(3,1))

end

subroutine invert5(a,LDA,na,det_l)
 implicit none
 double precision, intent(inout) :: a (LDA,na)
 integer, intent(in)             :: LDA
 integer, intent(in)             :: na
 double precision, intent(inout) :: det_l
 double precision :: b(5,5)
 !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: b
 integer :: i,j
 double precision :: f
 det_l = a(1,1)*(a(2,2)*(a(3,3)*(a(4,4)*a(5,5)-a(4,5)*a(5,4))-a(3,4)*( &
 a(4,3)*a(5,5)-a(4,5)*a(5,3))+a(3,5)*(a(4,3)*a(5,4)-a(4,4)*a(5,3)))- &
 a(2,3)*(a(3,2)*(a(4,4)*a(5,5)-a(4,5)*a(5,4))-a(3,4)*(a(4,2)*a(5,5)- &
 a(4,5)*a(5,2))+a(3,5)*(a(4,2)*a(5,4)-a(4,4)*a(5,2)))+a(2,4)*(a(3,2)*( &
 a(4,3)*a(5,5)-a(4,5)*a(5,3))-a(3,3)*(a(4,2)*a(5,5)-a(4,5)*a(5,2))+ &
 a(3,5)*(a(4,2)*a(5,3)-a(4,3)*a(5,2)))-a(2,5)*(a(3,2)*(a(4,3)*a(5,4)- &
 a(4,4)*a(5,3))-a(3,3)*(a(4,2)*a(5,4)-a(4,4)*a(5,2))+a(3,4)*(a(4,2)* &
 a(5,3)-a(4,3)*a(5,2))))-a(1,2)*(a(2,1)*(a(3,3)*(a(4,4)*a(5,5)-a(4,5)* &
 a(5,4))-a(3,4)*(a(4,3)*a(5,5)-a(4,5)*a(5,3))+a(3,5)*(a(4,3)*a(5,4)- &
 a(4,4)*a(5,3)))-a(2,3)*(a(3,1)*(a(4,4)*a(5,5)-a(4,5)*a(5,4))-a(3,4)*( &
 a(4,1)*a(5,5)-a(4,5)*a(5,1))+a(3,5)*(a(4,1)*a(5,4)-a(4,4)*a(5,1)))+ &
 a(2,4)*(a(3,1)*(a(4,3)*a(5,5)-a(4,5)*a(5,3))-a(3,3)*(a(4,1)*a(5,5)- &
 a(4,5)*a(5,1))+a(3,5)*(a(4,1)*a(5,3)-a(4,3)*a(5,1)))-a(2,5)*(a(3,1)*( &
 a(4,3)*a(5,4)-a(4,4)*a(5,3))-a(3,3)*(a(4,1)*a(5,4)-a(4,4)*a(5,1))+ &
 a(3,4)*(a(4,1)*a(5,3)-a(4,3)*a(5,1))))+a(1,3)*(a(2,1)*(a(3,2)*(a(4,4)* &
 a(5,5)-a(4,5)*a(5,4))-a(3,4)*(a(4,2)*a(5,5)-a(4,5)*a(5,2))+a(3,5)*( &
 a(4,2)*a(5,4)-a(4,4)*a(5,2)))-a(2,2)*(a(3,1)*(a(4,4)*a(5,5)-a(4,5)* &
 a(5,4))-a(3,4)*(a(4,1)*a(5,5)-a(4,5)*a(5,1))+a(3,5)*(a(4,1)*a(5,4)- &
 a(4,4)*a(5,1)))+a(2,4)*(a(3,1)*(a(4,2)*a(5,5)-a(4,5)*a(5,2))-a(3,2)*( &
 a(4,1)*a(5,5)-a(4,5)*a(5,1))+a(3,5)*(a(4,1)*a(5,2)-a(4,2)*a(5,1)))- &
 a(2,5)*(a(3,1)*(a(4,2)*a(5,4)-a(4,4)*a(5,2))-a(3,2)*(a(4,1)*a(5,4)- &
 a(4,4)*a(5,1))+a(3,4)*(a(4,1)*a(5,2)-a(4,2)*a(5,1))))-a(1,4)*(a(2,1)*( &
 a(3,2)*(a(4,3)*a(5,5)-a(4,5)*a(5,3))-a(3,3)*(a(4,2)*a(5,5)-a(4,5)* &
 a(5,2))+a(3,5)*(a(4,2)*a(5,3)-a(4,3)*a(5,2)))-a(2,2)*(a(3,1)*(a(4,3)* &
 a(5,5)-a(4,5)*a(5,3))-a(3,3)*(a(4,1)*a(5,5)-a(4,5)*a(5,1))+a(3,5)*( &
 a(4,1)*a(5,3)-a(4,3)*a(5,1)))+a(2,3)*(a(3,1)*(a(4,2)*a(5,5)-a(4,5)* &
 a(5,2))-a(3,2)*(a(4,1)*a(5,5)-a(4,5)*a(5,1))+a(3,5)*(a(4,1)*a(5,2)- &
 a(4,2)*a(5,1)))-a(2,5)*(a(3,1)*(a(4,2)*a(5,3)-a(4,3)*a(5,2))-a(3,2)*( &
 a(4,1)*a(5,3)-a(4,3)*a(5,1))+a(3,3)*(a(4,1)*a(5,2)-a(4,2)*a(5,1))))+ &
 a(1,5)*(a(2,1)*(a(3,2)*(a(4,3)*a(5,4)-a(4,4)*a(5,3))-a(3,3)*(a(4,2)* &
 a(5,4)-a(4,4)*a(5,2))+a(3,4)*(a(4,2)*a(5,3)-a(4,3)*a(5,2)))-a(2,2)*( &
 a(3,1)*(a(4,3)*a(5,4)-a(4,4)*a(5,3))-a(3,3)*(a(4,1)*a(5,4)-a(4,4)* &
 a(5,1))+a(3,4)*(a(4,1)*a(5,3)-a(4,3)*a(5,1)))+a(2,3)*(a(3,1)*(a(4,2)* &
 a(5,4)-a(4,4)*a(5,2))-a(3,2)*(a(4,1)*a(5,4)-a(4,4)*a(5,1))+a(3,4)*( &
 a(4,1)*a(5,2)-a(4,2)*a(5,1)))-a(2,4)*(a(3,1)*(a(4,2)*a(5,3)-a(4,3)* &
 a(5,2))-a(3,2)*(a(4,1)*a(5,3)-a(4,3)*a(5,1))+a(3,3)*(a(4,1)*a(5,2)- &
 a(4,2)*a(5,1))))

 do i=1,5
  b(1,i) = a(1,i)
  b(2,i) = a(2,i)
  b(3,i) = a(3,i)
  b(4,i) = a(4,i)
  b(5,i) = a(5,i)
 enddo
 
 a(1,1) = &
 (b(2,2)*(b(3,3)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))+b(3,5)*(b(4,3)*b(5,4)-b(4,4)*b(5,3)))-b(2,3)* &
 (b(3,2)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(3,5)*(b(4,2)*b(5,4)-b(4,4)*b(5,2)))+b(2,4)* &
 (b(3,2)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(3,3)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(3,5)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))-b(2,5)* &
 (b(3,2)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(3,3)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))+b(3,4)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))))
 a(2,1) = &
 (-b(2,1)*(b(3,3)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))+b(3,5)*(b(4,3)*b(5,4)-b(4,4)*b(5,3)))+b(2,3)* &
 (b(3,1)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,4)-b(4,4)*b(5,1)))-b(2,4)* &
 (b(3,1)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(3,3)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))+b(2,5)* &
 (b(3,1)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(3,3)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(3,4)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))))
 a(3,1) = &
 (b(2,1)*(b(3,2)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(3,5)*(b(4,2)*b(5,4)-b(4,4)*b(5,2)))-b(2,2)* &
 (b(3,1)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,4)-b(4,4)*b(5,1)))+b(2,4)* &
 (b(3,1)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))-b(3,2)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))-b(2,5)* &
 (b(3,1)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))-b(3,2)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(3,4)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))
 a(4,1) = &
 (-b(2,1)*(b(3,2)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(3,3)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(3,5)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))+b(2,2)* &
 (b(3,1)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(3,3)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))-b(2,3)* &
 (b(3,1)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))-b(3,2)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))+b(2,5)* &
 (b(3,1)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))-b(3,2)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))+b(3,3)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))
 a(5,1) = &
 (b(2,1)*(b(3,2)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(3,3)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))+b(3,4)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))-b(2,2)* &
 (b(3,1)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(3,3)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(3,4)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))+b(2,3)* &
 (b(3,1)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))-b(3,2)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(3,4)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))-b(2,4)* &
 (b(3,1)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))-b(3,2)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))+b(3,3)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))

 a(1,2) = &
 (-b(1,2)*(b(3,3)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))+b(3,5)*(b(4,3)*b(5,4)-b(4,4)*b(5,3)))+b(1,3)* &
 (b(3,2)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(3,5)*(b(4,2)*b(5,4)-b(4,4)*b(5,2)))-b(1,4)* &
 (b(3,2)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(3,3)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(3,5)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))+b(1,5)* &
 (b(3,2)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(3,3)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))+b(3,4)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))))
 a(2,2) = &
 (b(1,1)*(b(3,3)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))+b(3,5)*(b(4,3)*b(5,4)-b(4,4)*b(5,3)))-b(1,3)* &
 (b(3,1)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,4)-b(4,4)*b(5,1)))+b(1,4)* &
 (b(3,1)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(3,3)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))-b(1,5)* &
 (b(3,1)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(3,3)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(3,4)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))))
 a(3,2) = &
 (-b(1,1)*(b(3,2)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(3,5)*(b(4,2)*b(5,4)-b(4,4)*b(5,2)))+b(1,2)* &
 (b(3,1)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(3,4)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,4)-b(4,4)*b(5,1)))-b(1,4)* &
 (b(3,1)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))-b(3,2)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))+b(1,5)* &
 (b(3,1)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))-b(3,2)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(3,4)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))
 a(4,2) = &
 (b(1,1)*(b(3,2)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(3,3)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(3,5)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))-b(1,2)* &
 (b(3,1)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(3,3)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))+b(1,3)* &
 (b(3,1)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))-b(3,2)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(3,5)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))-b(1,5)* &
 (b(3,1)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))-b(3,2)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))+b(3,3)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))
 a(5,2) = &
 (-b(1,1)*(b(3,2)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(3,3)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))+b(3,4)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))+b(1,2)* &
 (b(3,1)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(3,3)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(3,4)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))-b(1,3)* &
 (b(3,1)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))-b(3,2)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(3,4)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))+b(1,4)* &
 (b(3,1)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))-b(3,2)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))+b(3,3)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))

 a(1,3) = &
 (b(1,2)*(b(2,3)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(2,4)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))+b(2,5)*(b(4,3)*b(5,4)-b(4,4)*b(5,3)))-b(1,3)* &
 (b(2,2)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(2,4)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(2,5)*(b(4,2)*b(5,4)-b(4,4)*b(5,2)))+b(1,4)* &
 (b(2,2)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(2,3)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(2,5)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))-b(1,5)* &
 (b(2,2)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(2,3)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))+b(2,4)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))))
 a(2,3) = &
 (-b(1,1)*(b(2,3)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(2,4)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))+b(2,5)*(b(4,3)*b(5,4)-b(4,4)*b(5,3)))+b(1,3)* &
 (b(2,1)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(2,4)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(2,5)*(b(4,1)*b(5,4)-b(4,4)*b(5,1)))-b(1,4)* &
 (b(2,1)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(2,3)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(2,5)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))+b(1,5)* &
 (b(2,1)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(2,3)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(2,4)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))))
 a(3,3) = &
 (b(1,1)*(b(2,2)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(2,4)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(2,5)*(b(4,2)*b(5,4)-b(4,4)*b(5,2)))-b(1,2)* &
 (b(2,1)*(b(4,4)*b(5,5)-b(4,5)*b(5,4))-b(2,4)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(2,5)*(b(4,1)*b(5,4)-b(4,4)*b(5,1)))+b(1,4)* &
 (b(2,1)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))-b(2,2)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(2,5)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))-b(1,5)* &
 (b(2,1)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))-b(2,2)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(2,4)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))
 a(4,3) = &
 (-b(1,1)*(b(2,2)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(2,3)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))+b(2,5)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))+b(1,2)* &
 (b(2,1)*(b(4,3)*b(5,5)-b(4,5)*b(5,3))-b(2,3)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(2,5)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))-b(1,3)* &
 (b(2,1)*(b(4,2)*b(5,5)-b(4,5)*b(5,2))-b(2,2)*(b(4,1)*b(5,5)-b(4,5)*b(5,1))+b(2,5)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))+b(1,5)* &
 (b(2,1)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))-b(2,2)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))+b(2,3)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))
 a(5,3) = &
 (b(1,1)*(b(2,2)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(2,3)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))+b(2,4)*(b(4,2)*b(5,3)-b(4,3)*b(5,2)))-b(1,2)* &
 (b(2,1)*(b(4,3)*b(5,4)-b(4,4)*b(5,3))-b(2,3)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(2,4)*(b(4,1)*b(5,3)-b(4,3)*b(5,1)))+b(1,3)* &
 (b(2,1)*(b(4,2)*b(5,4)-b(4,4)*b(5,2))-b(2,2)*(b(4,1)*b(5,4)-b(4,4)*b(5,1))+b(2,4)*(b(4,1)*b(5,2)-b(4,2)*b(5,1)))-b(1,4)* &
 (b(2,1)*(b(4,2)*b(5,3)-b(4,3)*b(5,2))-b(2,2)*(b(4,1)*b(5,3)-b(4,3)*b(5,1))+b(2,3)*(b(4,1)*b(5,2)-b(4,2)*b(5,1))))

 a(1,4) = &
 (-b(1,2)*(b(2,3)*(b(3,4)*b(5,5)-b(3,5)*b(5,4))-b(2,4)*(b(3,3)*b(5,5)-b(3,5)*b(5,3))+b(2,5)*(b(3,3)*b(5,4)-b(3,4)*b(5,3)))+b(1,3)* &
 (b(2,2)*(b(3,4)*b(5,5)-b(3,5)*b(5,4))-b(2,4)*(b(3,2)*b(5,5)-b(3,5)*b(5,2))+b(2,5)*(b(3,2)*b(5,4)-b(3,4)*b(5,2)))-b(1,4)* &
 (b(2,2)*(b(3,3)*b(5,5)-b(3,5)*b(5,3))-b(2,3)*(b(3,2)*b(5,5)-b(3,5)*b(5,2))+b(2,5)*(b(3,2)*b(5,3)-b(3,3)*b(5,2)))+b(1,5)* &
 (b(2,2)*(b(3,3)*b(5,4)-b(3,4)*b(5,3))-b(2,3)*(b(3,2)*b(5,4)-b(3,4)*b(5,2))+b(2,4)*(b(3,2)*b(5,3)-b(3,3)*b(5,2))))
 a(2,4) = &
 (b(1,1)*(b(2,3)*(b(3,4)*b(5,5)-b(3,5)*b(5,4))-b(2,4)*(b(3,3)*b(5,5)-b(3,5)*b(5,3))+b(2,5)*(b(3,3)*b(5,4)-b(3,4)*b(5,3)))-b(1,3)* &
 (b(2,1)*(b(3,4)*b(5,5)-b(3,5)*b(5,4))-b(2,4)*(b(3,1)*b(5,5)-b(3,5)*b(5,1))+b(2,5)*(b(3,1)*b(5,4)-b(3,4)*b(5,1)))+b(1,4)* &
 (b(2,1)*(b(3,3)*b(5,5)-b(3,5)*b(5,3))-b(2,3)*(b(3,1)*b(5,5)-b(3,5)*b(5,1))+b(2,5)*(b(3,1)*b(5,3)-b(3,3)*b(5,1)))-b(1,5)* &
 (b(2,1)*(b(3,3)*b(5,4)-b(3,4)*b(5,3))-b(2,3)*(b(3,1)*b(5,4)-b(3,4)*b(5,1))+b(2,4)*(b(3,1)*b(5,3)-b(3,3)*b(5,1))))
 a(3,4) = &
 (-b(1,1)*(b(2,2)*(b(3,4)*b(5,5)-b(3,5)*b(5,4))-b(2,4)*(b(3,2)*b(5,5)-b(3,5)*b(5,2))+b(2,5)*(b(3,2)*b(5,4)-b(3,4)*b(5,2)))+b(1,2)* &
 (b(2,1)*(b(3,4)*b(5,5)-b(3,5)*b(5,4))-b(2,4)*(b(3,1)*b(5,5)-b(3,5)*b(5,1))+b(2,5)*(b(3,1)*b(5,4)-b(3,4)*b(5,1)))-b(1,4)* &
 (b(2,1)*(b(3,2)*b(5,5)-b(3,5)*b(5,2))-b(2,2)*(b(3,1)*b(5,5)-b(3,5)*b(5,1))+b(2,5)*(b(3,1)*b(5,2)-b(3,2)*b(5,1)))+b(1,5)* &
 (b(2,1)*(b(3,2)*b(5,4)-b(3,4)*b(5,2))-b(2,2)*(b(3,1)*b(5,4)-b(3,4)*b(5,1))+b(2,4)*(b(3,1)*b(5,2)-b(3,2)*b(5,1))))
 a(4,4) = &
 (b(1,1)*(b(2,2)*(b(3,3)*b(5,5)-b(3,5)*b(5,3))-b(2,3)*(b(3,2)*b(5,5)-b(3,5)*b(5,2))+b(2,5)*(b(3,2)*b(5,3)-b(3,3)*b(5,2)))-b(1,2)* &
 (b(2,1)*(b(3,3)*b(5,5)-b(3,5)*b(5,3))-b(2,3)*(b(3,1)*b(5,5)-b(3,5)*b(5,1))+b(2,5)*(b(3,1)*b(5,3)-b(3,3)*b(5,1)))+b(1,3)* &
 (b(2,1)*(b(3,2)*b(5,5)-b(3,5)*b(5,2))-b(2,2)*(b(3,1)*b(5,5)-b(3,5)*b(5,1))+b(2,5)*(b(3,1)*b(5,2)-b(3,2)*b(5,1)))-b(1,5)* &
 (b(2,1)*(b(3,2)*b(5,3)-b(3,3)*b(5,2))-b(2,2)*(b(3,1)*b(5,3)-b(3,3)*b(5,1))+b(2,3)*(b(3,1)*b(5,2)-b(3,2)*b(5,1))))
 a(5,4) = &
 (-b(1,1)*(b(2,2)*(b(3,3)*b(5,4)-b(3,4)*b(5,3))-b(2,3)*(b(3,2)*b(5,4)-b(3,4)*b(5,2))+b(2,4)*(b(3,2)*b(5,3)-b(3,3)*b(5,2)))+b(1,2)* &
 (b(2,1)*(b(3,3)*b(5,4)-b(3,4)*b(5,3))-b(2,3)*(b(3,1)*b(5,4)-b(3,4)*b(5,1))+b(2,4)*(b(3,1)*b(5,3)-b(3,3)*b(5,1)))-b(1,3)* &
 (b(2,1)*(b(3,2)*b(5,4)-b(3,4)*b(5,2))-b(2,2)*(b(3,1)*b(5,4)-b(3,4)*b(5,1))+b(2,4)*(b(3,1)*b(5,2)-b(3,2)*b(5,1)))+b(1,4)* &
 (b(2,1)*(b(3,2)*b(5,3)-b(3,3)*b(5,2))-b(2,2)*(b(3,1)*b(5,3)-b(3,3)*b(5,1))+b(2,3)*(b(3,1)*b(5,2)-b(3,2)*b(5,1))))

 a(1,5) = &
 (b(1,2)*(b(2,3)*(b(3,4)*b(4,5)-b(3,5)*b(4,4))-b(2,4)*(b(3,3)*b(4,5)-b(3,5)*b(4,3))+b(2,5)*(b(3,3)*b(4,4)-b(3,4)*b(4,3)))-b(1,3)* &
 (b(2,2)*(b(3,4)*b(4,5)-b(3,5)*b(4,4))-b(2,4)*(b(3,2)*b(4,5)-b(3,5)*b(4,2))+b(2,5)*(b(3,2)*b(4,4)-b(3,4)*b(4,2)))+b(1,4)* &
 (b(2,2)*(b(3,3)*b(4,5)-b(3,5)*b(4,3))-b(2,3)*(b(3,2)*b(4,5)-b(3,5)*b(4,2))+b(2,5)*(b(3,2)*b(4,3)-b(3,3)*b(4,2)))-b(1,5)* &
 (b(2,2)*(b(3,3)*b(4,4)-b(3,4)*b(4,3))-b(2,3)*(b(3,2)*b(4,4)-b(3,4)*b(4,2))+b(2,4)*(b(3,2)*b(4,3)-b(3,3)*b(4,2))))
 a(2,5) = &
 (-b(1,1)*(b(2,3)*(b(3,4)*b(4,5)-b(3,5)*b(4,4))-b(2,4)*(b(3,3)*b(4,5)-b(3,5)*b(4,3))+b(2,5)*(b(3,3)*b(4,4)-b(3,4)*b(4,3)))+b(1,3)* &
 (b(2,1)*(b(3,4)*b(4,5)-b(3,5)*b(4,4))-b(2,4)*(b(3,1)*b(4,5)-b(3,5)*b(4,1))+b(2,5)*(b(3,1)*b(4,4)-b(3,4)*b(4,1)))-b(1,4)* &
 (b(2,1)*(b(3,3)*b(4,5)-b(3,5)*b(4,3))-b(2,3)*(b(3,1)*b(4,5)-b(3,5)*b(4,1))+b(2,5)*(b(3,1)*b(4,3)-b(3,3)*b(4,1)))+b(1,5)* &
 (b(2,1)*(b(3,3)*b(4,4)-b(3,4)*b(4,3))-b(2,3)*(b(3,1)*b(4,4)-b(3,4)*b(4,1))+b(2,4)*(b(3,1)*b(4,3)-b(3,3)*b(4,1))))
 a(3,5) = &
 (b(1,1)*(b(2,2)*(b(3,4)*b(4,5)-b(3,5)*b(4,4))-b(2,4)*(b(3,2)*b(4,5)-b(3,5)*b(4,2))+b(2,5)*(b(3,2)*b(4,4)-b(3,4)*b(4,2)))-b(1,2)* &
 (b(2,1)*(b(3,4)*b(4,5)-b(3,5)*b(4,4))-b(2,4)*(b(3,1)*b(4,5)-b(3,5)*b(4,1))+b(2,5)*(b(3,1)*b(4,4)-b(3,4)*b(4,1)))+b(1,4)* &
 (b(2,1)*(b(3,2)*b(4,5)-b(3,5)*b(4,2))-b(2,2)*(b(3,1)*b(4,5)-b(3,5)*b(4,1))+b(2,5)*(b(3,1)*b(4,2)-b(3,2)*b(4,1)))-b(1,5)* &
 (b(2,1)*(b(3,2)*b(4,4)-b(3,4)*b(4,2))-b(2,2)*(b(3,1)*b(4,4)-b(3,4)*b(4,1))+b(2,4)*(b(3,1)*b(4,2)-b(3,2)*b(4,1))))
 a(4,5) = &
 (-b(1,1)*(b(2,2)*(b(3,3)*b(4,5)-b(3,5)*b(4,3))-b(2,3)*(b(3,2)*b(4,5)-b(3,5)*b(4,2))+b(2,5)*(b(3,2)*b(4,3)-b(3,3)*b(4,2)))+b(1,2)* &
 (b(2,1)*(b(3,3)*b(4,5)-b(3,5)*b(4,3))-b(2,3)*(b(3,1)*b(4,5)-b(3,5)*b(4,1))+b(2,5)*(b(3,1)*b(4,3)-b(3,3)*b(4,1)))-b(1,3)* &
 (b(2,1)*(b(3,2)*b(4,5)-b(3,5)*b(4,2))-b(2,2)*(b(3,1)*b(4,5)-b(3,5)*b(4,1))+b(2,5)*(b(3,1)*b(4,2)-b(3,2)*b(4,1)))+b(1,5)* &
 (b(2,1)*(b(3,2)*b(4,3)-b(3,3)*b(4,2))-b(2,2)*(b(3,1)*b(4,3)-b(3,3)*b(4,1))+b(2,3)*(b(3,1)*b(4,2)-b(3,2)*b(4,1))))
 a(5,5) = &
 (b(1,1)*(b(2,2)*(b(3,3)*b(4,4)-b(3,4)*b(4,3))-b(2,3)*(b(3,2)*b(4,4)-b(3,4)*b(4,2))+b(2,4)*(b(3,2)*b(4,3)-b(3,3)*b(4,2)))-b(1,2)* &
 (b(2,1)*(b(3,3)*b(4,4)-b(3,4)*b(4,3))-b(2,3)*(b(3,1)*b(4,4)-b(3,4)*b(4,1))+b(2,4)*(b(3,1)*b(4,3)-b(3,3)*b(4,1)))+b(1,3)* &
 (b(2,1)*(b(3,2)*b(4,4)-b(3,4)*b(4,2))-b(2,2)*(b(3,1)*b(4,4)-b(3,4)*b(4,1))+b(2,4)*(b(3,1)*b(4,2)-b(3,2)*b(4,1)))-b(1,4)* &
 (b(2,1)*(b(3,2)*b(4,3)-b(3,3)*b(4,2))-b(2,2)*(b(3,1)*b(4,3)-b(3,3)*b(4,1))+b(2,3)*(b(3,1)*b(4,2)-b(3,2)*b(4,1))))

end

subroutine invert_update(a,LDA,na,det_l,b)
 implicit none
 double precision :: a (LDA,na)
 double precision :: b (LDA,na)
 integer          :: LDA
 integer          :: na
 double precision :: det_l

 double precision :: work(LDA*max(na,64))
!DIR$ ATTRIBUTES ALIGN: $IRP_ALIGN :: work
 integer          :: inf
 integer          :: ipiv(LDA)
!DIR$ ATTRIBUTES ALIGN: $IRP_ALIGN :: ipiv
  integer          :: lwork
  double precision :: f
  integer          :: i, j
  double precision :: bold(LDA,na)
  double precision :: ba(LDA,na)
  integer          :: k
  ba = a
  call dgetrf(na, na, ba, LDA, ipiv, inf )
  det_l = 1.d0
  j=0
  !DIR$ VECTOR ALIGNED
  do i=1,na
   if (ipiv(i) /= i) then
     j = j+1
   endif
   det_l = det_l*a(i,i)
  enddo
  if (iand(j,1) /= 0)  then
    det_l = -det_l
  endif
  do k=1,3
    bold = b
    call dgemm('N','N',na,na,na,1.d0,a,LDA,bold,LDA, &
      0.d0, ba, LDA)
    call dgemm('N','N',na,na,na,-1.d0,bold,LDA,ba,LDA, &
      2.d0, b, LDA)
  enddo
  b = b*det_l
end

