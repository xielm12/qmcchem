double precision function qmc_ranf()
! L'Ecuyer, P. (1999) `Tables of maximally equidistributed combined LFSR
! generators', Math. of Comput., 68, 261-269.
 implicit none
 integer*8 :: b(2)
 b(1) = ISHFT( IEOR( ISHFT(seed(1),1), seed(1)), -53)
 b(2) = ISHFT( IAND(seed(1),-2_8), 10)
 seed(1) = IEOR( b(2), b(1))

 b(1) = ISHFT( IEOR( ISHFT(seed(2),24), seed(2)), -50)
 b(2) = ISHFT( IAND(seed(2),-512_8), 5)
 seed(2) = IEOR( b(2), b(1))

 b(1) = ISHFT( IEOR( ISHFT(seed(3),3), seed(3)), -23)
 b(2) = ISHFT( IAND(seed(3),-4096_8), 29)
 seed(3) = IEOR( b(2), b(1))

 b(1) = ISHFT( IEOR( ISHFT(seed(4),5), seed(4)), -24)
 b(2) = ISHFT( IAND(seed(4),-131072_8), 23)
 seed(4) = IEOR( b(2), b(1))

 b(1) = ISHFT( IEOR( ISHFT(seed(5),3), seed(5)), -33)
 b(2) = ISHFT( IAND(seed(5),-8388608_8), 8)
 seed(5) = IEOR( b(2), b(1))

 qmc_ranf = IEOR( IEOR( IEOR( IEOR(seed(1),seed(2)), seed(3)), &
   seed(4)), seed(5)) * 5.4210108624275221D-20 + 0.5D0
 ASSERT ( qmc_ranf >= 0.d0 )
 ASSERT ( qmc_ranf <= 1.d0 )

end

subroutine ranf_array(isize,res)
 implicit none
 integer :: isize
 double precision :: res(isize)
 integer :: i
 double precision :: qmc_ranf

 do i=1,isize
   res(i) = qmc_ranf()
 enddo
end

BEGIN_PROVIDER  [ integer*8, seed, (5) ]
  implicit none
  BEGIN_DOC  
! Seeds data
! Initialized by init_random
  END_DOC
  integer                        :: iargc
  integer*8                      :: i,j
  integer*4                      :: clock(12)
  double precision               :: r
  integer*8                      :: pid8
  read(current_PID,*) pid8
  pid8 = iand( ishft(pid8, 32), pid8)
  do i=1,12
    clock(i) = i
  enddo
  call system_clock(count=clock(1))
  call random_seed(put=clock)  
  do i=1,5
    call random_number(r)
    seed(i) = (r-0.5d0)*huge(1_8)
    seed(i) = ieor( seed(i), pid8)
    do j=1,16
      seed(i) = ishft(seed(i),1)+1
    enddo
  enddo

END_PROVIDER

subroutine gauss_array(isize,res)
  implicit none
  include '../constants.F'
  integer isize
  double precision res(isize)

  double precision u1(isize),u2(isize)
  integer i

  call ranf_array(isize,u1)
  call ranf_array(isize,u2)
  do i=1,isize
    res(i)=sqrt(-2.d0*log(u1(i)))*cos(dtwo_pi*u2(i))
  enddo
end

double precision function gauss()
  implicit none
! include 'constants.F'
  double precision :: qmc_ranf
! double precision :: u1,u2
! u1=qmc_ranf()
! u2=qmc_ranf()
! gauss=sqrt(-2.d0*dlog(u1))*cos(dfour_pi*u2)
  double precision :: inverse_normal_cdf
  gauss = inverse_normal_cdf(qmc_ranf())
end

double precision function inverse_normal_cdf(p)
  implicit none
  double precision, intent(in)   :: p
  double precision               :: p_low,p_high
  double precision               :: a1,a2,a3,a4,a5,a6
  double precision               :: b1,b2,b3,b4,b5
  double precision               :: c1,c2,c3,c4,c5,c6
  double precision               :: d1,d2,d3,d4
  double precision               :: z,q,r
  double precision               :: qmc_ranf
  a1=-39.6968302866538d0
  a2=220.946098424521d0
  a3=-275.928510446969d0
  a4=138.357751867269d0
  a5=-30.6647980661472d0
  a6=2.50662827745924d0
  b1=-54.4760987982241d0
  b2=161.585836858041d0
  b3=-155.698979859887d0
  b4=66.8013118877197d0
  b5=-13.2806815528857d0
  c1=-0.00778489400243029d0
  c2=-0.322396458041136d0
  c3=-2.40075827716184d0
  c4=-2.54973253934373d0
  c5=4.37466414146497d0
  c6=2.93816398269878d0
  d1=0.00778469570904146d0
  d2=0.32246712907004d0
  d3=2.445134137143d0
  d4=3.75440866190742d0
  p_low=0.02425d0
  p_high=1.d0-0.02425d0
  if(p < p_low) then
    q=dsqrt(-2.d0*dlog(p))
    inverse_normal_cdf=(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)/((((d1*q+d2)*q+d3)*q+d4)*q+1.d0)
  else if(p <= p_high) then
    q=p-0.5d0
    r=q*q
    inverse_normal_cdf=(((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q/(((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1.d0)
  else
    q=dsqrt(-2.d0*dlog(max(tiny(1.d0),1.d0-p)))
    inverse_normal_cdf=-(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)/((((d1*q+d2)*q+d3)*q+d4)*q+1)
  endif
end

