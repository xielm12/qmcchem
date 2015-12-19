subroutine reconfigure(ipos,w)
  implicit none
  integer, intent(inout)         :: ipos(*)
  double precision, intent(in)   :: w(*)
  
  integer                        :: kp, km
  double precision               :: accup, accum
  integer                        :: k
  
  double precision               :: dwalk_num
  dwalk_num = dble(walk_num)
  
  integer                        :: kptab(walk_num), kmtab(walk_num)
  double precision               :: wp(walk_num), wm(walk_num)
  double precision               :: tmp
  
  do k=1,walk_num
    ipos(k) = k
  enddo
  
  kp=0
  km=0
  accup = 0.d0
  accum = 0.d0
  do k=1,walk_num
    tmp = dwalk_num*w(k)-1.d0
    if (tmp >= 0.d0) then
      kp += 1
      wp(kp) = abs(tmp)
      accup += wp(kp)
      kptab(kp) = k
    else
      km += 1
      wm(km) = abs(tmp)
      accum += wm(km)
      kmtab(km) = k
    endif
  enddo
  if(kp+km /= walk_num) then
    print *,  kp, km
    call abrt(irp_here,'pb in reconfiguration +/-')
  endif
  if(abs(accup-accum).gt.1.d-11) then
    print *,  accup, accum
    call abrt(irp_here,'pb in reconfiguration')
  endif
  
  double precision               :: qmc_ranf, rand
  double precision               :: rando(walk_num)
  rand = qmc_ranf()
  do k=1,walk_num
    rando(k) = dble(k-1)+rand
  enddo
  
  double precision               :: averageconf, current
  integer                        :: kcp
  integer                        :: kadd, kremove
  
  averageconf = accup
  kcp = 1
  rand = rando(kcp)
  do while (rand < averageconf)
    k=1
    current=wm(k)
    do while (rand > current)
      k += 1
      current += wm(k)
    enddo
    kremove = kmtab(k)
    
    k=1
    current=wp(k)
    do while (rand > current)
      k += 1
      current += wp(k)
    enddo
    kadd = kptab(k)
    ipos(kremove) = kadd
    kcp += 1
    rand = rando(kcp)
  enddo
  
end

