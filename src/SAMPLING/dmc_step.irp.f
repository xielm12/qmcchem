! Providers of *_dmc_block_walk
!==============================
BEGIN_SHELL [ /usr/bin/python ]
from properties import *

t = """
 BEGIN_PROVIDER [ $T, $X_dmc_block_walk $D1 ]
&BEGIN_PROVIDER [ $T, $X_dmc_block_walk_kahan $D2 ]
&BEGIN_PROVIDER [ $T, $X_2_dmc_block_walk $D1 ]
&BEGIN_PROVIDER [ $T, $X_2_dmc_block_walk_kahan $D2 ]
 implicit none
 BEGIN_DOC  
! DMC averages of $X. Computed in E_loc_dmc_block_walk
 END_DOC
 $X_dmc_block_walk = 0.d0
 $X_dmc_block_walk_kahan = 0.d0
 $X_2_dmc_block_walk = 0.d0
 $X_2_dmc_block_walk_kahan = 0.d0
END_PROVIDER
"""
for p in properties:
  if p[1] != 'e_loc':
    if p[2] == "":
      D1 = ""
      D2 = ", (3)"
    else:
      D1 = ", ("+p[2][1:-1]+")"
      D2 = ", ("+p[2][1:-1]+",3)"
    print t.replace("$X",p[1]).replace("$T",p[0]).replace("$D1",D1).replace("$D2",D2)

END_SHELL



 BEGIN_PROVIDER [ double precision, E_loc_dmc_block_walk       ]
&BEGIN_PROVIDER [ double precision, E_loc_2_dmc_block_walk     ]
&BEGIN_PROVIDER [ double precision, E_loc_dmc_block_walk_kahan, (3) ]
&BEGIN_PROVIDER [ double precision, E_loc_2_dmc_block_walk_kahan, (3) ]
 implicit none
 include '../types.F'
 BEGIN_DOC
! Properties averaged over the block using the DMC method
 END_DOC

  real, allocatable :: elec_coord_tmp(:,:,:)
  integer :: mod_align
  double precision :: psi_value_save(walk_num)
  double precision :: psi_value_save_tmp(walk_num)
  integer :: trapped_walk_tmp(walk_num)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: psi_value_save
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: psi_value_save_tmp
  allocate ( elec_coord_tmp(mod_align(elec_num+1),3,walk_num) )
  psi_value_save = 0.d0

! Initialization
 if (vmc_algo /= t_Brownian) then
   call abrt(irp_here,'DMC should run with Brownian algorithm')
 endif

 integer :: k, i_walk, i_step

BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
 if (calc_$X) then
   !DIR$ VECTOR ALIGNED
   $X_dmc_block_walk = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_dmc_block_walk_kahan = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_2_dmc_block_walk = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_2_dmc_block_walk_kahan = 0.d0
   $X_min = huge(1.)
   $X_max =-huge(1.)
 endif
"""
for p in properties:
 print  t.replace("$X",p[1])
END_SHELL

 logical                        :: loop
 integer*8                      :: cpu0, cpu1, cpu2, count_rate, count_max

 loop = .True.
 call system_clock(cpu0, count_rate, count_max)
 cpu2 = cpu0

 block_weight = 0.d0

 do while (loop)

  ! Every walker makes a step
  do i_walk=1,walk_num
    integer :: i,j,l
    do l=1,3
      do i=1,elec_num+1
        elec_coord(i,l) = elec_coord_full(i,l,i_walk)
      enddo
    enddo
    TOUCH elec_coord

BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
     if (calc_$X) then
   ! Kahan's summation algorithm to compute these sums reducing the rounding error:
   !  $X_dmc_block_walk    += $X * pop_weight_mult
   !  $X_2_dmc_block_walk  += $X_2 * pop_weight_mult
   ! see http://en.wikipedia.org/wiki/Kahan_summation_algorithm
   
      $X_dmc_block_walk_kahan($D2 3) = $X * pop_weight_mult - $X_dmc_block_walk_kahan($D2 1)
      $X_dmc_block_walk_kahan($D2 2) = $X_dmc_block_walk $D1  + $X_dmc_block_walk_kahan($D2 3)
      $X_dmc_block_walk_kahan($D2 1) = ($X_dmc_block_walk_kahan($D2 2) - $X_dmc_block_walk $D1 ) &
          - $X_dmc_block_walk_kahan($D2 3)
      $X_dmc_block_walk $D1  =  $X_dmc_block_walk_kahan($D2 2) 
   
   
      $X_2_dmc_block_walk_kahan($D2 3) = $X_2 * pop_weight_mult - $X_2_dmc_block_walk_kahan($D2 1)
      $X_2_dmc_block_walk_kahan($D2 2) = $X_2_dmc_block_walk $D1 + $X_2_dmc_block_walk_kahan($D2 3)
      $X_2_dmc_block_walk_kahan($D2 1) = ($X_2_dmc_block_walk_kahan($D2 2) - $X_2_dmc_block_walk $D1 ) &
          - $X_2_dmc_block_walk_kahan($D2 3)
      $X_2_dmc_block_walk $D1 =  $X_2_dmc_block_walk_kahan($D2 2) 
     endif
"""
for p in properties:
  if p[2] == "":
   D1 = ""
   D2 = ""
  else:
   D1 = "("+":"*(p[2].count(',')+1)+")"
   D2 = ":"*(p[2].count(',')+1)+","
  print t.replace("$X",p[1]).replace("$D1",D1).replace("$D2",D2)

END_SHELL


   ! Brownian step
   double precision               :: p,q
   real                           :: delta_x
   logical                        :: accepted
   call brownian_step(p,q,accepted,delta_x)
   if (accepted) then
      trapped_walk(i_walk) = 0
   else
      trapped_walk(i_walk) += 1
   endif

   if ( (trapped_walk(i_walk) < trapped_walk_max).and. &
        (psi_value * psi_value_save(i_walk) >= 0.d0) ) then
     dmc_weight(i_walk) = dexp(dtime_step*(E_ref - E_loc))
   else
     dmc_weight(i_walk) = 0.d0
     trapped_walk(i_walk) = 0
   endif
   
   elec_coord(elec_num+1,1) += p*time_step
   elec_coord(elec_num+1,2)  = E_loc
   elec_coord(elec_num+1,3)  = dmc_weight(i_walk)
   do l=1,3
      do i=1,elec_num+1
        elec_coord_full(i,l,i_walk) = elec_coord(i,l)
      enddo
   enddo

   psi_value_save(i_walk) = psi_value

  enddo

  ! Move to the next projection step
  if (dmc_projection > 0) then
    dmc_projection_step = mod(dmc_projection_step,dmc_projection)+1
  else
    dmc_projection_step = 1
  endif

  ! Eventually, recompute the weight of the population
  if (dmc_projection_step == 1) then
    pop_weight_mult = 1.d0
    do k=1,dmc_projection
      pop_weight_mult *= pop_weight(k)
    enddo
  endif

  ! Remove contribution of the old value of the weight at the new
  ! projection step
  pop_weight_mult *= 1.d0/pop_weight(dmc_projection_step)

  ! Compute the new weight of the population
  double precision :: sum_weight
  sum_weight = 0.d0
  do k=1,walk_num
    sum_weight += dmc_weight(k)
  enddo
  pop_weight(dmc_projection_step) = sum_weight/dble(walk_num)

  ! Update the running population weight
  pop_weight_mult *= pop_weight(dmc_projection_step)

  block_weight += pop_weight_mult * dble(walk_num)

! Reconfiguration
  integer :: ipos(walk_num)

  call reconfigure(ipos,dmc_weight)
   
  do k=1,walk_num
    do l=1,3
     do i=1,elec_num+1
      elec_coord_tmp(i,l,k) = elec_coord_full(i,l,k)
     enddo
    enddo
    psi_value_save_tmp(k) = psi_value_save(k)
    trapped_walk_tmp(k) = trapped_walk(k)
  enddo
  
  integer :: ipm
  do k=1,walk_num
   ipm = ipos(k)
   do l=1,3
    do i=1,elec_num+1
     elec_coord_full(i,l,k) = elec_coord_tmp(i,l,ipm)
    enddo
   enddo
   psi_value_save(k) = psi_value_save_tmp(ipm)
   trapped_walk(k) = trapped_walk_tmp(ipm)
  enddo

  call system_clock(cpu1, count_rate, count_max)
  if (cpu1 < cpu0) then
    cpu1 = cpu1+cpu0
  endif
  loop = dble(cpu1-cpu0) < dble(block_time)*dble(count_rate)
  if (cpu1-cpu2 > count_rate) then
    integer                        :: do_run
    call get_running(do_run)
    loop = do_run == t_Running
    cpu2 = cpu1
  endif

  SOFT_TOUCH elec_coord_full psi_value psi_grad_psi_inv_x psi_grad_psi_inv_y psi_grad_psi_inv_z elec_coord pop_weight_mult

 enddo

 double precision :: factor
 factor = 1.d0/block_weight
 SOFT_TOUCH block_weight

BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
 if (calc_$X) then
   $X_dmc_block_walk   *= factor
   $X_2_dmc_block_walk *= factor
 endif
"""
for p in properties:
 print  t.replace("$X",p[1])
END_SHELL

 deallocate ( elec_coord_tmp )

END_PROVIDER


BEGIN_PROVIDER [ double precision, E_ref ]
  implicit none
  BEGIN_DOC  
!  Weight  of the DMC population
  END_DOC
  E_ref = 0.d0
  call get_simulation_E_ref(E_ref)
END_PROVIDER

BEGIN_PROVIDER [ double precision, pop_weight_mult ]
 implicit none
 BEGIN_DOC  
! Population weight of DMC
 END_DOC
 pop_weight_mult = pop_weight(dmc_projection)
END_PROVIDER

 BEGIN_PROVIDER [ integer, dmc_projection ]
&BEGIN_PROVIDER [ integer, dmc_projection_step ]
 implicit none
 BEGIN_DOC  
! Number of projection steps for SRMC
 END_DOC
 real :: dmc_projection_time
 dmc_projection_time = 1.
 call get_simulation_dmc_projection_time(dmc_projection_time)
 dmc_projection = int( dmc_projection_time/time_step)
 dmc_projection_step = 0
END_PROVIDER

BEGIN_PROVIDER [ double precision, pop_weight, (0:dmc_projection+1) ]
 implicit none
 BEGIN_DOC  
! Population weight of DMC
 END_DOC
 pop_weight = 1.d0
 pop_weight(dmc_projection) = 1.d0/dble(size(pop_weight))
END_PROVIDER

 BEGIN_PROVIDER [ integer, trapped_walk, (walk_num_8) ]
&BEGIN_PROVIDER [ integer,  trapped_walk_max ]
 implicit none
 BEGIN_DOC  
! Number of steps when the walkers were trapped
 END_DOC
 trapped_walk = 0
 trapped_walk_max = 20
END_PROVIDER


BEGIN_PROVIDER [ double precision, dmc_weight, (walk_num_8) ]
 implicit none
 BEGIN_DOC
! Weight of the walkers in the DMC algorithm: exp(-time_step*(E_loc-E_ref))
 END_DOC
 !DIR$ VECTOR ALIGNED
 dmc_weight = 1.d0
END_PROVIDER


