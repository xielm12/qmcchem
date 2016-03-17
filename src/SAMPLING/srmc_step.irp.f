! Providers of *_srmc_block_walk
!==============================
BEGIN_SHELL [ /usr/bin/python ]
from properties import *

t = """
 BEGIN_PROVIDER [ $T, $X_srmc_block_walk $D1 ]
&BEGIN_PROVIDER [ $T, $X_srmc_block_walk_kahan $D2 ]
&BEGIN_PROVIDER [ $T, $X_2_srmc_block_walk $D1 ]
&BEGIN_PROVIDER [ $T, $X_2_srmc_block_walk_kahan $D2 ]
 implicit none
 BEGIN_DOC  
! SRMC averages of $X. Computed in E_loc_srmc_block_walk
 END_DOC
 $X_srmc_block_walk = 0.d0
 $X_srmc_block_walk_kahan = 0.d0
 $X_2_srmc_block_walk = 0.d0
 $X_2_srmc_block_walk_kahan = 0.d0
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



 BEGIN_PROVIDER [ double precision, E_loc_srmc_block_walk       ]
&BEGIN_PROVIDER [ double precision, E_loc_2_srmc_block_walk     ]
&BEGIN_PROVIDER [ double precision, E_loc_srmc_block_walk_kahan, (3) ]
&BEGIN_PROVIDER [ double precision, E_loc_2_srmc_block_walk_kahan, (3) ]
 implicit none
 include '../types.F'
 BEGIN_DOC
! Properties averaged over the block using the SRMC method
 END_DOC

  real, allocatable :: elec_coord_tmp(:,:,:)
  integer :: mod_align
  double precision :: E_loc_save(walk_num_dmc_max)
  double precision :: E_loc_save_tmp(walk_num_dmc_max)
  double precision :: psi_value_save(walk_num)
  double precision :: psi_value_save_tmp(walk_num)
  double precision :: srmc_weight(walk_num)
  double precision, allocatable :: psi_grad_psi_inv_save(:,:,:)
  double precision, allocatable :: psi_grad_psi_inv_save_tmp(:,:,:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: psi_grad_psi_inv_save
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: psi_grad_psi_inv_save_tmp
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: E_loc_save
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: E_loc_save_tmp
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: psi_value_save
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: psi_value_save_tmp
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: srmc_weight
  allocate ( psi_grad_psi_inv_save(elec_num_8,3,walk_num) ,          &
       psi_grad_psi_inv_save_tmp(elec_num_8,3,walk_num) ,            &
      elec_coord_tmp(mod_align(elec_num+1),3,walk_num) )
  psi_value_save = 0.d0
  psi_value_save_tmp = 0.d0
  srmc_weight = 1.d0

! Initialization
 if (vmc_algo /= t_Brownian) then
   call abrt(irp_here,'SRMC should run with Brownian algorithm')
 endif

 integer :: k, i_walk, i_step

BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
 if (calc_$X) then
   !DIR$ VECTOR ALIGNED
   $X_srmc_block_walk = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_srmc_block_walk_kahan = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_2_srmc_block_walk = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_2_srmc_block_walk_kahan = 0.d0
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

 real, external                 :: accep_rate
 double precision               :: delta, thr

 thr = 2.d0/time_step_sq

 logical :: first_loop
 first_loop = .True.

 do while (loop)

  ! Every walker makes a step
  do i_walk=1,walk_num
    
    if (.not.first_loop) then
      integer                        :: i,j,l
      do l=1,3
        do i=1,elec_num+1
          elec_coord(i,l) = elec_coord_full(i,l,i_walk)
        enddo
        do i=1,elec_num
          psi_grad_psi_inv_x(i) = psi_grad_psi_inv_save(i,1,i_walk)
          psi_grad_psi_inv_y(i) = psi_grad_psi_inv_save(i,2,i_walk)
          psi_grad_psi_inv_z(i) = psi_grad_psi_inv_save(i,3,i_walk)
        enddo
        psi_value = psi_value_save(i_walk)
        E_loc = E_loc_save(i_walk)
      enddo
      SOFT_TOUCH elec_coord psi_grad_psi_inv_x psi_grad_psi_inv_y psi_grad_psi_inv_z psi_value E_loc
    else
      do l=1,3
        do i=1,elec_num+1
          elec_coord(i,l) = elec_coord_full(i,l,i_walk)
        enddo
      enddo
      TOUCH elec_coord
      psi_value_save(i_walk) = psi_value
      E_loc_save(i_walk) = E_loc
    endif

   double precision               :: p,q
   real                           :: delta_x
   logical                        :: accepted
   call brownian_step(p,q,accepted,delta_x)

   if ( psi_value * psi_value_save(i_walk) >= 0.d0 ) then
     delta = ((E_loc+E_loc_save(i_walk))*0.5d0 - E_ref) * p
     if ( delta > thr ) then
       srmc_weight(i_walk) = dexp(-dtime_step*thr)
     else if ( delta < -thr ) then
       srmc_weight(i_walk) = dexp(dtime_step*thr)
     else
       srmc_weight(i_walk) = dexp(-dtime_step*delta)
     endif
     elec_coord(elec_num+1,1) += p*time_step
     elec_coord(elec_num+1,2)  = E_loc
     elec_coord(elec_num+1,3)  = srmc_weight(i_walk) * pop_weight_mult
     do l=1,3
        do i=1,elec_num+1
          elec_coord_full(i,l,i_walk) = elec_coord(i,l)
        enddo
     enddo
     do i=1,elec_num
       psi_grad_psi_inv_save(i,1,i_walk) = psi_grad_psi_inv_x(i)
       psi_grad_psi_inv_save(i,2,i_walk) = psi_grad_psi_inv_y(i)
       psi_grad_psi_inv_save(i,3,i_walk) = psi_grad_psi_inv_z(i)
     enddo

     psi_value_save(i_walk) = psi_value
     E_loc_save(i_walk) = E_loc

BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
     if (calc_$X) then
   ! Kahan's summation algorithm to compute these sums reducing the rounding error:
   !  $X_srmc_block_walk    += $X * pop_weight_mult * srmc_weight(i_walk)
   !  $X_2_srmc_block_walk  += $X_2 * pop_weight_mult * srmc_weight(i_walk)
   ! see http://en.wikipedia.org/wiki/Kahan_summation_algorithm
   
      $X_srmc_block_walk_kahan($D2 3) = $X * pop_weight_mult * srmc_weight(i_walk) - $X_srmc_block_walk_kahan($D2 1)
      $X_srmc_block_walk_kahan($D2 2) = $X_srmc_block_walk $D1  + $X_srmc_block_walk_kahan($D2 3)
      $X_srmc_block_walk_kahan($D2 1) = ($X_srmc_block_walk_kahan($D2 2) - $X_srmc_block_walk $D1 ) &
          - $X_srmc_block_walk_kahan($D2 3)
      $X_srmc_block_walk $D1  =  $X_srmc_block_walk_kahan($D2 2) 
   
   
      $X_2_srmc_block_walk_kahan($D2 3) = $X_2 * pop_weight_mult * srmc_weight(i_walk) - $X_2_srmc_block_walk_kahan($D2 1)
      $X_2_srmc_block_walk_kahan($D2 2) = $X_2_srmc_block_walk $D1 + $X_2_srmc_block_walk_kahan($D2 3)
      $X_2_srmc_block_walk_kahan($D2 1) = ($X_2_srmc_block_walk_kahan($D2 2) - $X_2_srmc_block_walk $D1 ) &
          - $X_2_srmc_block_walk_kahan($D2 3)
      $X_2_srmc_block_walk $D1 =  $X_2_srmc_block_walk_kahan($D2 2) 
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

    block_weight += pop_weight_mult * srmc_weight(i_walk)

   else
     srmc_weight(i_walk) = 0.d0
   endif
   

  enddo

  ! Move to the next projection step
  if (srmc_projection > 0) then
    srmc_projection_step = mod(srmc_projection_step,srmc_projection)+1
  else
    srmc_projection_step = 1
  endif

  ! Eventually, recompute the weight of the population
  if (srmc_projection_step == 1) then
    pop_weight_mult = 1.d0
    do k=1,srmc_projection
      pop_weight_mult *= pop_weight(k)
    enddo
  endif

  ! Remove contribution of the old value of the weight at the new
  ! projection step
  pop_weight_mult *= 1.d0/pop_weight(srmc_projection_step)

  ! Compute the new weight of the population
  double precision :: sum_weight
  sum_weight = 0.d0
  do k=1,walk_num
    sum_weight += srmc_weight(k)
  enddo
  pop_weight(srmc_projection_step) = sum_weight/dble(walk_num)

  ! Update the running population weight
  pop_weight_mult *= pop_weight(srmc_projection_step)

! Reconfiguration
  integer :: ipos(walk_num)

  call reconfigure(ipos,srmc_weight)
   
  do k=1,walk_num
    do l=1,3
     do i=1,elec_num+1
      elec_coord_tmp(i,l,k) = elec_coord_full(i,l,k)
     enddo
     do i=1,elec_num
      psi_grad_psi_inv_save_tmp(i,l,k) = psi_grad_psi_inv_save(i,l,k)
     enddo
    enddo
    psi_value_save_tmp(k) = psi_value_save(k)
    E_loc_save_tmp(k) = E_loc_save(k)
  enddo
  
  integer :: ipm
  do k=1,walk_num
   ipm = ipos(k)
   do l=1,3
    do i=1,elec_num+1
     elec_coord_full(i,l,k) = elec_coord_tmp(i,l,ipm)
    enddo
    do i=1,elec_num
      psi_grad_psi_inv_save(i,l,k) = psi_grad_psi_inv_save_tmp(i,l,ipm)
    enddo
   enddo
   psi_value_save(k) = psi_value_save_tmp(ipm)
   E_loc_save(k) = E_loc_save_tmp(ipm)
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

  SOFT_TOUCH elec_coord_full pop_weight_mult

  first_loop = .False.

 enddo

 double precision :: factor
 factor = 1.d0/block_weight
 SOFT_TOUCH block_weight

BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
 if (calc_$X) then
   $X_srmc_block_walk   *= factor
   $X_2_srmc_block_walk *= factor
 endif
"""
for p in properties:
 print  t.replace("$X",p[1])
END_SHELL

 deallocate ( elec_coord_tmp, psi_grad_psi_inv_save, psi_grad_psi_inv_save_tmp )

END_PROVIDER


BEGIN_PROVIDER [ double precision, pop_weight_mult ]
 implicit none
 BEGIN_DOC  
! Population weight of SRMC
 END_DOC
 pop_weight_mult = pop_weight(srmc_projection)
END_PROVIDER

 BEGIN_PROVIDER [ integer, srmc_projection ]
&BEGIN_PROVIDER [ integer, srmc_projection_step ]
 implicit none
 BEGIN_DOC  
! Number of projection steps for SRMC
 END_DOC
 real :: srmc_projection_time
 srmc_projection_time = 1.
 call get_simulation_srmc_projection_time(srmc_projection_time)
 srmc_projection = int( srmc_projection_time/time_step)
 srmc_projection_step = 0
END_PROVIDER

BEGIN_PROVIDER [ double precision, pop_weight, (0:srmc_projection+1) ]
 implicit none
 BEGIN_DOC  
! Population weight of SRMC
 END_DOC
 pop_weight = 1.d0
END_PROVIDER


