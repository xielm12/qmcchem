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
! VMC averages of $X
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



 BEGIN_PROVIDER [ double precision, E_loc_dmc_block_walk  ]
&BEGIN_PROVIDER [ double precision, E_loc_2_dmc_block_walk  ]
&BEGIN_PROVIDER [ double precision, E_loc_dmc_block_walk_kahan, (3) ]
&BEGIN_PROVIDER [ double precision, E_loc_2_dmc_block_walk_kahan, (3) 
 implicit none
 include '../types.F'
 BEGIN_DOC
! Properties averaged over the block using the DMC method
 END_DOC

  real, allocatable :: elec_coord_tmp(:,:,:)
  integer :: mod_align
  double precision, allocatable :: psi_grad_psi_inv_save_tmp(:,:,:)
  double precision :: psi_value_save_tmp(walk_num)
  integer :: trapped_walk_tmp(walk_num)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: psi_grad_psi_inv_save_tmp
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: psi_value_save_tmp
  allocate ( elec_coord_tmp(mod_align(elec_num+1),3,walk_num) )
  allocate ( psi_grad_psi_inv_save_tmp(elec_num_8,3,walk_num) )

! Initialization
 if (vmc_algo /= t_Brownian) then
   call abrt(irp_here,'DMC should run with Brownian algorithm')
 endif
 PROVIDE E_loc_vmc_block_walk

 integer :: k, i_walk, i_step

BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
 if (calc_$X) then
   $X_dmc_block_walk = 0.d0
   $X_dmc_block_walk_kahan = 0.d0
   $X_2_dmc_block_walk = 0.d0
   $X_2_dmc_block_walk_kahan = 0.d0
   $X_min = huge(1.)
   $X_max =-huge(1.)
 endif
"""
for p in properties:
 print  t.replace("$X",p[1])
END_SHELL

 double precision :: icount

 icount = 0.d0

 logical                        :: loop
 integer*8                      :: cpu0, cpu1, cpu2, count_rate, count_max

 loop = .True.
 call system_clock(cpu0, count_rate, count_max)
 cpu2 = cpu0
 do while (loop)
  dmc_projection_step = mod(dmc_projection_step+1,dmc_projection)+1

  pop_weight_mult *= 1.d0/pop_weight(dmc_projection_step)
  pop_weight(dmc_projection_step) = 0.d0
  do k=1,walk_num
    pop_weight(dmc_projection_step) += dmc_weight(k)
  enddo

  do k=1,walk_num
    dmc_weight(k) = dmc_weight(k)/pop_weight(dmc_projection_step)
  enddo

  pop_weight(dmc_projection_step) = pop_weight(dmc_projection_step)/dble(walk_num)
  pop_weight_mult *= pop_weight(dmc_projection_step)

BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
 if (calc_$X) then
! Kahan's summation algorithm to compute these sums reducing the rounding error:
!  $X_dmc_block_walk($D)   += $X * pop_weight_mult
!  $X_2_dmc_block_walk($D) += $X_2 * pop_weight_mult
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
  icount += pop_weight_mult

! Reconfiguration

  integer :: ipos(walk_num)
  call reconfigure(ipos,dmc_weight)
   
  do k=1,walk_num
    integer :: i, l
    do l=1,3
     do i=1,elec_num+1
      elec_coord_tmp(i,l,k) = elec_coord_full(i,l,k)
     enddo
     !DIR$ VECTOR ALIGNED
     !DIR$ LOOP COUNT(200)
     do i=1,elec_num
      psi_grad_psi_inv_save_tmp(i,l,k) = psi_grad_psi_inv_save(i,l,k)
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
    !DIR$ VECTOR ALIGNED
    !DIR$ LOOP COUNT(200)
    do i=1,elec_num
     psi_grad_psi_inv_save(i,l,k) = psi_grad_psi_inv_save_tmp(i,l,ipm)
    enddo
   enddo
   psi_value_save(k) = psi_value_save_tmp(ipm)
   trapped_walk(k) = trapped_walk_tmp(ipm)
  enddo

  ! Set 1st walker
  !DIR$ VECTOR ALIGNED
  !DIR$ LOOP COUNT(200)
  do i=1,elec_num
    psi_grad_psi_inv_x(i) = psi_grad_psi_inv_save(i,1,1)
    psi_grad_psi_inv_y(i) = psi_grad_psi_inv_save(i,2,1)
    psi_grad_psi_inv_z(i) = psi_grad_psi_inv_save(i,3,1)
  enddo

  !DIR$ VECTOR UNALIGNED
  !DIR$ LOOP COUNT(200)
  do i=1,elec_num
    elec_coord(i,1) = elec_coord_full(i,1,1)
    elec_coord(i,2) = elec_coord_full(i,2,1)
    elec_coord(i,3) = elec_coord_full(i,3,1)
  enddo
  psi_value = psi_value_save(1)

  TOUCH elec_coord_full psi_value_save psi_grad_psi_inv_save psi_value psi_grad_psi_inv_x psi_grad_psi_inv_y psi_grad_psi_inv_z elec_coord

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

 enddo



 double precision :: factor
 factor = 1.d0/icount
 block_weight *= icount
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
 deallocate ( psi_grad_psi_inv_save_tmp )

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
 pop_weight_mult = 1.d0
END_PROVIDER

 BEGIN_PROVIDER [ integer, dmc_projection ]
&BEGIN_PROVIDER [ integer, dmc_projection_step ]
 implicit none
 BEGIN_DOC  
! Number of projection steps for SRMC
 END_DOC
 dmc_projection = int( 10.d0/time_step)
 dmc_projection_step = 0
END_PROVIDER

BEGIN_PROVIDER [ double precision, pop_weight, (dmc_projection) ]
 implicit none
 BEGIN_DOC  
! Population weight of DMC
 END_DOC
 pop_weight = 1.d0
END_PROVIDER

