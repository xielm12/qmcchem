! Providers of *_vmc_block_walk
!==============================
BEGIN_SHELL [ /usr/bin/python ]
from properties import *

t = """
 BEGIN_PROVIDER [ $T, $X_vmc_block_walk $D1 ]
&BEGIN_PROVIDER [ $T, $X_vmc_block_walk_kahan $D2 ]
&BEGIN_PROVIDER [ $T, $X_2_vmc_block_walk $D1 ]
&BEGIN_PROVIDER [ $T, $X_2_vmc_block_walk_kahan $D2 ]
 implicit none
 BEGIN_DOC  
! VMC averages of $X
 END_DOC
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

 BEGIN_PROVIDER [ double precision, E_loc_vmc_block_walk ]
&BEGIN_PROVIDER [ double precision, E_loc_2_vmc_block_walk ]
&BEGIN_PROVIDER [ double precision, E_loc_vmc_block_walk_kahan, (3) ]
&BEGIN_PROVIDER [ double precision, E_loc_2_vmc_block_walk_kahan, (3) ]
 implicit none
 include '../types.F'
 BEGIN_DOC
! Properties averaged over the block per walker using the VMC method
 END_DOC

 integer :: i_walk

 PROVIDE time_step


BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
  if (calc_$X) then
   !DIR$ VECTOR ALIGNED
   $X_vmc_block_walk = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_vmc_block_walk_kahan = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_2_vmc_block_walk = 0.d0
   !DIR$ VECTOR ALIGNED
   $X_2_vmc_block_walk_kahan = 0.d0
   $X_min = huge(1.)
   $X_max =-huge(1.)
  endif
"""
for p in properties:
 print  t.replace("$X",p[1])
END_SHELL

 double precision :: dnorm
 !DIR$ VECTOR ALIGNED
 block_weight = 0.d0
 do i_walk=1,walk_num
   integer :: i,j,l
   if (i_walk > 1) then
     do l=1,3
      do i=1,elec_num+1
       elec_coord(i,l) = elec_coord_full(i,l,i_walk)
      enddo
     enddo
   endif
   
   PROVIDE psi_grad_psi_inv_save psi_value_save
   
   if (psi_value_save(walk_num) /= 0.) then 
       psi_value = psi_value_save(i_walk)
       !DIR$ VECTOR ALIGNED
       !DIR$ LOOP COUNT(200)
       do i=1,elec_num
         psi_grad_psi_inv_x(i) = psi_grad_psi_inv_save(i,1,i_walk)
         psi_grad_psi_inv_y(i) = psi_grad_psi_inv_save(i,2,i_walk)
         psi_grad_psi_inv_z(i) = psi_grad_psi_inv_save(i,3,i_walk)
       enddo
      TOUCH psi_value psi_grad_psi_inv_x psi_grad_psi_inv_y psi_grad_psi_inv_z elec_coord
   else
      if (i_walk > 1) then
        TOUCH elec_coord
      endif
   endif

   logical                        :: loop
   integer*8                      :: cpu0, cpu1, cpu2, count_rate, count_max
   loop = .True.

   call system_clock(cpu0, count_rate, count_max)
   cpu2 = cpu0
   do while (loop)
     double precision               :: p,q
     real                           :: delta_x
     logical                        :: accepted
     double precision               :: E_old
     if (vmc_algo == t_Brownian) then
       call brownian_step(p,q,accepted,delta_x)
     else if (vmc_algo == t_Langevin) then
       call langevin_step(p,q,accepted,delta_x)
     endif
     elec_coord(elec_num+1,1) += p*time_step
     elec_coord(elec_num+1,2) = E_loc
     elec_coord(elec_num+1,3) += p*time_step
     if (accepted) then
       trapped_walk(i_walk) = 0
     else
       trapped_walk(i_walk) += 1
     endif
   
     block_weight += 1.d0
   
BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
     if (calc_$X) then
   ! Kahan's summation algorithm to compute these sums reducing the rounding error:
   !  $X_vmc_block_walk $D1   += $X
   !  $X_2_vmc_block_walk $D1 += $X_2
   ! see http://en.wikipedia.org/wiki/Kahan_summation_algorithm
   
      $X_vmc_block_walk_kahan($D2 3) = $X - $X_vmc_block_walk_kahan($D2 1)
      $X_vmc_block_walk_kahan($D2 2) = $X_vmc_block_walk $D1  + $X_vmc_block_walk_kahan($D2 3)
      $X_vmc_block_walk_kahan($D2 1) = ($X_vmc_block_walk_kahan($D2 2) - $X_vmc_block_walk $D1 ) &
          - $X_vmc_block_walk_kahan($D2 3)
      $X_vmc_block_walk $D1  =  $X_vmc_block_walk_kahan($D2 2) 
   
   
      $X_2_vmc_block_walk_kahan($D2 3) = $X_2 - $X_2_vmc_block_walk_kahan($D2 1)
      $X_2_vmc_block_walk_kahan($D2 2) = $X_2_vmc_block_walk $D1 + $X_2_vmc_block_walk_kahan($D2 3)
      $X_2_vmc_block_walk_kahan($D2 1) = ($X_2_vmc_block_walk_kahan($D2 2) - $X_2_vmc_block_walk $D1 ) &
          - $X_2_vmc_block_walk_kahan($D2 3)
      $X_2_vmc_block_walk $D1 =  $X_2_vmc_block_walk_kahan($D2 2) 
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

   
     if ( qmc_method == t_VMC ) then
       call system_clock(cpu1, count_rate, count_max)
       if (cpu1 < cpu0) then
         cpu1 = cpu1+cpu0
       endif
       loop = dble(cpu1-cpu0)*dble(walk_num) < dble(block_time)*dble(count_rate)
       if (cpu1-cpu2 > count_rate) then
         integer                        :: do_run
         call get_running(do_run)
         loop = do_run == t_Running
         cpu2 = cpu1
       endif
     else
       loop = .False.
     endif

   enddo ! while (loop)

   do l=1,3
    do i=1,elec_num+1
     elec_coord_full(i,l,i_walk) = elec_coord(i,l)
    enddo
   enddo
   
   if (qmc_method == t_DMC) then
     psi_value_save(i_walk) = psi_value
     !DIR$ VECTOR ALIGNED
     !DIR$ LOOP COUNT (200)
     do i=1,elec_num
       psi_grad_psi_inv_save(i,1,i_walk) = psi_grad_psi_inv_x(i) 
       psi_grad_psi_inv_save(i,2,i_walk) = psi_grad_psi_inv_y(i) 
       psi_grad_psi_inv_save(i,3,i_walk) = psi_grad_psi_inv_z(i) 
     enddo

!     if ( (trapped_walk(i_walk) < trapped_walk_max).and. &
!          (psi_value * psi_value_save(i_walk) > 0.d0).and. &
!          (dabs(E_ref-E_loc)*time_step_sq < -.2d0*E_ref) ) then
     if ( (trapped_walk(i_walk) < trapped_walk_max).and. &
          (psi_value * psi_value_save(i_walk) > 0.d0) ) then
       dmc_weight(i_walk) = exp(time_step*(E_ref - E_loc))

     else
       dmc_weight(i_walk) = 0.d0
       trapped_walk(i_walk) = 0
     endif
   endif
 
 enddo


 double precision               :: factor
 factor = 1.d0/block_weight
 SOFT_TOUCH block_weight
BEGIN_SHELL [ /usr/bin/python ]
from properties import *
t = """
 if (calc_$X) then
   $X_vmc_block_walk *= factor
   $X_2_vmc_block_walk *= factor
 endif
"""
for p in properties:
 print  t.replace("$X",p[1])
END_SHELL

 SOFT_TOUCH elec_coord_full 

END_PROVIDER


BEGIN_PROVIDER [ double precision, dmc_weight, (walk_num_8) ]
 implicit none
 BEGIN_DOC
! Weight of the walkers in the DMC algorithm: exp(-time_step*(E_loc-E_ref))
 END_DOC
 !DIR$ VECTOR ALIGNED
 dmc_weight = 1.d0
END_PROVIDER




BEGIN_PROVIDER [ double precision, psi_grad_psi_inv_save, (elec_num_8,3,walk_num) ] 
&BEGIN_PROVIDER [ double precision, psi_value_save, (walk_num_8) ] 
 implicit none
 BEGIN_DOC  
! psi_grad_psi_inv of the previous step to accelerate DMC
! 
! updated in vmc_step
 END_DOC
 integer, save :: ifirst = 0
 if (ifirst == 0) then
   psi_grad_psi_inv_save = 0.d0
   psi_value_save = 0.d0
   ifirst = 1
 endif
END_PROVIDER                                                   

BEGIN_PROVIDER [ integer, trapped_walk, (walk_num_8) ]
 implicit none
 BEGIN_DOC  
! Number of steps when the walkers were trapped
 END_DOC
 trapped_walk = 0
END_PROVIDER

BEGIN_PROVIDER [ integer,  trapped_walk_max ]
 implicit none
 BEGIN_DOC  
! Max number of trapped MC steps before killing walker
 END_DOC
 trapped_walk_max = 5
END_PROVIDER

