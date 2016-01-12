BEGIN_SHELL [ /usr/bin/python ]

data = [ \
("electrons_elec_coord_pool_size"  , "integer"       , "" ),
("electrons_elec_coord_pool"       , "real"          , "(elec_num+1,3,elec_coord_pool_size)" ),
("nuclei_nucl_num"                 , "integer"       , ""                      ),
("nuclei_nucl_charge"              , "real"          , "(nucl_num)"            ),
("nuclei_nucl_coord"               , "real"          , "(nucl_num,3)"          ),
("nuclei_nucl_fitcusp_radius"      , "real"          , "(nucl_num)"            ),
("mo_basis_mo_coef"                , "real"          , "(ao_num,mo_tot_num)" ),
("electrons_elec_fitcusp_radius"   , "real"          , ""                      ),
("electrons_elec_alpha_num"        , "integer"       , ""                      ),
("electrons_elec_beta_num"         , "integer"       , ""                      ),
("electrons_elec_walk_num"         , "integer"       , ""                      ),
("electrons_elec_walk_num_tot"     , "integer"       , ""                      ),
("ao_basis_ao_num"                 , "integer"       , ""                      ),
("ao_basis_ao_prim_num"            , "integer"       , "(ao_num)"              ),
("ao_basis_ao_nucl"                , "integer"       , "(ao_num)"              ),
("ao_basis_ao_power"               , "integer"       , "(ao_num,3)"            ),
("ao_basis_ao_expo"                , "real"          , "(ao_num,ao_prim_num_max)" ),
("ao_basis_ao_coef"                , "real"          , "(ao_num,ao_prim_num_max)" ),
("jastrow_jast_a_up_up"            , "real"          , ""                   ),
("jastrow_jast_a_up_dn"            , "real"          , ""                   ),
("jastrow_jast_b_up_up"            , "real"          , ""                   ),
("jastrow_jast_b_up_dn"            , "real"          , ""                   ),
("jastrow_jast_pen"                , "real"          , "(nucl_num)"          ),
("jastrow_jast_eeN_e_a"            , "real"          , ""                   ),
("jastrow_jast_eeN_e_b"            , "real"          , ""                   ),
("jastrow_jast_eeN_N"              , "real"          , "(nucl_num)"          ),
("jastrow_jast_core_a1"            , "real"          , "(nucl_num)"          ),
("jastrow_jast_core_a2"            , "real"          , "(nucl_num)"          ),
("jastrow_jast_core_b1"            , "real"          , "(nucl_num)"          ),
("jastrow_jast_core_b2"            , "real"          , "(nucl_num)"          ),
("jastrow_jast_type"               , "character*(32)", ""                      ),
("simulation_stop_time"            , "integer"       , ""                      ),
("simulation_equilibration"        , "logical"       , ""                      ),
("simulation_block_time"           , "integer"       , ""                      ),
("simulation_time_step"            , "real"          , ""                      ),
("simulation_dmc_projection_time"  , "real"          , ""                      ),
("simulation_method"               , "character*(32)", ""                      ),
("simulation_save_data"            , "logical"       , ""                      ),
("simulation_print_level"          , "integer"       , ""                      ),
("simulation_do_nucl_fitcusp"      , "logical"       , ""                      ),
("simulation_sampling"             , "character*(32)", ""                      ),
("simulation_ci_threshold"         , "double precision"          , ""                      ),
("simulation_http_server"          , "character*(128)", ""                      ),
("simulation_md5_key"              , "character*(32)" , ""                     ),
("simulation_e_ref"                , "double precision" , ""                   ),
("simulation_do_run"               , "logical       " , ""                   ),
("pseudo_do_pseudo"         , "logical       " , ""                   ),

]

data_no_set = [\
("mo_basis_mo_tot_num"     ,  "integer"           ,  ""),
("mo_basis_mo_active_num"  ,  "integer"           ,  ""),
("mo_basis_mo_closed_num"  ,  "integer"           ,  ""),
("pseudo_ao_pseudo_grid"   ,  "double precision"  ,  "(ao_num,pseudo_lmax+pseudo_lmax+1,pseudo_lmax-0+1,nucl_num,pseudo_grid_size)"),
("pseudo_mo_pseudo_grid"   ,  "double precision"  ,  "(ao_num,pseudo_lmax+pseudo_lmax+1,pseudo_lmax-0+1,nucl_num,pseudo_grid_size)"),
("pseudo_pseudo_dz_k"      ,  "double precision"  ,  "(nucl_num,pseudo_klocmax)"),
("pseudo_pseudo_dz_kl"     ,  "double precision"  ,  "(nucl_num,pseudo_kmax,pseudo_lmax+1)"),
("pseudo_pseudo_grid_rmax" ,  "double precision"  ,  ""),
("pseudo_pseudo_grid_size" ,  "integer"           ,  ""),
("pseudo_pseudo_klocmax"   ,  "integer"           ,  ""),
("pseudo_pseudo_kmax"      ,  "integer"           ,  ""),
("pseudo_pseudo_lmax"      ,  "integer"           ,  ""),
("pseudo_pseudo_n_k"       ,  "integer"           ,  "(nucl_num,pseudo_klocmax)"),
("pseudo_pseudo_n_kl"      ,  "integer"           ,  "(nucl_num,pseudo_kmax,pseudo_lmax+1)"),
("pseudo_pseudo_v_k"       ,  "double precision"  ,  "(nucl_num,pseudo_klocmax)"),
("pseudo_pseudo_v_kl"      ,  "double precision"  ,  "(nucl_num,pseudo_kmax,pseudo_lmax+1)"),
("spindeterminants_n_det_alpha"              ,  "integer"           ,  ""),
("spindeterminants_n_det_beta"               ,  "integer"           ,  ""),
("spindeterminants_n_det"                    ,  "integer"           ,  ""),
("spindeterminants_n_int"                    ,  "integer"           ,  ""),
("spindeterminants_bit_kind"                 ,  "integer"           ,  ""),
("spindeterminants_n_states"                 ,  "integer"           ,  ""),
("spindeterminants_psi_det_alpha"            ,  "integer*8"         ,  "(N_int*bit_kind/8,det_alpha_num)"),
("spindeterminants_psi_det_beta"             ,  "integer*8"         ,  "(N_int*bit_kind/8,det_beta_num)"),
("spindeterminants_psi_coef_matrix_rows"     ,  "integer"           ,  "(det_num_input)"),
("spindeterminants_psi_coef_matrix_columns"  ,  "integer"           ,  "(det_num_input)"),
("spindeterminants_psi_coef_matrix_values"   ,  "double precision"  ,  "(det_num_input,N_states)"),


]

def do_subst(t0,d):
  t = t0
  t = t.replace("$X",d[0])
  t = t.replace("$T",d[1])
  t = t.replace("$D",d[2])
  if d[1].startswith("character"):
    size = d[1].split("*")[1][1:-1]
    u = "character"
  elif d[1].startswith("double precision"):
    u = d[1].replace(" ","_")
    size = "1"
  elif "*" in d[1]:
    size = "1"
    u = d[1].replace("*","")
  else:
     size = "1"
     u = d[1]
  t = t.replace("$U",u)
  if d[2] == "":
    t = t.replace("$S",size)
  else:
    if size == "1":
      t = t.replace("$S","size(res)")
    else:
      t = t.replace("$S","%s*size(res)"%(size))
  provide = ""
  tmp = d[2].replace('(','').replace(')','')
  for i in "+-*/":
    tmp = tmp.replace(i,',')
  for i in tmp.split(','):
    if ":" in i:
      i = i.split(':')[1]
    try:
     eval(i+"+1")
    except NameError:
     provide += "  PROVIDE "+i+"\n"
  t = t.replace("$P",provide)
  print  t
  
t0 = """
subroutine get_$X(res)
  implicit none
  BEGIN_DOC
! Calls EZFIO subroutine to get $X
  END_DOC
  $T                             :: res$D
  integer                        :: ierr, i
  logical                        :: exists
  PROVIDE ezfio_filename
  $P
  if (.not.is_worker) then
    call ezfio_has_$X(exists)
    if (exists) then
      call ezfio_get_$X(res)
      call ezfio_free_$X
    else
      call ezfio_set_$X(res)
    endif
  else
    call zmq_ezfio_has('$X',exists)
    if (exists) then
      call zmq_ezfio_get_$U('$X',res,$S)
    endif
  endif
  
end
"""


t1 = """
subroutine get_$X(res)
  implicit none
  BEGIN_DOC
! Calls EZFIO subroutine to get $X
  END_DOC
  $T                             :: res$D
  integer                        :: ierr
  PROVIDE ezfio_filename
  $P
  if (.not.is_worker) then
    call ezfio_get_$X(res)
    call ezfio_free_$X
  else
    call zmq_ezfio_get_$U('$X',res,$S)
  endif
  
end
"""

for i,d in enumerate(data):
  do_subst(t0,d)

for i,d in enumerate(data_no_set):
  i += len(data)
  do_subst(t1,d)

END_SHELL

