include ../../../mozip.inc

PWD=${shell pwd}
THISFLAG=$(FFLAG) $(DFLAG) -I$(INCDIR)
THISFLAGP=$(FFLAG) $(DFLAG) -I$(INCDIR) $(DFLAG)
INCDIR=../../../include
#-------------------------------------------------------------------------------

libpath=../..

staticlib=libflib.${STATICEXT}
sharedlib=libflib.${SHAREDEXT}

ifeq ($(PARA),YES)
  PARAMOD=mpi_kind.mod
  PARAOBJ=mpi_kind.${OBJEXT}
  MPIOBJ=mpi_write_log.${OBJEXT}   get_mpi_node_points.${OBJEXT}
  MPISCR=mpi_write_log.f90 get_mpi_node_points.f90
else
  PARAMOD=
  PARAOBJ=
  MPIOBJ=
  MPISCR=
  FCP=$(FC)
endif

mods=                                     \
  kinds.mod                               \
  param.mod                               \
  symmetry.mod                            \
  symmetry1.mod                           \
  string.mod                              \
  string_utility.mod                      \
  file.mod                                \
  qm_file.mod                             \
  math.mod                                \
  array.mod                               \
  cmd_progress.mod                        \
  cipher.mod $(PARAMOD)

PRE_COMPILED=                             \
  kinds.${OBJEXT}                         \
  constants.${OBJEXT}                     \
  param.${OBJEXT}                         \
  random_numbers.${OBJEXT}                \
  string.${OBJEXT}                        \
  string_utility.${OBJEXT}                \
  math.${OBJEXT}                          \
  file.${OBJEXT}                          \
  qm_file.${OBJEXT}                       \
  array.${OBJEXT}                         \
  cmd_progress.${OBJEXT}                  \
  cipher.${OBJEXT} $(PARAOBJ)

PROJ_HESS_SRC=                            \
  com.f90                                 \
  center_of_mass1.f90                     \
  dig.f                                   \
  dzero.f                                 \
  matab.f                                 \
  prntmat.f                               \
  projtrm.f90                             \
  projtrm_rot.f90                         \
  sprod.f                                 \
  vscal.f

PROJ_HESS_OBJ=                            \
  com.${OBJEXT}                           \
  center_of_mass1.${OBJEXT}               \
  dig.${OBJEXT}                           \
  dzero.${OBJEXT}                         \
  matab.${OBJEXT}                         \
  prntmat.${OBJEXT}                       \
  projtrm.${OBJEXT}                       \
  projtrm_rot.${OBJEXT}                   \
  sprod.${OBJEXT}                         \
  vscal.${OBJEXT}

FILESRCS=                                 \
  symmetry.f90                            \
  symmetry1.f90                           \
  a_to_lattice_constants.f90              \
  a2b.f90                                 \
  aswap.f90                               \
  broaden.f90                             \
  broaden_spec.f90                        \
  c_to_cm.f90                             \
  celldm_to_a.f90                         \
  celldm_to_lattice_constants.f90         \
  char_to_int.f90                         \
  char_to_real.f90                        \
  check_int.f90                           \
  check_license.f90                       \
  check_license_debug.f90                 \
  check_sym_matrix.f90                    \
  check_x.f90                             \
  coord_crys_to_cart.f90                  \
  coord_cart_to_crys.f90                  \
  change_phase.f90                        \
  copy_matrix.f90                         \
  ddiag.for                               \
  diag_symm.f90                           \
  disp.f90                                \
  del_char.f90                            \
  del_comment.f90                         \
  del_comment1.f90                        \
  del_space.f90                           \
  del_x_atom.f90                          \
  delta.f90                               \
  determinant3.f90                        \
  dorder.f90                              \
  eckart.f90                              \
  eos_fitting.f90                         \
  error.f90                               \
  extract_filename_xyz.f90                \
  extract_input.f90                       \
  fc_to_fcm.f90                           \
  file.f90                                \
  file_copy.f90                           \
  file_delete.f90                         \
  file_num_lines.f90                      \
  find_int_crd.f90                        \
  full_to_short_name.f90                  \
  gaussj.for                              \
  gaussj1.for                             \
  get_area.f90                            \
  get_array3.f90                          \
  get_array3s.f90                         \
  get_array9.f90                          \
  get_au.f90                              \
  get_au_inverse.f90                      \
  get_bond.f90                            \
  get_bond1.f90                           \
  get_bond_num.f90                        \
  get_coul.f90                            \
  get_dos.f90                             \
  get_dos_fix.f90                         \
  get_dgrid3.f90                          \
  get_dv.f90                              \
  get_elements.f90                        \
  get_first_mac.f90                       \
  get_first_mac_file.f90                  \
  get_free_fid.f90                        \
  get_gjf_atom_linenumber.f90             \
  get_gjf_coord.f90                       \
  get_gjf_coord_intx.f90                  \
  get_gjf_natom.f90                       \
  get_gjf_natom_onion_H.f90               \
  get_gjf_natom_onion_idx.f90             \
  get_gjf_route.f90                       \
  get_grid_int1.f90                       \
  get_inertm_rot_d.f90                    \
  get_proj_matrix.f90                     \
  get_n_product_index.f90                 \
  get_new_fid.f90                         \
  get_mac_array.f90                       \
  get_mac_line.f90                        \
  get_mac_num.f90                         \
  get_sym_num.f90                         \
  get_trans_rot_vec.f90                   \
  get_vars.f90                            \
  get_Wilson_G_matrix.f90                 \
  in_basis3.f90                           \
  in_basis9.f90                           \
  in_coord_basis.f90                      \
  in_coord_basis2.f90                     \
  in_cry_cell.f90                         \
  incell.f90                              \
  inertm.f90                              \
  inertm_rot.f90                          \
  inertmc.f90                             \
  inertmv.f90                             \
  inertmv_rot.f90                         \
  inertmv_ref.f90                         \
  inertmv_ref1.f90                        \
  inertmv_ref3.f90                        \
  inertmv_itype.f90                       \
  inertmv2_ref.f90                        \
  inertm_fc.f90                           \
  is_letter.f90                           \
  is_number.f90                           \
  is_number_letter.f90                    \
  is_mac_string.f90                       \
  is_mac_line.f90                         \
  integration.f90                         \
  lattice_constants_to_a.f90              \
  laguer.for                              \
  len_record.f90                          \
  lowercase.f90                           \
  mass.f90                                \
  math.f90                                \
  cmd_progress.f90                        \
  matrix_power.f90                        \
  max_abs.f90                             \
  n_set_intersection.f90                  \
  nat_to_mass.f90                         \
  nat_to_radius.f90                       \
  nat_to_symbol.f90                       \
  nexper.f90                              \
  normalize_area.f90                      \
  normalize_ref.f90                       \
  normalize_vectors.f90                   \
  number_of_lines.f90                     \
  number_of_lines_word.f90                \
  number_of_words.f90                     \
  number_filename.f90                     \
  number_filename_x.f90                   \
  number_filename3.f90                    \
  parse_variables.f90                     \
  print_matrix.f90                        \
  print_matrix_abs.f90                    \
  print_matrix_head.f90                   \
  print_matrix_head_del_zero.f90          \
  point_group.f90                         \
  point_group1.f90                        \
  polyfit.f90                             \
  polydzero.f90                           \
  project_xhessian.f90                    \
  project_xhessian_rot.f90                \
  project_mhessian.f90                    \
  project_mhessian_rot.f90                \
  qm_cube_info.f90                        \
  qm_cube_natom_nxyz.f90                  \
  qm_fchk_coord.f90                       \
  qm_file.f90                             \
  qm_file_basis.f90                       \
  qm_file_charge.f90                      \
  qm_file_coord.f90                       \
  qm_file_coord_fid.f90                   \
  qm_file_coord_input.f90                 \
  qm_file_crystal.f90                     \
  qm_file_energy.f90                      \
  qm_file_energy_fid.f90                  \
  qm_file_force.f90                       \
  qm_file_force_fid.f90                   \
  qm_file_force_orbital_fid.f90           \
  qm_file_fc.f90                          \
  qm_file_mass.f90                        \
  qm_file_Mcharge.f90                     \
  qm_file_nacme.f90                       \
  qm_file_nat.f90                         \
  qm_file_natom.f90                       \
  qm_file_nex.f90                         \
  qm_file_nbasis.f90                      \
  qm_file_nopt.f90                        \
  qm_file_nopt_x.f90                      \
  qm_file_success.f90                     \
  qm_file_success_fid.f90                 \
  qm_file_symbol.f90                      \
  qm_file_tdip.f90                        \
  qm_file_tdipd.f90                       \
  qm_file_type.f90                        \
  qm_file_type_fid.f90                    \
  qm_vasp_poscar_coord.f90                \
  qm_vasp_poscar_info.f90                 \
  qm_vasp_poscar_natom.f90                \
  qm_vasp_potcar_symbol.f90               \
  qm_vasp_potcar_zval.f90                 \
  qm_vasp_chgcar_info.f90                 \
  qm_vasp_chgcar_data.f90                 \
  qm_vasp_outcar_ntype.f90                \
  qm_vasp_outcar_symbols.f90              \
  qm_vasp_outcar_natoms.f90               \
  qm_vasp_symbol.f90                      \
  qm_wien2k_struct_natom.f90              \
  qm_wien2k_struct_info.f90               \
  qm_wien2k_psink_ngrid.f90               \
  qm_wien2k_psink_info.f90                \
  qm_xsf_info.f90                         \
  qm_xsf_natom_nxyz.f90                   \
  qm_cif_natom.f90                        \
  qm_cif_info.f90                         \
  qm_cif_symm.f90                         \
  reduce_int.f90                          \
  rewrite_input.f90                       \
  rhom_to_Hex.f90                         \
  rotn.f90                                \
  rot_fc.90                               \
  search_char.f90                         \
  search_op.f90                           \
  search_op1.f90                          \
  search_op3.f90                          \
  search_pg.f90                           \
  search_word.f90                         \
  search_word_back.f90                    \
  search_word_back_free.f90               \
  search_word_del_space.f90               \
  search_word_first_letter.f90            \
  search_word_first_number.f90            \
  search_word_first_word.f90              \
  search_word_free.f90                    \
  search_word2_free.f90                   \
  search_word_free_n.f90                  \
  search_word_free_nline.f90              \
  search_word2_free_nline.f90             \
  search_word_free_last.f90               \
  search_word_del_space.f90               \
  search_word_last.f90                    \
  search_word_last_del_space.f90          \
  search_symm_xyz.f90                     \
  set_matrix_unit.f90                     \
  set_sym_matrix.f90                      \
  sort_array.f90                          \
  sort_index.f90                          \
  sort_array_string.f90                   \
  sort_vec_large.f90                      \
  spline.f90                              \
  splinf.f90                              \
  splint.f90                              \
  std_orient.f90                          \
  std_orient1.f90                         \
  std_to_inp.f90                          \
  str_delete.f90                          \
  str_replace.f90                         \
  str_replace_same.f90                    \
  swapn.f90                               \
  symbol_to_nat.f90                       \
  testnexper.f90                          \
  upcase.f90                              \
  write_cif.f90                           \
  write_cube.f90                          \
  write_gjf.f90                           \
  write_gjf_route.f90                     \
  write_matrix.f90                        \
  write_n_char.f90                        \
  write_chgcar.f90                        \
  write_poscar.f90                        \
  write_axsf.f90                          \
  write_mol.f90                           \
  write_xsf.f90                           \
  write_xsf_vib.f90                       \
  write_xsf_3dgrid.f90                    \
  write_xyz.f90                           \
  write_xyz_charge.f90                    \
  write_xyz_n.f90                         \
  write_xyz_v.f90                         \
  write_xyz_vib.f90                       \
  write_xyz_v_n.f90                       \
  zmat_to_xyz.f90                         \
  $(PROJ_HESS_SRC) $(MPISCR)

FILELIBS =                                \
  $(PRE_COMPILED)                         \
  symmetry.${OBJEXT}                      \
  symmetry1.${OBJEXT}                     \
  a_to_lattice_constants.${OBJEXT}        \
  laguer.${OBJEXT}                        \
  a2b.${OBJEXT}                           \
  aswap.${OBJEXT}                         \
  broaden.${OBJEXT}                       \
  broaden_spec.${OBJEXT}                  \
  c_to_cm.${OBJEXT}                       \
  celldm_to_a.${OBJEXT}                   \
  celldm_to_lattice_constants.${OBJEXT}   \
  char_to_int.${OBJEXT}                   \
  char_to_real.${OBJEXT}                  \
  change_phase.${OBJEXT}                  \
  check_int.${OBJEXT}                     \
  check_license.${OBJEXT}                 \
  check_license_debug.${OBJEXT}           \
  check_sym_matrix.${OBJEXT}              \
  check_x.${OBJEXT}                       \
  coord_crys_to_cart.${OBJEXT}            \
  coord_cart_to_crys.${OBJEXT}            \
  copy_matrix.${OBJEXT}                   \
  ddiag.${OBJEXT}                         \
  diag_symm.${OBJEXT}                     \
  disp.${OBJEXT}                          \
  del_char.${OBJEXT}                      \
  del_comment.${OBJEXT}                   \
  del_comment1.${OBJEXT}                  \
  del_space.${OBJEXT}                     \
  del_x_atom.${OBJEXT}                    \
  delta.${OBJEXT}                         \
  determinant3.${OBJEXT}                  \
  dorder.${OBJEXT}                        \
  eckart.${OBJEXT}                        \
  eos_fitting.${OBJEXT}                   \
  error.${OBJEXT}                         \
  extract_filename_xyz.${OBJEXT}          \
  extract_input.${OBJEXT}                 \
  fc_to_fcm.${OBJEXT}                     \
  file_copy.${OBJEXT}                     \
  file_delete.${OBJEXT}                   \
  file_num_lines.${OBJEXT}                \
  find_int_crd.${OBJEXT}                  \
  full_to_short_name.${OBJEXT}            \
  gaussj.${OBJEXT}                        \
  gaussj1.${OBJEXT}                       \
  get_area.${OBJEXT}                      \
  get_array3.${OBJEXT}                    \
  get_array3s.${OBJEXT}                   \
  get_array9.${OBJEXT}                    \
  get_au.${OBJEXT}                        \
  get_au_inverse.${OBJEXT}                \
  get_bond.${OBJEXT}                      \
  get_bond1.${OBJEXT}                     \
  get_bond_num.${OBJEXT}                  \
  get_coul.${OBJEXT}                      \
  get_dgrid3.${OBJEXT}                    \
  get_dos.${OBJEXT}                       \
  get_dos_fix.${OBJEXT}                   \
  get_dv.${OBJEXT}                        \
  get_elements.${OBJEXT}                  \
  get_first_mac.${OBJEXT}                 \
  get_first_mac_file.${OBJEXT}            \
  get_free_fid.${OBJEXT}                  \
  get_gjf_atom_linenumber.${OBJEXT}       \
  get_gjf_coord.${OBJEXT}                 \
  get_gjf_coord_intx.${OBJEXT}            \
  get_gjf_natom.${OBJEXT}                 \
  get_gjf_natom_onion_H.${OBJEXT}         \
  get_gjf_natom_onion_idx.${OBJEXT}       \
  get_gjf_route.${OBJEXT}                 \
  get_grid_int1.${OBJEXT}                 \
  get_inertm_rot_d.${OBJEXT}              \
  get_proj_matrix.${OBJEXT}               \
  get_n_product_index.${OBJEXT}           \
  get_new_fid.${OBJEXT}                   \
  get_mac_array.${OBJEXT}                 \
  get_mac_line.${OBJEXT}                  \
  get_mac_num.${OBJEXT}                   \
  get_sym_num.${OBJEXT}                   \
  get_trans_rot_vec.${OBJEXT}             \
  get_vars.${OBJEXT}                      \
  get_Wilson_G_matrix.${OBJEXT}           \
  in_basis3.${OBJEXT}                     \
  in_basis9.${OBJEXT}                     \
  in_coord_basis.${OBJEXT}                \
  in_coord_basis2.${OBJEXT}               \
  in_cry_cell.${OBJEXT}                   \
  incell.${OBJEXT}                        \
  inertm.${OBJEXT}                        \
  inertm_rot.${OBJEXT}                    \
  inertmc.${OBJEXT}                       \
  inertmv.${OBJEXT}                       \
  inertmv_rot.${OBJEXT}                   \
  inertmv_ref.${OBJEXT}                   \
  inertmv_ref1.${OBJEXT}                  \
  inertmv_ref3.${OBJEXT}                  \
  inertmv_itype.${OBJEXT}                 \
  inertmv2_ref.${OBJEXT}                  \
  inertm_fc.${OBJEXT}                     \
  is_letter.${OBJEXT}                     \
  is_number.${OBJEXT}                     \
  is_number_letter.${OBJEXT}              \
  is_mac_string.${OBJEXT}                 \
  is_mac_line.${OBJEXT}                   \
  integration.${OBJEXT}                   \
  lattice_constants_to_a.${OBJEXT}        \
  len_record.${OBJEXT}                    \
  lowercase.${OBJEXT}                     \
  mass.${OBJEXT}                          \
  math.${OBJEXT}                          \
  cmd_progress.${OBJEXT}                  \
  matrix_power.${OBJEXT}                  \
  max_abs.${OBJEXT}                       \
  n_set_intersection.${OBJEXT}            \
  nat_to_mass.${OBJEXT}                   \
  nat_to_radius.${OBJEXT}                 \
  nat_to_symbol.${OBJEXT}                 \
  nexper.${OBJEXT}                        \
  normalize_area.${OBJEXT}                \
  normalize_ref.${OBJEXT}                 \
  normalize_vectors.${OBJEXT}             \
  number_of_lines.${OBJEXT}               \
  number_of_lines_word.${OBJEXT}          \
  number_of_words.${OBJEXT}               \
  number_filename.${OBJEXT}               \
  number_filename_x.${OBJEXT}             \
  number_filename3.${OBJEXT}              \
  project_xhessian.${OBJEXT}              \
  project_xhessian_rot.${OBJEXT}          \
  project_mhessian.${OBJEXT}              \
  project_mhessian_rot.${OBJEXT}          \
  parse_variables.${OBJEXT}               \
  print_matrix.${OBJEXT}                  \
  print_matrix_abs.${OBJEXT}              \
  print_matrix_head.${OBJEXT}             \
  print_matrix_head_del_zero.${OBJEXT}    \
  point_group.${OBJEXT}                   \
  point_group1.${OBJEXT}                  \
  polyfit.${OBJEXT}                       \
  polydzero.${OBJEXT}                     \
  qm_cube_info.${OBJEXT}                  \
  qm_cube_natom_nxyz.${OBJEXT}            \
  qm_fchk_coord.${OBJEXT}                 \
  qm_file_basis.${OBJEXT}                 \
  qm_file_charge.${OBJEXT}                \
  qm_file_coord.${OBJEXT}                 \
  qm_file_coord_fid.${OBJEXT}             \
  qm_file_coord_input.${OBJEXT}           \
  qm_file_crystal.${OBJEXT}               \
  qm_file_energy.${OBJEXT}                \
  qm_file_energy_fid.${OBJEXT}            \
  qm_file_force.${OBJEXT}                 \
  qm_file_force_fid.${OBJEXT}             \
  qm_file_force_orbital_fid.${OBJEXT}     \
  qm_file_fc.${OBJEXT}                    \
  qm_file_mass.${OBJEXT}                  \
  qm_file_Mcharge.${OBJEXT}               \
  qm_file_nacme.${OBJEXT}                 \
  qm_file_nat.${OBJEXT}                   \
  qm_file_natom.${OBJEXT}                 \
  qm_file_nex.${OBJEXT}                   \
  qm_file_nbasis.${OBJEXT}                \
  qm_file_nopt.${OBJEXT}                  \
  qm_file_nopt_x.${OBJEXT}                \
  qm_file_success.${OBJEXT}               \
  qm_file_success_fid.${OBJEXT}           \
  qm_file_symbol.${OBJEXT}                \
  qm_file_tdip.${OBJEXT}                  \
  qm_file_tdipd.${OBJEXT}                 \
  qm_file_type.${OBJEXT}                  \
  qm_file_type_fid.${OBJEXT}              \
  qm_vasp_poscar_coord.${OBJEXT}          \
  qm_vasp_poscar_info.${OBJEXT}           \
  qm_vasp_poscar_natom.${OBJEXT}          \
  qm_vasp_potcar_symbol.${OBJEXT}         \
  qm_vasp_potcar_zval.${OBJEXT}           \
  qm_vasp_chgcar_info.${OBJEXT}           \
  qm_vasp_chgcar_data.${OBJEXT}           \
  qm_vasp_outcar_ntype.${OBJEXT}          \
  qm_vasp_outcar_symbols.${OBJEXT}        \
  qm_vasp_outcar_natoms.${OBJEXT}         \
  qm_vasp_symbol.${OBJEXT}                \
  qm_wien2k_struct_natom.${OBJEXT}        \
  qm_wien2k_struct_info.${OBJEXT}         \
  qm_wien2k_psink_ngrid.${OBJEXT}         \
  qm_wien2k_psink_info.${OBJEXT}          \
  qm_xsf_info.${OBJEXT}                   \
  qm_xsf_natom_nxyz.${OBJEXT}             \
  qm_cif_natom.${OBJEXT}                  \
  qm_cif_info.${OBJEXT}                   \
  qm_cif_symm.${OBJEXT}                   \
  reduce_int.${OBJEXT}                    \
  rewrite_input.${OBJEXT}                 \
  rhom_to_Hex.${OBJEXT}                   \
  rotn.${OBJEXT}                          \
  rot_fc.${OBJEXT}                        \
  search_char.${OBJEXT}                   \
  search_op.${OBJEXT}                     \
  search_op1.${OBJEXT}                    \
  search_op3.${OBJEXT}                    \
  search_pg.${OBJEXT}                     \
  search_word.${OBJEXT}                   \
  search_word_back.${OBJEXT}              \
  search_word_back_free.${OBJEXT}         \
  search_word_del_space.${OBJEXT}         \
  search_word_first_letter.${OBJEXT}      \
  search_word_first_number.${OBJEXT}      \
  search_word_first_word.${OBJEXT}        \
  search_word_free.${OBJEXT}              \
  search_word2_free.${OBJEXT}             \
  search_word_free_n.${OBJEXT}            \
  search_word_free_nline.${OBJEXT}        \
  search_word2_free_nline.${OBJEXT}       \
  search_word_free_last.${OBJEXT}         \
  search_word_del_space.${OBJEXT}         \
  search_word_last.${OBJEXT}              \
  search_word_last_del_space.${OBJEXT}    \
  search_symm_xyz.${OBJEXT}               \
  set_matrix_unit.${OBJEXT}               \
  set_sym_matrix.${OBJEXT}                \
  sort_array.${OBJEXT}                    \
  sort_index.${OBJEXT}                    \
  sort_array_string.${OBJEXT}             \
  sort_vec_large.${OBJEXT}                \
  spline.${OBJEXT}                        \
  splinf.${OBJEXT}                        \
  splint.${OBJEXT}                        \
  std_orient.${OBJEXT}                    \
  std_orient1.${OBJEXT}                   \
  std_to_inp.${OBJEXT}                    \
  str_delete.${OBJEXT}                    \
  str_replace.${OBJEXT}                   \
  str_replace_same.${OBJEXT}              \
  swapn.${OBJEXT}                         \
  symbol_to_nat.${OBJEXT}                 \
  testnexper.${OBJEXT}                    \
  upcase.${OBJEXT}                        \
  write_cif.${OBJEXT}                     \
  write_cube.${OBJEXT}                    \
  write_gjf.${OBJEXT}                     \
  write_gjf_route.${OBJEXT}               \
  write_matrix.${OBJEXT}                  \
  write_n_char.${OBJEXT}                  \
  write_chgcar.${OBJEXT}                  \
  write_poscar.${OBJEXT}                  \
  write_axsf.${OBJEXT}                    \
  write_mol.${OBJEXT}                     \
  write_xsf.${OBJEXT}                     \
  write_xsf_vib.${OBJEXT}                 \
  write_xsf_3dgrid.${OBJEXT}              \
  write_xyz.${OBJEXT}                     \
  write_xyz_charge.${OBJEXT}              \
  write_xyz_n.${OBJEXT}                   \
  write_xyz_v.${OBJEXT}                   \
  write_xyz_vib.${OBJEXT}                 \
  write_xyz_v_n.${OBJEXT}                 \
  zmat_to_xyz.${OBJEXT}                   \
  $(PROJ_HESS_OBJ) $(MPIOBJ)

all: $(staticlib)

$(sharedlib):  $(FILELIBS)
	$(FC) $(THISFLAG) -shared -fpic $(FILESRCS) -o $@

$(staticlib): $(FILELIBS)
	$(AR) $(AROUT)$@ $^
	$(RM) $(LIBDIR)/$(staticlib)
	ln -s $(PWD)/$(staticlib)      $(LIBDIR)/$(staticlib)
	for exe in ${mods}; do $(RM) ${INCDIR}/$$exe; ln -s ${PWD}/$$exe ${INCDIR}/$$exe; done

kinds.${OBJEXT}: kinds.f90
	$(FC) $(FFLAG) -c $<
	$(RM) $(INCDIR)/kinds.mod
	$(LN) $(PWD)/kinds.mod           $(INCDIR)/kinds.mod

constants.${OBJEXT}: constants.f90
	$(FC) $(FFLAG) -I$(INCDIR) -c $<
	$(RM) $(INCDIR)/constants.mod
	$(LN) $(PWD)/constants.mod       $(INCDIR)/constants.mod

param.${OBJEXT}: param.f90
	$(FC) $(FFLAG) -I$(INCDIR) -c $<
	$(RM) $(INCDIR)/param.mod
	$(LN) $(PWD)/param.mod           $(INCDIR)/param.mod

symmetry.${OBJEXT}: symmetry.f90
	$(FC) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/symmetry.mod
	$(LN) $(PWD)/symmetry.mod    $(INCDIR)/symmetry.mod

symmetry1.${OBJEXT}: symmetry1.f90
	$(FC) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/symmetry1.mod
	$(LN) $(PWD)/symmetry1.mod    $(INCDIR)/symmetry1.mod

string.${OBJEXT}: string.f90
	$(FC) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/string.mod
	$(LN) $(PWD)/string.mod $(INCDIR)/string.mod

string_utility.${OBJEXT}: string_utility.f90
	$(FC) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/string_utility.mod
	$(LN) $(PWD)/string_utility.mod $(INCDIR)/string_utility.mod

file.${OBJEXT}: file.F90
	$(FC) $(THISFLAGP) -c $<
	$(RM) $(INCDIR)/file.mod
	$(LN) $(PWD)/file.mod $(INCDIR)/file.mod

math.${OBJEXT}: math.f90
	$(FC) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/math.mod
	$(LN) $(PWD)/math.mod $(INCDIR)/math.mod

cmd_progress.${OBJEXT}: cmd_progress.f90
	$(FC) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/cmd_progress.mod
	$(LN) $(PWD)/cmd_progress.mod $(INCDIR)/cmd_progress.mod

qm_file.${OBJEXT}: qm_file.f90
	$(FC) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/qm_file.mod
	$(LN) $(PWD)/qm_file.mod $(INCDIR)/qm_file.mod

array.${OBJEXT}: array.f90
	$(FC) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/array.mod
	$(LN) $(PWD)/array.mod $(INCDIR)/array.mod

mpi_kind.${OBJEXT}: mpi_kind.f90
	$(FCP) $(THISFLAG) -c $<
	$(RM) $(INCDIR)/array.mod
	$(LN) $(PWD)/array.mod $(INCDIR)/array.mod

get_mpi_node_points.${OBJEXT}: get_mpi_node_points.f90
	$(FCP) $(THISFLAG) -c $< -I$(INCDIR)

mpi_write_log.${OBJEXT}: mpi_write_log.f90
	$(FCP) $(THISFLAG) -c $< -I$(INCDIR)

%.${OBJEXT}: %.F90
	$(FC) $(THISFLAG) -c $<

%.${OBJEXT}: %.f90
	$(FC) $(THISFLAG) -c $<

%.${OBJEXT}: %.for
	$(FC) $(THISFLAG) -c $<

%.${OBJEXT}: %.f
	$(FC) $(THISFLAG) -c $<

install:
	$(RM) $(LIBDIR)/$(staticlib)
	ln -s $(PWD)/$(staticlib)      $(LIBDIR)/$(staticlib)
	for exe in ${mods}; do $(RM) ${INCDIR}/$$exe; ln -s ${PWD}/$$exe ${INCDIR}/$$exe; done

clean:
	$(RM) *.${OBJEXT} *.mod

cleanall:
	$(RM) *.${OBJEXT} *.mod
	$(RM) $(libpath)/$(staticlib)  $(PWD)/$(staticlib)
	for exe in ${mods}; do $(RM) ${INCDIR}/$$exe; done
