MAINDIR=..
include $(MAINDIR)/mozip.inc

PWD=$(shell pwd)

INCDIR=$(MAINDIR)/include
LIBDIR=$(MAINDIR)/lib

FF=$(FFLAG) $(OPENMP)

THISFLAG0=-I$(INCDIR) $(FLIB)
THISFLAG =-I$(INCDIR) $(FLIB) $(LAPACK)

FLIB=$(LIBDIR)/libflib.$(STATICEXT)

coms =                     \
  build_adsorb.exe          \
  build_cluster.exe         \
  charge2xyz.exe            \
  chg2cub.exe               \
  chg2xsf.exe               \
  chgcar2xsf.exe            \
  chgdiff2xsf.exe           \
  chgsum.exe                \
  cif2gjf.exe               \
  cif2poscar.exe            \
  cif2xyz.exe               \
  coord2gjf.exe             \
  crystal_vib.exe           \
  crystal2xyz_charge.exe    \
  cube_den2mo.exe           \
  cube_product.exe          \
  cube2org.exe              \
  cube2xsf.exe              \
  ebridge.exe               \
  elfcar2xsf.exe            \
  ff2afm.exe                \
  g09tomolekel.exe          \
  get_band.exe              \
  gjf_new.exe               \
  gjf2cif.exe               \
  gjf2xyz.exe               \
  gjf2xyz_charge.exe        \
  glog2xyz.exe              \
  glog2xyz_tdip.exe         \
  gout2xyz.exe              \
  gro_gather.exe            \
  gro2xyz.exe               \
  locpot2cub.exe            \
  modos.exe                 \
  mol2xyz.exe               \
  monoclinic_cif_to_pwi.exe \
  nw2gjf.exe                \
  nwout2gjf.exe             \
  orbit2org.exe             \
  outcar2axsf.exe           \
  outcar2xyz.exe            \
  pdb_divide.exe            \
  pdb2xyz.exe               \
  point_group.exe           \
  polar_inert.exe           \
  poscar_move.exe           \
  poscar_new.exe            \
  poscar2cif.exe            \
  poscar2gjf.exe            \
  poscar2xsf.exe            \
  poscar2xyz.exe            \
  printrn.exe               \
  print_orbitals.exe        \
  pwout2axsf.exe            \
  pwout2cif.exe             \
  raman_spectra.exe         \
  read_ao_info.exe          \
  rot_cube.exe              \
  shg.exe                   \
  shg_model.exe             \
  shg_mol.exe               \
  spin_function.exe         \
  supercell_pdb.exe         \
  tmoleout2gjf.exe          \
  trr2xyz.exe               \
  wavecar2xsf.exe           \
  wienpsi2xsf.exe           \
  xsf2cube.exe              \
  xsf2gjf.exe               \
  xyz2gjf.exe


	
all.exe: ${coms}

glog2xyz_tdip.exe: glog2xyz_tdip.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG0) -o $@

printrn.exe: printrn.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG0) -o $@

gout2xyz.exe: gout2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG0) -o $@

gjf2xyz_charge.exe: gjf2xyz_charge.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG0) -o $@

read_ao_info.exe: read_ao_info.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

modos.exe: modos.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

tmoleout2gjf.exe: tmoleout2gjf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG0) -o $@

chgsum.exe: chgsum.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

crystal2xyz_charge.exe: crystal2xyz_charge.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

get_band.exe: get_band.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

ff2afm.exe: ff2afm.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

coord2gjf.exe: coord2gjf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

crystal_vib.exe: crystal_vib.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

shg_mol.exe: shg_mol.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

supercell_pdb.exe: supercell_pdb.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

gro_gather.exe: gro_gather.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

glog2xyz.exe: glog2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

ebridge.exe: ebridge.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

spin_function.exe: spin_function.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

cube_product.exe: cube_product.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

cube_den2mo.exe: cube_den2mo.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

point_group.exe: point_group.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

pdb_divide.exe: pdb_divide.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

raman_spectra.exe: raman_spectra.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

shg_model.exe: shg_model.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

shg.exe: shg.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

charge2xyz.exe: charge2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

poscar_new.exe: poscar_new.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

build_cluster.exe: build_cluster.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

build_adsorb.exe: build_adsorb.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

print_orbitals.exe: print_orbitals.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

gjf_new.exe: gjf_new.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

gro2xyz.exe: gro2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

trr2xyz.exe: trr2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

mol2xyz.exe: mol2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

pdb2xyz.exe: pdb2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

orbit2org.exe: orbit2org.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

monoclinic_cif_to_pwi.exe: monoclinic_cif_to_pwi.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

wienpsi2xsf.exe: wienpsi2xsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

xsf2cube.exe: xsf2cube.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

polar_inert.exe: polar_inert.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

cube2org.exe: search_nearest_point_3D.$(OBJEXT) cube2org.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $^ $(THISFLAG) -o $@

pwout2axsf.exe: pwout2axsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

pwout2cif.exe: pwout2cif.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

cif2xyz.exe: cif2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

cif2gjf.exe: cif2gjf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

nw2gjf.exe: nw2gjf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

nwout2gjf.exe: nwout2gjf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

xyz2gjf.exe: xyz2gjf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

xsf2gjf.exe: xsf2gjf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

cif2poscar.exe: cif2poscar.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

gjf2cif.exe: gjf2cif.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

gjf2xyz.exe: gjf2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

cube2xsf.exe: cube2xsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

g09tomolekel.exe: g09tomolekel.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

elfcar2xsf.exe: elfcar2xsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

poscar_move.exe: poscar_move.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

poscar2gjf.exe: poscar2gjf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

poscar2xyz.exe: poscar2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

poscar2cif.exe: poscar2cif.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

poscar2xsf.exe: poscar2xsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

wavecar2xsf.exe: wavecar2xsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

locpot2cub.exe: locpot2cub.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

rot_cube.exe: rot_cube.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

chg2cub.exe: chg2cub.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

chg2xsf.exe: chg2xsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

chgdiff2xsf.exe: chgdiff2xsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

chgcar2xsf.exe: chgcar2xsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

outcar2axsf.exe: outcar2axsf.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

outcar2xyz.exe: outcar2xyz.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

%.$(OBJEXT): %.f90
	$(FC) $(FFLAG) -I$(INCDIR) -c $<

install:
	for exe in ${coms};  do $(RM) ${BINDIR}/$$exe; ln -s ${PWD}/$$exe ${BINDIR}/$$exe; done

clean:
	$(RM) *.$(OBJEXT) *.mod
	for exe in ${coms}; do rm -f $$exe; done

cleanall:
	rm -f *.$(OBJEXT) *.mod
	for exe in ${coms};  do rm -f ${GTBIN}/$$exe $$exe; done
