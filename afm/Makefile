MAINDIR=..
include $(MAINDIR)/mozip.inc

PWD=$(shell pwd)

INCDIR=$(MAINDIR)/include
LIBDIR=$(MAINDIR)/lib

FF=$(FFLAG) $(OPENMP)
FLIB=$(LIBDIR)/libflib.$(STATICEXT)
THISFLAG =-I$(INCDIR) $(FLIB) $(LAPACK)

#-------------------------------------------------------------------------------
FOBJ=                       \
  module.$(OBJEXT)           \
  readin.$(OBJEXT)           \
  init.$(OBJEXT)             \
  autoGeom.$(OBJEXT)         \
  readbas.$(OBJEXT)          \
  terminated.$(OBJEXT)       \
  loadAtomscolor.$(OBJEXT)   \
	autoGeom.$(OBJEXT)         \
	setTip.$(OBJEXT)           \
	get_LJ_param.$(OBJEXT)     \
	afmf.$(OBJEXT)             \
  readtip.$(OBJEXT)          \
	getCoulombFF.$(OBJEXT)     \
	getLenardJonesFF.$(OBJEXT) \
	getAtomsLJ.$(OBJEXT)       \
  relaxTipStroke.$(OBJEXT)   \
	plotModel.$(OBJEXT)        \
	relaxProbe.$(OBJEXT)       \
	read_force.$(OBJEXT)       \
  relaxTip.$(OBJEXT)         \
	get_force.$(OBJEXT)        \
	get_force_tip.$(OBJEXT)    \
  get_b.$(OBJEXT)            \
  get_a.$(OBJEXT)            \
  get_force_bas.$(OBJEXT)    \
  move_fire.$(OBJEXT)        \
  move_relax.$(OBJEXT)       \
  get_force_field.$(OBJEXT)
#-------------------------------------------------------------------------------
POBJ=                       \
  module.$(OBJEXT)           \
  plot_force.$(OBJEXT)       \
  plot_gnuplot.$(OBJEXT)     \
  plot_python.$(OBJEXT)      \
  get_df.$(OBJEXT)           \
  average_amp.$(OBJEXT)      \
  plot_xyz.$(OBJEXT)
#-------------------------------------------------------------------------------
com=            \
  get_afm.exe    \
  afmf.exe       \
  afm_slice.exe  \
  plot_force.exe

all: $(com)

plot_force.exe: $(POBJ) $(FLIB)
	$(FC) $(FF) $^ $(THISFLAG) -o $@

afm_slice.exe: afm_slice.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $^ $(THISFLAG) -o $@

afmf.exe: $(FOBJ) $(FLIB)
	$(FC) $(FF) $^ $(THISFLAG) -o $@

get_afm.exe: get_afm.$(OBJEXT) $(FLIB)
	$(FC) $(FF) $< $(THISFLAG) -o $@

%.$(OBJEXT): %.f90
	$(FC) $(FF) -I$(INCDIR) -c $<

install:
	for exe in $(coms); do $(RM) $(BINDIR)/$$exe; ln -s $(PWD)/$$exe $(BINDIR)/$$exe; done

clean:
	$(RM) *.$(OBJEXT) *.mod
	for exe in $(coms); do $(RM) $$exe; done

cleanall:
	rm -f *.$(OBJEXT) *.mod
	for exe in $(coms); do $(RM) $(BINDIR)/$$exe $$exe; done
