###############################################################################
#  make           ... generate executable for the REAL sequential version     #
#  make clean     ... delete unnecessary files                                #
###############################################################################
include $(wildcard make.d/*)
#
FC    = ifort
DEPS  = -fpp -gen-dep=$(@:%.o=%.d)
FLIB  = -mkl
# FOPT = -I${MKLROOT}/include
FOPT = -fast -fopenmp
# FOPT += -mcmodel=medium -xHOST -O2 -ipo -no-prec-div
# FOPT += -heap-arrays 10240
# FOPT += -fstack-protector-all
# FDBG  = -traceback -g -debug extended
# FDBG += -O0 -fno-eliminate-unused-debug-types
# FDBG += -check all
# FDBG += -warn all
# FDBG += -fpe0 -ffpe-trap=invalid,zero,overflow
# FDBG += -opt-report
# FPRP  = -Dverbose=1

###############################################################################
SRCDIR  = ./src
ifeq "$(strip $(SRCDIR))" ""
		SRCDIR  = .
endif
OBJDIR  = ./obj
ifeq "$(strip $(OBJDIR))" ""
		OBJDIR  = .
else
FMOD    = -module $(OBJDIR)
endif
LIBDIR  = ./lib
INCDIR  =
ifeq "$(strip $(INCDIR))" ""
else
		FLIB   += -I $(INCDIR)
		FMOD   += -module $(INCDIR)
endif
EXEDIR  = ./bin
###############################################################################

TARGET  = $(notdir $(SOURCES:%.f90=%))
SOURCES = $(wildcard $(SRCDIR)/*.f90)
LIBS    = $(wildcard $(LIBDIR)/*.f90)
OBJECTS = $(addprefix $(OBJDIR)/, $(notdir $(LIBS:%.f90=%.o)))
MODULES = $(wildcard $(OBJDIR)/*.mod)
DEPENDS = $(OBJECTS:.o=.d)

-include $(DEPENDS)

.SUFFIXES: .f90
.PHONY : clean all
###############################################################################

$(TARGET): $(OBJECTS)
		$(FC) $(FLIB) $(FMOD) $(FDBG) $(FPRP) -o $(EXEDIR)/$@ $^ \
		$(SRCDIR)/$@.f90

$(OBJDIR)/%.o: $(LIBDIR)/%.f90
#   -mkdir -p $(OBJDIR)
		$(FC) $(FLIB) $(FOPT) $(FMOD) $(FDBG) $(FPRP) $(DEPS) -o $@ -c $<

all: clean $(TARGET)

clean:
		-rm -f $(OBJECTS) $(MODULES) $(DEPENDS) $(addprefix $(EXEDIR)/, $(TARGET))

%.mod: %.f90 %.o
		@:
