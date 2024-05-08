
.SECONDEXPANSION:

#
# Default is QUIET operation. For verbose: make QUIET='' ${TARGETS}
#
QUIET := @

Default_flags := \
 -Wall \
 -Werror \
 +nowarn_export_all \
 +nowarn_deprecated_function \
 +debug_info

Compiler_flags := $(or $(ERLC_FLAGS),$(Default_flags))

empty:=
space:=$(empty) $(empty)
bullet:=$(space)$(space)*$(space)

bullet_list = \
    $(foreach item, \
      $(or $($(*)),[NONE]), \
      $(info $(bullet)$(item)))



Module_paths := $(wildcard ./*/src/*.erl)
Module_files := $(notdir $(Module_paths))
Module_names := $(sort $(Module_files:%.erl=%))

include_dirs = $(wildcard ./*/include)
Include_dirs := $(include_dirs:%=-I %)


module_of_source = $(notdir $(source:%.erl=%))
path_of_source = $(dir $(source))
path_of_target = $(path_of_source:%/src/=%/ebin/)
target_of_source = $(path_of_target:%=%$(module_of_source:%=%.beam))

$(foreach source,\
  $(Module_paths),\
  $(eval Source_of_$(module_of_source) := $(source)) \
  $(eval Target_of_$(module_of_source) := $(target_of_source)) )





build_mod_name = $(notdir $(*:%.beam=%))
build_source_file = $($(build_mod_name:%=Source_of_%))
build_output = $(dir $($(build_mod_name:%=Target_of_%)))

%.beam: $$(build_source_file)
	$(info $(space)$(space)ERLC $^)
	$(QUIET)mkdir -p $(build_output)
	$(QUIET)ERL_LIBS=lib erlc \
	$(Include_paths) \
	$(Compiler_flags) \
	-o $(build_output) \
	$^

?%.beam: $$(build_source_file)
	$(info $(build_output:%/=%))


All_targets := $(foreach module, $(Module_names), $($(module:%=Target_of_%)) )

?:
	$(info $(All_targets))

all: $$(All_targets)

.PHONY: clean
clean:
	rm -r ./*/ebin/
	rm ./*/src/*~

?%:
	$(info Values assigned to $(*))
	$(bullet_list)
