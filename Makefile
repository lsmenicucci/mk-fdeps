.SECONDARY:

BUILD_DIR ?= build
PREFIX ?= /usr/local/bin

CC=gcc
FC=gfortran
#FC_FLAGS=-O0 -pg -g -fbounds-check -J$(BUILD_DIR)
FC_FLAGS=-O3 -fbounds-check -J$(BUILD_DIR)

$(BUILD_DIR):
	mkdir -p $@

$(BUILD_DIR)/%.o: src/%.f90 | $(BUILD_DIR)
	$(FC) $(FC_FLAGS) -c $< -o $@

%.c: %.re
	re2c -W $< -o $@

$(BUILD_DIR)/%.o: src/%.c | $(BUILD_DIR)
	$(CC) -c $< -o $@

# Manual dependencies
$(BUILD_DIR)/lexer-wrapper.o: $(BUILD_DIR)/string_builder.o $(BUILD_DIR)/lexer.o
$(BUILD_DIR)/parser.o: $(BUILD_DIR)/lexer-wrapper.o
$(BUILD_DIR)/makefile_deps.o: $(BUILD_DIR)/parser.o $(BUILD_DIR)/hash_table.o $(BUILD_DIR)/string_arena.o $(BUILD_DIR)/int_darray.o $(BUILD_DIR)/graph.o

main_deps = makefile_deps 
$(BUILD_DIR)/main.o: $(main_deps:%=$(BUILD_DIR)/%.o)

# Link all
F_SRCS = $(wildcard src/*.f90)
F_OBJS = $(F_SRCS:src/%.f90=$(BUILD_DIR)/%.o)

C_SRCS = $(wildcard src/*.c)
C_OBJS = $(C_SRCS:src/%.c=$(BUILD_DIR)/%.o)

target_deps = $(F_OBJS) $(C_OBJS)

$(BUILD_DIR)/mk-fdeps: $(target_deps) | $(BUILD_DIR)
	$(FC) $(FC_FLAGS) $^ -o $@

install: $(BUILD_DIR)/mk-fdeps
	install -Dm755 $< $(PREFIX)/mk-fdeps

clean: 
	rm -r $(BUILD_DIR)

all: $(BUILD_DIR)/mk-fdeps
.PHONY: all install
