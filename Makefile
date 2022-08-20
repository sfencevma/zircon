#
# Copyright (c) 2022 Lyn
# Skew is licensed under Mulan PubL v2.
# You can use this software according to the terms and conditions of the Mulan PubL v2.
# You may obtain a copy of Mulan PubL v2 at:
#         http://license.coscl.org.cn/MulanPubL-2.0
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
# See the Mulan PubL v2 for more details.
#

TOP = ZirconTop
FPGATOP = top.TopMain
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

SIMTOP = top.TopMain
IMAGE ?= temp
NUM_CORES ?= 1

FPGA_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf --gen-mem-verilog full 
SIM_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf --gen-mem-verilog full --enable-difftest

.DEFAULT_GOAL = verilog

$(TOP_V): $(SCALA_FILE)
		mkdir -p $(@D)
		@echo "\n[mill] Generating Verilog files...."
		mill -i zircon.runMain $(FPGATOP)

verilog: $(TOP_V)

SIM_TOP = SimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
			mkdir -p $(@D)
			@echo "\n[mill] Generating Verilog files...."
			mill -i zircon.test.runMain $(SIMTOP)

sim-verilog: $(SIM_TOP_V)

clean:
	$(MAKE) -C ./difftest clean
	rm -rf ./build

idea:
	mill -i mill.scalalib.GenIdea/idea

emu:
	$(MAKE) -C ./difftest emu

emu-run:
	$(MAKE) -C ./difftest emu-run


.PHONY: verilog sim-verilog emu clean idea