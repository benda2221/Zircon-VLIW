PWD := $(shell pwd)
SCALA_SRC := $(shell find src -name "*.scala")
all: verilog

USE_SBT := 1

verilog: $(SCALA_SRC)
ifeq ($(USE_SBT), 1)
	@BUILD_MODE=SYNC sbt 'runMain Main' --batch
else
	@BUILD_MODE=SYNC ./mill -s -j0 _.runMain Main
endif

sim-verilog: $(SCALA_SRC)
ifeq ($(USE_SBT), 1)
	@BUILD_MODE=sim sbt 'runMain Main' --batch
else
	@BUILD_MODE=sim ./mill -s -j0 _.runMain Main
endif

test-fmem-verilator: verilog
	@mkdir -p build/fmem-verilator
	@verilator --cc --exe --build --top-module CPU -Wno-fatal \
		--Mdir build/fmem-verilator verilog/*.sv \
		src/test/cpp/ZirconFMemCPUHarness.cpp -CFLAGS '-std=c++17 -O2'
	@build/fmem-verilator/VCPU


run:
	@$(MAKE) -C ZirconSim run $(IMG)
# ifeq ($(USE_SBT), 1)
# 	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir BUILD_MODE=sim sbt 'testOnly EmuMain' --batch
# else
# 	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir BUILD_MODE=sim ./mill -s -j0 _.test.testOnly EmuMain
# endif
	
