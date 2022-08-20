#!/usr/bin/bash


# Step 1 compile source code
echo [INFO] Compiling Source Code ....
make sim-verilog
sleep 1

# Step 2 Copy files
echo [INFO] Copy Files
cp ./SimTop.v ./build/test/
mv ./SimTop.AXI4Ram* ./build/test
sleep 1

# Step 3 Change directory
cd ./build/test/

# Step 4 Verilator compiling
echo [INFO] Verilator Compiling ....
verilator --trace --cc SimTop.v --exe sim.cpp
sleep 1

# Step 5 Make
echo [INFO] Making ...
make -C obj_dir -f VSimTop.mk VSimTop
sleep 1

# Step 6 Run
./obj_dir/VSimTop
