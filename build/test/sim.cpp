#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include <verilated_vcd_c.h>
#include <verilated.h>

#include "VSimTop.h"
#include "VSimTop__Syms.h"
#include "VSimTop_IssueSlot.h"
#define MAX_SIM_TIME 100
vluint64_t sim_time = 0;

double sc_time_stamp() { return 0; }

int main(int argc, char **argv, char **env) {
  Verilated::commandArgs(argc,argv); 
  VSimTop *dut = new VSimTop();

  Verilated::traceEverOn(true);
  VerilatedVcdC *m_trace = new VerilatedVcdC();
  dut->trace(m_trace, 0);
  m_trace->open("simtop.vcd");
  while (sim_time < MAX_SIM_TIME && !Verilated::gotFinish()) {
    dut->io_reset_vector = 0; // 0xfffffffffff0;
    dut->io_logCtrl_log_begin = 0;
    dut->io_logCtrl_log_end = 0;
    dut->io_logCtrl_log_level = 0;

    dut->clock = !dut->clock;
    dut->reset = 0;
    if (sim_time > 1 && sim_time < 9.5) {
      dut->reset = 1;
    }
    dut->eval();
    m_trace->dump(sim_time);
    sim_time++;
  }
  m_trace->close();
  delete dut;
  return (0);
}