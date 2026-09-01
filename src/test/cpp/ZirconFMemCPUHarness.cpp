#include "VCPU.h"
#include "verilated.h"

#include <array>
#include <cstdint>
#include <cstdio>
#include <unordered_map>
#include <vector>

static constexpr uint32_t NOP = 0x00000013;
static constexpr uint8_t FSW_OP = 0x62;

static uint32_t op_fp(unsigned funct7, unsigned rs2, unsigned rs1, unsigned funct3, unsigned rd) {
    return (funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | 0x53;
}

static uint32_t addi(unsigned rd, unsigned rs1, int imm) {
    return ((uint32_t(imm) & 0xfff) << 20) | (rs1 << 15) | (rd << 7) | 0x13;
}

static uint32_t flw(unsigned rd, unsigned rs1, int imm) {
    return ((uint32_t(imm) & 0xfff) << 20) | (rs1 << 15) | (2 << 12) | (rd << 7) | 0x07;
}

static uint32_t fsw(unsigned rs2, unsigned rs1, int imm) {
    const uint32_t value = uint32_t(imm) & 0xfff;
    return ((value >> 5) << 25) | (rs2 << 20) | (rs1 << 15) |
           (2 << 12) | ((value & 0x1f) << 7) | 0x27;
}

static std::array<uint32_t, 8> bundle(unsigned slot, uint32_t instruction) {
    std::array<uint32_t, 8> result{};
    result.fill(NOP);
    result[slot] = instruction;
    return result;
}

static void drive_instructions(VCPU &top, const std::vector<std::array<uint32_t, 8>> &program) {
    const uint32_t pc = top.io_imem_pc;
    const uint32_t index = (pc >= 0x80000000u) ? (pc - 0x80000000u) / 32u : UINT32_MAX;
    const auto instructions = index < program.size() ? program[index] : bundle(0, NOP);
    top.io_imem_insts_0 = instructions[0];
    top.io_imem_insts_1 = instructions[1];
    top.io_imem_insts_2 = instructions[2];
    top.io_imem_insts_3 = instructions[3];
    top.io_imem_insts_4 = instructions[4];
    top.io_imem_insts_5 = instructions[5];
    top.io_imem_insts_6 = instructions[6];
    top.io_imem_insts_7 = instructions[7];
}

int main(int argc, char **argv) {
    Verilated::commandArgs(argc, argv);
    VCPU top;
    std::unordered_map<uint32_t, uint32_t> memory{{0x100u, 0x3fc00000u}};

    const std::vector<std::array<uint32_t, 8>> program = {
        bundle(3, addi(1, 0, 0x100)),
        bundle(5, flw(1, 1, 0)),
        bundle(0, op_fp(0x00, 1, 1, 0, 2)),                 // fadd.s f2,f1,f1
        bundle(5, fsw(2, 1, 4)),
        bundle(0, op_fp(0x60, 0, 2, 1, 2)),                 // fcvt.w.s x2,f2,rtz
        bundle(0, op_fp(0x68, 0, 2, 0, 3)),                 // fcvt.s.w f3,x2
        bundle(1, op_fp(0x70, 0, 3, 0, 3)),                 // fmv.x.w x3,f3
        bundle(2, op_fp(0x78, 0, 3, 0, 4)),                 // fmv.w.x f4,x3
        bundle(6, fsw(4, 1, 8)),
    };

    auto service_memory = [&]() {
        top.io_dmem_lsu0_rdata = memory.count(top.io_dmem_lsu0_addr)
            ? memory[top.io_dmem_lsu0_addr] : 0;
        top.io_dmem_lsu1_rdata = memory.count(top.io_dmem_lsu1_addr)
            ? memory[top.io_dmem_lsu1_addr] : 0;
    };

    auto capture_stores = [&]() {
        if (top.io_dmem_lsu0_op == FSW_OP)
            memory[top.io_dmem_lsu0_addr] = top.io_dmem_lsu0_wdata;
        if (top.io_dmem_lsu1_op == FSW_OP)
            memory[top.io_dmem_lsu1_addr] = top.io_dmem_lsu1_wdata;
    };

    auto tick = [&]() {
        top.clock = 0;
        drive_instructions(top, program);
        service_memory();
        top.eval();
        service_memory();
        top.eval();
        capture_stores();
        top.clock = 1;
        top.eval();
        top.clock = 0;
        top.eval();
    };

    top.reset = 1;
    tick();
    tick();
    top.reset = 0;
    for (unsigned cycle = 0; cycle < 80; ++cycle) tick();

    struct Check { const char *name; uint32_t actual; uint32_t expected; };
    const Check checks[] = {
        {"f1 FLW", top.io_debug_fpr_1, 0x3fc00000u},
        {"f2 FADD", top.io_debug_fpr_2, 0x40400000u},
        {"x2 FCVT.W.S", top.io_debug_gpr_2, 0x00000003u},
        {"f3 FCVT.S.W", top.io_debug_fpr_3, 0x40400000u},
        {"x3 FMV.X.W", top.io_debug_gpr_3, 0x40400000u},
        {"f4 FMV.W.X", top.io_debug_fpr_4, 0x40400000u},
        {"mem[0x104]", memory[0x104u], 0x40400000u},
        {"mem[0x108]", memory[0x108u], 0x40400000u},
    };
    bool passed = true;
    for (const auto &check : checks) {
        if (check.actual != check.expected) {
            std::fprintf(stderr, "%s: got %08x expected %08x\n",
                         check.name, check.actual, check.expected);
            passed = false;
        }
    }
    top.final();
    if (!passed) return 1;
    std::puts("CPU FLW/FSW/FCVT/FMV integration passed");
    return 0;
}
