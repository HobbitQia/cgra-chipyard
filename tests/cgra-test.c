// CGRA RoCC Accelerator Baremetal Test
// Tests basic CGRA control path via custom0 RoCC instructions
//
// Instruction encoding (funct7):
//   0 = LAUNCH
//   1 = CONFIG (rs1=tile_id, rs2=config_word)
//   2 = STATUS (rd=result)
//   3 = CONFIG_LOOP (rs1=count_per_iter, rs2=total_count)
//   4 = WAIT (blocking wait for completion)

#include "rocc.h"
#include <stdio.h>
#include <stdint.h>

// ---- CGRA RoCC macros ----

// LAUNCH: no rs1/rs2/rd
#define CGRA_LAUNCH() \
  ROCC_INSTRUCTION(0, 0)

// CONFIG: rs1=tile_id, rs2=config_word, no rd
#define CGRA_CONFIG(tile, cfg) \
  ROCC_INSTRUCTION_SS(0, tile, cfg, 1)

// STATUS: rd=result, no rs1/rs2
#define CGRA_STATUS(result) \
  ROCC_INSTRUCTION_D(0, result, 2)

// CONFIG_LOOP: rs1=count_per_iter, rs2=total_count, no rd
#define CGRA_CONFIG_LOOP(cpi, total) \
  ROCC_INSTRUCTION_SS(0, cpi, total, 3)

// WAIT: rd=result (blocking)
#define CGRA_WAIT(result) \
  ROCC_INSTRUCTION_D(0, result, 4)

// ---- Test ----

int main() {
  uint64_t status;

  printf("CGRA RoCC Test: Starting...\n");

  // 1. Query initial status (should be idle)
  CGRA_STATUS(status);
  printf("Initial status: 0x%lx\n", status);

  // 2. Send a simple CONFIG command to tile 0
  //    (config_word = 0 means OPT_START/no-op config)
  printf("Sending CONFIG to tile 0...\n");
  CGRA_CONFIG(0, 0);

  // 3. Query status again
  CGRA_STATUS(status);
  printf("Status after config: 0x%lx\n", status);

  // 4. WAIT should return immediately while the accelerator is idle.
  CGRA_WAIT(status);
  printf("WAIT result while idle: 0x%lx\n", status);

  printf("CGRA RoCC Test: PASS\n");

  return 0;
}
