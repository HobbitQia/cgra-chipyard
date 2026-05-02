// CGRA RoCC single-CGRA homogeneous_2x2 smoke test.
//
// This hand-ports the homogeneous_2x2 packet sequence from
// VectorCGRA/cgra/test/CgraTemplateRTL_test.py onto the current generated
// single-CGRA integration.
// It configures four tiles, launches each tile, waits for completion, and
// checks that the wrapper observed four CMD_COMPLETE responses.

#include "rocc.h"
#include <stdint.h>
#include <stdio.h>

// ---- RoCC macros ----

#define CGRA_STATUS(result) \
  ROCC_INSTRUCTION_D(0, result, 2)

#define CGRA_WAIT(result) \
  ROCC_INSTRUCTION_D(0, result, 4)

#define CGRA_SET_EXPECTED_COMPLETES(count) \
  ROCC_INSTRUCTION_S(0, count, 8)

#define CGRA_RAW_PKT_LO(lo) \
  ROCC_INSTRUCTION_S(0, lo, 5)

#define CGRA_RAW_PKT_MID(mid) \
  ROCC_INSTRUCTION_S(0, mid, 6)

#define CGRA_RAW_PKT_HI(hi) \
  ROCC_INSTRUCTION_S(0, hi, 7)

#define CGRA_RAW_PKT_TOP(top) \
  ROCC_INSTRUCTION_S(0, top, 10)

// ---- CGRA packet constants ----

enum {
  CGRA_NUM_TILES = 4,
  CGRA_INTRA_PKT_WIDTH = 212,
  CGRA_CTRL_NBITS = 139,
  CGRA_CTRL_HI_NBITS = 11,

  CGRA_CMD_LAUNCH = 0,
  CGRA_CMD_CONFIG = 3,
  CGRA_CMD_CONFIG_TOTAL_CTRL_COUNT = 7,
  CGRA_CMD_CONFIG_COUNT_PER_ITER = 8,
};

static const uint64_t DATA_ONE_RAW = 0x8ULL;

// Control words generated from the current mk_ctrl(4, 2, 8, 8, 16) layout.
static const uint64_t CTRL_INC_LO = 0x10002ULL;
static const uint64_t CTRL_INC_MID = 0x100000000000001ULL;
static const uint64_t CTRL_INC_HI = 0x30ULL;
static const uint64_t CTRL_NAH_LO = 0x0ULL;
static const uint64_t CTRL_NAH_MID = 0x0ULL;
static const uint64_t CTRL_NAH_HI = 0x10ULL;

typedef struct {
  uint64_t lo;
  uint64_t mid;
  uint64_t hi;
  uint64_t top;
} cgra_packet_t;

static void pkt_clear(cgra_packet_t *pkt) {
  pkt->lo = 0;
  pkt->mid = 0;
  pkt->hi = 0;
  pkt->top = 0;
}

static void pkt_set_bit(cgra_packet_t *pkt, int bit_idx, uint64_t bit_val) {
  if (!bit_val) {
    return;
  }

  if (bit_idx < 64) {
    pkt->lo |= (1ULL << bit_idx);
  } else if (bit_idx < 128) {
    pkt->mid |= (1ULL << (bit_idx - 64));
  } else if (bit_idx < 192) {
    pkt->hi |= (1ULL << (bit_idx - 128));
  } else {
    pkt->top |= (1ULL << (bit_idx - 192));
  }
}

static void pkt_set_bits(cgra_packet_t *pkt, int lsb, int width, uint64_t value) {
  for (int i = 0; i < width; ++i) {
    pkt_set_bit(pkt, lsb + i, (value >> i) & 1ULL);
  }
}

static void pkt_set_ctrl(cgra_packet_t *pkt,
                         int lsb,
                         uint64_t ctrl_lo,
                         uint64_t ctrl_mid,
                         uint64_t ctrl_hi) {
  pkt_set_bits(pkt, lsb, 64, ctrl_lo);
  pkt_set_bits(pkt, lsb + 64, 64, ctrl_mid);
  pkt_set_bits(pkt, lsb + 128, CGRA_CTRL_HI_NBITS, ctrl_hi);
}

static cgra_packet_t build_intra_pkt(uint8_t src_tile,
                                     uint8_t dst_tile,
                                     uint8_t src_cgra_id,
                                     uint8_t dst_cgra_id,
                                     uint8_t src_cgra_x,
                                     uint8_t src_cgra_y,
                                     uint8_t dst_cgra_x,
                                     uint8_t dst_cgra_y,
                                     uint8_t opaque,
                                     uint8_t vc_id,
                                     uint8_t cmd,
                                     uint64_t data_raw,
                                     uint8_t data_addr,
                                     uint64_t ctrl_lo,
                                     uint64_t ctrl_mid,
                                     uint64_t ctrl_hi,
                                     uint8_t ctrl_addr) {
  cgra_packet_t pkt;
  pkt_clear(&pkt);

  // Payload fields.
  pkt_set_bits(&pkt, 0, 3, ctrl_addr);
  pkt_set_ctrl(&pkt, 3, ctrl_lo, ctrl_mid, ctrl_hi);
  pkt_set_bits(&pkt, 142, 9, data_addr);
  pkt_set_bits(&pkt, 151, 35, data_raw);
  pkt_set_bits(&pkt, 186, 5, cmd);

  // Header fields.
  pkt_set_bits(&pkt, 191, 1, vc_id);
  pkt_set_bits(&pkt, 192, 8, opaque);
  pkt_set_bits(&pkt, 200, 1, dst_cgra_y);
  pkt_set_bits(&pkt, 201, 1, dst_cgra_x);
  pkt_set_bits(&pkt, 202, 1, src_cgra_y);
  pkt_set_bits(&pkt, 203, 1, src_cgra_x);
  pkt_set_bits(&pkt, 204, 1, dst_cgra_id);
  pkt_set_bits(&pkt, 205, 1, src_cgra_id);
  pkt_set_bits(&pkt, 206, 3, dst_tile);
  pkt_set_bits(&pkt, 209, 3, src_tile);

  return pkt;
}

static void send_packet(const cgra_packet_t *pkt) {
  CGRA_RAW_PKT_LO(pkt->lo);
  CGRA_RAW_PKT_MID(pkt->mid);
  CGRA_RAW_PKT_HI(pkt->hi);
  CGRA_RAW_PKT_TOP(pkt->top);
}

static cgra_packet_t build_total_count_pkt(uint8_t tile_id) {
  return build_intra_pkt(
      0, tile_id,
      0, 0,
      0, 0,
      0, 0,
      0, 0,
      CGRA_CMD_CONFIG_TOTAL_CTRL_COUNT,
      DATA_ONE_RAW,
      0,
      0,
      0,
      0,
      0);
}

static cgra_packet_t build_count_per_iter_pkt(uint8_t tile_id) {
  return build_intra_pkt(
      0, tile_id,
      0, 0,
      0, 0,
      0, 0,
      0, 0,
      CGRA_CMD_CONFIG_COUNT_PER_ITER,
      DATA_ONE_RAW,
      0,
      0,
      0,
      0,
      0);
}

static cgra_packet_t build_inc_config_pkt(uint8_t tile_id) {
  return build_intra_pkt(
      0, tile_id,
      0, 0,
      0, 0,
      0, 0,
      0, 0,
      CGRA_CMD_CONFIG,
      0,
      0,
      CTRL_INC_LO,
      CTRL_INC_MID,
      CTRL_INC_HI,
      0);
}

static cgra_packet_t build_launch_pkt(uint8_t tile_id) {
  return build_intra_pkt(
      0, tile_id,
      0, 0,
      0, 0,
      0, 0,
      0, 0,
      CGRA_CMD_LAUNCH,
      0,
      0,
      CTRL_NAH_LO,
      CTRL_NAH_MID,
      CTRL_NAH_HI,
      0);
}

int main(void) {
  uint64_t status = 0;
  uint64_t wait_result = 0;

  printf("CGRA RoCC homogeneous_2x2: Starting...\n");

  CGRA_STATUS(status);
  printf("Initial status: 0x%lx\n", status);

  printf("Configuring all four tiles...\n");
  for (uint8_t tile = 0; tile < CGRA_NUM_TILES; ++tile) {
    cgra_packet_t total_pkt = build_total_count_pkt(tile);
    cgra_packet_t count_pkt = build_count_per_iter_pkt(tile);
    cgra_packet_t config_pkt = build_inc_config_pkt(tile);
    send_packet(&total_pkt);
    send_packet(&count_pkt);
    send_packet(&config_pkt);
  }

  // status indicates the number of tiles that have completed, so it should start at 0.
  CGRA_STATUS(status);
  printf("Status after config: 0x%lx\n", status);

  printf("Launching all four tiles...\n");
  CGRA_SET_EXPECTED_COMPLETES(CGRA_NUM_TILES);
  for (uint8_t tile = 0; tile < CGRA_NUM_TILES; ++tile) {
    cgra_packet_t launch_pkt = build_launch_pkt(tile);
    send_packet(&launch_pkt);
  }

  CGRA_WAIT(wait_result);
  printf("WAIT result: 0x%lx\n", wait_result);

  CGRA_STATUS(status);
  printf("Final status: 0x%lx\n", status);

  uint64_t complete = status & 0x1ULL;
  uint64_t complete_count = (status >> 1) & 0xFFFFULL;

  if (wait_result != 1 || complete != 1 || complete_count != CGRA_NUM_TILES) {
    printf("CGRA RoCC homogeneous_2x2: FAIL\n");
    return 1;
  }

  printf("CGRA RoCC homogeneous_2x2: PASS\n");
  return 0;
}
