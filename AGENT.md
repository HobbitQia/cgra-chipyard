# Chipyard Agent Notes

This directory is the active SoC integration point for the CGRA work.

## Current CGRA integration state

The current integration is centered on:

- [generators/chipyard/src/main/scala/example/CGRA.scala](/mnt/public/sichuan_a/qjj/CGRA-SoC/chipyard/generators/chipyard/src/main/scala/example/CGRA.scala:1)
- [generators/chipyard/src/main/resources/vsrc/CgraRTL_2x2_wrapper.v](/mnt/public/sichuan_a/qjj/CGRA-SoC/chipyard/generators/chipyard/src/main/resources/vsrc/CgraRTL_2x2_wrapper.v:1)
- [generators/chipyard/src/main/resources/vsrc/CgraRTL_2x2__pickled.v](/mnt/public/sichuan_a/qjj/CGRA-SoC/chipyard/generators/chipyard/src/main/resources/vsrc/CgraRTL_2x2__pickled.v:1)

The integrated hardware is currently:

- `2x2`
- `Mesh`
- fixed to the vendored `CgraRTL_2x2` blackbox

This is not yet the `4x4` FIR reference architecture from `VectorCGRA/cgra/test/CgraRTL_fir_test.py`.

## RoCC wrapper behavior

The CGRA is attached as a RoCC accelerator via `custom0`.

Current funct encodings in `CGRA.scala`:

- `2`: `STATUS`
- `4`: `WAIT`
- `5`: `RAW_PKT_LO`
- `6`: `RAW_PKT_MID`
- `7`: `RAW_PKT_HI`
- `8`: `SET_EXPECTED_COMPLETES`
- `9`: `RESULT`

The wrapper no longer exposes the old high-level `CONFIG`, `LAUNCH`, or `CONFIG_LOOP` helpers. The CPU emits arbitrary `IntraCgraPkt` messages through the raw packet path, and the wrapper only interprets `CMD_LAUNCH`/`CMD_RESUME`/`CMD_COMPLETE` for host-side busy and completion tracking.

## Current test flow

The active baremetal test is:

- [tests/cgra-test.c](/mnt/public/sichuan_a/qjj/CGRA-SoC/chipyard/tests/cgra-test.c:1)

This test currently hand-ports the `homogeneous_2x2` packet sequence from:

- [VectorCGRA/cgra/test/CgraRTL_test.py](/mnt/public/sichuan_a/qjj/CGRA-SoC/VectorCGRA/cgra/test/CgraRTL_test.py:260)

It does the following:

- sends per-tile `CMD_CONFIG_TOTAL_CTRL_COUNT`
- sends per-tile `CMD_CONFIG_COUNT_PER_ITER`
- sends per-tile `CMD_CONFIG`
- sets expected complete count to `4`
- sends per-tile `CMD_LAUNCH`
- waits until four `CMD_COMPLETE` packets are observed

## Verified command

Use:

```bash
cd /mnt/public/sichuan_a/qjj/CGRA-SoC
./run-chipyard-cgra-test.sh --rebuild
```

The `--rebuild` form is required after editing CGRA hardware integration files such as:

- `CGRA.scala`
- vendored CGRA wrapper or blackbox Verilog
- config fragments

## Important constraints

- The current wrapper still assumes `IntraCgraPkt = 182 bits`, `InterCgraPkt = 185 bits`, and two boundary ports per side.
- Completion handling is wrapper-defined; the wrapper tracks expected `CMD_COMPLETE` count from the host side.
- The `RESULT` helper only exposes the last observed 32-bit payload from `CMD_COMPLETE`.
