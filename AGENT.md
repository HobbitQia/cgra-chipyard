# Chipyard Agent Notes

This directory is the active SoC integration point for the CGRA work. The
top-level repository owns CGRA configuration and generation; Chipyard consumes
the generated RTL, wrapper, and Scala parameter files.

## Current CGRA Integration State

The current integration is centered on:

- [generators/chipyard/src/main/scala/example/CGRA.scala](/mnt/public/sichuan_a/qjj/CGRA-SoC/chipyard/generators/chipyard/src/main/scala/example/CGRA.scala:1)
- [generators/chipyard/src/main/scala/example/CGRAGenerated.scala](/mnt/public/sichuan_a/qjj/CGRA-SoC/chipyard/generators/chipyard/src/main/scala/example/CGRAGenerated.scala:1)
- [generators/chipyard/src/main/resources/vsrc/CgraTemplateRTL_single_wrapper.v](/mnt/public/sichuan_a/qjj/CGRA-SoC/chipyard/generators/chipyard/src/main/resources/vsrc/CgraTemplateRTL_single_wrapper.v:1)
- [generators/chipyard/src/main/resources/vsrc/CgraTemplateRTL_single__pickled.v](/mnt/public/sichuan_a/qjj/CGRA-SoC/chipyard/generators/chipyard/src/main/resources/vsrc/CgraTemplateRTL_single__pickled.v:1)

The integrated hardware is generated from the layered top-level configs:

- `configs/arch/arch.yaml`
- `configs/soc/cgra_soc.yaml`

Do not use the old 2x2 `CgraRTL_2x2` resources as the active reference for this
flow. Do not source CGRA shape, interface, memory, or FU availability from
kernel YAMLs.

## Current OpenFPGA MMIO Demo State

The OpenFPGA demo is attached as a TileLink MMIO peripheral, not as a RoCC
accelerator. The top-level repository owns the YAML config and generation flow;
Chipyard consumes the generated wrapper, manifest, and Scala metadata.

The current integration is centered on:

- `generators/chipyard/src/main/scala/example/OpenFPGA.scala`
- `generators/chipyard/src/main/scala/example/OpenFPGAGenerated.scala`
- `generators/chipyard/src/main/scala/config/OpenFPGAConfigGenerated.scala`
- `generators/chipyard/src/main/resources/vsrc/openfpga_and2/OpenFPGAAnd2Wrapper.v`
- `generators/chipyard/src/main/resources/vsrc/openfpga_and2/openfpga_and2_manifest.v`

The verified config is `OpenFPGADemoRocketConfig`, generated from the top-level
`configs/openfpga/openfpga_and2.yaml`. The peripheral exposes control, status,
frame-based `CFG_WORD`, packed `USER_INPUT`, and packed `USER_OUTPUT` registers.
The `USER_INPUT`/`USER_OUTPUT` field layout and FPGA GPIO pad map are derived
from the OpenFPGA formal verification netlist, not from YAML fields. The
baremetal test streams prepacked bitstream words; it should not assemble config
packets at runtime.

The generated OpenFPGA manifest intentionally uses OpenFPGA cell-library
`inv.v`, `buf4.v`, and `tap_buf4.v` models instead of the generated
`inv_buf_passgate.v`, because the latter contains tri-state-aware expressions
that Verilator rejects. Do not reintroduce the removed Verilator shim backend or
a generated Verilator replacement tree unless the top-level OpenFPGA policy
changes.

## RoCC Wrapper Behavior

The CGRA is attached as a RoCC accelerator via `custom0`.

Current funct encodings in `CGRA.scala`:

- `2`: `STATUS`
- `4`: `WAIT`
- `5`: `RAW_PKT_LO`
- `6`: `RAW_PKT_MID`
- `7`: `RAW_PKT_HI`
- `8`: `SET_EXPECTED_COMPLETES`
- `9`: `RESULT`
- `10`: `RAW_PKT_TOP`

The wrapper exposes a raw packet injection path and tracks completion/readback
for the host. The CPU emits `IntraCgraPkt` messages through the generated C API;
the wrapper interprets CGRA command packets needed for host-side busy,
completion, result, and memory readback behavior.

## Current Test Flow

Run tests from the top-level repository, not from inside `chipyard`:

```bash
cd /mnt/public/sichuan_a/qjj/CGRA-SoC
python scripts/generate_single_cgra.py --arch-yaml configs/arch/arch.yaml --soc-yaml configs/soc/cgra_soc.yaml
python scripts/generate_cgra_c_api.py --arch-yaml configs/arch/arch.yaml --soc-yaml configs/soc/cgra_soc.yaml configs/kernels/kernel_fir4x4_4x4.yaml --output-dir tests/generated
./run-chipyard-cgra-test.sh --rebuild cgra-fir-yaml-4x4
```

For another kernel using the same generated RTL, regenerate that kernel's C API
and run the matching C test:

```bash
python scripts/generate_cgra_c_api.py --arch-yaml configs/arch/arch.yaml --soc-yaml configs/soc/cgra_soc.yaml configs/kernels/kernel_relu4x4_4x4.yaml --output-dir tests/generated
./run-chipyard-cgra-test.sh cgra-relu4x4
```

Use `--rebuild` after editing generated RTL, generated Chipyard Scala resources,
`CGRA.scala`, wrapper Verilog, or config fragments that affect hardware.

## Verified CPU+CGRA Tests

Currently verified through the top-level runner:

- `cgra-fir-yaml-4x4`: PASS, checks completion/result.
- `cgra-relu4x4`: PASS, checks readback for addresses `0..31`.

Other supported tests are tracked in the top-level `README.md` and `AGENT.md`.

## Generated Files Consumed Here

`scripts/generate_single_cgra.py` updates these Chipyard-side files:

- `generators/chipyard/src/main/resources/vsrc/CgraTemplateRTL_single__pickled.v`
- `generators/chipyard/src/main/resources/vsrc/CgraTemplateRTL_single_wrapper.v`
- `generators/chipyard/src/main/scala/example/CGRAGenerated.scala`

Do not hand-edit these generated files unless deliberately debugging generator
output. Regenerate from the top-level layered YAML configs instead.

`scripts/openfpga/generate.py` updates these Chipyard-side files:

- `generators/chipyard/src/main/resources/vsrc/openfpga_and2/`
- `generators/chipyard/src/main/scala/example/OpenFPGAGenerated.scala`
- `generators/chipyard/src/main/scala/config/OpenFPGAConfigGenerated.scala`

Do not hand-edit generated OpenFPGA collateral as a lasting fix. Regenerate from
the top-level OpenFPGA YAML config and derived formal-netlist interface instead.

## Important Constraints

- Packet layout is generated into the top-level `tests/include/cgra_layout.h`.
- Baremetal tests should use `tests/include/cgra_runtime.h` instead of hardcoded
  packet field offsets.
- Completion handling is wrapper-defined; the wrapper tracks expected
  `CMD_COMPLETE` count from the host side.
- The `RESULT` helper exposes the last observed 32-bit payload from
  `CMD_COMPLETE`.
- `read_mem(addr)` uses a CGRA load request/response path through the wrapper
  and is used by ReLU, GEMV, Histogram, and AXPY result checks.
