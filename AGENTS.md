# Chipyard integration instructions

This checkout is the Chipyard integration point for CGRA-SoC. Before editing, read the [workspace instructions](../AGENTS.md) and follow their mandatory coding style, working approach, generated-file policy, and GitHub workflow. Keep those shared rules in the workspace file so they have one maintained source.

## Ownership and code locations

CGRA-SoC owns architecture and SoC YAML, generators, runtime headers, and end-to-end runners. Chipyard consumes their generated RTL and parameters and owns Scala/Chisel integration. Use the conventions of nearby Chipyard code; the workspace's C/Python formatters apply only to files owned by the workspace.

| Area | Source |
| --- | --- |
| CGRA RoCC wrapper and DMA integration | [CGRA.scala](generators/chipyard/src/main/scala/example/CGRA.scala) |
| Generated CGRA parameters and command encodings | [CGRAGenerated.scala](generators/chipyard/src/main/scala/example/CGRAGenerated.scala) |
| OpenFPGA TileLink MMIO peripheral | [OpenFPGA.scala](generators/chipyard/src/main/scala/example/OpenFPGA.scala) |
| AutoLink protocol, routing, and joins | [socgen/link](generators/chipyard/src/main/scala/socgen/link) |
| Accelerator endpoint adapters | [socgen](generators/chipyard/src/main/scala/socgen) |

Read the relevant [hardware contracts](../docs/contracts.md) before changing interfaces or configurations. Keep common AutoLink control in `socgen/link` and accelerator-specific behavior in its matching adapter. Payload moves over TileLink.

## CGRA constraints

- Use the current generated top selected by the workspace generators. The single-CGRA flow uses `IntegratedCgraWithDmaRTL_single`; old `CgraRTL_2x2` resources are not its reference design.
- Architecture YAML owns CGRA shape and functional units; SoC YAML owns interface and memory settings. Kernel YAML supplies kernel metadata and execution counts.
- CGRA receives raw packets through RoCC `custom0`. Use `CGRARoCCGenerated` for funct encodings and the generated packet layout in the workspace's `tests/include/cgra_layout.h`. Baremetal code uses `tests/include/cgra_runtime.h` instead of duplicating packet field offsets.
- The wrapper interprets packets needed for host busy, completion, result, and memory readback. It tracks the host's expected `CMD_COMPLETE` count, and `RESULT` exposes the last observed 32-bit completion payload. `read_mem(addr)` uses the CGRA load request/response path.

## OpenFPGA constraints

OpenFPGA attaches as a TileLink MMIO peripheral. Its control, status, frame-based `CFG_WORD`, packed `USER_INPUT`, and packed `USER_OUTPUT` registers follow the generated metadata. The user register layout and GPIO pad map come from the formal verification netlist, not YAML fields. Baremetal tests stream prepacked bitstream words rather than assembling configuration packets at runtime.

The generated manifest uses cell-library `inv.v`, `buf4.v`, and `tap_buf4.v` models because generated `inv_buf_passgate.v` contains tri-state expressions rejected by Verilator. Keep that backend; do not reintroduce Verilator shims or replacement trees unless the workspace OpenFPGA policy changes.

## Generation and validation

Run integration generators and end-to-end tests from the CGRA-SoC workspace root. Use the current YAML and script `--help` when selecting arguments.

- [generate_single_cgra.py](../scripts/generate_single_cgra.py) and [generate_multi_cgra.py](../scripts/generate_multi_cgra.py) emit CGRA RTL, wrappers, and `CGRAGenerated.scala`. [cgra_fast_api.py](../scripts/cgra_fast_api.py) generates the single-CGRA C API.
- [openfpga/generate.py](../scripts/openfpga/generate.py) emits selected fabric resources under `generators/chipyard/src/main/resources/vsrc/`, `OpenFPGAGenerated.scala`, and `OpenFPGAConfigGenerated.scala`.
- Fix generated files through the owning generator. Read the [cgra-validate skill](../.agents/skills/cgra-validate/SKILL.md) for reference tests, runner selection, and rebuild rules. Changes to generated RTL, Scala integration, wrappers, or hardware configuration require the matching top-level runner with `--rebuild`.
- Documentation-only changes require Markdown/link checks and `git diff --check`; do not build a simulator for them.

Validate and commit Chipyard-owned changes here before updating the workspace submodule pointer. Preserve unrelated edits and nested submodule state. Update this file when Chipyard-specific ownership or integration contracts change.
