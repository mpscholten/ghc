# Test for dead strip support on AArch64/Darwin (GHC #24962)
# See Note [Dead strip prevention on AArch64/Darwin] in GHC.CmmToAsm.AArch64.Ppr
#
# This test verifies that the NCG generates correct assembly for Darwin's
# dead code stripping to work properly. Without the .alt_entry fix,
# info tables would be incorrectly stripped, causing crashes.

test('T24962',
     [when(not opsys('darwin'), skip),
      # Only test NCG, not LLVM (LLVM has its own handling)
      omit_ways(['llvm', 'optllvm', 'debugllvm']),
      # Pass -dead_strip to the linker
      extra_hc_opts('-optl-Wl,-dead_strip')],
     compile_and_run, [''])
