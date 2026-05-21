# Tests for whole-program dead code elimination (-fwhole-program-dce)

# Basic DCE test - dead function should not prevent compilation
test('T_dce_basic', normal, compile_and_run, ['-fwhole-program-dce'])

# Test that live functions are preserved
test('T_dce_live', normal, compile_and_run, ['-fwhole-program-dce'])

# Cross-module DCE test
test('T_dce_cross_mod',
     [extra_files(['T_dce_cross_mod_A.hs'])],
     multimod_compile_and_run,
     ['T_dce_cross_mod', '-fwhole-program-dce'])

# Test DCE with typeclass instances
test('T_dce_instance', normal, compile_and_run, ['-fwhole-program-dce'])

# Test that DCE doesn't break normal compilation
test('T_dce_noop', normal, compile_and_run, [''])
