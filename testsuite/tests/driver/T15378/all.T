test('T15378',
     [ req_ghc_smp
     , extra_files(['A.hs', 'B.hs'])
     ],
     makefile_test, [])
