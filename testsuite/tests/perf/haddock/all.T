# Note [Haddock runtime stats files]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When one of the build systems builds a complete GHC distribution,
# haddock gets built and then used to generate .haddock files for each
# library. For that last step, both build systems pass an extra
# `+RTS -t<some path>.t` to record runtime statistics to the given path.
#
# Those .t files are then used by a few haddock perf tests (which all live
# under testsuite/tests/perf/haddock/). Since each build system needs to produce
# those files in different places, the testsuite driver takes the directory
# under which those files are placed as a configuration parameter,
# `config.stats_files_dir`. Each individual test then specifies the name of
# the (runtime statistics) file against which some checks are to be performed,
# in addition to declaring the test's type to be `static_stats`.

# We do not add peak_megabytes_allocated and max_bytes_used to these tests, as
# they are somewhat unreliable, and it is harder to re-run these numbers to
# detect outliers, as described in Note [Measuring residency]. See #9556.

import re

def mark_perf_test(name, opts):
    opts.is_stats_test = True

def read_stats_ms(name, metric):
    def read(way):
        stats_path = in_testdir(statsFile(False, name))
        try:
            stats_file_contents = stats_path.read_text()
        except IOError as e:
            raise StatsException(str(e))

        match = re.search(r'\("' + re.escape(metric) + r'", "([0-9.]+)"\)', stats_file_contents)
        if match is None:
            raise StatsException("No such metric")
        return int(round(float(match.group(1)) * 1000))
    return read

async def haddock_parallel(name, way):
    cmd = (
        "python3 gen_haddock_parallel.py haddock-par-src 300 && "
        "{haddock} --html --quickjump --hyperlinked-source "
        "-t \"Haddock Concurrency Test\" -o haddock-out "
        "haddock-par-src/Mod*.hs -j2 +RTS -N2 -RTS"
    )
    return await simple_run(name, way, cmd, getTestOpts().extra_run_opts)

test('haddock.base',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',5, static_stats_file='base.t')
      ],
     static_stats,
     [])

test('haddock.Cabal',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',5, static_stats_file='Cabal.t')
      ],
     static_stats,
     [])

test('haddock.compiler',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',10, static_stats_file='ghc.t')
      ],
     static_stats,
     [])

test('haddock.parallel',
     [unless(in_tree_compiler(), skip), req_haddock
     # Generate a 300-module dependency chain and measure haddock wall time:
     # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/15173
     ,mark_perf_test
     ,only_ways(['normal'])
     ,ignore_stdout, ignore_stderr
     ,extra_files(['gen_haddock_parallel.py'])
     ,collect_generic_stat('total_wall_ms', 20, read_stats_ms('haddock.parallel', 'total_wall_seconds'))
      ],
     haddock_parallel,
     [])
