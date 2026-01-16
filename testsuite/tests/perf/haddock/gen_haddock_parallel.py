import os
import shutil
import sys


def write_file(path, text):
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(text)


def main():
    if len(sys.argv) != 3:
        raise SystemExit("usage: gen_haddock_parallel.py <out-dir> <count>")

    out_dir = sys.argv[1]
    count = int(sys.argv[2])

    if os.path.exists(out_dir):
        shutil.rmtree(out_dir)
    os.makedirs(out_dir, exist_ok=True)

    write_file(
        os.path.join(out_dir, "Mod0.hs"),
        "module Mod0 where\n\n"
        "-- | Base value.\n"
        "f0 :: Int\n"
        "f0 = 0\n",
    )

    for i in range(1, count + 1):
        prev = i - 1
        write_file(
            os.path.join(out_dir, f"Mod{i}.hs"),
            f"module Mod{i} where\n"
            f"import Mod{prev}\n\n"
            "-- | Adds 1 to the previous module's value.\n"
            f"f{i} :: Int\n"
            f"f{i} = f{prev} + 1\n",
        )


if __name__ == "__main__":
    main()
