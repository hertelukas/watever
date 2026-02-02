#!/usr/bin/python3
# SPDX-FileCopyrightText: 2025 Contributors to TPDE <https://tpde.org>
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

import argparse
import subprocess
import tempfile

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--timeout", "-t", type=int, default=10)
    parser.add_argument("--steps", "-n", type=int, default=10)
    parser.add_argument("generator")
    parser.add_argument("cmd", nargs=argparse.REMAINDER)
    args = parser.parse_args()

    res = []
    # Double size until we reach suitable runtimes (>0.1s), then increase
    # size linearly multiple times.
    n, step = 1, 0
    while step == 0 or n <= step * args.steps:
        try:
            inp = subprocess.run(["python3", args.generator, f"{n}"],
                                 capture_output=True, check=True).stdout
            with tempfile.NamedTemporaryFile(mode="r", encoding="ascii") as tmp:
                cmd = ["/usr/bin/time", "-f%e %M", "-o", tmp.name] + args.cmd
                subprocess.run(cmd, check=True, input=inp,
                               timeout=args.timeout,
                               stdout=subprocess.DEVNULL,
                               stderr=subprocess.PIPE)
                time, mem = map(float, tmp.read().split())
        except KeyboardInterrupt:
            # Catch KeyboardInterrupt to print summary.
            break
        except subprocess.CalledProcessError as e:
            print(f"n={n}:", e)
            print(e.stderr.decode("utf-8"))
            break
        except Exception as e:
            print(f"n={n}:", e)
            break

        res.append((n, time, mem))
        print(n, time, mem)

        est = time / n
        if step > 0:
            n += step
        else:
            if time > 0.1:
                step = n
            n *= 2
        # Don't attempt new size if it will likely hit the timeout
        if est * n > args.timeout:
            break

    if not res:
        exit(1)

    try:
        import numpy as np
        # Try a linear or quadratic polynomial fit. Normalize values to (0,1) so
        # that it is easy to determine whether a linear approximation is close
        # enough or whether the behavior is more likely to be super-linear.
        xs, ys = [x[0]/res[-1][0] for x in res], [x[1]/res[-1][1] for x in res]
        for deg in range(1, 4):
            pfit, resid, rank, sv, rcond = np.polyfit(xs, ys, deg, full=True)
            if resid < 0.01:
                print("Polynomial:", np.polynomial.Polynomial(pfit[::-1]))
                break
    except ImportError:
        pass
