import resource, subprocess, sys, os
env = dict(os.environ, GC_DONT_GC="1")
r = subprocess.run(sys.argv[2:], env=env, capture_output=True, text=True, timeout=300)
u = resource.getrusage(resource.RUSAGE_CHILDREN)
print(f"{sys.argv[1]}: exit {r.returncode}, {u.ru_utime:.2f} s user, peak {u.ru_maxrss // 1024} MB")
for line in (r.stdout + r.stderr).splitlines()[-2:]: print("   ", line[:200])
