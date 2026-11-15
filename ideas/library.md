# Library

1. **Programs of the system lack the environment and the network**: no variables of the environment, no sockets, and no processes (Build 1), so a mail client took its credentials from a file and its server on stdin and stdout. Solution: `os_env` of a variable as an option, and later sockets as streams of `IO` of `ulang/prelude/io.u`, in both backends.
