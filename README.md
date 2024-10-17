# Code Profiling Demos
```
 ____             __ _ _ _               ____
|  _ \ _ __ ___  / _(_) (_)_ __   __ _  |  _ \  ___ _ __ ___   ___
| |_) | '__/ _ \| |_| | | | '_ \ / _` | | | | |/ _ \ '_ ` _ \ / _ \
|  __/| | | (_) |  _| | | | | | | (_| | | |_| |  __/ | | | | | (_) |
|_|   |_|  \___/|_| |_|_|_|_| |_|\__, | |____/ \___|_| |_| |_|\___/
                                 |___/
```

This repo contains example code to demonstrate a range of
"realistic" scenarios for code profiles. We try and show
system calls (blocked in the kernel), long running functions,
much repeated functions etc. 

The idea is to give an introduction to reading profiles for
optimisation without needing to understand complicated code.

# How to use the Code
All cases let us run a few different "scenarios" which change
the balance between different sorts of "compute". We mock all of the compute
using a busy-sleep. Each run takes around half a minute, to get good
samples. This can be altered using the `scal` variable in the various codes.

## Fortran
In the Fortran subdirectory we have a single main `example.f90`
which takes an (optional) command-line parameter "run=N".
The build script `build` should build this using gfortran.
It's a trivial build so easy to modify for other compilers,
you just need to specify the F2008 standard and include
profiling flags as relevant. 

After building, run the code and follow the relevant profiling
instructions.

## Other Languages
WIP


## Acknowledgements

Banner text generated using `figlet`

Fortran helper functions from Warwick RSE's previous Fortran courses

