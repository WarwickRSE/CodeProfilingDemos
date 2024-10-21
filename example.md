# Generating Profile Graphs

We'll show examples of two tools here: gprof and valgrind's callgrind

In both cases, we make use of two other tools, gprof2dot (installable
via pip) to generate dot format files (a graph definition language)
and dot to turn this into an image. This is part of graphviz (https://www.graphviz.org/)

gprof is older, and instruments the code at compile time. This means
we need to rebuild the code to use it. This has some downsides.
Valgrind involves running code within a special environment - most
codes run a bit slower, some run extremely slowly (note: the profiling
results do account for this), which has its own downsides. 

## Important terms

When discussing the time taken by a given function, there are two
important metrics. 'self' time refers to the time spent within the
specific function, for instance doing arithmetic. 'cumulative' time
refers to the time taken by a function _and_ any functions it calls.

In these demos, almost all of the actual time is in a busy\_sleep
function, which mimics "real" compute, and the 'self' times are rather low.
We wanted to keep the graphs simple, so we just have to imagine
the layers of stuff that would be there in a real code.
The callgrind docs refer to these as exclusive (i.e. exclusively within
this function) and inclusive (i.e. including all functions this calls).

## Old style - Gprof

gprof works by getting the compiler to add calls into your code to
track function calls and time taken. This means we need to compile
with the correct flags, and then run the code to get the results.

Once the code has run a file, gmon.out, is produced. We can then run
the gprof tool on this to generate the data we want. We provide both
the executable (program) file, to tell the tool what symbols are in our code
and the gmon.out file, e.g. `gprof example gmon.out > profile.txt`

This generates a lot of data, so we've redirected it to a file. We can
read this file directly, but we can also make a nice picture to get us started.
The command `gprof2dot profile.txt > profile.dot` followed by 
`dot -Tpng profile.dot -o profile.png` will produce a png file. 

This puts the percentages of the runtime for each function into their
nodes, both the self and the cumulative (see above).
Heavily used functions are coloured red and orange, little used are blue, and ones
in the middle are green.

Reading the raw gprof output is also useful - there are many good tutorials
on how to do this.

## Valgrind tool

Valgrind is very useful as a memory checker, but it also is used as a platform
for many other tools. It effectively intercepts all the calls your code
makes to things like memory allocation and so on, and substitutes its own,
instrumented, ones. This is why large codes can take a long time to run.

We're interested in the 'callgrind' tool for profiling. This time we don't need
to compile with any special flags, we just run the code inside valgrind, using e.g.
`valgrind --tool=callgrind example`. This creates a file `callgrind.out.<PID>` where
`<PID>` is the process id. 

Like before, we can use a tool to transform this raw data into tabular and graph form.
In this case we use `callgrind_annotate --inclusive=yes <filename>` The `inclusive` flag
means to include the time spent in functions called by the named one, as well
as time spent inside it specifically. This prints a summary of the time spent
in each function. 

We can use the `grprof2dot` tool like above to produce a graph. We use the `callgrind.out.<PID>`
file directly, e.g. `gprof2dot --format=callgrind callgrind.out.<PID> > profile.dot` and
`dot -Tpng profile.dot -o profile.png` as before to produce a png file.


