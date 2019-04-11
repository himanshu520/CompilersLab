**Compilers Project**

**Himanshu Rai**
**111601032**

**Source - **Tiger
**Target - **MIPS Assembly

The project contains modules to implement lexing, parsing, semantic analysis, conversion to IR, instruction selection and finally register allocation. The external language runtime (which should be written in C) has not been implemented. This is required for using library functions and to declare strings, arrays and records.

The tiger language specifications can be found at [tiger-spec.pdf](https://cs.nyu.edu/courses/fall13/CSCI-GA.2130-001/tiger-spec.pdf)

The final submission can be found in **Demo** directory. The test files can be found in **Testfiles** directory.
To run a tiger code copy the code in **a.tig** file in the Demo directory and from inside the directory on terminal run the command **sml test.sml**. The output MIPS assembly code would be in **a.tig.s** file.

Of the above mentioned features (at the time of submission) upto instruction selection everything is working fine but there are problems with the register allocation. It either raises exception or falls into infinite loop when there are function declarations (the functions are allocated registers different from the main skeleton code (which itself is something like main() function in C)).
However semantic analysis, conversion to IR and instruction selection phases works fine for them.