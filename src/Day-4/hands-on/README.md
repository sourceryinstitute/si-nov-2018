Hands-on Exercise
=================

This directory provides a tests to drive your development of 
1. An asynchronous emulated co_sum collective subroutine.
2. An Abstract Calculus pattern.

Build instructions
------------------
Once completed, the code willl build with `cmake` as follows:
```
  mkdir build
  cd build
  FC=caf cmake ..
  make
```

Exercise 1
----------
Following the asynchronous hell world example in ../code-jam,
try replacing all uses of `sync images` with counting semaphores
as implemented via `event_type`.

Exercise 2
----------
Follow the build errors to complete the `scalar_field` test.
