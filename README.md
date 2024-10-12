Ray (path) tracer based on the book [Ray Tracing in One Weekend](https://raytracing.github.io/books/RayTracingInOneWeekend.html) by P. Shirlley, T.D. Black and S. Hollasch.

There is a commit for every chapter of the book (plus some extra for special subsections) where the results of the chapter can be seen. The code can be built and executed with `dune exec raytracing` inside the `bin` folder (due to the path for the result images being harcoded).

Possible improvements:

- Non-harcoded result paths.
- Parallelization of the calculation (final image takes 8h. to be rendered on a 2018 Macbook Air).
