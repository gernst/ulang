# Library

1. **No binary data**: strings are code points, and the procedures of `os` read and write UTF-8 only, so a file that is not UTF-8 cannot be read. Solution: a type `Byte` with `Array[Byte]`, and procedures of `os` that read and write bytes, with explicit encodings between them and strings.
2. **Common helpers are missing**, which programs write again, as the Markdown converter of an agent did: `min` and `max` of integers, `at` of lists, `trim`, `lines`, `take_while`, `drop_while`, and `span`. Solution: add those that are clearly general, as Code 4 asks, since every name of the library is a candidate of overloading in every module.
