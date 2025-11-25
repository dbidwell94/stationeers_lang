# Stationeers Language (slang)

This is an ambitious attempt at creating:

- A new programming language (slang)
- A compiler to translate slang -> IC10
- A mod to allow direct input of slang in the in-game script editor to
  automatically compile to IC10 before running

This project currently outputs 3 files:

- A Linux CLI
- A Windows CLI
- A Windows FFI dll
  - Contains a single function: `compile_from_string`

The aim of this project is to lower the amount of time it takes to code simple
scripts in Stationeers so you can get back to engineering atmospherics or
whatever you are working on. This project is NOT meant to fully replace IC10.
Obviously hand-coded assembly written by an experienced programmer is more
optimized and smaller than something that a C compiler will spit out. This is
the same way. It WILL produce valid IC10, but for large complicated projects it
might produce over the allowed limit of lines the in-game editor supports.

Current Unknowns

- Should I support a configurable script line length in-game to allow larger
  scripts to be saved?
- Should compilation be "behind the scenes" (in game editor will ALWAYS be what
  you put in. IC10 will be IC10, slang will be slang)
