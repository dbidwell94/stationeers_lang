# Getting Started

<!--toc:start-->

- [Getting Started](#getting-started)
  - [Program Structure](#program-structure)
  - [The `yield()` Function](#the-yield-function)
  - [Your First Program](#your-first-program)
    - [Explanation](#explanation)
  - [Comments](#comments)
  - [See Also](#see-also)
  <!--toc:end-->

This guide covers the basics of writing your first Slang program.

## Program Structure

A Slang program consists of top-level declarations and a main loop:

```slang
// Device declarations
device self = "db";
device sensor = "d0";

// Constants
const THRESHOLD = 100;

// Variables
let counter = 0;

// Main program loop
loop {
    yield();
    // Your logic here
}
```

## The `yield()` Function

IC10 programs run continuously. The `yield()` function pauses execution for one
game tick, preventing the script from consuming excessive resources.

**Important:** You should always include `yield()` in your main loop unless you
know what you're doing.

```slang
loop {
    yield(); // Recommended!
    // ...
}
```

## Your First Program

Here's a simple program that turns on a light when a gas sensor detects low
pressure:

```slang
device gasSensor = "d0";
device light = "d1";

const LOW_PRESSURE = 50;

loop {
    yield();
    light.On = gasSensor.Pressure < LOW_PRESSURE;
}
```

### Explanation

1. `device gasSensor = "d0"` — Binds the device at port `d0` to the name
   `gasSensor`
2. `device light = "d1"` — Binds the device at port `d1` to the name `light`
3. `const LOW_PRESSURE = 50` — Defines a compile-time constant
4. `loop { ... }` — Creates an infinite loop
5. `yield()` — Pauses for one tick
6. `light.On = gasSensor.Pressure < LOW_PRESSURE` — Reads the pressure and sets
   the light state

## Comments

Slang supports single-line comments and documentation comments:

```slang
// This is a regular comment

/// This is a documentation comment
/// It can span multiple lines
fn myFunction() {
    // ...
}
```

## See Also

- [Language Reference](language-reference.md) — Complete syntax guide
- [Built-in Functions](builtins.md) — Available system calls
- [Examples](examples.md) — Real-world programs and patterns
