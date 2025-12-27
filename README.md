# Slang Language Documentation

Slang is a high-level programming language that compiles to IC10 assembly for [Stationeers](https://store.steampowered.com/app/544550/Stationeers/).
It provides a familiar C-like syntax while targeting the limited instruction set
of in-game IC10.

## Quick Links

- [Getting Started](docs/getting-started.md) - Installation and first program
- [Language Reference](docs/language-reference.md) - Complete syntax guide
- [Built-in Functions](docs/builtins.md) - System calls and math functions
- [Examples](docs/examples.md) - Real-world code samples

## Overview

Slang aims to reduce the time spent writing IC10 assembly by providing:

- **Familiar syntax** - C-like declarations, control flow, and expressions
- **Device abstraction** - Named device bindings with property access
- **Automatic register allocation** - No manual register management
- **Built-in functions** - Math operations and device I/O as function calls
- **Temperature literals** - Native support for Celsius, Fahrenheit, and Kelvin

## Example

```mips
device gasSensor = "d0";
device airCon = "d1";

const TARGET_TEMP = 20c;

loop {
    yield();
    airCon.On = gasSensor.Temperature > TARGET_TEMP;
}
```

This compiles to IC10 that monitors temperature and controls an air
conditioner.

## Project Status

Slang is under active development. It may produce suboptimal code for complex programs.
It is not a replacement for IC10, for performance-critical or large scripts,
hand-written IC10 may still be preferred.
