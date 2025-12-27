# Language Reference

<!--toc:start-->

- [Language Reference](#language-reference)
  - [Literals](#literals)
    - [Numbers](#numbers)
    - [Temperature Literals](#temperature-literals)
    - [Booleans](#booleans)
    - [Strings](#strings)
  - [Variables](#variables)
    - [`let` - Mutable Variables](#let-mutable-variables)
    - [`const` - Constants](#const-constants)
  - [Device Declarations](#device-declarations)
    - [Device Property Access](#device-property-access)
    - [Device Property Assignment](#device-property-assignment)
  - [Operators](#operators)
    - [Arithmetic Operators](#arithmetic-operators)
    - [Comparison Operators](#comparison-operators)
    - [Logical Operators](#logical-operators)
    - [Ternary Operator](#ternary-operator)
    - [Operator Precedence](#operator-precedence)
  - [Control Flow](#control-flow)
    - [`if` / `else`](#if-and-else)
    - [`loop`](#loop)
    - [`while`](#while)
    - [`break`](#break)
    - [`continue`](#continue)
  - [Functions](#functions)
    - [Declaration](#declaration)
    - [Invocation](#invocation)
    - [Return Values](#return-values)
  - [Parentheses for Grouping](#parentheses-for-grouping)
  - [See Also](#see-also)
  <!--toc:end-->

Complete syntax reference for the Slang programming language.

## Literals

### Numbers

Numbers can be integers or decimals. Underscores are allowed as visual
separators:

```rust
const integer = 42;                 // Integer
const decimal = 3.14;               // Decimal
const million = 1_000_000;          // Integer with separators
const decimalSeparators = 5_000.50; // Decimal with separators
```

### Temperature Literals

Append a unit suffix to specify temperature. Values are automatically converted
to Kelvin at compile time:

| Suffix | Unit       | Example |
| ------ | ---------- | ------- |
| `c`    | Celsius    | `20c`   |
| `f`    | Fahrenheit | `68f`   |
| `k`    | Kelvin     | `293k`  |

```rust
const ROOM_TEMP = 20c;       // Converts to 293.15 Kelvin
const FREEZING  = 32f;       // Converts to 273.15 Kelvin
const ABSOLUTE1 = 0k;        // Already in Kelvin
const ABSOLUTE2 = 0;         // Assumed to be in Kelvin
```

### Booleans

Booleans compile to integer values `1` and `0` in IC10.

```rust
device ac = "d0";

ac.Mode = false;
ac.On = true;
```

### Strings

Strings use double or single quotes. They are primarily used for prefab and
name hashes.

```rust
const AC_HASH = hash("StructureAirConditioner");
const AC_NAME_HASH = hash("Greenhouse Air Conditioner");
```

## Variables

### `let` Mutable Variables

Declares a variable that can be reassigned:

```rust
let counter = 0;
// ...
counter = counter + 1;
```

### `const` Constants

Declares a compile-time constant. Constants are inlined and do not consume
registers:

```rust
const MAX_PRESSURE = 10_000;
const DOOR_HASH = hash("StructureCompositeDoor");
```

Constants support the `hash()` function for compile-time hash computation.

## Device Declarations

The `device` keyword binds a device port or reference ID to a named variable:

```rust
device self = "db";   // IC housing, or device the IC is plugged into (eg. an AC)
device sensor = "d0"; // Device at port d0
device valve = "d1";  // Device at port d1
device ac1 = "$3FC";  // Device with reference ID $3FC (hexadecimal 1020)
device ac2 = "1020";  // Device with reference ID 1020 (decimal)
```

**Note:** Reference IDs can be found in-game using the Configuration cartridge.

### Device Property Access

Read device properties using dot notation:

```rust
let temp = sensor.Temperature;
let pressure = sensor.Pressure;
let isOn = valve.On;
```

### Device Property Assignment

Write to device properties using dot notation:

```rust
valve.On = true;
valve.Setting = 100;
```

## Operators

### Arithmetic Operators

| Operator | Description    | Example  |
| -------- | -------------- | -------- |
| `+`      | Addition       | `a + b`  |
| `-`      | Subtraction    | `a - b`  |
| `*`      | Multiplication | `a * b`  |
| `/`      | Division       | `a / b`  |
| `%`      | Modulo         | `a % b`  |
| `**`     | Exponentiation | `a ** b` |
| `-`      | Negation       | `-a`     |

### Comparison Operators

| Operator | Description           | Example  |
| -------- | --------------------- | -------- |
| `==`     | Equal                 | `a == b` |
| `!=`     | Not equal             | `a != b` |
| `<`      | Less than             | `a < b`  |
| `>`      | Greater than          | `a > b`  |
| `<=`     | Less than or equal    | `a <= b` |
| `>=`     | Greater than or equal | `a >= b` |

### Logical Operators

| Operator | Description | Example    |
| -------- | ----------- | ---------- |
| `&&`     | Logical AND | `a && b`   |
| `\|\|`   | Logical OR  | `a \|\| b` |
| `!`      | Logical NOT | `!a`       |

### Ternary Operator

Conditional expressions using `?` and `:`:

```rust
let result = condition ? valueIfTrue : valueIfFalse;
```

### Operator Precedence

Operators are evaluated in the following order, from highest to lowest
precedence:

| Precedence | Operator(s)       | Description                      |
| ---------- | ----------------- | -------------------------------- |
| 1          | `()` `.`          | Grouping, Property access        |
| 2          | `!` `-`           | Logical NOT, Negation            |
| 3          | `**`              | Exponentiation                   |
| 4          | `*` `/` `%`       | Multiplication, Division, Modulo |
| 5          | `+` `-`           | Addition, Subtraction            |
| 6          | `<` `<=` `>` `>=` | Comparison                       |
| 7          | `==` `!=`         | Equality                         |
| 8          | `&&`              | Logical AND                      |
| 9          | `\|\|`            | Logical OR                       |
| 10         | `?:`              | Ternary conditional              |
| 11         | `=`               | Assignment                       |

Use parentheses to override precedence:

```rust
let result = (20 + 10) * 5;
```

## Control Flow

### if and else

Conditional branching:

```rust
if (tank.Temperature > 30c) {
    ac.On = true;
} else {
    ac.On = false;
}
```

### `loop`

Infinite loop that runs until `break`:

```rust
loop {
    yield();
    // Loop body
    if (condition) {
        break;  // Exit the loop
    }
}
```

### `while`

Conditional loop that runs while the condition is true:

```rust
while (counter < 100) {
    counter = counter + 1;
    yield();
}
```

### `break`

Exits the current loop:

```rust
loop {
    yield();
    // ...
    if (done) {
        break;
    }
}
```

### `continue`

Skips to the next iteration of the current loop:

```rust
loop {
    yield();
    if (shouldSkip) {
        continue;
    }
    // This code is skipped when shouldSkip is true
    // ...
}
```

## Functions

**Warning:** Functions are currently experimental and may produce suboptimal code.

### Declaration

```rust
fn functionName(arg1, arg2) {
    // Function body
    return arg1 + arg2;
}
```

### Invocation

```rust
let result = functionName(10, 20);
```

### Return Values

Use `return` to exit a function and optionally return a value:

```rust
fn calculate(x) {
    if (x < 0) {
        return 0;  // Early return
    }

    return x * 2;
}

fn doWork() {
    // No return value
    return;
}
```

## Parentheses for Grouping

Use parentheses to control operator precedence:

```rust
let result = (a + b) * c;

let complex = (
    temp > 0c &&
    stress < 50 &&
    (pressure < 10_000 || temp > 20c)
);
```

## See Also

- [Getting Started](getting-started.md) — First steps with Slang
- [Built-in Functions](builtins.md) — System calls and math functions
- [Examples](examples.md) — Real-world code samples
