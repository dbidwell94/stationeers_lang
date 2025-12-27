# Built-in Functions

<!--toc:start-->

- [Built-in Functions](#built-in-functions)
  - [System Functions](#system-functions)
    - [`yield()`](#yield)
    - [`sleep(ticks)`](#sleepticks)
    - [`hash(prefabName)`](#hashprefabname)
  - [Device I/O Functions](#device-io-functions)
    - [Reading from Devices](#reading-from-devices)
      - [Load from device](#load-from-device)
      - [Load From Device Batched](#load-from-device-batched)
      - [Load From Device Batched Named](#load-from-device-batched-named)
      - [Load Slot](#load-slot)
      - [Load Reagent](#load-reagent)
    - [Writing to Devices](#writing-to-devices)
      - [Set On Device](#set-on-device)
      - [Set On Device Batched](#set-on-device-batched)
      - [Set On Device Batched Named](#set-on-device-batched-named)
      - [Set Slot](#set-slot)
  - [Math Functions](#math-functions)
    - [Trigonometric Functions](#trigonometric-functions)
      - [Trig Example](#trig-example)
    - [Rounding Functions](#rounding-functions)
      - [Rounding Example](#rounding-example)
    - [Other Math Functions](#other-math-functions)
      - [Math Example](#math-example)
  - [See Also](#see-also)
  <!--toc:end-->

Slang provides built-in functions for device I/O and mathematical operations.
These map directly to IC10 instructions.

## System Functions

### `yield()`

Pauses execution for exactly one game tick.

```rust
yield();
```

**IC10:** `yield`

---

### `sleep(ticks)`

Pauses execution for the specified number of ticks.

```rust
sleep(10);  // Sleep for 10 ticks
```

**IC10:** `sleep ticks`

---

### `hash(prefabName)`

Computes the in-game hash for a prefab name. The hash is computed at compile
time and no runtime code is generated.

```rust
const AC_HASH = hash("StructureAirConditioner");
```

**Note:** This is different from IC10's `hash` instruction, which computes the
hash at runtime.

```rust
setBatched(AC_HASH, "On", 0);
```

**IC10:** `sb -2087593337 On 0` (no hash computation at runtime)

---

## Device I/O Functions

### Reading from Devices

#### Load from device

`load(device, property)` / `l(device, property)`

Loads a property value from a device:

```rust
let temp = load(sensor, "Temperature");
let temp = l(sensor, "Temperature");

// Preferred: use dot notation
let temp = sensor.Temperature;
```

**IC10:** `l r? d? var`

---

#### Load From Device Batched

`loadBatched(deviceHash, property, batchMode)` / `lb(...)`

Loads a property from all devices matching a hash, aggregated by batch mode:

```rust
const SENSOR = hash("StructureGasSensor");
let avgTemp = loadBatched(SENSOR, "Temperature", "Average");
let maxTemp = lb(SENSOR, "Temperature", "Maximum");
```

**Batch Modes:** `"Average"`, `"Sum"`, `"Minimum"`, `"Maximum"`

**IC10:** `lb r? deviceHash logicType batchMode`

---

#### Load From Device Batched Named

`loadBatchedNamed(deviceHash, nameHash, property, batchMode)` / `lbn(...)`

Loads a property from devices matching both device hash and name hash:

```rust
const SENSOR_HASH = hash("StructureGasSensor");
const SENSOR_NAME_HASH = hash("Outdoor Gas Sensor");
let avgTemp = loadBatchedNamed(SENSOR_HASH, SENSOR_NAME_HASH, "Temperature", "Average");
let maxTemp = lbn(SENSOR_HASH, SENSOR_NAME_HASH, "Temperature", "Maximum");
```

**IC10:** `lbn r? deviceHash nameHash logicType batchMode`

**Note:** This function is useful when a script interfaces with a lot of
devices, as it allows for arbitrary device access without limited to the 6 `dx` pins.

---

#### Load Slot

`loadSlot(device, slotIndex, property)` / `ls(...)`

Loads a slot property from a device:

```rust
let occupied = loadSlot(sorter, 0, "Occupied");
let occupied = ls(sorter, 0, "Occupied");
```

**IC10:** `ls r? d? slotIndex logicSlotType`

---

#### Load Reagent

`loadReagent(device, reagentMode, reagentHash)` / `lr(...)`

Loads reagent information from a device:

```rust
let amount = loadReagent(furnace, "Contents", reagentHash);
let amount = lr(furnace, "Contents", reagentHash);
```

**IC10:** `lr r? d? reagentMode reagentHash`

---

### Writing to Devices

#### Set On Device

`set(device, property, value)` / `s(...)`

Sets a property on a device:

```rust
set(valve, "On", true);
s(valve, "On", true);

// Preferred: use dot notation
valve.On = true;
```

**IC10:** `s d? logicType r?`

---

#### Set On Device Batched

`setBatched(deviceHash, property, value)` / `sb(...)`

Sets a property on all devices matching a hash:

```rust
const LIGHT_HASH = hash("StructureWallLight");
setBatched(LIGHT_HASH, "On", true);
sb(LIGHT_HASH, "On", true);
```

**IC10:** `sb deviceHash logicType r?`

**Note:** This function is useful when a script interfaces with a lot of devices,
as it allows for arbitrary device access without limited to the 6 `dx` pins.

---

#### Set On Device Batched Named

`setBatchedNamed(deviceHash, nameHash, property, value)` / `sbn(...)`

Sets a property on devices matching both device hash and name hash:

```rust
const SENSOR_HASH = hash("StructureGasSensor");
const SENSOR_NAME_HASH = hash("Outdoor Gas Sensor");
setBatchedNamed(SENSOR_HASH, SENSOR_NAME_HASH, "On", true);
sbn(SENSOR_HASH, SENSOR_NAME_HASH, "On", true);
```

**IC10:** `sbn deviceHash nameHash logicType r?`

---

#### Set Slot

`setSlot(device, slotIndex, property, value)` / `ss(...)`

Sets a slot property on a device:

```rust
setSlot(sorter, 0, "Open", true);
ss(sorter, 0, "Open", true);
```

**IC10:** `ss d? slotIndex logicSlotType r?`

---

## Math Functions

All math functions accept numbers, variables, or expressions as arguments.

### Trigonometric Functions

| Function      | Description                  | IC10    |
| ------------- | ---------------------------- | ------- |
| `sin(x)`      | Sine of angle in radians     | `sin`   |
| `cos(x)`      | Cosine of angle in radians   | `cos`   |
| `tan(x)`      | Tangent of angle in radians  | `tan`   |
| `asin(x)`     | Arc sine, returns radians    | `asin`  |
| `acos(x)`     | Arc cosine, returns radians  | `acos`  |
| `atan(x)`     | Arc tangent, returns radians | `atan`  |
| `atan2(y, x)` | Two-argument arc tangent     | `atan2` |

#### Trig Example

```rust
let angle = atan2(y, x);
let sineValue = sin(angle);
```

### Rounding Functions

| Function   | Description                   | IC10    |
| ---------- | ----------------------------- | ------- |
| `ceil(x)`  | Round up to nearest integer   | `ceil`  |
| `floor(x)` | Round down to nearest integer | `floor` |
| `trunc(x)` | Remove decimal portion        | `trunc` |
| `abs(x)`   | Absolute value                | `abs`   |

#### Rounding Example

```rust
let rounded = floor(3.7);  // 3
let positive = abs(-5);    // 5
```

### Other Math Functions

| Function    | Description                   | IC10   |
| ----------- | ----------------------------- | ------ |
| `sqrt(x)`   | Square root                   | `sqrt` |
| `log(x)`    | Natural logarithm             | `log`  |
| `max(a, b)` | Maximum of two values         | `max`  |
| `min(a, b)` | Minimum of two values         | `min`  |
| `rand()`    | Random number between 0 and 1 | `rand` |

#### Math Example

```rust
let root = sqrt(16);        // 4
let bigger = max(a, b);
let randomVal = rand();
```

## See Also

- [Language Reference](language-reference.md) — Complete syntax guide
- [Examples](examples.md) — Real-world code samples
