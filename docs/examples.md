# Examples

Real-world Slang programs demonstrating common patterns.

## Temperature Control

Basic thermostat that controls an air conditioner based on room temperature:

```rust
device ac = "db";
device roomGasSensor = "d0";

const TARGET_TEMP = 22c;
const HYSTERESIS = 1;

loop {
    yield();
    let temp = roomGasSensor.Temperature;

    if (temp > TARGET_TEMP + HYSTERESIS) {
        ac.On = true;
    } else if (temp < TARGET_TEMP - HYSTERESIS) {
        ac.On = false;
    }
}
```

**Note:** The IC10 chip is assumed to be inserted in the air conditioner's IC slot.

---

## Two-Axis Solar Panel Tracking

Handles two-axis solar panel tracking based on the sun's position:

```rust
device sensor = "d0";

const H_PANELS = hash("StructureSolarPanelDual");

loop {
    setBatched(H_PANELS, "Horizontal", sensor.Horizontal);
    setBatched(H_PANELS, "Vertical", sensor.Vertical + 90);
    yield();
}
```

**Note:** Assumes the daylight sensor is mounted with its port looking 90
degrees east of the solar panel's data port, an offset can be added on the
horizontal angle if needed.

---

## Day/Night Lighting

Controls grow lights during the day and ambient lights at night:

```rust
device greenhouseSensor = "d0";

const daylightSensor = hash("StructureDaylightSensor");
const growLight = hash("StructureGrowLight");
const wallLight = hash("StructureLightLong");

loop {
    yield();
    let solarAngle = lb(daylightSensor, "SolarAngle", "Average");
    let isDaylight = solarAngle < 90;

    sb(growLight, "On", isDaylight);
    sb(wallLight, "On", !isDaylight);
}
```

---

## Pressure Relief Valve

Controls a volume pump based on pressure readings for emergency pressure relief:

```rust
device volumePump = "d0";
device pipeSensor = "d1";

const MAX_PRESSURE = 10_000;
const R = 8.314;

loop {
    yield();

    let pressure = pipeSensor.Pressure;

    if (pressure > MAX_PRESSURE) {
        // Use PV=nRT to calculate the amount of mols we need to move
        // n = PV / RT
        let molsToMove = (pressure - MAX_PRESSURE) *
          pipeSensor.Volume / (R * pipeSensor.Temperature);

        // V = nRT / P
        let setting = molsToMove * R * pipeSensor.Temperature / pressure;
        volumePump.Setting = setting;
        volumePump.On = true;
    } else {
        volumePump.On = false;
    }
}
```

---

## Greenhouse Environment Controller

Complete greenhouse control with pressure, temperature, and lighting:

```rust
device self = "db";
device emergencyRelief = "d0";
device greenhouseSensor = "d1";
device recycleValve = "d2";

const MAX_INTERIOR_PRESSURE = 80;
const MAX_INTERIOR_TEMP = 28c;
const MIN_INTERIOR_PRESSURE = 75;
const MIN_INTERIOR_TEMP = 25c;
const daylightSensor = 1076425094;
const growLight = hash("StructureGrowLight");
const wallLight = hash("StructureLightLong");
const lightRound = hash("StructureLightRound");

let shouldPurge = false;

loop {
    yield();
    let interiorPress = greenhouseSensor.Pressure;
    let interiorTemp = greenhouseSensor.Temperature;

    shouldPurge = (
        interiorPress > MAX_INTERIOR_PRESSURE ||
        interiorTemp > MAX_INTERIOR_TEMP
    ) || shouldPurge;

    emergencyRelief.On = shouldPurge;
    recycleValve.On = !shouldPurge;

    if (
        shouldPurge && (
          interiorPress < MIN_INTERIOR_PRESSURE &&
          interiorTemp < MIN_INTERIOR_TEMP
          )
        ) {
        shouldPurge = false;
    }

    let solarAngle = lb(daylightSensor, "SolarAngle", "Average");
    let isDaylight = solarAngle < 90;

    sb(growLight, "On", isDaylight);
    sb(wallLight, "On", !isDaylight);
    sb(lightRound, "On", !isDaylight);
}
```

---

## Advanced Furnace Pressure Control

Automates multi-furnace pump control based on dial setting for pressure target:

```rust
const FURNACE1 = 1234;
const DIAL1 = 1123;
const ANALYZER1 = 1223;

const FURNACE2 = 1235;
const DIAL2 = 1124;
const ANALYZER2 = 1224;

const FURNACE3 = 1236;
const DIAL3 = 1124;
const ANALYZER3 = 1225;

const R = 8.314;

fn handleFurnace(furnace, dial, analyzer) {
    let pressure = furnace.Pressure;
    let targetPressure = max(dial.Setting, 0.1) * 1000;

    if (abs(targetPressure - pressure) <= 0.1) {
        furnace.On = false;
        return;
    }

    let molsToMove = max(furnace.TotalMoles, 1) * (
      (targetPressure / pressure) - 1
    );

    // V = nRT / P
    if (molsToMove > 0) {
        // Calculate volume required
        if (analyzer.Pressure == 0) {
            // No more gas to add
            furnace.On = false;
            return;
        }
        let volume = molsToMove * R * analyzer.Temperature / analyzer.Pressure;

        furnace.On = true;
        furnace.SettingOutput = 0;
        furnace.SettingInput = volume;

        return;
    }

    // Calculate volume required
    let volume = (-molsToMove) * R * furnace.Temperature / pressure;

    furnace.On = true;
    furnace.SettingInput = 0;
    furnace.SettingOutput = volume;

    return;
}

loop {
    yield();

    handleFurnace(FURNACE1, DIAL1, ANALYZER1);
    handleFurnace(FURNACE2, DIAL2, ANALYZER2);
    handleFurnace(FURNACE3, DIAL3, ANALYZER3);
}
```

**Note:** This example does not handle edge cases such as insufficient gas in
the input network or overfilling the furnace/pipe network.

---

## Common Patterns

### Waiting for a Condition

```rust
fn waitForDeviceToTurnOff(device) {
    while (device.On) {
        yield();
    }
}
```

## See Also

- [Getting Started](getting-started.md) — First steps with Slang
- [Language Reference](language-reference.md) — Complete syntax guide
- [Built-in Functions](builtins.md) — System calls and math functions
