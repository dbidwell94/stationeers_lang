// Pressure numbers are in KPa

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
  let interiorPress = greenhouseSensor.Pressure;
  let interiorTemp = greenhouseSensor.Temperature;
  
  shouldPurge = (
    interiorPress > MAX_INTERIOR_PRESSURE ||
    interiorTemp > MAX_INTERIOR_TEMP
    ) || shouldPurge;

  emergencyRelief.On = shouldPurge;
  recycleValve.On = !shouldPurge;

  if (shouldPurge && (interiorPress < MIN_INTERIOR_PRESSURE && interiorTemp < MIN_INTERIOR_TEMP)) {
    shouldPurge = false;
  }

  let solarAngle = lb(daylightSensor, "SolarAngle", "Average");
  let isDaylight = solarAngle < 90;

  sb(growLight, "On", isDaylight);
  sb(wallLight, "On", !isDaylight);
  sb(lightRound, "On", !isDaylight);
}