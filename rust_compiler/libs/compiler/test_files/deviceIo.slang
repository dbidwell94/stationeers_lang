device airConditioner = "d0";
device gasSensor = "d1";

loop {
  yield();
  let indoorTemp = loadFromDevice(gasSensor, "Temperature");
  let shouldSet = indoorTemp > 30c;
  setOnDevice(airConditioner, "On", shouldSet);
}
