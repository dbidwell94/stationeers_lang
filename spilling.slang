device self = "db";
device gasSensor  = "d0";
device atmosAnal  = "d1";
device atmosValve = "d2";
device atmosTank  = "d3";
device atmosInlet = "d4";

atmosInlet.Lock = true;
atmosInlet.Mode = 1;
atmosValve.On = false;
atmosValve.Lock = true;

let isPumping = false;
let tempPressure = 0;

loop {
  yield();
  let temp = gasSensor.Temperature;
  let pres = atmosAnal.Pressure;
  let liqV = atmosAnal.VolumeOfLiquid;
  let tempVol = atmosAnal.Volume;

  let stress = 5_000 * liqV / tempVol;

  tempPressure = isPumping ? 1_000 : 10_000;

  let shouldTurnOnInlet = (
    temp > 0c &&
    pres < tempPressure &&
    stress < 50
  );

  isPumping = (
    !shouldTurnOnInlet &&
    atmosTank.Pressure < 35_000 &&
    atmosAnal.RatioPollutant == 0 &&
    atmosAnal.RatioLiquidPollutant == 0 &&
    atmosAnal.Pressure > 1_000
  );

  atmosValve.On = isPumping;
  atmosInlet.On = shouldTurnOnInlet;
}
