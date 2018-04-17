import math, bitops, random

func lerp*(min, max, amount: SomeFloat): SomeFloat =
  return min + (max - min) * amount

func align*(value, alignment: SomeInteger): SomeInteger =
  let mask = alignment - 1
  return (value + mask) and not mask

func isPowerOfTwo*(value: SomeInteger): bool =
  ((value - 1) and value) == 0

func nextPowerOfTwo*(value: SomeUnsignedInt): SomeUnsignedInt =
  const bits = sizeof(value) * 8
  return 1 shl (bits - countLeadingZeroBits(value - 1))

func powerOfTwoBase*(value: SomeUnsignedInt): SomeUnsignedInt =
  const bits = sizeof(value) * 8
  return if value != 0: bits - countTrailingZeroBits(value) else: 0

func saturate*(value: SomeFloat): SomeFloat =
  clamp(value, (SomeFloat)0, (SomeFloat)1)

func sRgbToLinear*(value: SomeFloat): SomeFloat =
  if value < 0.04045: value / 12.92
  else: pow((value + 0.055) / 1.055, 2.4)

func linearToSRgb*(value: SomeFloat): SomeFloat =
  if value < 0.0031308: value * 12.92
  else: 1.055 * pow(value, 1.0 / 2.4) - 0.055

func zigZagEncode*(value: SomeSignedInt): SomeSignedInt =
  ## Maps negative values to positive values while going back and forth
  let bits = sizeof(value) * 8 - 1
  return (value shl 1) xor (value shr bits)

func zigZagDecode*(value: SomeSignedInt): SomeSignedInt =
  return (value shr 1) xor -(value and 1)

func gaussian*(value, mean, standardDeviation: SomeFloat): SomeFloat =
  const normalizationFactor = 1.0 / sqrt(2.0 * PI)
  let offset = value - mean
  return normalizationFactor / standardDeviation * exp(-0.5 * offset * offset / (standardDeviation * standardDeviation))

# Box-Mueller
func randNormal*(mean, standardDeviation: SomeFloat): SomeFloat =
  var u1 = 0.0
  while u1 == 0.0:
    u1 = 1.0 - rand(1.0)

  let 
    u2 = 1.0 - rand(1.0)
    randStdNormal = sqrt(-2.0 * ln(u1)) * sin(2.0 * PI * u2)

  return mean + standardDeviation * randStdNormal

if isMainModule:
  assert(15.nextPowerOfTwo == 16)
  assert(-15.nextPowerOfTwo == -16)
  assert(not 3.isPowerOfTwo)
  assert(64.isPowerOfTwo)
  assert(0.isPowerOfTwo)
  assert(1024.powerOfTwoBase == 10)