# fixed.zig

Fixed-point arithmetic library for Zig.
Note that we use TI notation. The integer bits (m) do not include the sign bit.

## Usage

```zig
const fixed = @import("fixed");

const Q15_16 = fixed.Q(15, 16); // backed by i32
const x = Q15_16.fromFloat(f64, 3.14159);
const y = Q15_16.fromInt(2);
const z = x.add(y);
```

## API

- `Q(int_bits, frac_bits)` - Fixed-point type constructor
- `fromRaw(raw)` - Construct from raw storage value
- `fromFloat(T, x)` - Convert from float
- `fromInt(x)` - Convert from integer
- `fromQ(q)` - Convert from another fixed-point type
- `toFloat(T)` - Convert to float
- `toInt()` - Convert to integer
- `toRaw()` - Get raw storage value
- `add(other)`, `sub(other)`, `mul(other)`, `div(other)` - Arithmetic operations
- `negate()`, `abs()` - Unary operations
- `order(other)` - Comparison
- `format(writer)` - String formatting
- `parse(input)` - Parse from string

## Limitations

Currently no support for unsigned types

## Build

```bash
zig build
zig build test
```

## License

MIT
