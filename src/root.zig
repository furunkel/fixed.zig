const std = @import("std");
const testing = std.testing;

pub const ParseFixedError = error{
    InvalidCharacter,
    Overflow,
    Underflow, // from std.math.powi
};

pub fn Q(comptime int_bits_: comptime_int, comptime frac_bits_: comptime_int) type {
    comptime {
        if (int_bits_ < 0) @compileError("Integer bits cannot be negative");
        if (frac_bits_ < 0) @compileError("Fractional bits cannot be negative");
    }

    return struct {
        pub const int_bits = int_bits_;
        pub const frac_bits = frac_bits_;

        // maximum number of decimal digits affecting the number
        // 1/10^k < 1/2^f  →  10^k > 2^f  →  k > f * log10(2) ≈ f × 0.30103
        pub const max_decimal_digits = @as(comptime_int, @intFromFloat(@ceil(@as(comptime_float, @floatFromInt(frac_bits)) * std.math.log10(2.0))));
        pub const max_integer_digits = @as(comptime_int, @intFromFloat(@ceil(@as(comptime_float, @floatFromInt(int_bits)) * std.math.log10(2.0))));

        pub const resolution: f64 = std.math.ldexp(@as(f64, 1.0), -frac_bits);
        pub const bits = int_bits + frac_bits + 1;
        pub const StorageInt = std.meta.Int(.signed, bits);
        pub const IntInt = std.meta.Int(.signed, int_bits + 1);
        const overflow_bits = bits + frac_bits;
        pub const OverflowInt = std.meta.Int(.signed, overflow_bits);

        numerator: StorageInt,

        const Self = @This();

        pub fn withNumerator(raw: StorageInt) Self {
            return .{ .numerator = raw };
        }

        pub fn fromFloat(comptime T: type, x: T) Self {
            const scaled = std.math.ldexp(x, frac_bits);
            const rounded = if (scaled > 0.0) scaled + 0.5 else if (scaled <= 0.0) scaled - 0.5 else scaled;

            // @intFromFloat "rounds" towards zero
            return .{ .numerator = @intFromFloat(rounded) };
        }

        pub fn fromInt(x: IntInt) Self {
            return .{ .numerator = @shlExact(@as(StorageInt, @intCast(x)), frac_bits) };
        }

        pub fn fromQ(q: anytype) Self {
            const T = @TypeOf(q);
            if (T.int_bits > int_bits or T.frac_bits > frac_bits) {
                comptime {
                    const msg = std.fmt.comptimePrint("Cannot convert from Q({d}, {d}) to Q({d}, {d}): source has more bits than destination", .{ T.int_bits, T.frac_bits, int_bits, frac_bits });
                    @compileError(msg);
                }
            }
            return .{ .numerator = @shlExact(@as(StorageInt, q.numerator), (frac_bits - T.frac_bits)) };
        }

        pub fn toFloat(self: Self, comptime T: type) T {
            const scaled = std.math.ldexp(@as(T, @floatFromInt(self.numerator)), -frac_bits);
            return scaled;
        }

        pub fn toInt(self: Self) IntInt {
            return @as(IntInt, @intCast(@divTrunc(self.numerator, 1 << frac_bits)));
        }

        pub fn add(self: Self, other: Self) Self {
            return .{ .numerator = self.numerator + other.numerator };
        }

        pub fn sub(self: Self, other: Self) Self {
            return .{ .numerator = self.numerator - other.numerator };
        }

        pub fn mul(self: Self, other: Self) Self {
            const product = @as(OverflowInt, self.numerator) * @as(OverflowInt, other.numerator);

            // round half away from zero
            const round_offset = if (product > 0) @as(OverflowInt, 1) << (frac_bits - 1) else (@as(OverflowInt, 1) << (frac_bits - 1)) - 1;
            const rounded = product + round_offset;

            const shifted = rounded >> frac_bits; // floor(x / 2^frac_bits)
            return .{ .numerator = @intCast(shifted) };
        }

        pub fn div(self: Self, other: Self) Self {
            // Shift left by frac_bits before division to maintain scale
            const shifted = @as(OverflowInt, self.numerator) << frac_bits;
            const round_offset = @divTrunc(@as(OverflowInt, other.numerator), @as(OverflowInt, 2));

            // round half away from zero
            const rounded = if ((shifted >= 0 and other.numerator > 0) or (shifted < 0 and other.numerator < 0))
                shifted + round_offset
            else
                shifted - round_offset;
            const result = @divTrunc(rounded, @as(OverflowInt, @intCast(other.numerator)));
            return .{ .numerator = @intCast(result) };
        }

        pub fn negate(self: Self) Self {
            return .{ .numerator = -self.numerator };
        }

        pub fn abs(self: Self) Self {
            // obviously, abs(min) overflows...
            return .{ .numerator = @intCast(@abs(self.numerator)) };
        }

        pub fn order(self: Self, other: Self) std.math.Order {
            return std.math.order(self.numerator, other.numerator);
        }

        pub fn format(self: Self, w: *std.Io.Writer) std.Io.Writer.Error!void {
            const abs_value = @abs(self.numerator);
            const int_part = abs_value >> frac_bits;

            if (self.numerator < 0) {
                try w.writeByte('-');
            }
            try w.printInt(int_part, 10, .lower, .{});

            if (frac_bits > 0) {
                // k / 2^f is k * 5^f / 10^f, so we need at most f decimal places
                // 5^frac_bits = (2^log2(5))^frac_bits < 2^(3* frac_bits)
                // we need at most frac_bits additional bits for the multiply.
                const FracInt = std.meta.Int(.unsigned, 4 * frac_bits);
                const frac_part = @as(FracInt, abs_value & ((1 << frac_bits) - 1));

                if (frac_part != 0) {
                    // k / 2^f * 10^f = k * 5^f
                    const frac_scale = std.math.powi(FracInt, 5, frac_bits) catch return std.Io.Writer.Error.WriteFailed;

                    var frac = (frac_part * frac_scale);
                    var trailing_zeros: usize = 0;
                    inline for (0..frac_bits) |_| {
                        if (@rem(frac, 10) == 0) {
                            frac = @divTrunc(frac, 10);
                            trailing_zeros += 1;
                        } else break;
                    }
                    try w.writeByte('.');
                    // Pad with leading zeros to maintain correct decimal places
                    const num_digits = if (frac == 0) @as(usize, 0) else std.math.log10_int(frac) + 1;
                    const leading_zeros = frac_bits - trailing_zeros - num_digits;
                    for (0..leading_zeros) |_| {
                        try w.writeByte('0');
                    }
                    if (frac != 0) {
                        try w.printInt(frac, 10, .lower, .{});
                    }
                }
            }
        }

        pub fn parse(input: []const u8) ParseFixedError!Self {
            // maximum representable number is 2^i − 2^(-n)
            // since 2^(-n) > 0, the maximum integer is 2^i - 1
            const max_int = (1 << int_bits) - 1;
            const abs_min_int = 1 << int_bits;

            // we can take at most max_integer_digits, so
            // 10^max_integer_digits = 2^(log2(10) * max_integer_digits) < 2^(4 * max_integer_digits)
            const IntPartInt = std.meta.Int(.unsigned, 4 * max_integer_digits);

            // normalizer needs 10^max_decimal_digits = 2^(log2(10) * max_decimal_digits) < 2^(4 * max_decimal_digits)
            // since the accumulated value is shifted we need scale_shift additional bits
            const scale_shift = frac_bits - max_decimal_digits;
            const FracPartInt = std.meta.Int(.unsigned, 4 * max_decimal_digits + scale_shift);

            var int_accum: IntPartInt = 0;

            if (input.len == 0) {
                return ParseFixedError.InvalidCharacter;
            }

            var negative: bool = undefined;
            var str: []const u8 = undefined;

            switch (input[0]) {
                '+' => {
                    negative = false;
                    str = input[1..];
                },
                '-' => {
                    negative = true;
                    str = input[1..];
                },
                '0'...'9' => {
                    negative = false;
                    str = input;
                },
                else => {
                    return ParseFixedError.InvalidCharacter;
                },
            }

            var dot_pos: ?usize = null;

            for (str, 0..) |c, i| {
                switch (c) {
                    '0'...'9' => {
                        const digit = c - '0';
                        const mul_result = @mulWithOverflow(int_accum, 10);
                        if (mul_result[1] != 0) return ParseFixedError.Overflow;
                        const add_result = @addWithOverflow(mul_result[0], digit);
                        if (add_result[1] != 0) return ParseFixedError.Overflow;
                        int_accum = add_result[0];
                        if (negative and int_accum > abs_min_int or !negative and int_accum > max_int) {
                            return ParseFixedError.Overflow;
                        }
                    },
                    '.' => {
                        dot_pos = i;
                        if (frac_bits == 0) {
                            return ParseFixedError.InvalidCharacter;
                        }
                        break;
                    },
                    else => return ParseFixedError.InvalidCharacter,
                }
            }

            var frac_part: FracPartInt = undefined;
            var frac_accum: FracPartInt = 0;

            if (frac_bits > 0) {
                if (dot_pos) |dot_pos_| {
                    var pos = dot_pos_ + 1;
                    while (pos < str.len and pos <= max_decimal_digits + dot_pos_) : (pos += 1) {
                        const c = str[pos];
                        switch (c) {
                            '0'...'9' => {
                                frac_accum *= 10;
                                frac_accum += c - '0';
                            },
                            else => return ParseFixedError.InvalidCharacter,
                        }
                    }

                    frac_accum += @intFromBool(str.len > pos and std.ascii.isDigit(str[pos]) and str[pos] > '5');

                    const max_decimal_digits_usize = @as(usize, max_decimal_digits);
                    const decimal_digits = str.len - (dot_pos_ + 1);

                    if (max_decimal_digits_usize > decimal_digits) {
                        for (0..max_decimal_digits_usize - decimal_digits) |_| {
                            frac_accum *= 10;
                        }
                    }
                }

                const frac_accum_norm = comptime try std.math.powi(FracPartInt, 5, max_decimal_digits);

                frac_part = @divTrunc(
                    @shlExact(
                        frac_accum,
                        scale_shift,
                    ) + @divTrunc(frac_accum_norm, 2),
                    frac_accum_norm,
                );
            } else {
                frac_part = 0;
            }

            if (negative and int_accum == abs_min_int) {
                if (frac_accum == 0) {
                    return .{ .numerator = std.math.minInt(StorageInt) };
                } else {
                    return ParseFixedError.Overflow;
                }
            }

            // this should never overflow, given the min/max checks above
            const storage_int: StorageInt = @shlExact(@as(StorageInt, @intCast(int_accum)), frac_bits);
            const storage_frac = @as(StorageInt, @intCast(frac_part));
            const storage = if (negative) -storage_int - storage_frac else storage_int + storage_frac;
            return .{ .numerator = storage };
        }
    };
}

pub fn forRangeAndScale(comptime min_value: comptime_float, comptime max_value: comptime_float, comptime scale: comptime_int) type {
    comptime {

        // we want a resolution <= 0.5 * 1/(10^scale), hence the + 1 in the log expression
        const scale_power = std.math.powi(usize, 10, scale) catch unreachable;
        const frac_bits = std.math.log2_int_ceil(usize, scale_power) + 1;

        const max_storage = std.math.ldexp(@as(f64, @floatCast(max_value)), frac_bits);
        const min_storage = std.math.ldexp(@as(f64, @floatCast(min_value)), frac_bits);

        const bits_min = @ceil(std.math.log2(@abs(min_storage) + @intFromBool(min_value > 0)));
        const bits_max = @ceil(std.math.log2(@abs(max_storage) + @intFromBool(max_value > 0)));

        const int_bits = @max(bits_min, bits_max) - frac_bits;

        // @compileLog(std.fmt.comptimePrint("int_bits: {d}, max_storage: {d}, min_storage: {d}, bits_min: {d}, bits_max: {d}, bits_frac: {d}", .{ int_bits, max_storage, min_storage, bits_min, bits_max, frac_bits }));
        return Q(int_bits, frac_bits);
    }
}

test "fromRaw" {
    const Q4_11 = Q(4, 11);
    const a = Q4_11.withNumerator(5 * 2048);

    try testing.expectEqual(5, a.toInt());
    try testing.expectEqual(5 * 2048, a.numerator);
}

test "fromInt" {
    {
        const Q8_7 = Q(8, 7);
        const a = Q8_7.fromInt(5);

        try testing.expectEqual(5 * 128, a.numerator);
        try testing.expectEqual(5, a.toInt());

        const b = Q8_7.fromInt(-5);
        try testing.expectEqual(@as(i16, -5 * 128), b.numerator);
    }

    {
        const Q4_11 = Q(4, 11);

        const a = Q4_11.fromInt(3);
        try testing.expectEqual(3 * 2048, a.numerator);

        const b = Q4_11.fromInt(-3);
        try testing.expectEqual(-3 * 2048, b.numerator);
    }

    {
        const Q16_16 = Q(16, 16);

        const a = Q16_16.fromInt(1000);
        try testing.expectEqual(1000 * 65536, a.numerator);

        const b = Q16_16.fromInt(-1000);
        try testing.expectEqual(-1000 * 65536, b.numerator);
    }

    {
        const Q100_100 = Q(100, 100);

        const a = Q100_100.fromInt(100_000_000_000_000);
        try testing.expectEqual(100_000_000_000_000 * 1267650600228229401496703205376, a.numerator);

        const b = Q100_100.fromInt(-100_000_000_000_000);
        try testing.expectEqual(-100_000_000_000_000 * 1267650600228229401496703205376, b.numerator);
    }
}

test "fromQ" {
    const Q8_7 = Q(8, 7);
    const Q8_8 = Q(8, 8);

    const a = Q8_7.withNumerator(3);
    const b = Q8_8.fromQ(a);

    try testing.expectEqual(6, b.numerator);
    try testing.expectEqual(0.0234375, a.toFloat(f64));
    try testing.expectEqual(0.0234375, b.toFloat(f64));
}

test "fromInt (edge-cases)" {
    {
        const Q16_16 = Q(16, 16);

        const min = std.math.minInt(i16);
        const max = std.math.maxInt(i16) - 1;

        const a = Q16_16.fromInt(min);
        try testing.expectEqual(min, a.toInt());

        const b = Q16_16.fromInt(max);
        try testing.expectEqual(max, b.toInt());
    }

    {
        const Q64_64 = Q(64, 64);

        const min = std.math.minInt(i64);
        const max = std.math.maxInt(i64) - 1;

        const a = Q64_64.fromInt(min);
        try testing.expectEqual(min, a.toInt());

        const b = Q64_64.fromInt(max);
        try testing.expectEqual(max, b.toInt());
    }
}

test "toInt" {
    const Q5_7 = Q(5, 7);

    const a = Q5_7.withNumerator(0);
    try testing.expectEqual(0, a.toInt());

    const b = Q5_7.withNumerator(std.math.minInt(i13));
    try testing.expectEqual(-32, b.toInt());

    const c = Q5_7.withNumerator(std.math.maxInt(i13) - 1);
    try testing.expectEqual(31, c.toInt());

    const d = Q5_7.withNumerator(640); // 5
    try testing.expectEqual(5, d.toInt());

    const e = Q5_7.withNumerator(320); // 2.5
    try testing.expectEqual(2, e.toInt());
}

test "toInt (rounding)" {
    // toInt has truncation semantics
    const Q5_7 = Q(5, 7);

    // below midpoint
    const a = Q5_7.withNumerator(1); // 0.0078125
    try testing.expectEqual(0, a.toInt());

    // below midpoint (negative)
    const b = Q5_7.withNumerator(-1); // -0.0078125
    try testing.expectEqual(0, b.toInt());

    // above midpoint
    const c = Q5_7.withNumerator(206); // 1.609375
    try testing.expectEqual(1, c.toInt());

    // above midpoint (negative)
    const d = Q5_7.withNumerator(-206); // -1.609375
    try testing.expectEqual(-1, d.toInt());

    // ad midpoint
    const e = Q5_7.withNumerator(192); // 1.5
    try testing.expectEqual(1, e.toInt());

    // ad midpoint (negative)
    const f = Q5_7.withNumerator(-192); // -1.5
    try testing.expectEqual(-1, f.toInt());
}

test "fromFloat" {
    const Q8_7 = Q(8, 7);

    {
        const a = Q8_7.fromFloat(f32, 1.5);
        try testing.expectEqual(192, a.numerator);

        const b = Q8_7.fromFloat(f32, -2.25);
        try testing.expectEqual(-288, b.numerator);

        const c = Q8_7.fromFloat(f32, 0.0);
        try testing.expectEqual(0, c.numerator);

        const d = Q8_7.fromFloat(f32, -256.0);
        try testing.expectEqual(-32768, d.numerator);

        const e = Q8_7.fromFloat(f32, 255.9921875);
        try testing.expectEqual(32767, e.numerator);
    }

    {
        const a = Q8_7.fromFloat(f64, 1.5);
        try testing.expectEqual(192, a.numerator);

        const b = Q8_7.fromFloat(f64, -2.25);
        try testing.expectEqual(-288, b.numerator);

        const c = Q8_7.fromFloat(f64, 0.0);
        try testing.expectEqual(0, c.numerator);

        const d = Q8_7.fromFloat(f64, -256.0);
        try testing.expectEqual(-32768, d.numerator);

        const e = Q8_7.fromFloat(f64, 255.9921875);
        try testing.expectEqual(32767, e.numerator);
    }
}

test "fromFloat (zero integer bits)" {
    const Q0_7 = Q(0, 7);

    {
        const a = Q0_7.fromFloat(f32, 0.0);
        try testing.expectEqual(0, a.numerator);

        const b = Q0_7.fromFloat(f32, -1.0);
        try testing.expectEqual(-128, b.numerator);

        const c = Q0_7.fromFloat(f32, 0.9921875);
        try testing.expectEqual(127, c.numerator);

        const d = Q0_7.fromFloat(f32, 0.5);
        try testing.expectEqual(64, d.numerator);

        const e = Q0_7.fromFloat(f32, -0.5);
        try testing.expectEqual(-64, e.numerator);
    }

    {
        const a = Q0_7.fromFloat(f64, 0.0);
        try testing.expectEqual(0, a.numerator);

        const b = Q0_7.fromFloat(f64, -1.0);
        try testing.expectEqual(-128, b.numerator);

        const c = Q0_7.fromFloat(f64, 0.9921875);
        try testing.expectEqual(127, c.numerator);

        const d = Q0_7.fromFloat(f64, 0.5);
        try testing.expectEqual(64, d.numerator);

        const e = Q0_7.fromFloat(f64, -0.5);
        try testing.expectEqual(-64, e.numerator);
    }
}

test "fromFloat (rounding)" {
    {
        const Q8_7 = Q(8, 7);

        // rounding at midpoint (half away from zero) 5.0 * (res / 2.0) = 5.0/2 * res should round to 3 * res = raw(3)
        const a = Q8_7.fromFloat(f32, 0.01953125);
        try testing.expectEqual(3, a.numerator);

        // rounding at midpoint (same but negative)
        const b = Q8_7.fromFloat(f32, -0.01953125);
        try testing.expectEqual(-3, b.numerator);

        // rounding above midpoint (half away from zero) 5.0 * (res / 3.0) = 5.0/3 * res should round to 2 * res = raw(2)
        const c = Q8_7.fromFloat(f32, 0.013020833333333334);
        try testing.expectEqual(2, c.numerator);

        // rounding above midpoint (same but negative)
        const d = Q8_7.fromFloat(f32, -0.013020833333333334);
        try testing.expectEqual(-2, d.numerator);

        // rounding below midpoint (half away from zero) 5.0 * (res / 4.0) = 5.0/4 * res should round to 1 * res = raw(1)
        const e = Q8_7.fromFloat(f32, 0.009765625);
        try testing.expectEqual(1, e.numerator);

        // rounding above midpoint (same but negative)
        const f = Q8_7.fromFloat(f32, -0.009765625);
        try testing.expectEqual(-1, f.numerator);
    }

    {
        const Q8_7 = Q(8, 7);

        // rounding at midpoint (half away from zero) 5.0 * (res / 2.0) = 5.0/2 * res should round to 3 * res = raw(3)
        const a = Q8_7.fromFloat(f64, 0.01953125);
        try testing.expectEqual(3, a.numerator);

        // rounding at midpoint (same but negative)
        const b = Q8_7.fromFloat(f64, -0.01953125);
        try testing.expectEqual(-3, b.numerator);

        // rounding above midpoint (half away from zero) 5.0 * (res / 3.0) = 5.0/3 * res should round to 2 * res = raw(2)
        const c = Q8_7.fromFloat(f64, 0.013020833333333334);
        try testing.expectEqual(2, c.numerator);

        // rounding above midpoint (same but negative)
        const d = Q8_7.fromFloat(f64, -0.013020833333333334);
        try testing.expectEqual(-2, d.numerator);

        // rounding below midpoint (half away from zero) 5.0 * (res / 4.0) = 5.0/4 * res should round to 1 * res = raw(1)
        const e = Q8_7.fromFloat(f64, 0.009765625);
        try testing.expectEqual(1, e.numerator);

        // rounding above midpoint (same but negative)
        const f = Q8_7.fromFloat(f64, -0.009765625);
        try testing.expectEqual(-1, f.numerator);
    }
}

fn expectApproxEqToFloat(T: type, expected: T, q: anytype) !void {
    const tolerance = std.math.pow(f64, 10.0, -@TypeOf(q).frac_bits) / 2.0;
    try testing.expectApproxEqAbs(expected, q.toFloat(T), tolerance);
}

test "toFloat" {
    {
        const Q8_7 = Q(8, 7);

        const a = Q8_7.withNumerator(10);
        try expectApproxEqToFloat(f32, 0.078125, a);

        const b = Q8_7.withNumerator(-10);
        try expectApproxEqToFloat(f32, -0.078125, b);

        const c = Q8_7.withNumerator(0);
        try expectApproxEqToFloat(f32, 0, c);

        const d = Q8_7.withNumerator(std.math.minInt(i16));
        try expectApproxEqToFloat(f32, -256.0, d);

        const e = Q8_7.withNumerator(std.math.maxInt(i16));
        try expectApproxEqToFloat(f32, 255.9921875, e);
    }

    {
        const Q8_7 = Q(8, 7);

        const a = Q8_7.withNumerator(10);
        try expectApproxEqToFloat(f64, 0.078125, a);

        const b = Q8_7.withNumerator(-10);
        try expectApproxEqToFloat(f64, -0.078125, b);

        const c = Q8_7.withNumerator(0);
        try expectApproxEqToFloat(f64, 0, c);

        const d = Q8_7.withNumerator(std.math.minInt(i16));
        try expectApproxEqToFloat(f64, -256.0, d);

        const e = Q8_7.withNumerator(std.math.maxInt(i16));
        try expectApproxEqToFloat(f64, 255.9921875, e);
    }
}

test "addition" {
    const Q8_7 = Q(8, 7);

    // trivially correct
    const a = Q8_7.withNumerator(34);
    const b = Q8_7.withNumerator(3);
    const c = a.add(b);

    try testing.expectEqual(37, c.numerator);
}

test "subtraction" {
    const Q8_7 = Q(8, 7);

    // trivially correct
    const a = Q8_7.withNumerator(34);
    const b = Q8_7.withNumerator(3);
    const c = a.sub(b);

    try testing.expectEqual(31, c.numerator);
}

test "multiplication" {
    const Q8_7 = Q(8, 7);

    {
        const a = Q8_7.withNumerator(256); // 2.0
        const b = Q8_7.withNumerator(384); // 3.0
        const c = a.mul(b);

        try testing.expectEqual(768, c.numerator);
    }

    {
        const a = Q8_7.withNumerator(256); // 2.0
        const b = Q8_7.withNumerator(0); // 0
        const c = a.mul(b);

        try testing.expectEqual(0, c.numerator);
    }

    {
        const a = Q8_7.withNumerator(256); // 2.0
        const b = Q8_7.withNumerator(64); // 0.5
        const c = a.mul(b);

        try testing.expectEqual(128, c.numerator);
    }

    {
        const a = Q8_7.withNumerator(256); // 2.0
        const b = Q8_7.withNumerator(-64); // -0.5
        const c = a.mul(b);

        try testing.expectEqual(-128, c.numerator);
    }
}

test "multiplication (big)" {
    const Q100_100 = Q(100, 100);

    {
        const a = Q100_100.withNumerator(2535301200456458802993406410752); // 2.0
        const b = Q100_100.withNumerator(3802951800684688204490109616128); // 3.0
        const c = a.mul(b);

        try testing.expectEqual(7605903601369376408980219232256, c.numerator);
    }

    {
        const a = Q100_100.withNumerator(2535301200456458802993406410752); // 2.0
        const b = Q100_100.withNumerator(-3802951800684688204490109616128); // -3.0
        const c = a.mul(b);

        try testing.expectEqual(-7605903601369376408980219232256, c.numerator);
    }

    {
        const a = Q100_100.withNumerator(2535301200456458802993406410752); // 2.0
        const b = Q100_100.withNumerator(-633825300114114700748351602688); // -0.5
        const c = a.mul(b);

        try testing.expectEqual(-1267650600228229401496703205376, c.numerator);
    }
}

test "multiplication (rounding)" {
    const Q8_7 = Q(8, 7);

    // multiplication rounds away from zero

    // at midpoint
    {
        {
            const a = Q8_7.withNumerator(64); // 64/128
            const b = Q8_7.withNumerator(11); // 11/128
            const c = a.mul(b); // 11/256 = 5.5/128

            try testing.expectEqual(6, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(-64);
            const b = Q8_7.withNumerator(-11);
            const c = a.mul(b);

            try testing.expectEqual(6, c.numerator);
        }
    }

    // at midpoint (negative)
    {
        {
            const a = Q8_7.withNumerator(64);
            const b = Q8_7.withNumerator(-11);
            const c = a.mul(b);

            try testing.expectEqual(-6, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(-64);
            const b = Q8_7.withNumerator(11);
            const c = a.mul(b);

            try testing.expectEqual(-6, c.numerator);
        }
    }

    // above midpoint
    {
        {
            const a = Q8_7.withNumerator(32); // 32/128
            const b = Q8_7.withNumerator(11); // 11/128
            const c = a.mul(b); // 11/512 = 2.75/128

            try testing.expectEqual(3, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(-32);
            const b = Q8_7.withNumerator(-11);
            const c = a.mul(b);

            try testing.expectEqual(3, c.numerator);
        }
    }

    // above midpoint (negative)
    {
        {
            const a = Q8_7.withNumerator(-32);
            const b = Q8_7.withNumerator(11);
            const c = a.mul(b);

            try testing.expectEqual(-3, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(32);
            const b = Q8_7.withNumerator(-11);
            const c = a.mul(b);

            try testing.expectEqual(-3, c.numerator);
        }
    }

    // below midpoint
    {
        {
            const a = Q8_7.withNumerator(16); // 16/128
            const b = Q8_7.withNumerator(11); // 11/128
            const c = a.mul(b); // 11/1024 = 1.375/128

            try testing.expectEqual(1, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(-16);
            const b = Q8_7.withNumerator(-11);
            const c = a.mul(b);

            try testing.expectEqual(1, c.numerator);
        }
    }

    // below midpoint (negative)
    {
        {
            const a = Q8_7.withNumerator(-16);
            const b = Q8_7.withNumerator(11);
            const c = a.mul(b);

            try testing.expectEqual(-1, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(16);
            const b = Q8_7.withNumerator(-11);
            const c = a.mul(b);

            try testing.expectEqual(-1, c.numerator);
        }
    }
}

test "division" {
    const Q8_7 = Q(8, 7);

    const a = Q8_7.fromFloat(f64, 6.0);
    const b = Q8_7.fromFloat(f64, 2.0);
    const c = a.div(b);

    try testing.expectApproxEqAbs(3.0, c.toFloat(f64), 0.01);
}

test "division large" {
    const T = Q(100, 100);

    {
        const a = T.fromFloat(f64, 0.8697620772582899);
        const b = T.fromFloat(f64, 0.8602583449105785);
        const c = a.div(b);

        try testing.expectApproxEqAbs(1.011047532876533, c.toFloat(f64), 0.0000000000000001);
    }

    {
        const a = T.fromFloat(f64, 0.8697620772582899);
        const b = T.fromFloat(f64, -0.8602583449105785);
        const c = a.div(b);

        try testing.expectApproxEqAbs(-1.011047532876533, c.toFloat(f64), 0.0000000000000001);
    }

    {
        const a = T.fromFloat(f64, -0.8697620772582899);
        const b = T.fromFloat(f64, -0.8602583449105785);
        const c = a.div(b);

        try testing.expectApproxEqAbs(1.011047532876533, c.toFloat(f64), 0.0000000000000001);
    }
}

test "division (rounding)" {
    const Q8_7 = Q(8, 7);

    // division rounds away from zero

    // at midpoint
    {
        {
            const a = Q8_7.withNumerator(25); // 25/128
            const b = Q8_7.withNumerator(1280); // 1280/128 = 10
            const c = a.div(b); // 2.5/128

            try testing.expectEqual(3, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(-25);
            const b = Q8_7.withNumerator(-1280);
            const c = a.div(b);

            try testing.expectEqual(3, c.numerator);
        }
    }

    // at midpoint (negative)
    {
        {
            const a = Q8_7.withNumerator(25);
            const b = Q8_7.withNumerator(-1280);
            const c = a.div(b);

            try testing.expectEqual(-3, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(-25);
            const b = Q8_7.withNumerator(1280);
            const c = a.div(b);

            try testing.expectEqual(-3, c.numerator);
        }
    }

    // above midpoint
    {
        {
            const a = Q8_7.withNumerator(12); // 12/128
            const b = Q8_7.withNumerator(14); // 14/128
            const c = a.div(b); // 109.71428/128

            try testing.expectEqual(110, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(-12);
            const b = Q8_7.withNumerator(-14);
            const c = a.div(b);

            try testing.expectEqual(110, c.numerator);
        }
    }

    // above midpoint (negative)
    {
        {
            const a = Q8_7.withNumerator(-12);
            const b = Q8_7.withNumerator(14);
            const c = a.div(b);

            try testing.expectEqual(-110, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(12);
            const b = Q8_7.withNumerator(-14);
            const c = a.div(b);

            try testing.expectEqual(-110, c.numerator);
        }
    }

    // below midpoint
    {
        {
            const a = Q8_7.withNumerator(23); // 23/128
            const b = Q8_7.withNumerator(9); // 9/128
            const c = a.div(b); // 327.11111/128

            try testing.expectEqual(327, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(-23);
            const b = Q8_7.withNumerator(-9);
            const c = a.div(b);

            try testing.expectEqual(327, c.numerator);
        }
    }

    // below midpoint (negative)
    {
        {
            const a = Q8_7.withNumerator(-23);
            const b = Q8_7.withNumerator(9);
            const c = a.div(b);

            try testing.expectEqual(-327, c.numerator);
        }

        {
            const a = Q8_7.withNumerator(23);
            const b = Q8_7.withNumerator(-9);
            const c = a.div(b);

            try testing.expectEqual(-327, c.numerator);
        }
    }
}

test "negation" {
    const Q8_7 = Q(8, 7);

    // trivially correcet
    const a = Q8_7.withNumerator(3);
    const b = a.negate();

    try testing.expectEqual(-3, b.numerator);
}

test "absolute value" {
    const Q8_7 = Q(8, 7);

    // trivially correct
    const a = Q8_7.withNumerator(-19);
    const b = a.abs();

    try testing.expectEqual(19, b.numerator);
}

test "order" {
    const Q8_7 = Q(8, 7);

    const a = Q8_7.withNumerator(3);
    const b = Q8_7.withNumerator(5);
    const c = Q8_7.withNumerator(3);

    try testing.expectEqual(std.math.Order.lt, a.order(b));
    try testing.expectEqual(std.math.Order.gt, b.order(a));
    try testing.expectEqual(std.math.Order.eq, a.order(c));
}

test "complex calculation" {
    const Q8_7 = Q(8, 7);

    // Calculate: (3.5 * 2.0) + (4.0 / 2.0) - 1.0 = 7.0 + 2.0 - 1.0 = 8.0
    // = (448 * 256) + (512 / 256) - 128 = 1024.0
    const a = Q8_7.withNumerator(448);
    const b = Q8_7.withNumerator(256);
    const c = Q8_7.withNumerator(512);
    const d = Q8_7.withNumerator(256);
    const e = Q8_7.withNumerator(128);

    const result = a.mul(b).add(c.div(d)).sub(e);
    try testing.expectEqual(1024, result.numerator);
}

fn expectFormat(expected: []const u8, actual: anytype) !void {
    var buffer: [512]u8 = undefined;
    var writer = std.Io.Writer.fixed(&buffer);
    try actual.format(&writer);
    const actual_str = writer.buffered();
    try testing.expectEqualStrings(expected, actual_str);
}

test "format" {
    {
        const Q8_7 = Q(8, 7);
        const a = Q8_7.withNumerator(std.math.maxInt(i16));
        try expectFormat("255.9921875", a);

        const b = Q8_7.withNumerator(std.math.minInt(i16));
        try expectFormat("-256", b);

        const c = Q8_7.withNumerator(0);
        try expectFormat("0", c);

        const d = Q8_7.withNumerator(1);
        try expectFormat("0.0078125", d);

        const e = Q8_7.withNumerator(-1);
        try expectFormat("-0.0078125", e);
    }

    {
        const Q100_100 = Q(100, 100);
        const a = Q100_100.withNumerator(std.math.maxInt(Q100_100.StorageInt));
        try expectFormat("1267650600228229401496703205375.9999999999999999999999999999992111390947789881945882714347172137703267935648909769952297210693359375", a);

        const b = Q100_100.withNumerator(std.math.minInt(Q100_100.StorageInt));
        try expectFormat("-1267650600228229401496703205376", b);

        const c = Q100_100.withNumerator(0);
        try expectFormat("0", c);

        const d = Q100_100.withNumerator(1);
        try expectFormat("0.0000000000000000000000000000007888609052210118054117285652827862296732064351090230047702789306640625", d);

        const e = Q100_100.withNumerator(-1);
        try expectFormat("-0.0000000000000000000000000000007888609052210118054117285652827862296732064351090230047702789306640625", e);
    }
}

test "parse" {
    {
        const Q8_7 = Q(8, 7);

        const a = try Q8_7.parse("255.9921875");
        try testing.expectEqual(std.math.maxInt(i16), a.numerator);

        const b = try Q8_7.parse("-256");
        try testing.expectEqual(std.math.minInt(i16), b.numerator);

        const c = try Q8_7.parse("0.0");
        try testing.expectEqual(0, c.numerator);

        const d = try Q8_7.parse("0.0078125");
        try testing.expectEqual(1, d.numerator);

        const e = try Q8_7.parse("-0.0078125");
        try testing.expectEqual(-1, e.numerator);

        const f = try Q8_7.parse("0");
        try testing.expectEqual(0, f.numerator);

        const g = try Q8_7.parse("-0");
        try testing.expectEqual(0, g.numerator);
    }

    {
        const Q100_100 = Q(100, 100);

        const a = try Q100_100.parse("1267650600228229401496703205375.9999999999999999999999999999992111390947789881945882714347172137703267935648909769952297210693359375");
        try testing.expectEqual(std.math.maxInt(Q100_100.StorageInt), a.numerator);

        const b = try Q100_100.parse("-1267650600228229401496703205376");
        try testing.expectEqual(std.math.minInt(Q100_100.StorageInt), b.numerator);

        const c = try Q100_100.parse("0.0");
        try testing.expectEqual(0, c.numerator);

        const d = try Q100_100.parse("0.0000000000000000000000000000007888609052210118054117285652827862296732064351090230047702789306640625");
        try testing.expectEqual(1, d.numerator);

        const e = try Q100_100.parse("-0.0000000000000000000000000000007888609052210118054117285652827862296732064351090230047702789306640625");
        try testing.expectEqual(-1, e.numerator);

        const f = try Q100_100.parse("0");
        try testing.expectEqual(0, f.numerator);

        const g = try Q100_100.parse("-0");
        try testing.expectEqual(0, g.numerator);
    }
}

test "parse (overflow)" {
    {
        const Q8_7 = Q(8, 7);

        const a = Q8_7.parse("256");
        try testing.expectError(ParseFixedError.Overflow, a);

        // NOTE: we ignore decimals below resolution, so -256.000000001 won't trigger overflow
        // For this to work, we'd need to scan the entire string for zeros
        const b = Q8_7.parse("-256.001");
        try testing.expectError(ParseFixedError.Overflow, b);

        const c = Q8_7.parse("-257");
        try testing.expectError(ParseFixedError.Overflow, c);
    }
}

test "forRangeAndScale" {
    {
        const T = forRangeAndScale(-40.0, 120.0, 1);
        try testing.expectEqual(7, T.int_bits);
        //one decimal place requires resolution of 2**-5 = 0.03125 <= 0.05
        try testing.expectEqual(5, T.frac_bits);
    }

    {
        // should fit in a Q8_8
        const T = forRangeAndScale(-256, 255.99609375, 2);
        try testing.expectEqual(8, T.int_bits);
        try testing.expectEqual(8, T.frac_bits);
    }

    {
        // does *not* fit in a Q8_8
        const T = forRangeAndScale(-256, 256, 2);
        try testing.expectEqual(9, T.int_bits);
        try testing.expectEqual(8, T.frac_bits);
    }

    {
        const T = forRangeAndScale(-40.0, 255.0, 10);
        try testing.expectEqual(8, T.int_bits);

        // 10 decimal places require resolution of 2**-35 = 0.00000000002910383045673370361328125 <= 0.5e-10
        try testing.expectEqual(35, T.frac_bits);
    }

    {
        // both negative
        const T = forRangeAndScale(-256, -1, 2);
        try testing.expectEqual(8, T.int_bits);
        try testing.expectEqual(8, T.frac_bits);
    }

    {
        // both positive
        const T = forRangeAndScale(1, 7, 2);
        try testing.expectEqual(3, T.int_bits);
        try testing.expectEqual(8, T.frac_bits);
    }
}
