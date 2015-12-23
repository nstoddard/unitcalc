# unitcalc
A calculator supporting units of measurement, written in Haskell.

Run it by installing the [Haskell Platform](https://www.haskell.org/platform/), cloning this repo, and running `cabal run`.

It can do conversions between units of measurement:
```
> 60 mph -> m/s
26.8224 m/s
> lightYear -> km
9.454254955488e12 km
```

Defining new units is easy:
```
> unit pound/pounds(lb) = 0.45359 kg

> 100 lb -> kg
45.359 kg
> 100 kg -> lb
220.46341409643068 lb
```

It also supports variables:
```
> lightSpeed = 299792458 m/s
2.99792458e8 meter/second
> lightSpeed -> mph
6.706166293843952e8 mph
> lightSpeed * 1 nanosecond
0.29979245800000004 meter
```

It supports bits and bytes, and the corresponding prefixes:
```
> 1 -> bits/byte
8.0 bits/byte
> 1 MB -> bytes
1000000.0 bytes
> 1 MiB -> bytes
1048576.0 bytes
```

The standard library currently includes definitions of most SI units and a few others.

Units and variables defined in the REPL are automatically saved. Files are also supported (through the `load` command), but the REPL is the primary interface.


## Commands

These can only be used in the REPL, not in a file.

`> exit`  
Self-explanatory.

`> load <filename>`  
Loads the file and runs every line in it.

`> reset`  
Resets the environment, deleting all units and variables that have been defined, except for those in the standard library.


### Notes

Operator precedence is a bit strange compared to most languages. It matters whether there's spaces around an operator. For example:

```
> 2 + 3 * 4
14.0
> 2+3 * 4
20.0
```

There's two commands for defining units: `unit` and `si-unit`. They have the same syntax, but the difference is that `si-unit` automatically generates versions of the unit with SI prefixes. For instance, `unit meter/meters(m)` will automatically define the units `centimeter`, `cm`, `kilometer`, `km`, etc.

Units and variables are case-sensitive. For instance, `5 KM` won't work.
