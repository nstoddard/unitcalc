# unitcalc
A calculator supporting units of measurement, written in Haskell.

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


Currently includes definitions of most SI units and a few others.
