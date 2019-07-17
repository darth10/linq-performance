# LINQ performance

## Results

* `First()`:

```
BenchmarkDotNet=v0.11.5, OS=ubuntu 18.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.2.301
  [Host]     : .NET Core 2.2.6 (CoreCLR 4.6.27817.03, CoreFX 4.6.27818.02), 64bit RyuJIT
  DefaultJob : .NET Core 2.2.6 (CoreCLR 4.6.27817.03, CoreFX 4.6.27818.02), 64bit RyuJIT


|               Method |      Mean |     Error |    StdDev | Ratio | RatioSD | Gen 0 | Gen 1 | Gen 2 | Allocated |
|--------------------- |----------:|----------:|----------:|------:|--------:|------:|------:|------:|----------:|
|   IterativeFirstList | 153.39 us | 2.3878 us | 2.1167 us |  1.00 |    0.00 |     - |     - |     - |         - |
|        LinqFirstList | 478.37 us | 4.2302 us | 3.7500 us |  3.12 |    0.04 |     - |     - |     - |      40 B |
|  LinqFasterFirstList | 138.72 us | 1.0194 us | 0.9036 us |  0.90 |    0.01 |     - |     - |     - |         - |
|  IterativeFirstArray |  30.63 us | 0.0569 us | 0.0444 us |  0.20 |    0.00 |     - |     - |     - |         - |
|       LinqFirstArray | 390.25 us | 7.4436 us | 7.3106 us |  2.55 |    0.07 |     - |     - |     - |      32 B |
| LinqFasterFirstArray | 138.69 us | 1.2221 us | 1.1431 us |  0.90 |    0.02 |     - |     - |     - |         - |
```

* `Select()` and `Where()`:

```
BenchmarkDotNet=v0.11.5, OS=ubuntu 18.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.2.301
  [Host]     : .NET Core 2.2.6 (CoreCLR 4.6.27817.03, CoreFX 4.6.27818.02), 64bit RyuJIT
  DefaultJob : .NET Core 2.2.6 (CoreCLR 4.6.27817.03, CoreFX 4.6.27818.02), 64bit RyuJIT


|                     Method |     Mean |    Error |   StdDev | Ratio | RatioSD |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
|--------------------------- |---------:|---------:|---------:|------:|--------:|---------:|---------:|---------:|----------:|
|   IterativeWhereSelectList | 497.2 us | 9.904 us | 9.264 us |  1.00 |    0.00 |  62.5000 |  15.6250 |        - | 128.33 KB |
|        LinqWhereSelectList | 462.0 us | 8.960 us | 8.381 us |  0.93 |    0.02 |  62.0117 |  15.1367 |        - | 128.48 KB |
|  LinqFasterWhereSelectList | 457.1 us | 3.676 us | 3.439 us |  0.92 |    0.02 |  62.5000 |  15.6250 |        - | 128.33 KB |
|  IterativeWhereSelectArray | 225.4 us | 4.192 us | 4.305 us |  0.45 |    0.01 |  75.4395 |  18.7988 |        - | 167.41 KB |
|       LinqWhereSelectArray | 411.0 us | 3.993 us | 3.540 us |  0.83 |    0.02 |  66.4063 |        - |        - |  103.8 KB |
| LinqFasterWhereSelectArray | 415.5 us | 3.324 us | 2.947 us |  0.84 |    0.02 | 124.5117 | 124.5117 | 124.5117 | 429.73 KB |

```

## Haskell equivalent and results

```Haskell
import Criterion.Main

items :: [Int]
items = [1..100000]

doSameThingAsFirst :: [Int] -> Int
doSameThingAsFirst xs = head $ filter (\i -> (i * 2) == 100000) xs

main :: IO ()
main = defaultMain [
  bgroup "doSameThingAsFirst" [
      bench "1"  $
        whnf doSameThingAsFirst items
      ]
  ]
```

Compiled with `-O2`

```
benchmarking doSameThingAsFirst/1
time                 180.8 μs   (179.2 μs .. 181.9 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 180.3 μs   (179.2 μs .. 182.0 μs)
std dev              4.508 μs   (3.249 μs .. 7.936 μs)
variance introduced by outliers: 19% (moderately inflated)
```
