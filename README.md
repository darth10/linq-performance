# LINQ performance

## Results

* `First()`:

```
BenchmarkDotNet=v0.10.14, OS=ubuntu 16.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.300
  [Host]     : .NET Core 2.1.0 (CoreCLR 4.6.26515.07, CoreFX 4.6.26515.06), 64bit RyuJIT
  DefaultJob : .NET Core 2.1.0 (CoreCLR 4.6.26515.07, CoreFX 4.6.26515.06), 64bit RyuJIT


               Method |      Mean |      Error |     StdDev |    Median | Scaled |  Allocated |
--------------------- |----------:|-----------:|-----------:|----------:|-------:|-----------:|
   IterativeFirstList | 171.72 us |  5.2406 us | 14.6086 us | 166.67 us |   1.00 |        0 B |
        LinqFirstList | 510.86 us | 15.1985 us | 21.3062 us | 503.74 us |   2.99 |       40 B |
  LinqFasterFirstList | 139.00 us |  0.9755 us |  0.9125 us | 139.07 us |   0.81 |        0 B |
  IterativeFirstArray |  31.08 us |  0.3263 us |  0.2725 us |  31.04 us |   0.18 |        0 B |
       LinqFirstArray | 406.34 us |  8.0453 us | 16.4345 us | 403.84 us |   2.38 |       32 B |
 LinqFasterFirstArray | 140.86 us |  0.5012 us |  0.4186 us | 140.92 us |   0.83 |        0 B |

// * Hints *
Outliers
  WhereSelectBenchmarks.IterativeList: Default -> 1 outlier  was  removed

// * Legends *
  Mean      : Arithmetic mean of all measurements
  Error     : Half of 99.9% confidence interval
  StdDev    : Standard deviation of all measurements
  Scaled    : Mean(CurrentBenchmark) / Mean(BaselineBenchmark)
  ScaledSD  : Standard deviation of ratio of distribution of [CurrentBenchmark] and [BaselineBenchmark]
  Gen 0     : GC Generation 0 collects per 1k Operations
  Allocated : Allocated memory per single operation (managed only, inclusive, 1KB = 1024B)
  1 us      : 1 Microsecond (0.000001 sec)
```

* `Select()` and `Where()`:

```
BenchmarkDotNet=v0.10.14, OS=ubuntu 16.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.300
  [Host]     : .NET Core 2.1.0 (CoreCLR 4.6.26515.07, CoreFX 4.6.26515.06), 64bit RyuJIT
  DefaultJob : .NET Core 2.1.0 (CoreCLR 4.6.26515.07, CoreFX 4.6.26515.06), 64bit RyuJIT


                     Method |     Mean |    Error |    StdDev |   Median | Scaled | Allocated |
--------------------------- |---------:|---------:|----------:|---------:|-------:|----------:|
   IterativeWhereSelectList | 430.6 us | 8.237 us | 11.274 us | 428.8 us |   1.00 | 128.33 KB |
        LinqWhereSelectList | 432.5 us | 3.976 us |  3.320 us | 432.3 us |   1.01 | 128.48 KB |
  LinqFasterWhereSelectList | 468.4 us | 4.619 us |  4.320 us | 467.7 us |   1.09 | 128.33 KB |
  IterativeWhereSelectArray | 227.5 us | 4.147 us |  3.879 us | 227.1 us |   0.53 | 167.41 KB |
       LinqWhereSelectArray | 399.5 us | 7.957 us | 21.915 us | 392.7 us |   0.93 |  103.8 KB |
 LinqFasterWhereSelectArray | 437.4 us | 8.730 us | 15.742 us | 430.3 us |   1.02 | 429.73 KB |

// * Hints *
Outliers
  FirstBenchmarks.LinqFirst: Default -> 2 outliers were removed

// * Legends *
  Mean      : Arithmetic mean of all measurements
  Error     : Half of 99.9% confidence interval
  StdDev    : Standard deviation of all measurements
  Scaled    : Mean(CurrentBenchmark) / Mean(BaselineBenchmark)
  ScaledSD  : Standard deviation of ratio of distribution of [CurrentBenchmark] and [BaselineBenchmark]
  Allocated : Allocated memory per single operation (managed only, inclusive, 1KB = 1024B)
  1 us      : 1 Microsecond (0.000001 sec)

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
