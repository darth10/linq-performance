# LINQ performance

## Results

* `First()`:

```
BenchmarkDotNet=v0.10.14, OS=ubuntu 16.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.300
  [Host]     : .NET Core 2.1.0 (CoreCLR 4.6.26515.07, CoreFX 4.6.26515.06), 64bit RyuJIT
  DefaultJob : .NET Core 2.1.0 (CoreCLR 4.6.26515.07, CoreFX 4.6.26515.06), 64bit RyuJIT


               Method |      Mean |     Error |    StdDev | Scaled | ScaledSD | Allocated |
--------------------- |----------:|----------:|----------:|-------:|---------:|----------:|
   IterativeFirstList | 16.630 us | 0.4226 us | 1.1639 us |   1.00 |     0.00 |       0 B |
        LinqFirstList | 49.500 us | 0.2885 us | 0.2409 us |   2.99 |     0.19 |      40 B |
  LinqFasterFirstList | 14.970 us | 0.2989 us | 0.4826 us |   0.90 |     0.06 |       0 B |
  IterativeFirstArray |  3.285 us | 0.0655 us | 0.0960 us |   0.20 |     0.01 |       0 B |
       LinqFirstArray | 40.706 us | 0.7966 us | 1.3742 us |   2.46 |     0.18 |      32 B |
 LinqFasterFirstArray | 14.126 us | 0.1577 us | 0.1475 us |   0.85 |     0.06 |       0 B |

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


                     Method |     Mean |     Error |    StdDev | Scaled | ScaledSD |   Gen 0 | Allocated |
--------------------------- |---------:|----------:|----------:|-------:|---------:|--------:|----------:|
   IterativeWhereSelectList | 40.07 us | 0.8403 us | 1.0627 us |   1.00 |     0.00 |  5.3101 |   8.23 KB |
        LinqWhereSelectList | 42.06 us | 0.8222 us | 1.0398 us |   1.05 |     0.04 |  5.4321 |   8.38 KB |
  LinqFasterWhereSelectList | 47.32 us | 0.9427 us | 2.6742 us |   1.18 |     0.07 |  5.3101 |   8.23 KB |
  IterativeWhereSelectArray | 20.35 us | 0.3983 us | 0.7481 us |   0.51 |     0.02 |  7.9041 |  12.16 KB |
       LinqWhereSelectArray | 38.22 us | 0.2628 us | 0.2458 us |   0.95 |     0.02 |  5.4321 |    8.4 KB |
 LinqFasterWhereSelectArray | 39.87 us | 0.4218 us | 0.3946 us |   1.00 |     0.03 | 27.7710 |  43.02 KB |

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
items = [1..10000]

is5000 :: Int -> Bool
is5000 i = (i * 2) == 10000

doSameThingAsFirst :: [Int] -> Int
doSameThingAsFirst xs = head $ filter is5000 xs

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
time                 16.31 us   (16.18 us .. 16.45 us)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 16.18 us   (16.08 us .. 16.29 us)
std dev              352.7 ns   (266.6 ns .. 468.7 ns)
variance introduced by outliers: 21% (moderately inflated)
```
