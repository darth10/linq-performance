# Linq performance

## Results

* `Select()` and `Where()`:

```
BenchmarkDotNet=v0.10.14, OS=ubuntu 16.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.4
  [Host]     : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT
  DefaultJob : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT


               Method |      Mean |     Error |    StdDev | Scaled | ScaledSD | Allocated |
--------------------- |----------:|----------:|----------:|-------:|---------:|----------:|
   IterativeFirstList | 15.355 us | 0.1101 us | 0.1030 us |   1.00 |     0.00 |       0 B |
        LinqFirstList | 52.695 us | 0.2218 us | 0.2075 us |   3.43 |     0.03 |      40 B |
  LinqFasterFirstList | 18.596 us | 0.0908 us | 0.0849 us |   1.21 |     0.01 |       0 B |
  IterativeFirstArray |  3.121 us | 0.0188 us | 0.0176 us |   0.20 |     0.00 |       0 B |
       LinqFirstArray | 40.527 us | 0.2536 us | 0.2373 us |   2.64 |     0.02 |      32 B |
 LinqFasterFirstArray | 16.950 us | 0.0740 us | 0.0692 us |   1.10 |     0.01 |       0 B |

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

* `First()`:

```
BenchmarkDotNet=v0.10.14, OS=ubuntu 16.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.4
  [Host]     : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT
  DefaultJob : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT


                     Method |     Mean |     Error |    StdDev |   Median | Scaled | ScaledSD |   Gen 0 | Allocated |
--------------------------- |---------:|----------:|----------:|---------:|-------:|---------:|--------:|----------:|
   IterativeWhereSelectList | 51.57 us | 0.1178 us | 0.1044 us | 51.57 us |   1.00 |     0.00 |  5.3101 |   8.23 KB |
        LinqWhereSelectList | 41.70 us | 0.7279 us | 0.6809 us | 41.46 us |   0.81 |     0.01 |  5.4321 |   8.38 KB |
  LinqFasterWhereSelectList | 41.72 us | 0.2526 us | 0.2363 us | 41.73 us |   0.81 |     0.00 |  5.3101 |   8.23 KB |
  IterativeWhereSelectArray | 20.97 us | 0.4187 us | 1.1878 us | 20.47 us |   0.41 |     0.02 |  7.9041 |  12.16 KB |
       LinqWhereSelectArray | 44.96 us | 0.5785 us | 0.4831 us | 44.82 us |   0.87 |     0.01 |  5.4321 |    8.4 KB |
 LinqFasterWhereSelectArray | 41.25 us | 0.8212 us | 1.2038 us | 40.83 us |   0.80 |     0.02 | 27.7710 |  43.02 KB |

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
