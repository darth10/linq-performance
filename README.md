# Linq performance

## Results

* `IEnumerable<T>.Select()`, `IEnumerable<T>.Where()`, `IEnumerable<T>.ToList()` using `Array<T>`:

```
BenchmarkDotNet=v0.10.14, OS=ubuntu 16.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.4
  [Host]     : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT
  DefaultJob : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT


        Method |     Mean |     Error |    StdDev | Scaled | ScaledSD |  Gen 0 | Allocated |
-------------- |---------:|----------:|----------:|-------:|---------:|-------:|----------:|
 IterativeList | 19.25 us | 0.2457 us | 0.2178 us |   1.00 |     0.00 | 5.3406 |   8.23 KB |
      LinqList | 41.15 us | 0.2604 us | 0.2436 us |   2.14 |     0.03 | 5.3711 |   8.34 KB |

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

* `IEnumerable<T>.First()`, `IEnumerable<T>.Single()` using `Array<T>`:

```
BenchmarkDotNet=v0.10.14, OS=ubuntu 16.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.4
  [Host]     : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT
  DefaultJob : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT


         Method |      Mean |     Error |    StdDev | Scaled | ScaledSD | Allocated |
--------------- |----------:|----------:|----------:|-------:|---------:|----------:|
 IterativeFirst |  3.113 us | 0.0160 us | 0.0150 us |   1.00 |     0.00 |       0 B |
      LinqFirst | 41.126 us | 1.9823 us | 1.8542 us |  13.21 |     0.58 |      32 B |
     LinqSingle | 78.866 us | 0.5188 us | 0.4852 us |  25.33 |     0.19 |      32 B |

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

* `IEnumerable<T>.First()` and `List<T>.Find()` using `List<T>`:

```
BenchmarkDotNet=v0.10.14, OS=ubuntu 16.04
Intel Core i5-3320M CPU 2.60GHz (Ivy Bridge), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.4
  [Host]     : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT
  DefaultJob : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT


         Method |     Mean |     Error |    StdDev | Scaled | Allocated |
--------------- |---------:|----------:|----------:|-------:|----------:|
 IterativeFirst | 17.09 us | 0.1502 us | 0.1405 us |   1.00 |       0 B |
  ListFindFirst | 16.97 us | 0.1004 us | 0.0939 us |   0.99 |       0 B |

// * Legends *
  Mean      : Arithmetic mean of all measurements
  Error     : Half of 99.9% confidence interval
  StdDev    : Standard deviation of all measurements
  Scaled    : Mean(CurrentBenchmark) / Mean(BaselineBenchmark)
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
