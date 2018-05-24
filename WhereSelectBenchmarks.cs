using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Diagnosers;
using BenchmarkDotNet.Validators;
using Nessos.LinqOptimizer.CSharp;

using System.Collections.Generic;
using System.Linq;

namespace linq_perf
{
    class BenchmarkConfig : ManualConfig
    {
        public BenchmarkConfig()
        {
            Add(JitOptimizationsValidator.FailOnError);
            Add(new MemoryDiagnoser());
        }
    }
    
    [Config(typeof(BenchmarkConfig))]
    public class WhereSelectBenchmarks
    {
        private static readonly int[] items = Enumerable.Range(0, 10000).ToArray();

        [Benchmark(Baseline = true)]
        public List<int> IterativeList()
        {
            var results = new List<int>();
            foreach (var item in items)
            {
                if (item % 10 == 0)
                    results.Add(item + 5);
            }

            return results;
        }

        [Benchmark]
        public List<int> LinqList()
        {
            var results = items
                .Where(i => i % 10 == 0)
                .Select(i => i + 5)
                .ToList();
            return results;
        }
    }

    [Config(typeof(BenchmarkConfig))]
    public class FirstBenchmarks
    {
        private static readonly int[] items = Enumerable.Range(0, 10000).ToArray();

        [Benchmark(Baseline = true)]
        public int IterativeFirst()
        {
            int result = -1;
            foreach (var item in items)
            {
                if (item * 2 == 10000)
                {
                    result = item;
                    break;
                }
            }

            return result;
        }

        [Benchmark]
        public int LinqFirst()
        {
            int result = items.First(i => (i * 2) == 10000);
            return result;
        }

        [Benchmark]
        public int LinqSingle()
        {
            int result = items.Single(i => (i * 2) == 10000);
            return result;
        }
    }


    [Config(typeof(BenchmarkConfig))]
    public class FindBenchmarks
    {
        private static readonly List<int> items = Enumerable.Range(0, 10000).ToList();

        [Benchmark(Baseline = true)]
        public int IterativeFirst()
        {
            int result = -1;
            foreach (var item in items)
            {
                if (item * 2 == 10000)
                {
                    result = item;
                    break;
                }
            }

            return result;
        }

        [Benchmark]
        public int ListFindFirst()
        {
            int result = items.Find(i => (i * 2) == 10000);
            return result;
        }
    }
}
