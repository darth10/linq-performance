using BenchmarkDotNet.Attributes;
using Cistern.ValueLinq;
using System.Collections.Generic;
using System.Linq;

namespace linq_perf
{
    public partial class WhereSelectBenchmarks
    {
        struct Mod10 : IFunc<int, bool> { public bool Invoke(int i) => i % 10 == 0; }
        struct Add5 : IFunc<int, int> { public int Invoke(int i) => i + 5;  }

        [Benchmark, BenchmarkCategory("ToList")]
        public List<int> ValueLinqWhereSelectList()
        {
            var results = itemsList
                .Where(i => i % 10 == 0)
                .Select(i => i + 5)
                .ToList();
            return results;
        }

        [Benchmark, BenchmarkCategory("ToList")]
        public List<int> ValueLinqWhereSelectListUsePool()
        {
            var results = itemsList
                .Where(i => i % 10 == 0)
                .Select(i => i + 5)
                .ToListUsePool();
            return results;
        }

        [Benchmark, BenchmarkCategory("ToList")]
        public List<int> ValueLinqIFuncWhereSelectList()
        {
            var results = itemsList
                .Where(new Mod10())
                .Select(new Add5(), default(int))
                .ToList();
            return results;
        }

        [Benchmark, BenchmarkCategory("ToList")]
        public List<int> ValueLinqIFuncWhereSelectListUsePool()
        {
            var results = itemsList
                .Where(new Mod10())
                .Select(new Add5(), default(int))
                .ToListUsePool();
            return results;
        }

        [Benchmark, BenchmarkCategory("ToArray")]
        public int[] ValueLinqWhereSelectArray()
        {
            var results = itemsArray
                .Where(i => i % 10 == 0)
                .Select(i => i + 5)
                .ToArray();
            return results;
        }

        [Benchmark, BenchmarkCategory("ToArray")]
        public int[] ValueLinqWhereSelectArrayUsePool()
        {
            var results = itemsArray
                .Where(i => i % 10 == 0)
                .Select(i => i + 5)
                .ToArrayUsePool();
            return results;
        }
        [Benchmark, BenchmarkCategory("ToArray")]
        public int[] ValueLinqIFuncWhereSelectArray()
        {
            var results = itemsArray
                .Where(new Mod10())
                .Select(new Add5(), default(int))
                .ToArray();
            return results;
        }

        [Benchmark, BenchmarkCategory("ToArray")]
        public int[] ValueLinqIFuncWhereSelectArrayUsePool()
        {
            var results = itemsArray
                .Where(new Mod10())
                .Select(new Add5(), default(int))
                .ToArrayUsePool();
            return results;
        }
    }

    public partial class FirstBenchmarks
    {
        struct Times2Eq100000 : IFunc<int, bool> { public bool Invoke(int i) => (i * 2) == 100000; }


        [Benchmark]
        public int ValueLinqFirstList()
        {
            int result = itemsList.First(i => (i * 2) == 100000);
            return result;
        }

        [Benchmark]
        public int ValueLinqIFuncFirstList()
        {
            int result = itemsList.Where(new Times2Eq100000()).First();
            return result;
        }

        [Benchmark]
        public int ValueLinqFirstArray()
        {
            int result = itemsArray.First(i => (i * 2) == 100000);
            return result;
        }

        [Benchmark]
        public int ValueLinqIFuncFirstArray()
        {
            int result = itemsArray.Where(new Times2Eq100000()).First();
            return result;
        }
    }
}
