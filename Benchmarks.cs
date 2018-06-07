using BenchmarkDotNet.Attributes;
using JM.LinqFaster;
using System.Collections.Generic;
using System.Linq;

namespace linq_perf
{
    [Config(typeof(BenchmarkConfig))]
    public class TransducerBenchmarks
    {
        private static readonly List<int> itemsList = Enumerable.Range(0, 100000).ToList();

        // [Benchmark(Baseline = true)]
        // public List<int> IterativeWhereSelectList()
        // {
        //     var results = new List<int>();
        //     for (int i = 0; i < itemsList.Count; i++)
        //     {
        //         int item = itemsList[i];
        //         if (item % 10 == 0)
        //             results.Add(item + 5);
        //     }

        //     return results;
        // }

        // [Benchmark]
        [Benchmark(Baseline = true)]
        public List<int> LinqWhereSelectList()
        {
            var results = itemsList
                .Where(i => i % 10 == 0)
                .Select(i => i + 5)
                .ToList();
            return results;
        }

        [Benchmark]
        public List<int> TransducerWhereSelectList()
        {
            var filterXForm = new FilterTransducer<int>(i => i % 10 == 0);
            var mapXForm = new MapTransducer<int, int>(i => i + 5);

            var reducer =
                filterXForm.Transduce(
                    mapXForm.Transduce(
                        new ToListReducer<int>()));

            var results = new List<int>();
            for (int i = 0; i < itemsList.Count; i++)
            {
                results = reducer.Reduce(results, itemsList[i]);
            }
            return results;
        }
    }

    [Config(typeof(BenchmarkConfig))]
    public class WhereSelectBenchmarks
    {
        private static readonly List<int> itemsList = Enumerable.Range(0, 100000).ToList();

        [Benchmark(Baseline = true)]
        public List<int> IterativeWhereSelectList()
        {
            var results = new List<int>();
            for (int i = 0; i < itemsList.Count; i++)
            {
                int item = itemsList[i];
                if (item % 10 == 0)
                    results.Add(item + 5);
            }

            return results;
        }

        [Benchmark]
        public List<int> LinqWhereSelectList()
        {
            var results = itemsList
                .Where(i => i % 10 == 0)
                .Select(i => i + 5)
                .ToList();
            return results;
        }

        [Benchmark]
        public List<int> LinqFasterWhereSelectList()
        {
            var results = itemsList
                .WhereSelectF(i => i % 10 == 0, i => i + 5);
            return results;
        }

        private static readonly int[] itemsArray = Enumerable.Range(0, 100000).ToArray();

        [Benchmark]
        public int[] IterativeWhereSelectArray()
        {
            var accumulator = new List<int>();
            foreach (var item in itemsArray)
            {
                if (item % 10 == 0)
                    accumulator.Add(item + 5);
            }

            int[] results = new int[accumulator.Count];
            accumulator.CopyTo(results);
            return results;
        }

        [Benchmark]
        public int[] LinqWhereSelectArray()
        {
            var results = itemsArray
                .Where(i => i % 10 == 0)
                .Select(i => i + 5)
                .ToArray();
            return results;
        }

        [Benchmark]
        public int[] LinqFasterWhereSelectArray()
        {
            var results = itemsArray
                .WhereSelectF(i => i % 10 == 0, i => i + 5);
            return results;
        }
    }

    [Config(typeof(BenchmarkConfig))]
    public class FirstBenchmarks
    {
        private static readonly List<int> itemsList = Enumerable.Range(0, 100000).ToList();

        [Benchmark(Baseline = true)]
        public int IterativeFirstList()
        {
            int result = -1;
            foreach (var item in itemsList)
            {
                if (item * 2 == 100000)
                {
                    return result;
                }
            }

            return result;
        }

        [Benchmark]
        public int LinqFirstList()
        {
            int result = itemsList.First(i => (i * 2) == 100000);
            return result;
        }

        [Benchmark]
        public int LinqFasterFirstList()
        {
            int result = itemsList.FirstF(i => (i * 2) == 100000);
            return result;
        }

        private static readonly int[] itemsArray = Enumerable.Range(0, 100000).ToArray();

        [Benchmark]
        public int IterativeFirstArray()
        {
            int result = -1;
            foreach (var item in itemsArray)
            {
                if (item * 2 == 100000)
                    return result;
            }

            return result;
        }

        [Benchmark]
        public int LinqFirstArray()
        {
            int result = itemsArray.First(i => (i * 2) == 100000);
            return result;
        }

        [Benchmark]
        public int LinqFasterFirstArray()
        {
            int result = itemsArray.FirstF(i => (i * 2) == 100000);
            return result;
        }
    }
}
