using System;
using System.Collections.Generic;
using System.Linq;
using BenchmarkDotNet.Running;

namespace linq_perf
{
    class Program
    {
        static void Main(string[] args)
        {
            // BenchmarkRunner.Run<WhereSelectBenchmarks>();
            // BenchmarkRunner.Run<FirstBenchmarks>();
            // BenchmarkRunner.Run<TransducerBenchmarks>();

//            List<int> itemsList = Enumerable.Range(0, 70).ToList();
            List<int> results =
                Enumerable.Range(0, 70).Transduce(
                    new FilterTransducer<int>(i => i % 10 == 0).Transduce(
                        new MapTransducer<int, int>(i => i + 5).Transduce(
                            new ToListReducer<int>())));

            Console.WriteLine("Results: " + string.Join(',', results));

            // Output: Results: 5,15,25,35,45,55,65
        }
    }
}
