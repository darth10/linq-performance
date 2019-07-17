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
            BenchmarkRunner.Run<WhereSelectBenchmarks>();
            // BenchmarkRunner.Run<FirstBenchmarks>();
        }
    }
}
