using BenchmarkDotNet.Running;

namespace linq_perf
{
    class Program
    {
        static void Main(string[] args)
        {
            // BenchmarkRunner.Run<WhereSelectBenchmarks>();
            // BenchmarkRunner.Run<FirstBenchmarks>();
            BenchmarkRunner.Run<TransducerBenchmarks>();
        }
    }
}
