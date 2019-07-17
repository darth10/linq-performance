using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Diagnosers;
using BenchmarkDotNet.Validators;

namespace linq_perf
{
    class BenchmarkConfig : ManualConfig
    {
        public BenchmarkConfig()
        {
            Add(JitOptimizationsValidator.FailOnError);
            Add(MemoryDiagnoser.Default);
        }
    }
}
