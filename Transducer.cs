using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace linq_perf
{
    public interface IReducer<T, R>
    {
        R Reduce(R result, T input);
    }

    // public abstract class Reducer<T, R> : IReducer<T, R>
    // {
    //     private readonly Func<R, T, R> reduce;

    //     public Reducer(Func<R, T, R> reduce)
    //     {
    //         this.reduce = reduce;
    //     }

    //     // [MethodImpl(MethodImplOptions.AggressiveInlining)]
    //     public R Reduce(R result, T input)
    //     {
    //         return reduce(result, input);
    //     }
    // }

    public class MapReducer<TIn, TOut, R> : IReducer<TIn, R>
    {
        private readonly Func<TIn, TOut> map;
        private readonly IReducer<TOut, R> reducer;

        public MapReducer(Func<TIn, TOut> map, IReducer<TOut, R> reducer)
        {
            this.map = map;
            this.reducer = reducer;
        }

        public R Reduce(R result, TIn input)
        {
            return reducer.Reduce(result, map(input));
        }
    }

    public class FilterReducer<T, R> : IReducer<T, R>
    {
        // [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private readonly Func<T, bool> filter;
        private readonly IReducer<T, R> reducer;

        public FilterReducer(Func<T, bool> filter, IReducer<T, R> reducer)
        {
            this.filter = filter;
            this.reducer = reducer;
        }

        public R Reduce(R result, T input)
        {
            if (filter(input))
            {
                return reducer.Reduce(result, input);
            }
            else
            {
                return result;
            }
        }
    }

    public class ToListReducer<T> : IReducer<T, List<T>>
    {
        // [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public List<T> Reduce(List<T> result, T input)
        {
            result.Add(input);
            return result;
        }
    }

    public interface ITransducer<TIn, TOut>
    {
        IReducer<TIn, R> Transduce<R>(IReducer<TOut, R> reducer);
    }

    public class MapTransducer<TIn, TOut> : ITransducer<TIn, TOut>
    {
        private Func<TIn, TOut> map;

        public MapTransducer(Func<TIn, TOut> map)
        {
            this.map = map;
        }

        public IReducer<TIn, R> Transduce<R>(IReducer<TOut, R> reducer) =>
            new MapReducer<TIn, TOut, R>(map, reducer);
        // {
        //     return new Reducer<TIn, R>(
        //         (R result, TIn value) =>
        //         {
        //             return reducer.Reduce(result, map(value));
        //         });
        // }
    }

    public class FilterTransducer<T> : ITransducer<T, T>
    {
        private Func<T, bool> filter;

        public FilterTransducer(Func<T, bool> filter)
        {
            this.filter = filter;
        }

        public IReducer<T, R> Transduce<R>(IReducer<T, R> reducer) =>
            new FilterReducer<T, R>(filter, reducer);
        // {
        //     return new Reducer<T, R>(
        //         (R result, T input) =>
        //         {
        //             if (filter(input))
        //             {
        //                 return reducer.Reduce(result, input);
        //             }
        //             else
        //             {
        //                 return result;
        //             }
        //         });
        // }
    }
}
