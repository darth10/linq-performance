using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace linq_perf
{
    public interface IReducer<T, R>
    {
        // [MethodImpl(MethodImplOptions.AggressiveInlining)]
        R Reduce(R result, T input);
    }

    public class MapReducer<TIn, TOut, R> : IReducer<TIn, R>
    {
        private readonly Func<TIn, TOut> _map;
        private readonly IReducer<TOut, R> _reducer;

        public MapReducer(Func<TIn, TOut> map, IReducer<TOut, R> reducer) =>
            (_map, _reducer) = (map, reducer);

        // [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public R Reduce(R result, TIn input)
        {
            return _reducer.Reduce(result, _map(input));
        }
    }

    public class FilterReducer<T, R> : IReducer<T, R>
    {
        private readonly Func<T, bool> _filter;
        private readonly IReducer<T, R> _reducer;

        public FilterReducer(Func<T, bool> filter, IReducer<T, R> reducer) =>
            (_filter, _reducer) = (filter, reducer);

        // [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public R Reduce(R result, T input)
        {
            if (_filter(input))
                return _reducer.Reduce(result, input);
            else
                return result;
        }
    }

    // TODO replace with NOP reducer
    public class ToListReducer<T> : IReducer<T, List<T>>
    {
        // [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public List<T> Reduce(List<T> result, T input)
        {
            result.Add(input);
            return result;
        }
    }

    public static class ListReduce
    {
        public static List<T> Reduce<T>(this List<T> input, IReducer<T, List<T>> reducer)
        {
            var result = new List<T>();

            for (int i = 0; i< input.Count; i++)
            {
                reducer.Reduce(result, input[i]);
            }

            return result;
        }
    }

    public interface ITransducer<TIn, TOut>
    {
        IReducer<TIn, R> Transduce<R>(IReducer<TOut, R> reducer);
    }

    public class MapTransducer<TIn, TOut> : ITransducer<TIn, TOut>
    {
        private readonly Func<TIn, TOut> _map;

        public MapTransducer(Func<TIn, TOut> map) =>
            _map = map;

        public IReducer<TIn, R> Transduce<R>(IReducer<TOut, R> reducer) =>
            new MapReducer<TIn, TOut, R>(_map, reducer);
    }

    public class FilterTransducer<T> : ITransducer<T, T>
    {
        private readonly Func<T, bool> _filter;

        public FilterTransducer(Func<T, bool> filter) =>
            _filter = filter;

        public IReducer<T, R> Transduce<R>(IReducer<T, R> reducer) =>
            new FilterReducer<T, R>(_filter, reducer);
    }
}
