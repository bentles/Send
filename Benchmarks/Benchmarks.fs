module Benchmarks

open System

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs

type Data = { A: int; B: int }


[<MemoryDiagnoser>]
[<SimpleJob(RuntimeMoniker.Net60)>]
type Benchmarks() =
    //[<Params(100, 1000, 10000, 100000, 1000000)>]
    //member val size = 0 with get, set

    member val data = { A = 22; B = 44 }

    //[<Benchmark(Baseline = true)>]
    //member this.Array() =
    //    [| 0 .. this.size |] |> Array.map ((+) 1)

    //[<Benchmark>]
    //member this.List() = [ 0 .. this.size ] |> List.map ((+) 1)

    //[<Benchmark>]
    //member this.Seq() =
    //    seq { 0 .. this.size } |> Seq.map ((+) 1) |> Seq.length // force evaluation


    //wanna know if nested list yields allocate more than
    //nested seq yields


    member this.YieldList(func: Data list) =
        [ for i in 1..500 do
              yield! func ]
    
    [<Benchmark(Baseline = true)>]
    member this.ListTest() =
        [ for i in 1..500 do
              yield! this.YieldList(this.YieldList [ this.data ]) ]

    member this.YieldSeq(func: Data seq) =
        seq {
            for i in 1..100 do
                yield! func
        }

    [<Benchmark>]
    member this.SeqTest() =
        [ for i in 1..100 do
              yield! this.YieldSeq(this.YieldSeq(seq { this.data })) ]

BenchmarkRunner.Run<Benchmarks>() |> ignore
