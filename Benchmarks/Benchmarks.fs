module Benchmarks

open System

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs

[<SimpleJob (RuntimeMoniker.Net60)>]
type Benchmarks() =
    [<Params(100, 1000, 10000, 100000, 1000000)>]
    member val size = 0 with get, set

    [<Benchmark(Baseline = true)>]
    member this.Array () = [| 0 .. this.size |] |> Array.map ((+) 1)
    [<Benchmark>]
    member this.List () = [ 0 .. this.size ] |> List.map ((+) 1)
    [<Benchmark>]
    member this.Seq () = seq { 0 .. this.size } |> Seq.map ((+) 1) |> Seq.length // force evaluation

BenchmarkRunner.Run<Benchmarks>() |> ignore