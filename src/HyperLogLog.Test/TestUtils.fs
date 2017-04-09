module TestUtils
open System
open System.Diagnostics
open Main

let log2 x = 
    let ln2 = 0.693147180559945309417232121458;
    (log x) / ln2

let GetAccuracyInBits stdError =
    let sqrtm = 1.04 / stdError;
    ceil (log2 (sqrtm*sqrtm)) |> byte

let GetGcMemory =
    GC.Collect()
    GC.GetTotalMemory true

let ReportMemoryCost gcMemoryAtStart =
    let memoryCost = GetGcMemory - gcMemoryAtStart
    Console.WriteLine("Appx. memory cost: {0} bytes", memoryCost);

let RunTest stdError expectedCount =
    let b = GetAccuracyInBits stdError
    let runStopwatch = new Stopwatch()
    let gcMemoryAtStart = GetGcMemory

    runStopwatch.Start();
    let estimator = 
        seq { 0..(expectedCount-1) } 
            |> Seq.fold 
                (fun e i -> i |> HLL.AddElement e)
                (HLL.CreateEstimator b (fun e -> e |> hash |> uint64))

    runStopwatch.Stop();
    ReportMemoryCost(gcMemoryAtStart)

//let RunTest stdError expectedCount maxAcceptedError numHllInstances sequential =
//    let elementSizeInBytes = 20
//    let rand = new Random()
//    let acceptedError = 
//        match maxAcceptedError with
//            | Some a -> a
//            | None   -> 4.0 * stdError
//    let b = GetAccuracyInBits stdError
//    let runStopwatch = new Stopwatch()
//    let gcMemoryAtStart = GetGcMemory
//
//    let hlls = 
//        seq { 
//            for _ in 0 .. numHllInstances - 1 do 
//                yield HLL.CreateEstimator b (fun e -> e |> hash |> uint64)
//        } |> Seq.toList
//        
//    let nextMember : byte array = Array.zeroCreate elementSizeInBytes 
//    runStopwatch.Start();
//
//    for i in 0 .. expectedCount - 1 do 
//        let chosenHll = rand.Next(numHllInstances)
//        hlls.[chosenHll] <- if sequential then
//                                HLL.AddElement hlls.[chosenHll] i
//                            else
//                                rand.NextBytes(nextMember)
//                                HLL.AddElement hlls.[chosenHll] nextMember
//    runStopwatch.Stop();
//    ReportMemoryCost(gcMemoryAtStart)
