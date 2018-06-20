module Stopwatch

type Stopwatch = Stopwatch of System.Diagnostics.Stopwatch

let create() = System.Diagnostics.Stopwatch() |> Stopwatch

let start (Stopwatch sw) = sw.Start()

let stop (Stopwatch sw) = sw.Stop()

let frequency = System.Diagnostics.Stopwatch.Frequency

let elapsedMilliseconds (Stopwatch sw) = sw.ElapsedMilliseconds

let elapsedTicks (Stopwatch sw) = sw.ElapsedTicks