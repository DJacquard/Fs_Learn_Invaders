module TimingLoop

open System.Threading
open NativeMethods
open System
open Stopwatch

type TimingData = {
    FrameTime : int64
    MainStopwatch: Stopwatch
    FpsStopwatch: Stopwatch 
    }

type TimingState = {
    FrameCount : int64
    NextFrameTime : int64
    Fps : int
    }

let InitialiseTiming() =
    let frameTime = Stopwatch.frequency / 60L;

    let timingData = {
        FrameTime = frameTime
        MainStopwatch = Stopwatch.create()
        FpsStopwatch = Stopwatch.create()
        }

    let timingState = {
        FrameCount = 0L
        NextFrameTime = frameTime
        Fps = 0
        }

    timingData.MainStopwatch |> start
    timingData.FpsStopwatch |> start

    (timingData, timingState)

let HasNoMessages() =
            let mutable result = NativeMessage()
            PeekMessage(&result, IntPtr.Zero, 0u, 0u, 0u) = 0

let MessageLoop timingData timingStateRef update =
    let timingState = !timingStateRef
    let spinWait = SpinWait()
    while HasNoMessages() do
        timingStateRef :=
            if timingData.MainStopwatch |> elapsedTicks >= timingState.NextFrameTime then
                let frameCount = timingState.FrameCount + 1L
                let fps = 
                    if frameCount % 60L = 0L then
                        frameCount * 1000L / (timingData.FpsStopwatch |> elapsedMilliseconds)
                        |> int
                    else
                        timingState.Fps
                let newTimingState = { Fps = fps; FrameCount = frameCount; NextFrameTime = timingState.NextFrameTime + timingData.FrameTime }
                update newTimingState
                newTimingState
            else
                for _i = 1 to 10 do spinWait.SpinOnce()
                timingState


