module TimingLoop

open System.Diagnostics
open System.Threading
open NativeMethods
open System

type TimingData = {
    SpinsPerMilliSecond : int64
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
    let sw = Stopwatch();
    let wait = SpinWait();
    let mutable count = 0L;
    sw.Start();
    while sw.ElapsedMilliseconds < 2000L do
        for _i = 1 to 200 do wait.SpinOnce()
        count <- count + 200L;
    sw.Stop();

    let spinsPerMilliSecond = count / 2000L;

    let frameTime = Stopwatch.Frequency / 60L;

    let timingData = {
        SpinsPerMilliSecond = spinsPerMilliSecond
        FrameTime = frameTime
        MainStopwatch = Stopwatch()
        FpsStopwatch = Stopwatch()
        }

    let timingState = {
        FrameCount = 0L
        NextFrameTime = frameTime
        Fps = 0
        }

    timingData.MainStopwatch.Start()
    timingData.FpsStopwatch.Start()

    (timingData, timingState)

let HasNoMessages() =
            let mutable result = NativeMessage()
            PeekMessage(&result, IntPtr.Zero, 0u, 0u, 0u) = 0

let MessageLoop timingData timingStateRef update =
    let timingState = !timingStateRef
    let spinWait = SpinWait()
    while HasNoMessages() do
        timingStateRef :=
            if timingData.MainStopwatch.ElapsedTicks >= timingState.NextFrameTime then
                let frameCount = timingState.FrameCount + 1L
                let fps = 
                    if frameCount % 60L = 0L then
                        frameCount * 1000L / timingData.FpsStopwatch.ElapsedMilliseconds
                        |> int
                    else
                        timingState.Fps
                let newTimingState = { Fps = fps; FrameCount = frameCount; NextFrameTime = timingState.NextFrameTime + timingData.FrameTime }
                update newTimingState
                newTimingState
            else
                for _i = 1 to 10 do spinWait.SpinOnce()
                timingState


