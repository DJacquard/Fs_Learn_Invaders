module NativeMethods

open System.Runtime.InteropServices
open System.Drawing
open System

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type NativeMessage =
    val mutable Handle : IntPtr
    val mutable Message : uint32
    val mutable WParameter : IntPtr
    val mutable LParameter : IntPtr 
    val mutable Time : uint32
    val mutable Location : Point 

[<DllImport("user32.dll")>]
extern int PeekMessage(NativeMessage& message, IntPtr window, uint32 filterMin, uint32 filterMax, uint32 remove)

