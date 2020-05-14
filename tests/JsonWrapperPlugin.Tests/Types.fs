namespace Example
open System
open Myriad.Plugins
open Newtonsoft.Json.Linq

[<Generator.Fields2Attribute>]
type Test1 = { one: int; two: string; }

type IHaveJToken  =
    abstract member InnerData : JToken

type Test1Wrapper( jtoken : JToken ) =

    member this.one
        with get () : int = jtoken.["one"] |> string |> Int32.Parse
        and set (value : int) = jtoken.["one"] <- JToken.op_Implicit value

    member this.two
        with get () : string = jtoken.["two"] |> string
        and set (value : string) = jtoken.["two"] <- JToken.op_Implicit value

    interface IHaveJToken with
        member this. InnerData = jtoken
