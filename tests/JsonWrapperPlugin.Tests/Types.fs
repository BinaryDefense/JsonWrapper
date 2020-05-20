namespace Example
open System
open Myriad.Plugins
open Newtonsoft.Json.Linq
open  Newtonsoft.Json
open BinaryDefense.JsonWrapper.Core



[<Generator.JsonWrapper>]
type SimpleSchema = {
    one: int
    two: string
    three : System.Guid
}

[<Generator.JsonWrapper>]
type DifferentBackingFieldSchema = {
    [<JsonProperty("not_one")>]
    one: int
    two: int
}

[<Generator.JsonWrapper>]
type NullableFieldSchema = {
    one: System.Nullable<int>
    two: string
}


[<Generator.JsonWrapper>]
type NullableMissingFieldSchema = {
    [<Attributes.MustExist>]
    one: System.Nullable<int>
    two: int
}

[<Generator.JsonWrapper>]
type OptionalFieldSchema = {
    one: int option
    two: int
}


[<Generator.JsonWrapper>]
type InnerType = {
    one: int option
    two: string
}

[<Generator.JsonWrapper>]
type OuterType = {
    foo: InnerType
    count : int
}

type Test1Wrapper( jtoken : JToken ) =

    member this.one
        with get () : int = jtoken.["one"] |> string |> Int32.Parse
        and set (value : int) = jtoken.["one"] <- JToken.op_Implicit value

    member this.two
        with get () : string = jtoken.["two"].Value<string>()
        and set (value : string) = jtoken.["two"] <- JToken.op_Implicit value

    interface IHaveJToken with
        member this. InnerData = jtoken


type Test2Wrapper( jtoken : JToken ) =

    member this.one
        with get () : int =
            let v = jtoken.["one"]
            if isNull v then MissingJsonFieldException("one", jtoken) |> raise
            v.ToObject<int>()
        and set (value : int) = jtoken.["one"] <- JToken.op_Implicit value

type Test3Wrapper( jtoken : JToken, serializer: Newtonsoft.Json.JsonSerializer ) =

    member this.one
        with get () : int = 3
    override this.Equals(o : obj) =
        match o with
        | :? IHaveJToken as it -> Newtonsoft.Json.Linq.JToken.DeepEquals(it.InnerData, jtoken)
        | _ -> false


type Test4Wrapper() =

  let mutable _one = [|0|]
  let mutable _two = 0
  let mutable _three = ""

  member _.one
    with get () = _one
    and set (newValue) =
      _one <- newValue

  member _.two
    with get () = _two
    and set (newValue: int) =
      _two <- newValue


  member _.three
    with get () = _three
    and set (newValue: string) =
      _three <- newValue

  member this.Deconstruct([<System.Runtime.InteropServices.Out>]one: _ outref, [<System.Runtime.InteropServices.Out>]two: _ outref, [<System.Runtime.InteropServices.Out>]three: _ outref) =
    one <- this.one
    two <- this.two
    _three <- this.three
