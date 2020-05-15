//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace rec DataSchema


namespace rec DataSchema


namespace rec DataSchema

type SimpleSchema(jtoken: Newtonsoft.Json.Linq.JToken) =

    member this.one
        with get () =
            let v = jtoken.["one"]
            v.ToObject<int>()
        and set (x: int) = jtoken.["one"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    member this.two
        with get () =
            let v = jtoken.["two"]
            v.ToObject<string>()
        and set (x: string) = jtoken.["two"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    member this.three
        with get () =
            let v = jtoken.["three"]
            v.ToObject<System.Guid>()
        and set (x: System.Guid) = jtoken.["three"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    interface Example.IHaveJToken with
        member this.InnerData = jtoken

type DifferentBackingFieldSchema(jtoken: Newtonsoft.Json.Linq.JToken) =

    member this.one
        with get () =
            let v = jtoken.["not_one"]
            v.ToObject<int>()
        and set (x: int) = jtoken.["not_one"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    member this.two
        with get () =
            let v = jtoken.["two"]
            v.ToObject<int>()
        and set (x: int) = jtoken.["two"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    interface Example.IHaveJToken with
        member this.InnerData = jtoken

type NullableFieldSchema(jtoken: Newtonsoft.Json.Linq.JToken) =

    member this.one
        with get () =
            let v = jtoken.["one"]
            v.ToObject<System.Nullable<int>>()
        and set (x: System.Nullable<int>) = jtoken.["one"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    member this.two
        with get () =
            let v = jtoken.["two"]
            v.ToObject<int>()
        and set (x: int) = jtoken.["two"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    interface Example.IHaveJToken with
        member this.InnerData = jtoken

type NullableMissingFieldSchema(jtoken: Newtonsoft.Json.Linq.JToken) =

    member this.one
        with get () =
            let v = jtoken.["one"]
            if isNull v then Example.MissingJsonFieldException("one", jtoken) |> raise
            v.ToObject<System.Nullable<int>>()
        and set (x: System.Nullable<int>) = jtoken.["one"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    member this.two
        with get () =
            let v = jtoken.["two"]
            v.ToObject<int>()
        and set (x: int) = jtoken.["two"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    interface Example.IHaveJToken with
        member this.InnerData = jtoken
