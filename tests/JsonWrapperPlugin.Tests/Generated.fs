//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace rec DataSchema


namespace rec DataSchema


namespace rec DataSchema

type SimpleSchema(jtoken: Newtonsoft.Json.Linq.JToken) =

    member this.one
        with get () = jtoken.["one"].ToObject<int>()
        and set (x: int) = jtoken.["one"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    member this.two
        with get () = jtoken.["two"].ToObject<string>()
        and set (x: string) = jtoken.["two"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    member this.three
        with get () = jtoken.["three"].ToObject<System.Guid>()
        and set (x: System.Guid) = jtoken.["three"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    interface Example.IHaveJToken with
        member this.InnerData = jtoken

type DifferentBackingFieldSchema(jtoken: Newtonsoft.Json.Linq.JToken) =

    member this.one
        with get () = jtoken.["not_one"].ToObject<int>()
        and set (x: int) = jtoken.["not_one"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    member this.two
        with get () = jtoken.["two"].ToObject<int>()
        and set (x: int) = jtoken.["two"] <- Newtonsoft.Json.Linq.JToken.op_Implicit x

    interface Example.IHaveJToken with
        member this.InnerData = jtoken
