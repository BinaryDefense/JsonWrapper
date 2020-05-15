namespace Example
open System
open Myriad.Plugins
open Newtonsoft.Json.Linq
open  Newtonsoft.Json


type IHaveJToken  =
    abstract member InnerData : JToken

[<RequireQualifiedAccess>]
module Attribute =
    type MustExist() =
        inherit Attribute()

type MissingJsonFieldException(fieldName : string, ?jtoken : JToken) =
    inherit Exception(sprintf "Failed to find json key \"%s\" on JToken" fieldName )
    member __.JsonFieldName = fieldName
    member __.JToken = jtoken


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
    two: int
}


[<Generator.JsonWrapper>]
type NullableMissingFieldSchema = {
    [<Attribute.MustExist>]
    one: System.Nullable<int>
    two: int
}

// [<Generator.JsonWrapper>]
// type OptionalFieldSchema = {
//     one: int option
//     two: int
// }


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
