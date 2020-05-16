namespace Example
open System
open Myriad.Plugins
open Newtonsoft.Json.Linq
open  Newtonsoft.Json

open FSharp.Reflection
open System.Reflection


type IHaveJToken  =
    abstract member InnerData : JToken

module Converters =

    type IHaveJTokenConverter() =
        inherit JsonConverter()
        override __.CanConvert t =
            typeof<IHaveJToken>.IsAssignableFrom(t)

        override __.WriteJson(writer, value, serializer) =
            match value with
            | :? IHaveJToken as it -> serializer.Serialize(writer, it.InnerData)
            | _ -> InvalidCastException("Did not implement type IHaveJToken") |> raise

        override __.ReadJson(reader, t, _existingValue, serializer) =
            let jtoken = JToken.ReadFrom reader
            let ctor = t.GetConstructor([|typeof<JToken>; typeof<JsonSerializer>|])
            ctor.Invoke([|
                jtoken
                serializer
            |])
    /// F# options-converter
    type OptionConverter() =
        inherit JsonConverter()
        let optionTy = typedefof<option<_>>

        override __.CanConvert t =
            t.IsGenericType
            && optionTy.Equals (t.GetGenericTypeDefinition())

        override __.WriteJson(writer, value, serializer) =
            let value =
                if isNull value then
                    null
                else
                    let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                    fields.[0]
            serializer.Serialize(writer, value)

        override __.ReadJson(reader, t, _existingValue, serializer) =
            let innerType = t.GetGenericArguments().[0]

            let innerType =
                if innerType.IsValueType then
                    typedefof<Nullable<_>>.MakeGenericType([| innerType |])
                else
                    innerType

            let value = serializer.Deserialize(reader, innerType)
            let cases = FSharpType.GetUnionCases t

            if isNull value then
                FSharpValue.MakeUnion(cases.[0], [||])
            else
                FSharpValue.MakeUnion(cases.[1], [|value|])

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
    two: string
}


[<Generator.JsonWrapper>]
type NullableMissingFieldSchema = {
    [<Attribute.MustExist>]
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
        with get () : int =
            let v = jtoken.["one"]
            if isNull v then MissingJsonFieldException("one", jtoken) |> raise
            v.ToObject<int>(serializer)
        and set (value : int) = jtoken.["one"] <- JToken.FromObject(value, serializer)
    override this.Equals(o : obj) =
        match o with
        | :? IHaveJToken as it -> Newtonsoft.Json.Linq.JToken.DeepEquals(it.InnerData, jtoken)
        | _ -> false
