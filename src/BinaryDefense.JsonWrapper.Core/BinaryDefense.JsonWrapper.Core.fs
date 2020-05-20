namespace BinaryDefense.JsonWrapper.Core

open Newtonsoft.Json.Linq
open Newtonsoft.Json
open System

type IHaveJToken  =
    abstract member InnerData : JToken

module Attributes =
    type MustExist() =
        inherit Attribute()

type MissingJsonFieldException(fieldName : string, ?jtoken : JToken) =
    inherit Exception(sprintf "Failed to find json key \"%s\" on JToken" fieldName )
    member __.JsonFieldName = fieldName
    member __.JToken = jtoken


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


    let recommendedConverters : JsonConverter list = [
        IHaveJTokenConverter ()
        Newtonsoft.Json.FSharp.Idiomatic.OptionConverter()
    ]
