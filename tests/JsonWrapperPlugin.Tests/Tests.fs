module Tests

open System
open Expecto
open DataSchema
open Newtonsoft.Json
open Newtonsoft.Json.Linq


let scrubDefaultDUConverter (s: System.Collections.Generic.IList<JsonConverter>) =
    s
    |> Seq.tryFind (fun c -> c.GetType () = typeof<Converters.DiscriminatedUnionConverter>)
    |> Option.iter (s.Remove >> ignore)

// Serializer Settings
// If you change any of these, you need to decide if they also need to be applied to the Marten configurations. (see Server/Collection boostrapping)
let converters: JsonConverter [] = [|
    Example.Converters.OptionConverter()
    |]

let serializationSettings requireAllProps =
    let s = JsonSerializerSettings()
    scrubDefaultDUConverter s.Converters
    for c in converters do s.Converters.Add c
    if requireAllProps
    then s.MissingMemberHandling <- MissingMemberHandling.Error
    s

let looseSettings, strictSettings = serializationSettings false, serializationSettings true
let looseSerializer, strictSerializer =
  let loose, strict = JsonSerializer.CreateDefault looseSettings, JsonSerializer.CreateDefault strictSettings
  loose, strict

[<Tests>]
let simpleTests =
    testList "simple schema" [
        let jsonStr =
            """{
                "one": 42,
                "two": "Hitchhikers Guide",
                "three": "f971a6c0-ed00-46e5-b657-3fea2e368ba9"
                }
            """
        testCase "Gets keys it knows about" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = SimpleSchema(jtoken, looseSerializer)

            Expect.equal test1.one 42 ""
            Expect.equal test1.two "Hitchhikers Guide" ""
            Expect.equal test1.three (Guid.Parse "f971a6c0-ed00-46e5-b657-3fea2e368ba9") ""
        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = SimpleSchema(jtoken, looseSerializer)

            Expect.equal test1.one 42 ""
            test1.one <- 100
            Expect.equal test1.one 100 ""
            Expect.equal (jtoken.Value<int>("one")) 100 ""
    ]

[<Tests>]
let jsonPropertyTests =
    testList "JsonProperty tests" [
        let jsonStr =
                """{
                    "one": 42,
                    "not_one": 9001,
                    "two" : -1000
                    }
                """
        testCase "Gets keys supplied by user" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = DifferentBackingFieldSchema(jtoken, looseSerializer)

            Expect.equal test1.one 9001 ""
            Expect.equal test1.two -1000 ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = DifferentBackingFieldSchema(jtoken, looseSerializer)

            Expect.equal test1.one 9001 ""
            test1.one <- 100
            Expect.equal test1.one 100 ""
            Expect.equal (jtoken.Value<int>("one")) 42 ""
            Expect.equal (jtoken.Value<int>("not_one")) 100 ""
    ]



[<Tests>]
let nullablePropertyFieldExistTests =
    testList "Nullable Field Exists Property tests" [
        let jsonStr =
                """{
                    "one": null,
                    "two" : null
                    }
                """
        let nullInt = (Nullable<int>())
        let inline nullable x = Nullable<_> x
        testCase "Gets keys supplied by user" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = NullableFieldSchema(jtoken, looseSerializer)

            Expect.equal test1.one nullInt ""
            Expect.equal test1.two null ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = NullableFieldSchema(jtoken, looseSerializer)

            Expect.equal test1.one nullInt ""
            test1.one <- nullable 100
            Expect.equal test1.one (nullable 100) ""
            Expect.equal (jtoken.Value<Nullable<int>>("one")) (nullable 100) ""
    ]



[<Tests>]
let nullablePropertyFieldDoesNotExistTests =
    testList "Nullable Field Does Not Exists Property tests" [
        let jsonStr =
                """{
                    "two" : -1000
                    }
                """
        let nullInt = (Nullable<int>())
        let inline nullable x = Nullable<_> x
        testCase "Gets keys supplied by user" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let action () =
                let test1 = NullableMissingFieldSchema(jtoken, looseSerializer)

                Expect.equal test1.one nullInt ""
                Expect.equal test1.two -1000 ""

            Expect.throwsT<Example.MissingJsonFieldException> action ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = NullableMissingFieldSchema(jtoken, looseSerializer)

            test1.one <- nullable 100
            Expect.equal test1.one (nullable 100) ""
            Expect.equal (jtoken.Value<Nullable<int>>("one")) (nullable 100) ""
    ]



[<Tests>]
let optionPropertyTests =
    testList "Option Field  Exists Property tests" [
        let jsonStr =
                """{
                    "one" : null,
                    "two" : -1000
                    }
                """
        let nullInt = (Nullable<int>())
        let inline nullable x = Nullable<_> x
        testCase "Gets keys supplied by user" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = OptionalFieldSchema(jtoken, looseSerializer)

            Expect.equal test1.one None ""
            Expect.equal test1.two -1000 ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = OptionalFieldSchema(jtoken, looseSerializer)

            test1.one <- Some 100
            Expect.equal test1.one (Some 100) ""
            Expect.equal (jtoken.["one"].ToObject<_>(looseSerializer)) (Some 100) ""
    ]
