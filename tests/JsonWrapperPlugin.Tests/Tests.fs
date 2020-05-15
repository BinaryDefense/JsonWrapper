module Tests

open System
open Expecto
open DataSchema
open Newtonsoft.Json.Linq
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

            let test1 = SimpleSchema(jtoken)

            Expect.equal test1.one 42 ""
            Expect.equal test1.two "Hitchhikers Guide" ""
            Expect.equal test1.three (Guid.Parse "f971a6c0-ed00-46e5-b657-3fea2e368ba9") ""
        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = SimpleSchema(jtoken)

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

            let test1 = DifferentBackingFieldSchema(jtoken)

            Expect.equal test1.one 9001 ""
            Expect.equal test1.two -1000 ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = DifferentBackingFieldSchema(jtoken)

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
                    "two" : -1000
                    }
                """
        let nullInt = (Nullable<int>())
        let inline nullable x = Nullable<_> x
        testCase "Gets keys supplied by user" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = NullableFieldSchema(jtoken)

            Expect.equal test1.one nullInt ""
            Expect.equal test1.two -1000 ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = NullableFieldSchema(jtoken)

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
                let test1 = NullableMissingFieldSchema(jtoken)

                Expect.equal test1.one nullInt ""
                Expect.equal test1.two -1000 ""

            Expect.throwsT<Example.MissingJsonFieldException> action ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = NullableMissingFieldSchema(jtoken)

            test1.one <- nullable 100
            Expect.equal test1.one (nullable 100) ""
            Expect.equal (jtoken.Value<Nullable<int>>("one")) (nullable 100) ""
    ]
