module Tests

open System
open Expecto
open DataSchema
open Newtonsoft.Json.Linq
[<Tests>]
let simpleTests =
    testList "simple schema" [
        testCase "Gets keys it knows about" <| fun _ ->
            let jsonStr =
                """{
                    "one": 42,
                    "two": "Hitchhikers Guide",
                    "three": "f971a6c0-ed00-46e5-b657-3fea2e368ba9"
                    }
                """
            let jtoken = JToken.Parse jsonStr

            let test1 = Test1(jtoken)

            Expect.equal test1.one 42 ""
            Expect.equal test1.two "Hitchhikers Guide" ""
            Expect.equal test1.three (Guid.Parse "f971a6c0-ed00-46e5-b657-3fea2e368ba9") ""
    ]

[<Tests>]
let jsonPropertyTests =
    testList "JsonProperty tests" [
        testCase "Gets keys supplied by user" <| fun _ ->
            let jsonStr =
                """{
                    "one": 42,
                    "not_one": 9001,
                    "two" : -1000
                    }
                """
            let jtoken = JToken.Parse jsonStr

            let test1 = Test2(jtoken)

            Expect.equal test1.one 9001 ""
            Expect.equal test1.two -1000 ""
    ]
