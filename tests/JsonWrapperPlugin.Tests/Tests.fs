module Tests

open System
open Expecto
open DataSchema
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open BinaryDefense.JsonWrapper.Core
open Myriad.Plugins
open DataSchema.Example

let scrubDefaultDUConverter (s: System.Collections.Generic.IList<JsonConverter>) =
    s
    |> Seq.tryFind (fun c -> c.GetType () = typeof<Converters.DiscriminatedUnionConverter>)
    |> Option.iter (s.Remove >> ignore)

// Serializer Settings
let converters = Converters.recommendedConverters

let serializationSettings =
    let s = JsonSerializerSettings()
    scrubDefaultDUConverter s.Converters
    for c in converters do s.Converters.Add c
    s

let jsonSettings = serializationSettings
let jsonSerializer =JsonSerializer.CreateDefault jsonSettings


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

            let test1 = SimpleSchema(jtoken, jsonSerializer)

            Expect.equal test1.one 42 ""
            Expect.equal test1.two "Hitchhikers Guide" ""
            Expect.equal test1.three (Guid.Parse "f971a6c0-ed00-46e5-b657-3fea2e368ba9") ""
        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = SimpleSchema(jtoken, jsonSerializer)

            Expect.equal test1.one 42 ""
            test1.one <- 100
            Expect.equal test1.one 100 ""
            Expect.equal (jtoken.Value<int>("one")) 100 ""

        testCase "Deconstruct" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = SimpleSchema(jtoken, jsonSerializer)
            let foo = System.Guid.Parse("f971a6c0-ed00-46e5-b657-3fea2e368ba9")
            match test1.Deconstruct() with
            | (42, "Hitchhikers Guide", foo) -> ()
            | fallthru -> failwithf "Couldn't match %A" fallthru
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

            let test1 = DifferentBackingFieldSchema(jtoken, jsonSerializer)

            Expect.equal test1.one 9001 ""
            Expect.equal test1.two -1000 ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = DifferentBackingFieldSchema(jtoken, jsonSerializer)

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

            let test1 = NullableFieldSchema(jtoken, jsonSerializer)

            Expect.equal test1.one nullInt ""
            Expect.equal test1.two null ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = NullableFieldSchema(jtoken, jsonSerializer)

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
                let test1 = NullableMissingFieldSchema(jtoken, jsonSerializer)

                Expect.equal test1.one nullInt ""
                Expect.equal test1.two -1000 ""

            Expect.throwsT<MissingJsonFieldException> action ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = NullableMissingFieldSchema(jtoken, jsonSerializer)

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

            let test1 = OptionalFieldSchema(jtoken, jsonSerializer)

            Expect.equal test1.one None ""
            Expect.equal test1.two -1000 ""

        testCase "Set keys works" <| fun _ ->
            let jtoken = JToken.Parse jsonStr

            let test1 = OptionalFieldSchema(jtoken, jsonSerializer)

            test1.one <- Some 100
            Expect.equal test1.one (Some 100) ""
            Expect.equal (jtoken.["one"].ToObject<_>(jsonSerializer)) (Some 100) ""
    ]



[<Tests>]
let traversalTests =
    testList "traversal tests" [
        let innerJsonStr =
            """ {
                "one" : null,
                "two" : "-1000"
                }
            """
        let jsonStr =
                sprintf """{
                    "foo" : %s,
                    "count" : 10
                }
                """ innerJsonStr
        testCase "Gets keys supplied by user" <| fun _ ->
            let outerJToken = JToken.Parse jsonStr

            let outer = OuterType(outerJToken, jsonSerializer)

            let innerJToken = JToken.Parse innerJsonStr
            let innerExpected = InnerType(innerJToken, jsonSerializer)
            let innerActual = outer.foo

            Expect.equal innerActual.one innerExpected.one ""
            Expect.equal innerActual.two innerExpected.two ""

            Expect.equal innerActual innerExpected ""

        testCase "Deconstruct" <| fun _ ->
            let outerJToken = JToken.Parse jsonStr

            let outer = OuterType(outerJToken, jsonSerializer)
            match outer.Deconstruct() with
            | ((None,"-1000"), 10) -> ()
            | fallthru -> failwithf "Couldn't match %A" fallthru
    ]


open FSharp.Compiler.SyntaxTree
open FsAst

let rec flatten (modules : ModuleTree list) =
    modules
    |> List.collect(fun m ->
        [
            yield m
            yield!
                match m with
                | Module(_, ms) ->
                    flatten ms
                | Class _ ->
                    []
        ]
    )


// search tags: debug, debugger, attach
module Debugging =
  let waitForDebuggerAttached (programName) =
#if DEBUG
    if not(System.Diagnostics.Debugger.IsAttached) then
      printfn "Please attach a debugger for %s, PID: %d" programName (System.Diagnostics.Process.GetCurrentProcess().Id)
    while not(System.Diagnostics.Debugger.IsAttached) do
      System.Threading.Thread.Sleep(100)
    System.Diagnostics.Debugger.Break()
#else
    ()
#endif


let rec filter predicate (modules : ModuleTree list) =
    modules
    |> flatten
    |> List.filter(predicate)

let filterModuleByName name (modules : ModuleTree list)  =
    let predicate ``module`` =
        match ``module`` with
        | Module(s, _) -> s = name
        | Class _ -> false
    filter predicate modules
    |> Seq.tryHead



// let createSynTypeDefn () =
//     SynComponentInfo.ComponentInfo()
//     SynTypeDefn.TypeDefn

// [<Tests>]
// let moduleTreeTests =
//     ftestList "ModuleTree tests" [
//         testCase "test 1" <| fun () ->
//             let input : list<LongIdent * list<_>>= [
//                     (Ident.CreateLong "Nested.OneThing"),
//                     [
//                         "LOL"
//                     ]
//                 ]
//             let expected = [
//                 ModuleTree.Module("Nested",[
//                     ModuleTree.Module("OneThing", [
//                         ModuleTree.Class ""
//                     ])
//                 ])
//             ]

//             let actual = ModuleTree.fromExtractRecords input
//             Expect.equal actual expected ""

//         testCase "test 2" <| fun () ->
//             let input : list<LongIdent * list<_>>= [
//                     (Ident.CreateLong "Nested.OneThing"), ["lol"]
//                     (Ident.CreateLong "Nested.TwoThing"), ["nope"]
//                 ]
//             let expected = [
//                 ModuleTree.Module("Nested",[
//                     ModuleTree.Module("OneThing", [
//                         ModuleTree.Class ""
//                     ])
//                     ModuleTree.Module("TwoThing", [
//                         ModuleTree.Class ""
//                     ])
//                 ])
//             ]
//             // Debugging.waitForDebuggerAttached "test2"
//             let actual = ModuleTree.fromExtractRecords input
//             Expect.equal actual expected ""
//     ]
