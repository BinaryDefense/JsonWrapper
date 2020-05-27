namespace Example
open System
open Myriad.Plugins
open Newtonsoft.Json.Linq
open  Newtonsoft.Json
open BinaryDefense.JsonWrapper.Core

module consts =
    let [<Literal>] not_one = "not_one"

[<Generator.JsonWrapper>]
type SimpleSchema = {
    one: int
    two: string
    three : System.Guid
}

[<Generator.JsonWrapper>]
type DifferentBackingFieldSchema = {
    // [<JsonProperty(consts.not_one)>]
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

module Nested =
    module OneThing =
        [<Generator.JsonWrapper>]
        type Data = {
            Foo : string
        }
        [<Generator.JsonWrapper>]
        type Data2 = {
            Foo : string
            Another : Data
        }
    module TwoThing =
        [<Generator.JsonWrapper>]
        type Data = {
            Fee : int
        }
        [<Generator.JsonWrapper>]
        type Data2 = {
            Bar : string
            Another : OneThing.Data
            Another22 : Data
        }
