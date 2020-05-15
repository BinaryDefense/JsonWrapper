namespace Myriad.Plugins

open System
open FSharp.Compiler.Ast
open FsAst
open Myriad.Core

module internal Create =
    open FSharp.Compiler.Range

    let createWrapperClass  (parent: LongIdent) (fields: SynFields) (jtokenInterface : string) =
        let jtokenNamespace = "Newtonsoft.Json.Linq.JToken"
        let info = SynComponentInfoRcd.Create parent
        let jtokenIdenName =  "jtoken"
        let jtokenIdent = Ident.Create jtokenIdenName
        let selfIden = "this"
        let createCtor () =
            let ctorArgs =
                let lol =
                    let ssp = SynSimplePat.Id(jtokenIdent, None, false, false, false, range.Zero)
                    SynSimplePat.Typed(ssp, SynType.CreateLongIdent(LongIdentWithDots.CreateString jtokenNamespace), range.Zero )
                SynSimplePats.SimplePats ([lol], range.Zero)
            SynMemberDefn.ImplicitCtor(None, [], ctorArgs, None, range.Zero )

        let createGetter () =
            let memberFlags : MemberFlags = {
                IsInstance = true
                IsDispatchSlot = false
                IsOverrideOrExplicitImpl = false
                IsFinal = false
                MemberKind = MemberKind.PropertyGet
            }
            SynValData.SynValData(Some memberFlags, SynValInfo.Empty, None)

        let createSetter () =
            let memberFlags : MemberFlags = {
                IsInstance = true
                IsDispatchSlot = false
                IsOverrideOrExplicitImpl = false
                IsFinal = false
                MemberKind = MemberKind.PropertySet
            }
            SynValData.SynValData(Some memberFlags, SynValInfo.Empty, None)


        let createGetSetMember (fieldName : Ident) (jsonFieldName : string) (ty : SynType) =
            let unitArg = SynPatRcd.Const { SynPatConstRcd.Const = SynConst.Unit ; Range = range.Zero }

            let getMemberExpr =
                //Generates the jtoken.["jsonFieldName"]
                let idx = [SynIndexerArg.One(SynExpr.CreateConstString jsonFieldName, false, range.Zero )]
                let jTokenAccessor = SynExpr.DotIndexedGet( SynExpr.Ident jtokenIdent, idx, range.Zero, range.Zero )

                //Generates the {jtoken}.Value
                let valueExpr = SynExpr.DotGet(jTokenAccessor, range0, LongIdentWithDots.CreateString "ToObject", range0)

                //Generates the Generic part of {jtoken}.Value<mytype>
                let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, [ty], [], None, range0, range0 )
                //Generates the function call {jtoken}.Value<mytype>()
                SynExpr.CreateApp(valueExprWithType, SynExpr.CreateConst SynConst.Unit)

            let getMember =
                { SynBindingRcd.Null with
                    Kind =  SynBindingKind.NormalBinding
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; fieldName.idText]) , [unitArg])
                    ValData = createGetter ()
                    ReturnInfo = SynBindingReturnInfoRcd.Create ty |> Some
                    Expr = getMemberExpr
                }



            let argVarName = "x"

            let setArg =
                let arg =
                    let named = SynPatRcd.CreateNamed(Ident.Create argVarName, SynPatRcd.CreateWild )
                    SynPatRcd.CreateTyped(named, ty)
                    |> SynPatRcd.CreateParen
                arg

            let setMemberExpr =
                //Generates Newtonsoft.Json.Linq.JToken.op_Implicit function
                let jtokenOpImplicit = SynExpr.CreateLongIdent(false, LongIdentWithDots.CreateString(sprintf "%s.op_Implicit" jtokenNamespace),None )
                //Generates Newtonsoft.Json.Linq.JToken.op_Implicit x
                let argVarExpr = SynExpr.CreateApp(jtokenOpImplicit, SynExpr.CreateIdentString argVarName)
                //Generates the jtoken.["jsonFieldName"] <- {argVarExpr}
                let idx = [SynIndexerArg.One(SynExpr.CreateConstString jsonFieldName, false, range.Zero )]
                SynExpr.DotIndexedSet( SynExpr.Ident jtokenIdent, idx,argVarExpr, range.Zero, range.Zero, range0 )

            let setMember =
                { SynBindingRcd.Null with
                    Kind =  SynBindingKind.NormalBinding
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; fieldName.idText]) , [setArg])
                    ValData = createSetter ()
                    Expr = setMemberExpr
                }

            [ getMember; setMember]
            |> List.map SynMemberDefn.CreateMember

        let createGetSetMembersFromRecord () =
            fields
            |> List.collect(fun f ->
                let frcd = f.ToRcd
                let fieldIdent = match frcd.Id with None -> failwith "no field name" | Some f -> f

                let jsonFieldName =
                    // Find the JsonProperty attribute
                    let attr =
                        frcd.Attributes
                        |> Seq.collect (fun a -> a.Attributes)
                        |> Seq.tryFind(fun a ->
                            let attrName = a.TypeName.AsString
                            attrName.Contains "JsonProperty"
                        )
                    match attr |> Option.map (fun a -> a.ArgExpr) with
                    | Some (SynExpr.Paren(SynExpr.Const(SynConst.String(suppliedJsonFieldName, _), _), _ , _, _)) -> suppliedJsonFieldName
                    | _ -> fieldIdent.idText
                createGetSetMember fieldIdent jsonFieldName frcd.Type
            )

        let createInterfaceImpl (jtokenInterface : string) =
            let implementedMembers =
                let createInnerDataMemberVal  =
                    let memberFlags : MemberFlags = {
                        IsInstance = true
                        IsDispatchSlot = false
                        IsOverrideOrExplicitImpl = true
                        IsFinal = false
                        MemberKind = MemberKind.Member
                    }
                    SynValData.SynValData(Some memberFlags, SynValInfo.Empty, None)
                let bindingRecord = {
                    SynBindingRcd.Null with
                        ValData = createInnerDataMemberVal
                        Pattern = SynPatRcd.CreateLongIdent (LongIdentWithDots.Create [selfIden; "InnerData"], [])
                        Expr = SynExpr.CreateIdent jtokenIdent
                }

                [ SynMemberDefn.CreateMember(bindingRecord)]
            SynMemberDefn.Interface(SynType.CreateLongIdent(jtokenInterface), Some implementedMembers, range0)

        let members = [
            createCtor ()
            yield! createGetSetMembersFromRecord ()
            createInterfaceImpl jtokenInterface
        ]

        SynModuleDecl.CreateType(info, members)


    let createRecordModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        let jtokenInferface = "Example.IHaveJToken" //TODO: Scan and get fully qualified
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->

            let createWrapperClass = createWrapperClass recordId recordFields jtokenInferface

            let declarations = [
                createWrapperClass
            ]

            declarations
        | _ -> failwithf "Not a record type"


[<RequireQualifiedAccess>]
module Generator =
    type JsonWrapperAttribute() =
        inherit Attribute()

[<MyriadGenerator("TheAngryByrd.jsonwrapper")>]
type JsonWrapperGenerator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', ast: ParsedInput) =
            let namespaceAndrecords = Ast.extractRecords ast
            let modules =
                namespaceAndrecords
                |> List.collect (fun (ns, records) ->
                                    records
                                    |> List.filter (Ast.hasAttribute<Generator.JsonWrapperAttribute>)
                                    |> List.collect (Create.createRecordModule ns))

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = modules }

            namespaceOrModule

