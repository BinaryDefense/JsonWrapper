namespace Myriad.Plugins

open System
open FSharp.Compiler.Ast
open FsAst
open Myriad.Core
open FSharp.Compiler.Range


module DSL =

    let createLet leftSide rightSide continuation =
        let emptySynValData = SynValData.SynValData(None, SynValInfo.Empty, None)
        let headPat = SynPat.Named(SynPat.Wild range0, leftSide, false, None, range0)
        let binding = SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, emptySynValData, headPat, None, rightSide, range0, SequencePointInfoForBinding.NoSequencePointAtLetBinding )
        SynExpr.LetOrUse(false, false, [binding], continuation, range0)



module internal Create =

    type GetterAccessor =
    /// The backing field can be missing on the JToken, should be used with Nullable or Option types
    | CanBeMissing
    /// The backing field must exist on the JToken, should be used with Nullable or Option types
    | MustExist

    let createWrapperClass  (parent: LongIdent) (fields: SynFields) (jtokenInterface : string) =
        let jtokenNamespace = "Newtonsoft.Json.Linq.JToken"
        let missingJsonFieldException = "Example.MissingJsonFieldException"
        let missingJsonFieldExceptionIdent = LongIdentWithDots.CreateString missingJsonFieldException
        let info = SynComponentInfoRcd.Create parent
        let jtokenIdenName =  "jtoken"
        let jtokenIdent = Ident.Create jtokenIdenName
        let selfIden = "this"

        let pipeRightIdent = Ident.Create "op_PipeRight"

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


        let createGetSetMember (fieldName : Ident) (jsonFieldName : string) (ty : SynType) (getAccessor : GetterAccessor) =
            let unitArg = SynPatRcd.Const { SynPatConstRcd.Const = SynConst.Unit ; Range = range.Zero }

            let getMemberExpr =

                let varName = "v"
                let continuation =
                    let toObjectCall =
                        //Generates the {jtoken}.ToObject
                        let valueExpr = SynExpr.CreateLongIdent(false, LongIdentWithDots.CreateString (sprintf "%s.ToObject" varName), None)
                        //Generates the Generic part of {jtoken}.ToObject<mytype>
                        let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, [ty], [], None, range0, range0 )
                        //Generates the function call {jtoken}.ToObject<mytype>()
                        SynExpr.CreateApp(valueExprWithType, SynExpr.CreateConst SynConst.Unit)
                    match getAccessor with
                    | MustExist ->
                        let ifCheck = SynExpr.CreateApp(SynExpr.CreateIdentString "isNull", SynExpr.CreateIdentString varName )
                        let ifBody =
                            let createException =
                                let func = SynExpr.CreateLongIdent(false, missingJsonFieldExceptionIdent, None )
                                let args =
                                    let arg1 = SynExpr.CreateConst(SynConst.CreateString(jsonFieldName))
                                    let arg2 = SynExpr.CreateIdent jtokenIdent
                                    SynExpr.Tuple(false, [arg1; arg2], [], range0)
                                    |> SynExpr.CreateParen
                                SynExpr.CreateApp(func, args)
                            SynExpr.CreateApp(SynExpr.CreateAppInfix(SynExpr.CreateIdent pipeRightIdent, createException), SynExpr.CreateIdentString "raise")
                        let existCheck = SynExpr.IfThenElse(ifCheck, ifBody, None, SequencePointInfoForBinding.NoSequencePointAtLetBinding, false, range0, range0)
                        SynExpr.Sequential(SequencePointInfoForSeq.SequencePointsAtSeq, false, existCheck, toObjectCall, range0)

                    | CanBeMissing ->
                        toObjectCall

                let vIdent = Ident.Create varName
                //Generates the jtoken.[{jsonFieldName}]
                let idx = [SynIndexerArg.One(SynExpr.CreateConstString jsonFieldName, false, range.Zero )]
                let jTokenAccessor = SynExpr.DotIndexedGet( SynExpr.Ident jtokenIdent, idx, range.Zero, range.Zero )
                // Generates let v = jtoken.[{jsonFieldName}]
                DSL.createLet vIdent jTokenAccessor continuation

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


                let getAccessorCreation =
                    // Find the Attribute.MustExist attribute
                    let attr =
                        frcd.Attributes
                        |> Seq.collect (fun a -> a.Attributes)
                        |> Seq.tryFind(fun a ->
                            let attrName = a.TypeName.AsString
                            attrName.Contains "Attribute.MustExist"
                        )
                    match attr with
                    | Some _ -> GetterAccessor.MustExist
                    | None -> GetterAccessor.CanBeMissing
                createGetSetMember fieldIdent jsonFieldName frcd.Type getAccessorCreation
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

