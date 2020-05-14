namespace Myriad.Plugins

open System
open FSharp.Compiler.Ast
open FsAst
open Myriad.Core

module internal Create =
    open FSharp.Compiler.Range

    let createFieldMap (parent: LongIdent) (field: SynField)  =
        let field = field.ToRcd
        let fieldName = match field.Id with None -> failwith "no field name" | Some f -> f

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let varName = "x"
        let pattern =
            let name = LongIdentWithDots.CreateString fieldName.idText
            let arg =
                let named = SynPatRcd.CreateNamed(Ident.Create varName, SynPatRcd.CreateWild )
                SynPatRcd.CreateTyped(named, recordType)
                |> SynPatRcd.CreateParen

            SynPatRcd.CreateLongIdent(name, [arg])

        let expr =
            let ident = LongIdentWithDots.Create [varName; fieldName.idText]
            SynExpr.CreateLongIdent(false, ident, None)

        let valData =
            let argInfo = SynArgInfo.CreateIdString "x"
            let valInfo = SynValInfo.SynValInfo([[argInfo]], SynArgInfo.Empty)
            SynValData.SynValData(None, valInfo, None)

        SynModuleDecl.CreateLet [{SynBindingRcd.Let with
                                    Pattern = pattern
                                    Expr = expr
                                    ValData = valData }]

    let createCreate (parent: LongIdent) (fields: SynFields) =
        let varIdent = LongIdentWithDots.CreateString "create"

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let arguments =
                fields
                |> List.map (fun f ->let field = f.ToRcd
                                     let name = SynPatRcd.CreateNamed(field.Id.Value, SynPatRcd.CreateWild)
                                     SynPatRcd.CreateTyped(name, field.Type) |> SynPatRcd.CreateParen)

            SynPatRcd.CreateLongIdent(varIdent, arguments)

        let expr =
            let fields =
                fields
                |> List.map (fun f ->   let field = f.ToRcd
                                        let fieldIdent = match field.Id with None -> failwith "no field name" | Some f -> f
                                        let name = LongIdentWithDots.Create([fieldIdent.idText])
                                        let ident = SynExpr.CreateIdent fieldIdent
                                        RecordFieldName(name, true), Some ident, None)

            let newRecord = SynExpr.Record(None, None, fields, range.Zero )
            SynExpr.CreateTyped(newRecord, recordType)

        let returnTypeInfo = SynBindingReturnInfoRcd.Create recordType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]

    let createWrapperClass  (parent: LongIdent) (fields: SynFields) =
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
            SynMemberDefn.ImplicitCtor(None, [], ctorArgs, Some (Ident.Create selfIden), range.Zero )

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
                let valueExpr = SynExpr.DotGet(jTokenAccessor, range0, LongIdentWithDots.CreateString "Value", range0)

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
                createGetSetMember fieldIdent fieldIdent.idText frcd.Type
            )

        let members = [
            createCtor ()
            yield! createGetSetMembersFromRecord ()
        ]

        SynModuleDecl.CreateType(info, members)

    let createMap (recordId: LongIdent) (recordFields: SynFields) : SynModuleDecl =
        let varIdent = LongIdentWithDots.CreateString "map"
        let recordPrimeIdent =  Ident.Create "record'"

        let createFieldMapNameIdent field =
            Ident.Create ("map" + field.Id.Value.idText)

        let pattern =
            let arguments =
                recordFields
                |> List.map (fun f ->let field = f.ToRcd
                                     let fieldType = field.Type
                                     let funType = SynType.Fun(fieldType, fieldType, range0 )
                                     let ident = createFieldMapNameIdent field
                                     let name = SynPatRcd.CreateNamed(ident, SynPatRcd.CreateWild)
                                     SynPatRcd.CreateTyped(name, funType)
                                     |> SynPatRcd.CreateParen)

            let recordParam =
                let name = SynPatRcd.CreateNamed(recordPrimeIdent, SynPatRcd.CreateWild)
                let typ =
                    LongIdentWithDots.Create (recordId |> List.map (fun i -> i.idText))
                    |> SynType.CreateLongIdent

                SynPatRcd.CreateTyped(name, typ)
                |> SynPatRcd.CreateParen

            let allArgs = [yield! arguments; yield recordParam]

            SynPatRcd.CreateLongIdent(varIdent, allArgs)

        let expr =
            let copyInfo =
                let blockSep = (range.Zero, None) : BlockSeparator
                Some (SynExpr.CreateIdent recordPrimeIdent, blockSep)

            let fieldUpdates =
                let mapField (f: SynField) =
                    let f = f.ToRcd
                    let lid = LongIdentWithDots.Create [f.Id.Value.idText]
                    let rfn = RecordFieldName(lid, true)

                    let update =
                        let funcExpr = SynExpr.CreateIdent (createFieldMapNameIdent f)
                        let argExpr =
                            let longIdentWithDots = LongIdentWithDots.Create [recordPrimeIdent.idText; f.Id.Value.idText]
                            SynExpr.CreateLongIdent(false, longIdentWithDots, None)
                        SynExpr.CreateApp(funcExpr, argExpr)

                    rfn, Some update, (None : Option<BlockSeparator>)

                let arguments =
                    recordFields
                    |> List.map mapField

                arguments

            SynExpr.Record(None, copyInfo, fieldUpdates, range.Zero )

        let returnTypeInfo =
            LongIdentWithDots.Create (recordId |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent
            |> SynBindingReturnInfoRcd.Create

        SynModuleDecl.CreateLet
            [ { SynBindingRcd.Let with
                    Pattern = pattern
                    Expr = expr
                    ReturnInfo = Some returnTypeInfo }
            ]

    let createRecordModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->

            let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText)))

            let fieldMaps = recordFields |> List.map (createFieldMap recordId)

            let create = createCreate recordId recordFields

            let map = createMap recordId recordFields

            let createWrapperClass = createWrapperClass recordId recordFields

            let declarations = [
                // yield openParent
                // yield!fieldMaps
                // yield create
                // yield map
                createWrapperClass
            ]

            // let info = SynComponentInfoRcd.Create recordId
            // SynModuleDecl.CreateNestedModule(info, declarations)
            declarations
        | _ -> failwithf "Not a record type"


[<RequireQualifiedAccess>]
module Generator =
    type Fields2Attribute() =
        inherit Attribute()

[<MyriadGenerator("fields2")>]
type Fields2Generator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', ast: ParsedInput) =
            let namespaceAndrecords = Ast.extractRecords ast
            let modules =
                namespaceAndrecords
                |> List.collect (fun (ns, records) ->
                                    records
                                    |> List.filter (Ast.hasAttribute<Generator.Fields2Attribute>)
                                    |> List.collect (Create.createRecordModule ns))

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = modules }

            namespaceOrModule

