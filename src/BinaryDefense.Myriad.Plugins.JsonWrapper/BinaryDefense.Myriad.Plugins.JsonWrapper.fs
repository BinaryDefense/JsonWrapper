namespace Myriad.Plugins

open System
open FSharp.Compiler
open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core
open FSharp.Compiler.Range
open FSharp.Compiler.XmlDoc
open BinaryDefense.JsonWrapper.Core
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Collections.Generic


module Reflection =

    open Microsoft.FSharp.Quotations.Patterns
    /// Gets a statically type property name of an instance
    let rec propertyName quotation =
        match quotation with
        | PropertyGet(_, propertyInfo, _) -> propertyInfo.Name
        | Lambda(_, expr) -> propertyName expr
        | _ -> ""

    /// Gets a statically typed method name of an instance
    let methodName quotation=
        match quotation with
        | Lambda (_, Call (_, mi, _)) -> mi.Name
        | _ -> failwith "%A is not a valid getMethodName expression, expected Lamba(_ Call(_, _, _))"


module DSL =

    let hashDirective (parsedHashDirective : ParsedHashDirective) =
        SynModuleDecl.HashDirective (parsedHashDirective, range0)

    let noWarn args =
        ParsedHashDirective("nowarn", args, range0 )
        |> hashDirective

    /// Creates : open {{namespace}}
    let openNamespace (``namespace`` : LongIdentWithDots) =
        SynModuleDecl.CreateOpen (``namespace``)

    /// Creates a tuple for method signatures
    let createTypleSynType args =
        SynType.Tuple(false, args |> List.map(fun s -> false,s), range0)

    /// Creates : if {{ifCheck}} then {{ifbody}} else {{elseBody}}
    let createIfThenElse ifCheck ifBody elseBody =
        SynExpr.IfThenElse(ifCheck, ifBody, elseBody, DebugPointForBinding.DebugPointAtBinding range0, false, range0, range0)

    /// Creates : let {{leftSide}} = {{rightSide}}
    ///
    /// A more concrete example: let myVar = "something"
    let createLetAssignment leftSide rightSide continuation =
        let emptySynValData = SynValData.SynValData(None, SynValInfo.Empty, None)
        let headPat = SynPat.Named(SynPat.Wild range0, leftSide, false, None, range0)
        let binding = SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, emptySynValData, headPat, None, rightSide, range0, DebugPointForBinding.DebugPointAtBinding range0 )
        SynExpr.LetOrUse(false, false, [binding], continuation, range0)


    let private pipeRightIdent = Ident.Create "op_PipeRight"

    // Creates : {{synExpr1}} |> {{synExpr2}}
    let pipeRight synExpr2 synExpr1 =
        SynExpr.CreateApp(SynExpr.CreateAppInfix(SynExpr.CreateIdent pipeRightIdent, synExpr1), synExpr2)


module SystemObject =
    let getHashCodeMethod =
        Reflection.methodName <@ fun (x : obj) -> x.GetHashCode() @>

    let equalsMethod =
        Reflection.methodName <@ fun (x : obj) -> x.Equals(null) @>

module FSharpCore =
    let raiseIdent = Ident.Create (nameof raise)

    let isNullIdent = Ident.Create (nameof isNull)
    let isNull ident =
        SynExpr.CreateApp(SynExpr.CreateIdent isNullIdent , SynExpr.CreateIdent ident )


module System =
    let ``namespace`` = typeof<DateTimeOffset>.Namespace

module JToken =
    let ``namespace`` = typeof<JToken>.Namespace
    let name = typeof<JToken>.Name
    let nameLongIdent = LongIdentWithDots.CreateString name

    let private toObjectMethod =
        Reflection.methodName <@ fun (x : JToken) -> x.ToObject() @>

    let instanceToObject (instance : Ident) (generic : SynType) (serializer : Ident) =
        let instanceAndMethod =  LongIdentWithDots.Create [instance.idText; toObjectMethod]
        let args =  SynExpr.CreateIdent serializer
        SynExpr.CreateInstanceMethodCall(instanceAndMethod, [generic], args)


    let private fromObjectMethod =
        nameof JToken.FromObject

    let staticFromObject jtoken serializer =
        let instanceAndMethod =  LongIdentWithDots.Create [name; fromObjectMethod]
        let args =
            SynExpr.CreateParenedTuple [
                SynExpr.CreateIdent jtoken
                SynExpr.CreateIdent serializer
            ]
        SynExpr.CreateInstanceMethodCall(instanceAndMethod, args)

    let private getHashCodeMethod =
        Reflection.methodName <@ fun (x : JToken) -> x.GetHashCode() @>

    let instanceGetHashCode (instance : Ident) =
        let instanceAndMethod =  LongIdentWithDots.Create [instance.idText; getHashCodeMethod]
        SynExpr.CreateInstanceMethodCall instanceAndMethod

    let private deepEqualsMethod =
        nameof JToken.DeepEquals

    let staticDeepEquals arg1 arg2 =
        let deepEqualFunc =
            SynExpr.CreateLongIdent (
                LongIdentWithDots.Create [
                    name
                    deepEqualsMethod
                ]
            )
        let deepEqualArgs = SynExpr.CreateParenedTuple [arg1; arg2]
        SynExpr.CreateApp(deepEqualFunc, deepEqualArgs)

module IHaveJToken =

    let name = typeof<IHaveJToken>.Name
    let ``namespace`` = typeof<IHaveJToken>.Namespace

    let innerDataProperty =
        Reflection.propertyName <@fun (x : IHaveJToken) -> x.InnerData @>

    let instanceInnerData (instance : Ident) =
        SynExpr.CreateLongIdent(
            LongIdentWithDots.Create [
                instance.idText
                innerDataProperty
            ]
        )

module MissingJsonFieldException =
    let name = typeof<MissingJsonFieldException>.Name
    let nameLongIdent = LongIdentWithDots.CreateString name

    let ctor fieldName jtoken =
        let func = SynExpr.CreateLongIdent nameLongIdent
        let args = SynExpr.CreateParenedTuple [fieldName; jtoken]
        SynExpr.CreateApp(func, args)

type ModuleTree =
| Module of string * ModuleTree list
| Class of ``namespace``: LongIdent * SynTypeDefn

module ModuleTree =

    let fromExtractRecords  (xs : list<LongIdent * list<_>>) =
        let rec addPath subFilePath parts nodes =
            match parts with
            | [] -> nodes
            | hp :: tp ->
                addHeadPath subFilePath hp tp nodes
        and addHeadPath subFilePath (part : string) remainingParts (nodes : ModuleTree list)=
            match nodes with
            | [] ->
                let classes =
                    if remainingParts |> List.isEmpty then
                        (snd subFilePath) |> List.map(fun c -> Class(fst subFilePath, c))
                    else
                        List.empty
                Module(part, addPath subFilePath remainingParts classes )
                |> List.singleton
            | Module(title, subnodes) :: nodes when title = part -> Module(title, addPath subFilePath remainingParts subnodes ) :: nodes
            | hn :: tn -> hn :: addHeadPath subFilePath part remainingParts tn

        ([], xs)
        ||> List.fold(fun state (moduleIdent, synTypeDefns) ->
            let pathParts = moduleIdent |> List.map(fun i -> i.idText)
            addPath (moduleIdent, synTypeDefns) pathParts state
        )



module internal Create =

    /// Keeps a map of known generated types and their Deconstruct out parameters.  Used in nesting Deconstructs.
    let knownDeconstructs = new List<string>()

    type GetterAccessor =
    /// The backing field can be missing on the JToken, should be used with Nullable or Option types
    | CanBeMissing
    /// The backing field must exist on the JToken, should be used with Nullable or Option types
    | MustExist

    let createWrapperClass (parentNamespace : LongIdent) (parent: LongIdent) (fields: SynField list) =

        let jsonSerializerFullName = typeof<JsonSerializer>.Name
        let jsonSerializerFullNameLongIdent = LongIdentWithDots.CreateString  jsonSerializerFullName

        let info = SynComponentInfoRcd.Create parent
        let jtokenIdenName =  "jtoken"
        let jtokenIdent = Ident.Create jtokenIdenName
        let jsonSerializerName =  "serializer"
        let jsonSerializerNameIdent = Ident.Create jsonSerializerName
        let selfIden = "this"
        let unitArg = SynPatRcd.Const { SynPatConstRcd.Const = SynConst.Unit ; Range = range.Zero }


        let createCtor =
            let jtokenArg = SynSimplePat.CreateTyped(jtokenIdent, (SynType.CreateLongIdent JToken.nameLongIdent))
            let serializerArg = SynSimplePat.CreateTyped(jsonSerializerNameIdent, (SynType.CreateLongIdent jsonSerializerFullNameLongIdent))
            SynMemberDefn.CreateImplicitCtor [jtokenArg; serializerArg]

        let createGetterSynValdatea  =
            let memberFlags : MemberFlags = {
                IsInstance = true
                IsDispatchSlot = false
                IsOverrideOrExplicitImpl = false
                IsFinal = false
                MemberKind = MemberKind.PropertyGet
            }
            SynValData.SynValData(Some memberFlags, SynValInfo.Empty, None)

        let createSetterSynValData () =
            let memberFlags : MemberFlags = {
                IsInstance = true
                IsDispatchSlot = false
                IsOverrideOrExplicitImpl = false
                IsFinal = false
                MemberKind = MemberKind.PropertySet
            }
            SynValData.SynValData(Some memberFlags, SynValInfo.Empty, None)


        let createGetSetMember (fieldName : Ident) (jsonFieldName : string) (fieldTy : SynType) (getAccessor : GetterAccessor) =

            let getMemberExpr =

                let varName = Ident.Create "selectedToken"
                let continuation =
                     //Generates the function call {varName}.ToObject<fieldTy>(serializer)
                    let toObjectCall =
                        JToken.instanceToObject varName fieldTy jsonSerializerNameIdent

                    match getAccessor with
                    | MustExist ->
                        let ifCheck = FSharpCore.isNull varName
                        let ifBody =
                            let createException =
                                let arg1 = SynExpr.CreateConst(SynConst.CreateString(jsonFieldName))
                                let arg2 = SynExpr.CreateIdent jtokenIdent
                                MissingJsonFieldException.ctor arg1 arg2
                            // Generates exception |> raise
                            createException |> DSL.pipeRight (SynExpr.CreateIdent FSharpCore.raiseIdent)
                        // Generates if isNull {varName} then raise exception
                        let existCheck = DSL.createIfThenElse ifCheck ifBody None
                        SynExpr.CreateSequential [
                            existCheck
                            toObjectCall
                        ]

                    | CanBeMissing ->
                        toObjectCall

                //Generates the jtoken.[{jsonFieldName}]
                let idx = [SynIndexerArg.One(SynExpr.CreateConstString jsonFieldName, false, range.Zero )]
                let jTokenAccessor = SynExpr.DotIndexedGet( SynExpr.Ident jtokenIdent, idx, range.Zero, range.Zero )
                // Generates let v = jtoken.[{jsonFieldName}]
                DSL.createLetAssignment varName jTokenAccessor continuation

            let getMember =
                { SynBindingRcd.Null with
                    Kind =  SynBindingKind.NormalBinding
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; fieldName.idText]) , [unitArg])
                    ValData = createGetterSynValdatea
                    ReturnInfo = SynBindingReturnInfoRcd.Create fieldTy |> Some
                    Expr = getMemberExpr
                }

            let argVarName = Ident.Create "newValue"

            let setArg =
                let arg =
                    let named = SynPatRcd.CreateNamed(argVarName, SynPatRcd.CreateWild)
                    SynPatRcd.CreateTyped(named, fieldTy)
                    |> SynPatRcd.CreateParen
                arg

            let setMemberExpr =
                //Generates JToken.FromObject(x, serializer)
                let fromObjectFuncWithArgs = JToken.staticFromObject argVarName jsonSerializerNameIdent
                //Generates the jtoken.["jsonFieldName"] <- Newtonsoft.Json.Linq.JToken.FromObject(x, serializer)
                let idx = [SynIndexerArg.One(SynExpr.CreateConstString jsonFieldName, false, range.Zero )]
                SynExpr.DotIndexedSet( SynExpr.Ident jtokenIdent, idx, fromObjectFuncWithArgs, range.Zero, range.Zero, range0 )

            let setMember =
                { SynBindingRcd.Null with
                    Kind =  SynBindingKind.NormalBinding
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; fieldName.idText]) , [setArg])
                    ValData = createSetterSynValData ()
                    Expr = setMemberExpr
                }

            [ getMember; setMember]
            |> List.map SynMemberDefn.CreateMember

        let createGetSetMembersFromRecord  =
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
                            attrName.Contains (typeof<Attributes.MustExist>.Name)
                        )
                    match attr with
                    | Some _ -> GetterAccessor.MustExist
                    | None -> GetterAccessor.CanBeMissing
                createGetSetMember fieldIdent jsonFieldName frcd.Type getAccessorCreation
            )

        let createOverrideGetHashCode =
            let body = JToken.instanceGetHashCode jtokenIdent

            let memberFlags : MemberFlags = {
                IsInstance = true
                IsDispatchSlot = false
                IsOverrideOrExplicitImpl = true
                IsFinal = false
                MemberKind = MemberKind.Member
            }
            let valData = SynValData.SynValData(Some memberFlags, SynValInfo.Empty, None)
            let getHashCodeMember =
                { SynBindingRcd.Null with
                    Kind =  SynBindingKind.NormalBinding
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; SystemObject.getHashCodeMethod]) , [unitArg])
                    ValData = valData
                    Expr = body
                }
            getHashCodeMember
            |> SynMemberDefn.CreateMember

        let createOverrideEquals =
            let arg1VarName = "objToCompare"
            let arg1VarNameIdent = Ident.Create arg1VarName

            let matchStatement =
                let clause1 =
                    let aliasedName = "jTokenToCompare"
                    let aliasedNameIdent = Ident.Create aliasedName
                    let leftSide =
                        let castedToInteface = SynPat.IsInst(SynType.CreateLongIdent(LongIdentWithDots.CreateString IHaveJToken.name), range0)
                        SynPat.Named (castedToInteface, aliasedNameIdent,false, None, range0)
                    let rightSide =
                        let arg1 = IHaveJToken.instanceInnerData aliasedNameIdent
                        let arg2 = SynExpr.CreateIdent jtokenIdent
                        JToken.staticDeepEquals arg1 arg2
                    SynMatchClause.Clause(leftSide, None, rightSide, range0, DebugPointForTarget.Yes)
                let clause2 =
                    SynMatchClause.Clause(SynPat.Wild range0, None, SynExpr.CreateConst (SynConst.Bool(false)), range0, DebugPointForTarget.Yes)
                let matchCheck = SynExpr.CreateIdent arg1VarNameIdent
                SynExpr.CreateMatch(matchCheck, [clause1; clause2])
            let memberFlags : MemberFlags = {
                IsInstance = true
                IsDispatchSlot = false
                IsOverrideOrExplicitImpl = true
                IsFinal = false
                MemberKind = MemberKind.Member
            }
            let valData = SynValData.SynValData(Some memberFlags, SynValInfo.Empty, None)
            let equalArg =
                let arg =
                    let named = SynPatRcd.CreateNamed(arg1VarNameIdent, SynPatRcd.CreateWild )
                    SynPatRcd.CreateTyped(named, SynType.CreateLongIdent "obj")
                    |> SynPatRcd.CreateParen
                arg
            let equalMember =
                { SynBindingRcd.Null with
                    Kind =  SynBindingKind.NormalBinding
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; SystemObject.equalsMethod]) , [equalArg])
                    ValData = valData
                    Expr = matchStatement
                }
            equalMember
            |> SynMemberDefn.CreateMember

        let createInterfaceImpl =
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
                        Pattern = SynPatRcd.CreateLongIdent (LongIdentWithDots.Create [selfIden; IHaveJToken.innerDataProperty], [])
                        Expr = SynExpr.CreateIdent jtokenIdent
                }

                [ SynMemberDefn.CreateMember(bindingRecord)]
            SynMemberDefn.Interface(SynType.CreateLongIdent(IHaveJToken.name), Some implementedMembers, range0)


        let getDeconstructType fieldTy =
            match fieldTy with
            | SynType.LongIdent ident ->
                match knownDeconstructs |> Seq.tryFind(fun (k : string) -> k.EndsWith ident.AsString)  with
                | Some key -> Some key
                | _ -> None
            | _ -> None

        /// Allows for pattern matching against properties
        let createDeconstruct =
            let deconstructMethodName ="Deconstruct"
            let outField str = sprintf "out%s" str
            let memberArgs =
                let arg argName fieldTy =
                    let named = SynPatRcd.CreateNamed(Ident.Create argName, SynPatRcd.CreateWild )
                    let typ = SynType.CreateApp(SynType.CreateLongIdent "outref", [SynType.Anon(range0)], false )
                    SynPatRcd.CreateTyped(named, typ)


                fields
                |> Seq.map(fun f ->
                    let rcd = f.ToRcd
                    let x = rcd.Id |> Option.get
                    let argRcd = arg (outField x.idText) rcd.Type
                    argRcd
                )
                |> Seq.toList
                |> SynPatRcd.CreateTuple
                |> SynPatRcd.CreateParen
                |> List.singleton
            let createInnerDataMemberVal  =
                    let memberFlags : MemberFlags = {
                        IsInstance = true
                        IsDispatchSlot = false
                        IsOverrideOrExplicitImpl = false
                        IsFinal = false
                        MemberKind = MemberKind.Member
                    }
                    SynValData.SynValData(Some memberFlags, SynValInfo.Empty, None)
            let body =
                fields
                |> List.map(fun f ->
                    let rcd = f.ToRcd
                    let ident = rcd.Id |> Option.get
                    let fieldTy = rcd.Type
                    let fieldName = ident.idText
                    let rightside =
                        match getDeconstructType fieldTy with
                        | Some _ ->
                            LongIdentWithDots.Create [selfIden; fieldName; deconstructMethodName ]
                            |> SynExpr.CreateInstanceMethodCall
                        | None -> SynExpr.CreateLongIdent( LongIdentWithDots.Create [selfIden; fieldName ])

                    SynExpr.LongIdentSet (LongIdentWithDots.CreateString (outField fieldName), rightside, range0 )
                )
                |> SynExpr.CreateSequential

            let bindingRecord = {
                    SynBindingRcd.Null with
                        ValData = createInnerDataMemberVal
                        Pattern = SynPatRcd.CreateLongIdent (LongIdentWithDots.Create [selfIden; deconstructMethodName], memberArgs)
                        Expr = body
                        XmlDoc = PreXmlDoc.Create ["This allows the class to be pattern matched against"]
                }
            let parentName =  parent.Head.idText
            let synExprType =
                fields
                |> List.map(fun f ->
                    let rcd = f.ToRcd
                    rcd.Type
                )
                |> DSL.createTypleSynType

            let fullTypeName = sprintf "%s.%s" (String.Join('.', parentNamespace)) parentName
            knownDeconstructs.Add(fullTypeName)

            SynMemberDefn.CreateMember(bindingRecord)

        let members = [
            createCtor
            yield! createGetSetMembersFromRecord
            createOverrideGetHashCode
            createOverrideEquals
            createDeconstruct
            createInterfaceImpl
        ]

        SynModuleDecl.CreateType(info, members)


    let createJsonWrapperClass (namespaceId: LongIdent) (typeDefn: SynTypeDefn) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->

            let createWrapperClass = createWrapperClass namespaceId  recordId recordFields

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
            let findRecordsByOurMarkerAttribute xs =
                xs
                |> List.map(fun (ident, rcds) ->
                    ident, rcds |> List.filter (Ast.hasAttribute<Generator.JsonWrapperAttribute>)
                )

            let groupRecordsByNamespace xs =
                xs
                |> List.map((fun (ident : LongIdent, records) -> String.Join('.', ident), records))
                |> List.groupBy(fst)
                |> List.map(fun (key, (xs)) ->
                    Ident.CreateLong(key), xs |> List.collect(snd)
                )

            let filterOutEmptyModules xs =
                xs
                |> List.choose(fun (ident,records) ->
                    match records with
                    | [] -> None
                    | _ -> Some(ident,records)
                )

            let namespaceAndrecords =
                Ast.extractRecords ast
                |> findRecordsByOurMarkerAttribute
                |> groupRecordsByNamespace
                |> filterOutEmptyModules

            let moduleTrie =
                namespaceAndrecords
                |> ModuleTree.fromExtractRecords

            let rec createModulesAndClasses (moduleTree) =
                ([], moduleTree)
                ||> List.fold(fun state x ->
                    state @
                        match x with
                        | Module(name, mods) ->
                            [
                                let moduleId = SynComponentInfoRcd.Create (Ident.CreateLong name)
                                let decls = createModulesAndClasses mods
                                SynModuleDecl.CreateNestedModule(moduleId, decls)
                            ]
                        | Class (ns, rcd) ->
                            Create.createJsonWrapperClass ns rcd
                )



            let openNamespaces = [
                DSL.noWarn ["0058"] //TODO: https://github.com/BinaryDefense/JsonWrapper/issues/2
                DSL.openNamespace (LongIdentWithDots.CreateString (System.``namespace``) )
                DSL.openNamespace (LongIdentWithDots.CreateString (JToken.``namespace``) )
                DSL.openNamespace (LongIdentWithDots.CreateString (typeof<JsonSerializer>.Namespace) )
                DSL.openNamespace (LongIdentWithDots.CreateString (IHaveJToken.``namespace``) )
            ]

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = [
                                yield! openNamespaces
                                yield! createModulesAndClasses moduleTrie
                            ] }

            namespaceOrModule

