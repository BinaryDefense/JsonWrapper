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

module DSL =

    let unitSynExpr = SynExpr.CreateConst SynConst.Unit

    open Microsoft.FSharp.Quotations.Patterns
    let rec propertyName quotation =
        match quotation with
        | PropertyGet(_, propertyInfo, _) -> propertyInfo.Name
        | Lambda(_, expr) -> propertyName expr
        | _ -> ""

    let methodName (e) =
      match e with
      | Lambda (_, Call (_, mi, _)) -> mi.Name
      | _ -> failwith "%A is not a valid getMethodName expression, expected Lamba(_ Call(_, _, _))"

    let openNamespace (``namespace`` : LongIdentWithDots) =
        SynModuleDecl.CreateOpen (``namespace``)

    let createTypleSynType args =
        SynType.Tuple(false, args |> List.map(fun s -> false,s), range0)

    /// Creates : arg1, arg2... argN
    let createTuple args =
        SynExpr.Tuple(false, args, [], range0)


    /// Creates : (arg1, arg2... argN0
    let createParenedTuple args =
        createTuple args
        |> SynExpr.CreateParen

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

    /// Creates type MyClass({{ctorArgs}})
    let createCtor (ctorArgs : SynSimplePat list) =
        let ctorArgs = SynSimplePats.SimplePats(ctorArgs, range0)
        SynMemberDefn.ImplicitCtor(None, [], ctorArgs, None, range.Zero )

    /// Creates {{ident}} : {{ty}}
    ///
    /// A more concrete example : jtoken : JToken
    let createTypedCtorArg ident ``type`` =
        let ssp = SynSimplePat.Id(ident, None, false, false, false, range.Zero)
        SynSimplePat.Typed(ssp, ``type``, range.Zero )

    let createInstanceMethodCall instanceAndMethod args =
        let valueExpr = SynExpr.CreateLongIdent(false, instanceAndMethod, None)
        SynExpr.CreateApp(valueExpr, args)

    let createInstanceMethodCallUnit instanceAndMethod =
        createInstanceMethodCall instanceAndMethod unitSynExpr

    /// Creates {{instanceAndMethod}}<{{types}}>({{args}})
    ///
    /// A more concrete example would be jtoken.ToObject<int>(serializer)
    let createGenericInstanceMethodCall instanceAndMethod types args =
        //Generates the methodCall {jtoken}.ToObject
        let valueExpr = SynExpr.CreateLongIdent(false, instanceAndMethod, None)
        //Generates the Generic part of {jtoken}.ToObject<mytype>
        let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, types, [], None, range0, range0 )
        //Generates the function call {jtoken}.ToObject<mytype>(serializer)
        SynExpr.CreateApp(valueExprWithType, args)


    let pipeRightIdent = Ident.Create "op_PipeRight"

    // Creates : {{synExpr1}} |> {{synExpr2}}
    let pipeRight synExpr2 synExpr1 =
        SynExpr.CreateApp(SynExpr.CreateAppInfix(SynExpr.CreateIdent pipeRightIdent, synExpr1), synExpr2)

    /// Creates: expr1; expr2; ... exprN
    let sequentialExpressions exprs =
        let seqExpr expr1 expr2 = SynExpr.Sequential(DebugPointAtSequential.Both, false, expr1, expr2, range0)
        let rec inner exprs state =
            match state, exprs with
            | None, [] -> SynExpr.CreateConst SynConst.Unit
            | Some expr, [] -> expr
            | None, [single] -> single
            | None, [one;two] -> seqExpr one two
            | Some exp, [single] -> seqExpr exp single
            | None, head::shoulders::tail ->
                seqExpr head shoulders
                |> Some
                |> inner tail
            | Some expr, head::tail ->
                seqExpr expr head
                |> Some
                |> inner tail
        inner exprs None

module JToken =
    let fullName =
        typeof<Newtonsoft.Json.Linq.JToken>.Name
    let fullNameLongIdent = LongIdentWithDots.CreateString fullName

    let private toObjectMethod =
        DSL.methodName <@ fun (x : Newtonsoft.Json.Linq.JToken) -> x.ToObject() @>

    let instanceToObject (instance : string) (generic : SynType) (serializer : Ident)=
        let instanceAndMethod =  LongIdentWithDots.CreateString (sprintf "%s.%s" instance toObjectMethod)
        let args =  SynExpr.CreateIdent serializer
        DSL.createGenericInstanceMethodCall instanceAndMethod [generic] args

module internal Create =

    let knownDeconstructs = new Dictionary<_,_>()

    type GetterAccessor =
    /// The backing field can be missing on the JToken, should be used with Nullable or Option types
    | CanBeMissing
    /// The backing field must exist on the JToken, should be used with Nullable or Option types
    | MustExist

    let createWrapperClass  (parent: LongIdent) (fields: SynField list) (jtokenInterface : string) =

        let jsonSerializerFullName = typeof<JsonSerializer>.Name
        let jsonSerializerFullNameLongIdent = LongIdentWithDots.CreateString  jsonSerializerFullName
        let missingJsonFieldException = typeof<MissingJsonFieldException>.Name
        let missingJsonFieldExceptionIdent = LongIdentWithDots.CreateString missingJsonFieldException
        let info = SynComponentInfoRcd.Create parent
        let jtokenIdenName =  "jtoken"
        let jtokenIdent = Ident.Create jtokenIdenName
        let jsonSerializerName =  "serializer"
        let jsonSerializerNameIdent = Ident.Create jsonSerializerName
        let selfIden = "this"
        let unitArg = SynPatRcd.Const { SynPatConstRcd.Const = SynConst.Unit ; Range = range.Zero }


        let createCtor () =
            let arg1 = DSL.createTypedCtorArg jtokenIdent (SynType.CreateLongIdent JToken.fullNameLongIdent)
            let arg2 = DSL.createTypedCtorArg jsonSerializerNameIdent (SynType.CreateLongIdent jsonSerializerFullNameLongIdent)
            DSL.createCtor [arg1; arg2;]

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


        let createGetSetMember (fieldName : Ident) (jsonFieldName : string) (fieldTy : SynType) (getAccessor : GetterAccessor) =

            let getMemberExpr =

                let varName = "selectedToken"
                let continuation =
                     //Generates the function call {jtoken}.ToObject<mytype>(serializer)
                    let toObjectCall =
                        JToken.instanceToObject varName fieldTy jsonSerializerNameIdent
                        // let instanceAndMethod =  LongIdentWithDots.CreateString (sprintf "%s.ToObject" varName)
                        // let args =  SynExpr.CreateIdent jsonSerializerNameIdent
                        // DSL.createGenericInstanceMethodCall instanceAndMethod [fieldTy] args

                    match getAccessor with
                    | MustExist ->
                        let ifCheck = SynExpr.CreateApp(SynExpr.CreateIdentString "isNull", SynExpr.CreateIdentString varName )
                        let ifBody =
                            let createException =
                                let func = SynExpr.CreateLongIdent(false, missingJsonFieldExceptionIdent, None )
                                let args =
                                    let arg1 = SynExpr.CreateConst(SynConst.CreateString(jsonFieldName))
                                    let arg2 = SynExpr.CreateIdent jtokenIdent
                                    DSL.createParenedTuple [arg1; arg2]
                                SynExpr.CreateApp(func, args)
                            createException |> DSL.pipeRight (SynExpr.CreateIdentString "raise")
                        let existCheck = DSL.createIfThenElse ifCheck ifBody None
                        DSL.sequentialExpressions [
                            existCheck
                            toObjectCall
                        ]

                    | CanBeMissing ->
                        toObjectCall

                let vIdent = Ident.Create varName
                //Generates the jtoken.[{jsonFieldName}]
                let idx = [SynIndexerArg.One(SynExpr.CreateConstString jsonFieldName, false, range.Zero )]
                let jTokenAccessor = SynExpr.DotIndexedGet( SynExpr.Ident jtokenIdent, idx, range.Zero, range.Zero )
                // Generates let v = jtoken.[{jsonFieldName}]
                DSL.createLetAssignment vIdent jTokenAccessor continuation

            let getMember =
                { SynBindingRcd.Null with
                    Kind =  SynBindingKind.NormalBinding
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; fieldName.idText]) , [unitArg])
                    ValData = createGetter ()
                    ReturnInfo = SynBindingReturnInfoRcd.Create fieldTy |> Some
                    Expr = getMemberExpr
                }

            let argVarName = "newValue"

            let setArg =
                let arg =
                    let named = SynPatRcd.CreateNamed(Ident.Create argVarName, SynPatRcd.CreateWild )
                    SynPatRcd.CreateTyped(named, fieldTy)
                    |> SynPatRcd.CreateParen
                arg

            let setMemberExpr =
                //Generates Newtonsoft.Json.Linq.JToken.FromObject function
                let fromObjectFunc =
                    SynExpr.CreateLongIdent(false, LongIdentWithDots.CreateString(sprintf "%s.FromObject" JToken.fullName),None )
                // Generates (x,serializer)
                let fromObjectArgs =
                    let arg1 = SynExpr.CreateIdentString argVarName
                    let arg2 = SynExpr.CreateIdent jsonSerializerNameIdent
                    SynExpr.CreateTuple([arg1; arg2])
                    |> SynExpr.CreateParen
                //Generates Newtonsoft.Json.Linq.JToken.FromObject(x, serializer)
                let fromObjectFuncWithArgs = SynExpr.CreateApp(fromObjectFunc, fromObjectArgs)
                //Generates the jtoken.["jsonFieldName"] <- Newtonsoft.Json.Linq.JToken.FromObject(x, serializer)
                let idx = [SynIndexerArg.One(SynExpr.CreateConstString jsonFieldName, false, range.Zero )]
                SynExpr.DotIndexedSet( SynExpr.Ident jtokenIdent, idx, fromObjectFuncWithArgs, range.Zero, range.Zero, range0 )

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
                            attrName.Contains (typeof<Attributes.MustExist>.Name)
                        )
                    match attr with
                    | Some _ -> GetterAccessor.MustExist
                    | None -> GetterAccessor.CanBeMissing
                createGetSetMember fieldIdent jsonFieldName frcd.Type getAccessorCreation
            )

        let createOverrideGetHashCode =
            let body =
                let instanceAndCall = LongIdentWithDots.Create [jtokenIdenName.ToString(); "GetHashCode"]
                DSL.createInstanceMethodCall instanceAndCall (SynExpr.CreateConst SynConst.Unit)

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
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; "GetHashCode"]) , [unitArg])
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
                        let castedToInteface = SynPat.IsInst(SynType.CreateLongIdent(LongIdentWithDots.CreateString jtokenInterface), range0)
                        SynPat.Named (castedToInteface, aliasedNameIdent,false, None, range0)
                    let rightSide =
                        let deepEqualFunc = SynExpr.CreateLongIdent(false, LongIdentWithDots.CreateString (sprintf "%s.DeepEquals" JToken.fullName), None)
                        let deepEqualArgs =
                            let arg1 = SynExpr.CreateLongIdent(false, LongIdentWithDots.CreateString(sprintf "%s.InnerData" aliasedName), None )
                            let arg2 = SynExpr.CreateIdentString jtokenIdenName
                            SynExpr.CreateTuple([arg1; arg2])
                            |> SynExpr.CreateParen
                        SynExpr.CreateApp(deepEqualFunc, deepEqualArgs)
                    SynMatchClause.Clause(leftSide, None, rightSide, range0, DebugPointForTarget.Yes)
                let clause2 =
                    SynMatchClause.Clause(SynPat.Wild range0, None, SynExpr.CreateConst (SynConst.Bool(false)), range0, DebugPointForTarget.Yes)
                SynExpr.Match(DebugPointForBinding.DebugPointAtBinding range0, SynExpr.CreateIdent arg1VarNameIdent, [clause1; clause2], range0)
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
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ([selfIden; "Equals"]) , [equalArg])
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
                        Pattern = SynPatRcd.CreateLongIdent (LongIdentWithDots.Create [selfIden; "InnerData"], [])
                        Expr = SynExpr.CreateIdent jtokenIdent
                }

                [ SynMemberDefn.CreateMember(bindingRecord)]
            SynMemberDefn.Interface(SynType.CreateLongIdent(jtokenInterface), Some implementedMembers, range0)


        let getDeconstructType fieldTy =
            match fieldTy with
            | SynType.LongIdent ident ->
                match knownDeconstructs.TryGetValue(ident.AsString) with
                |(true ,v) -> Some v
                | _ -> None
            | _ -> None

        /// Allows for pattern matching against properties
        let createDeconstruct =

            let memberArgs =
                let arg argName fieldTy =
                    let create fieldTy =
                        let named = SynPatRcd.CreateNamed(Ident.Create argName, SynPatRcd.CreateWild )
                        let typ = SynType.CreateApp(SynType.CreateLongIdent "outref", [fieldTy], false )
                        SynPatRcd.CreateTyped(named, typ)

                    match getDeconstructType fieldTy with
                    | Some fieldTy -> create fieldTy
                    | None -> create fieldTy

                fields
                |> Seq.map(fun f ->
                    let rcd = f.ToRcd
                    let x = rcd.Id |> Option.get
                    let rcd = arg x.idText rcd.Type
                    let attribute : SynAttribute= {
                        AppliesToGetterAndSetter = false
                        ArgExpr = SynExpr.CreateConst SynConst.Unit
                        SynAttribute.TypeName = LongIdentWithDots.CreateString "System.Runtime.InteropServices.Out"
                        SynAttribute.Target = None
                        SynAttribute.Range = range0

                    }
                    let attrs : SynAttributeList = { Attributes = [attribute]; Range = range0}
                    let attrs = SynAttributes.Cons(attrs, SynAttributes.Empty)

                    // SynPatRcd.CreateAttrib(rcd, attrs)
                    rcd
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
                    let fieldName = string ident
                    let rightside =
                        match getDeconstructType fieldTy with
                        | Some _ ->
                            LongIdentWithDots.Create [selfIden; fieldName; "Deconstruct" ]
                            |> DSL.createInstanceMethodCallUnit
                        | None -> SynExpr.CreateLongIdent(false,LongIdentWithDots.Create [selfIden; fieldName ], None)

                    SynExpr.LongIdentSet (LongIdentWithDots.CreateString fieldName, rightside, range0 )
                )
                |> DSL.sequentialExpressions

            let bindingRecord = {
                    SynBindingRcd.Null with
                        ValData = createInnerDataMemberVal
                        Pattern = SynPatRcd.CreateLongIdent (LongIdentWithDots.Create [selfIden; "Deconstruct"], memberArgs)
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

            knownDeconstructs.Add(parentName, synExprType)

            SynMemberDefn.CreateMember(bindingRecord)

        let members = [
            createCtor ()
            yield! createGetSetMembersFromRecord ()
            createOverrideGetHashCode
            createOverrideEquals
            createDeconstruct
            createInterfaceImpl
        ]

        SynModuleDecl.CreateType(info, members)


    let createRecordModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        let jtokenInferface = "IHaveJToken" //TODO: Scan and get fully qualified
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



            let openNamespaces = [
                DSL.openNamespace (LongIdentWithDots.CreateString (typeof<JToken>.Namespace) )
                DSL.openNamespace (LongIdentWithDots.CreateString (typeof<JsonSerializer>.Namespace) )
                DSL.openNamespace (LongIdentWithDots.CreateString (typeof<IHaveJToken>.Namespace) )
            ]

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = [
                                yield! openNamespaces
                                yield! modules
                            ] }

            namespaceOrModule

