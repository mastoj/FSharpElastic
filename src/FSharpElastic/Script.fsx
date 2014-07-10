// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open FSharpElastic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

exception InvalidPropertyExpression
exception NotALambdaExpression

let propExprToString expr = 
    let rec innerExprToString x expr = 
        match expr with
        | PropertyGet(Some(x), y, __) -> y.Name
        | PropertyGet(Some(a), y, []) -> sprintf "%s.%s" (innerExprToString x a) y.Name
        | PropertyGet(Some(a), y, _) -> sprintf "%s" (innerExprToString x a)
        | _ -> raise InvalidPropertyExpression
    match expr with
    | Lambda(x, expr') -> innerExprToString x expr'
    | _ -> raise NotALambdaExpression

// Define your library scripting code here

type Operator =
    | And
    | Or

let operatorToJson o = 
    match o with
    | And -> "\"and\""
    | Or -> "\"or\""

type ZeroTermsQuery = 
    | All
    | None

let zeroTermsQueryToJson ztq =
    match ztq with 
    | All -> "\"all\""
    | None -> "\"none\""

type MatchOption = 
    | Operator of Operator
    | ZeroTermsQuery of ZeroTermsQuery
    | CutoffFrequency of double

let matchOptionToJson mo =
    match mo with
    | Operator o -> sprintf "\"operator: %s" (operatorToJson o)
    | ZeroTermsQuery ztq -> sprintf "\"zero_terms_query: %s" (zeroTermsQueryToJson ztq)
    | CutoffFrequency cfq -> sprintf "\"cutoff_frequency: %f" cfq

type Field<'T, 'TR> = Expr<'T -> 'TR> * 'TR
type Fields<'T, 'TR> = Expr<'T -> 'TR> list * 'TR

type SingelField<'T> =
    | All of query:string
    | IntField of Field<'T, int>
    | StringField of Field<'T, int>

let keyValueToString k v =
    sprintf "\"%s\": \"%O\"" k v

let singleFieldToJson field =
    match field with 
    | All(q) -> keyValueToString "_all" q

type MatchQuery<'T> = 
    | Simple of SingelField<'T>
//    | Options of propertySelector: Expr<'T -> string> * query: string * options: MatchOption list

type QueryStringOptions = 
    | DefaultField of string
    | Query of string
    | DefaultOperator of Operator
    | Analyzer of string
    | AllowLeadingWildCard of bool
    | LowercaseExpandedTerms of bool
    | EnablePositionIncrements of bool
    | FuzzyMaxExpansions of int

type MultiMatchOptions = 
    | Fields of string list

type BoolOptions = 
    | MinimumShouldMatch
    | Boost

type Query<'T> =
    | Match of MatchQuery<'T>
    | QueryString of options: QueryStringOptions list
    | MultiMatch of query: string * MultiMatchOptions
    | Bool of clauses: BoolClause<'T> list * options: BoolOptions

and BoolClause<'T> = 
    | Must of query: Query<'T>
    | MustNot of query: Query<'T>
    | Should of queries: Query<'T> list

type SearchQuery<'T> = 
    | Query of Query<'T>

type searchDocument = {PropX: string}

//let simpleMatch = Query(Match(Match.QueryString("tomas")))
//let complexMatch = Query(Match(Options("tomas", [Operator(And); ZeroTermsQuery(All); CutoffFrequency(0.001)])))

let matchToJson matchQuery =
    match matchQuery with
    | Simple(field) -> 
        singleFieldToJson field
//    | Options (q, mos) -> 
//        let query = sprintf "\"query\": \"%s\"" q
//        let options = query :: (List.map messageOptionToJson mos)
//        let optionsString = String.concat ", " options
//        sprintf "{ %s }" optionsString
        //sprintf "{ \"query\": \"%s\", hello" q

let queryToJson query = 
    match query with
    | Match message -> sprintf "{ \"match\" : { %s } }" (matchToJson message)

type Sample = {PropX: string}
let lambda = <@ fun (x:Sample) -> x.PropX @>
let jsonQuery = queryToJson (Match(Simple(lambda, "tomas")))
//let jsonQuery2 = queryToJson (Match(Options("tomas", [Operator(And); ZeroTermsQuery(All); CutoffFrequency(0.001)])))


//open System
//open Microsoft.FSharp.Quotations
//open Microsoft.FSharp.Quotations.Patterns
type Matchx = 
    | Simple of Expr * string
//    | WithOptions of string * string 

type Y = {ya: string; yb: int}
type X = {a: string; b: int; ys: Y list}
let lambda2 = <@ fun (x:X) -> x.ys.[0].ya @>
let x = {a = "Tomas"; b = 10; ys = []}
let compXa = <@ x.a @>
let compXYa = <@ x.ys.[0].ya @>
let compXY = <@ x.ys @>
let simpleMatchx = Simple(compXa, "this is my query")
let advMatchx = Simple(compXYa, "this is my query")
let lambdaComp = <@ fun (x:X) -> x.a @>


//(PropertyGet (
//    Some (PropertyGet (
//        Some (PropertyGet (
//            Some (PropertyGet (
//                None, x, []))
//            , ys, []))
//        , Item, [Value (0)]))
//    , ya,[]))
//type SearchQuery2<'T> =
//    | Match of Matchx<'T>

