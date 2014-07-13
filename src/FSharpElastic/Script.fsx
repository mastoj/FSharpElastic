// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open FSharpElastic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

exception InvalidPropertyExpression
exception NotALambdaExpression

let getPropertyChain expr = 
    let rec innerExprToString expr res = 
        match expr with
        | PropertyGet(Some(a), y, []) -> 
            match a with
            | PropertyGet(z) -> innerExprToString a (y.Name::res)
            | _ -> (y.Name::res)            
        | PropertyGet(Some(a), y, _) -> innerExprToString a res
        | _ -> raise InvalidPropertyExpression
    match expr with
    | Lambda(x, expr') -> innerExprToString expr' []
    | _ -> raise NotALambdaExpression

let getPropExprString expr = 
    expr |> getPropertyChain |> List.map (fun s -> s.ToLower()) |> String.concat "."

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

let keyValueToString k v =
    sprintf "\"%s\": \"%O\"" k v

let fieldToJson ((expr, value):Field<'T, 'TR>) = 
    let propertyKey = getPropExprString expr
    keyValueToString propertyKey value

let getFieldKey (expr, _) = getPropExprString expr
let getFieldValue (_, value) = sprintf "%O" value

type SingleField<'T> =
    | All of query:string
    | IntField of Field<'T, int>
    | StringField of Field<'T, string>
    with
        member this.GetKey =
            match this with
            | All(_) -> "_all"
            | IntField(f) -> getFieldKey f
            | StringField(f) -> getFieldKey f
        member this.GetQuery =
            match this with
            | All(query) -> query
            | IntField(f) -> getFieldValue f
            | StringField(f) -> getFieldValue f

let singleFieldToJson field =
    match field with 
    | All(q) -> keyValueToString "_all" q
    | StringField(f) -> fieldToJson f

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
    | Match of SingleField<'T> * options: MatchOption list
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

let matchToJson<'T> (field: SingleField<'T>, options: MatchOption list) =
    match field, options with
    | (field, []) -> 
        singleFieldToJson field
    | (field, options) -> 
        let options = options |> List.map matchOptionToJson
        let queryKey = sprintf "\"%s\"" field.GetKey
        let value = field.GetQuery
        let query = keyValueToString "query" value 
        let allOptionsString = String.concat ", " (query::options)
        sprintf "%s: { %s }" queryKey allOptionsString

let queryToJson query = 
    match query with
    | Match (q, options) -> sprintf "{ \"match\" : { %s } }" (matchToJson (q, options))

type Y = {ya: string; yb: int}
type X = {a: string; b: int; ys: Y list}
let lambda2 = <@ fun (x:X) -> x.ys.[0].ya @>

//let jsonQuery = queryToJson (Match(StringField(lambda, "tomas"), []))
let jsonQuery2 = queryToJson (Match(StringField(lambda2, "tomas"), [ZeroTermsQuery(ZeroTermsQuery.All); Operator(And)]))
    