// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r "../packages/FSharp.Data.2.0.9/lib/net40/FSharp.Data.dll"

open FSharp.Data
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

// ----------------------------------------------------------------------------
// Start by defining the domain model - once this is moved to a library, this
// will probably be in the first file in your project. This defines just
// the types - all processing can come after that...
// ----------------------------------------------------------------------------
exception InvalidPropertyExpression

exception NotALambdaExpression

type Operator = 
    | And
    | Or

type ZeroTermsQuery = 
    | All
    | None

type MatchOption = 
    | Operator of Operator
    | ZeroTermsQuery of ZeroTermsQuery
    | CutoffFrequency of double

//type QueryStringOptions = 
//    | DefaultField of string
//    | Query of string
//    | DefaultOperator of Operator
//    | Analyzer of string
//    | AllowLeadingWildCard of bool
//    | LowercaseExpandedTerms of bool
//    | EnablePositionIncrements of bool
//    | FuzzyMaxExpansions of int
//
//type MultiMatchOptions = 
//    | Fields of string list
//type BoolOptions = 
//    | MinimumShouldMatch
//    | Boost

type PropertySelector<'T, 'TR> = Expr<'T -> 'TR>
type Field<'T, 'TR> = PropertySelector<'T, 'TR> * 'TR

type Fields<'T, 'TR> = PropertySelector<'T, 'TR> list * 'TR

type SingleField<'T> = 
    | All of string
    | IntField of Field<'T, int>
    | StringField of Field<'T, string>

type Query<'T> = 
    | Match of SingleField<'T> * MatchOption list
    | Bool of BoolClause<'T>

//    | QueryString of options: QueryStringOptions list
//    | MultiMatch of query: string * MultiMatchOptions
//    | Bool of clauses: BoolClause<'T> list * options: BoolOptions
and BoolClause<'T> = 
    | Must of Query<'T>
    | MustNot of Query<'T>
    | Should of Query<'T> list
//

// ----------------------------------------------------------------------------
// The rest of the file contains the processing (formatting) functions...
// ----------------------------------------------------------------------------
let getPropertyChain expr = 
    let rec innerExprToString expr res = 
        match expr with
        | PropertyGet(Some(a), y, []) -> 
            match a with
            | PropertyGet(z) -> innerExprToString a (y.Name :: res)
            | _ -> (y.Name :: res)
        | PropertyGet(Some(a), y, _) -> innerExprToString a res
        | _ -> raise InvalidPropertyExpression
    match expr with
    | Lambda(x, expr') -> innerExprToString expr' []
    | _ -> raise NotALambdaExpression

let getPropExprString expr = 
    expr
    |> getPropertyChain
    |> List.map (fun s -> s.ToLower())
    |> String.concat "."

let operatorToJson o = 
    match o with
    | And -> "\"and\""
    | Or -> "\"or\""

let zeroTermsQueryToJson ztq = 
    match ztq with
    | ZeroTermsQuery.All -> "\"all\""
    | ZeroTermsQuery.None -> "\"none\""

let matchOptionToJson mo = 
    match mo with
    | Operator o -> sprintf "\"operator: %s" (operatorToJson o)
    | ZeroTermsQuery ztq -> sprintf "\"zero_terms_query: %s" (zeroTermsQueryToJson ztq)
    | CutoffFrequency cfq -> sprintf "\"cutoff_frequency: %f" cfq

let keyValueToString k v = sprintf "\"%s\": \"%O\"" k v

let fieldToJson ((expr, value) : Field<'T, 'TR>) = 
    let propertyKey = getPropExprString expr
    keyValueToString propertyKey value

let getFieldKey (expr, _) = getPropExprString expr
let getFieldValue (_, value) = sprintf "%O" value

type SingleField<'T> with
    
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

let operatorToJValue o = 
    match o with
    | And -> JsonValue.String("and")
    | Or -> JsonValue.String("or")

let zeroTermsQueryToJValue (ztq : ZeroTermsQuery) = 
    match ztq with
    | ZeroTermsQuery.All -> JsonValue.String("all")
    | None -> JsonValue.String("none")

let matchOptionToToken option = 
    match option with
    | Operator(o) -> ("operator", (operatorToJValue o))
    | ZeroTermsQuery(ztq) -> ("zero_terms_query", (zeroTermsQueryToJValue ztq))
    | CutoffFrequency(v) -> ("cutoff_frequency", (JsonValue.Float(v)))

let matchToToken<'T> ((f : SingleField<'T>), opts) = 
    match (f, opts) with
    | (_, []) -> JsonValue.Record([| (f.GetKey, JsonValue.String(f.GetQuery)) |])
    | (_, _) -> 
        let obj = ("query", JsonValue.String(f.GetQuery)) :: (opts |> List.map matchOptionToToken)
                  |> List.toArray
        JsonValue.Record(obj)

    
//        | Must of Query<'T>
//    | MustNot of Query<'T>
//    | Should of Query<'T> list


let boolToToken queryF = function
    | Must(q) -> JsonValue.Record([|("must", (queryF q))|])
    | MustNot(q) -> JsonValue.Record([|("must_not", (queryF q))|])
    | Should(qs) -> JsonValue.Record([|("should", JsonValue.Array(qs |> List.map queryF |> List.toArray))|])
let rec toJsonValue query = 
    let jRecord = match query with
        | Match(f, o) -> ("match", (matchToToken (f, o)))
        | Bool(b) -> ("bool", boolToToken toJsonValue b)
    JsonValue.Record([|jRecord|])

type Y = 
    { ya : string
      yb : int }

type X = 
    { a : string
      b : int
      ys : Y list }

let lambda2 = <@ fun (x : X) -> x.ys.[0].ya @>
let y = Match(
            StringField(<@ fun (y : Y) -> y.ya @>, "tomas"), [])
let x = Match(
            StringField(lambda2, "tomas"), 
            [ZeroTermsQuery(ZeroTermsQuery.All); Operator(And); CutoffFrequency(0.100)]
        )

let z = Bool(
            Must(
                Match(
                    StringField(lambda2, "tomas"), 
                    [ZeroTermsQuery(ZeroTermsQuery.All); Operator(And); CutoffFrequency(0.100)]
        )))

let b2 = Bool(
            Should([
                    Match(
                        StringField(lambda2, "tomas"), 
                        [ZeroTermsQuery(ZeroTermsQuery.All); Operator(And); CutoffFrequency(0.100)]);
                    Match(StringField(<@ fun (x : X) -> x.a @>, "tomas"), [])
        ]))

let xson = toJsonValue x
let yson = toJsonValue y
let zson = toJsonValue z
let b2son = toJsonValue b2

let xsonString = xson.ToString()
let ysonString = yson.ToString()
let zsonString = zson.ToString()
let b2sonString = b2son.ToString()