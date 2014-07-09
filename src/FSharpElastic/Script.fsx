// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open FSharpElastic

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

type MessageOption = 
    | Operator of Operator
    | ZeroTermsQuery of ZeroTermsQuery
    | CutoffFrequency of double

let messageOptionToJson mo =
    match mo with
    | Operator o -> sprintf "\"operator: %s" (operatorToJson o)
    | ZeroTermsQuery ztq -> sprintf "\"zero_terms_query: %s" (zeroTermsQueryToJson ztq)
    | CutoffFrequency cfq -> sprintf "\"cutoff_frequency: %f" cfq

type Message = 
    | QueryString of string
    | Options of query: string * MessageOption list

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

type Query =
    | Match of message: Message
    | QueryString of options: QueryStringOptions list
    | MultiMatch of query: string * MultiMatchOptions
    | Bool of clauses: BoolClause list * options: BoolOptions

and BoolClause = 
    | Must of query: Query
    | MustNot of query: Query
    | Should of queries: Query list

type SearchQuery = 
    | Query of Query


let simpleMatch = Query(Match(Message.QueryString("tomas")))
let complexMatch = Query(Match(Options("tomas", [Operator(And); ZeroTermsQuery(All); CutoffFrequency(0.001)])))

let messageToJson message =
    match message with
    | Message.QueryString str -> sprintf "\"%s\"" str
    | Options (q, mos) -> 
        let query = sprintf "\"query\": \"%s\"" q
        let options = query :: (List.map messageOptionToJson mos)
        let optionsString = String.concat ", " options
        sprintf "{ %s }" optionsString
        //sprintf "{ \"query\": \"%s\", hello" q

let queryToJson query = 
    match query with
    | Match message -> sprintf "{ \"match\" : %s }" (messageToJson message)

let jsonQuery = queryToJson (Match(Message.QueryString("tomas")))
let jsonQuery2 = queryToJson (Match(Options("tomas", [Operator(And); ZeroTermsQuery(All); CutoffFrequency(0.001)])))
