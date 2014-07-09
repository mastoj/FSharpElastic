// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open FSharpElastic

// Define your library scripting code here

type Operator =
    | And
    | Or

type ZeroTermsQuery = 
    | All

type MessageOptions = 
    | Operator of Operator
    | ZeroTermsQuery of ZeroTermsQuery
    | CutoffFrequency of double

type Message = 
    | QueryString of string
    | Options of query: string * MessageOptions list

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