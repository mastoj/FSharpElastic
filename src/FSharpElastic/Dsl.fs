namespace FSharpElastic.Dsl

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

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
//type BoolOptions = 
//    | MinimumShouldMatch
//    | Boost

type CommonOption = 
    | Boost of double

type MultiMatchType =
    | BestField
    | MostFields
    | CrossFields
    | Phrase
    | PhrasePrefix

type MultiMatchOption = 
    | Type of MultiMatchType
    | TieBreaker of double
    | Option of CommonOption

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
    | MultiMatch of string * PropertySelector<'T, string> list * MultiMatchOption list

//    | QueryString of options: QueryStringOptions list
//    | Bool of clauses: BoolClause<'T> list * options: BoolOptions
and BoolClause<'T> = 
    | Must of Query<'T>
    | MustNot of Query<'T>
    | Should of Query<'T> list


