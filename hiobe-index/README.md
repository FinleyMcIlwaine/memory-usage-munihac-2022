# The **H**askell **I**s **O**bviously **B**etter at **E**verything (HIOBE) Index

The [TIOBE Index](https://www.tiobe.com/tiobe-index/) is a programming language
popularity index. At the time of writing, the TIOBE Index has Haskell ranked at
#43, behind D and LabVIEW (?). This is blasphemy and we shouldn't stand for it.

The HIOBE Index is my rebuttal. It is a simple web application (in `server`)
that stores (a subset of) the
[2022 Stack Overflow Developer Survey](https://survey.stackoverflow.co/2022/)
results in a sqlite database (over 70000 rows). The database has a single table
`survey_data` that looks like this:
```sql
CREATE TABLE IF NOT EXISTS "survey_data" (
    ResponseId INTEGER PRIMARY KEY AUTOINCREMENT,
    LanguageHaveWorkedWith TEXT,
    LanguageWantToWorkWith TEXT,
    ConvertedCompYearly INTEGER
);
```
We can issue requests to the following endpoints of the web server:
```
POST /survey/respond
```
> Submit your own response to the HIOBE Index

```
GET /truth
```
> Serves truth

```
GET /languages/list
```
> List all languages stored in the database

```
GET /languages/count/have/:lang
```
> The number of survey respondants that have worked with `lang`

```
GET /languages/count/want/:lang
```
> The number of survey respondants that want to work with `lang`

```
GET /languages/hist/have
```
> A histogram (map from language to count) of the languages respondants have
> worked with

```
GET /languages/hist/have
```
> A histogram (map from language to count) of the languages respondants want to
> work with

We want to make sure that the HIOBE Index makes Haskell look as good as we know
it is, so we also have an executable application that generates traffic (in
`traffic`) which automatically submits lots of honest survey responses to the
HIOBE Index, and also generates some nice request traffic.

Unfortunately, in some sick twist of fate, the HIOBE Index server application
has been having some memory issues. In this portion of the workshop, we will
analyse the HIOBE Index server application using `eventlog2html` and
`ghc-debug`.
