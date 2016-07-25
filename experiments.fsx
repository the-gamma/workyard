#load "packages/FsLab/FsLab.fsx"
#r "../rest-provider/bin/TheGamma.RestProvider.dll"
open XPlot.GoogleCharts
open TheGamma

type Pivot = RestProvider<"http://localhost:10042/pivot", 1000, "source=http://localhost:10042/olympics">
type Olympics = RestProvider<"http://localhost:10042/olympics", 1000>

// ============================================================================
// USING F# FUNCTIONS
// ============================================================================

let allData = Olympics.data |> Array.ofSeq

// ----------------------------------------------------------------------------
// Best individual medalists
// ----------------------------------------------------------------------------

allData
|> Seq.groupBy (fun c -> c.Athlete)
|> Seq.map (fun (a, items) ->
    items |> Seq.sumBy (fun i -> i.Gold),
    items |> Seq.sumBy (fun i -> i.Silver),
    items |> Seq.sumBy (fun i -> i.Bronze),
    a,
    (items |> Seq.head).NOC)
|> Seq.sortDescending
|> Seq.take 20
|> Seq.iter (printfn "%A")

// ----------------------------------------------------------------------------
// Countries with most medals
// ----------------------------------------------------------------------------

allData
|> Seq.countBy (fun c -> c.NOC)
|> Seq.sortByDescending snd
|> Seq.take 10 
|> Seq.iter (printfn "%A")

let data1 = 
  [ [ Olympics.``by country``.``United States``.data ]
    [ Olympics.``by country``.``Soviet Union``.data
      Olympics.``by country``.``Russian Empire``.data
      Olympics.``by country``.``Russian Federation``.data
      Olympics.``by country``.``Unified Team``.data ]
    [ Olympics.``by country``.``United Kingdom (Great Britain)``.data ]
    [ Olympics.``by country``.France.data ]
    [ Olympics.``by country``.Germany.data 
      Olympics.``by country``.``West Germany``.data
      Olympics.``by country``.``East Germany``.data
      Olympics.``by country``.``Unified Team of Germany``.data ] ]

let realign data = 
  let lookup = Map.ofSeq data
  [ for y in 1896 .. 4 .. 2012 -> y, defaultArg (lookup.TryFind y) 0 ]

data1 
|> Seq.map (fun teams ->
  Seq.concat teams
  |> Seq.groupBy (fun c -> c.Edition)
  |> Seq.map (fun (k, vs) -> k, vs |> Seq.map (fun i -> i.Event) |> Seq.distinct |> Seq.length )
  |> realign )
|> Chart.Line
|> Chart.WithLabels ["USA"; "Russia"; "UK"; "France"; "Germany" ]
|> Chart.WithLegend(true)

// ----------------------------------------------------------------------------
// With or without team events
// ----------------------------------------------------------------------------

let country1 = 
  Seq.concat
    [ Olympics.``by country``.``Czech Republic``.data
      Olympics.``by country``.Bohemia.data
      Olympics.``by country``.Czechoslovakia.data
      Olympics.``by country``.Slovakia.data ]

[ country1
  |> Seq.groupBy (fun c -> c.Edition)
  |> Seq.map (fun (k, vs) -> k, vs |> Seq.map (fun i -> i.Event) |> Seq.distinct |> Seq.length )
  |> Seq.sortBy fst

  country1
  |> Seq.groupBy (fun c -> c.Edition)
  |> Seq.map (fun (k, vs) -> k, vs |> Seq.length )
  |> Seq.sortBy fst ]
|> Chart.Line



// ============================================================================
// USING PIVOT TYPE PROVIDER
// ============================================================================


// ----------------------------------------------------------------------------
// Best individual medalists
// ----------------------------------------------------------------------------

Pivot.data
  .``group data``.``by Athlete``
    .``count all``.``sum Gold``.``sum Silver``.``sum Bronze``
    .``count distinct City``.``concatenate values of NOC``
    .``concatenate values of Discipline``.``then``
  .``sort data``
    .``by Gold descending``.``and by Silver descending``.``and by Bronze descending``.``then``
  .paging.take(10).``get the data``
|> Seq.iter (fun k -> printfn "%s (%d, %d, %d) - %s" k.Athlete k.Gold k.Silver k.Bronze k.Discipline)

Olympics.``by athlete``.``United States``.``PHELPS, Michael``.data
|> Seq.iter (fun i -> printfn "%s (%d) - %s, %s" i.City i.Edition i.Sport i.Event)

// GUI: Pick country and athlete

// ----------------------------------------------------------------------------
// Countries with most medals
// ----------------------------------------------------------------------------

Pivot.data
  .``group data``.``by NOC``.``count all``.``then``
  .``sort data``.``by count descending``.``then``
  .paging.take(10)
  .``get the data``
|> Seq.iter (fun i -> printfn "%s (%d)" i.NOC i.count)

let data2 = 
  [ Pivot.``by countries``.``United States``.``then``.data
    Pivot.``by countries``.``Soviet Union``.``or Russian Empire``
      .``or Russian Federation``.``or Unified Team``.``then``.data
    Pivot.``by countries``.``United Kingdom (Great Britain)``.``then``.data
    Pivot.``by countries``.France.``then``.data
    Pivot.``by countries``.Germany.``or West Germany``
      .``or East Germany``.``or Unified Team of Germany``.``then``.data ]

// GUI: Adding options to a chain of 'Foo'.'or Bar'.'or Goo' (also for grouping)
// GUI: Add/remove items to/from the list of coutnries

data2
|> List.map (fun data ->
  data.``group data``.``by Edition``.``count distinct Event``.``then``.``get the data``
  |> Seq.map (fun i -> i.Edition, i.Event)
  |> realign )
|> Chart.Line
|> Chart.WithLabels ["USA"; "Russia"; "UK"; "France"; "Germany" ]
|> Chart.WithLegend(true)


// Note #1: WW2
// Note #2: Boycots

// ----------------------------------------------------------------------------
// With or without team events
// ----------------------------------------------------------------------------

let country2 = 
  Pivot
    .``by countries``
        .``Czech Republic``
        .``or Bohemia``
        .``or Czechoslovakia``
        .``or Slovakia``.``then``.data
    .``group data``
        .``by Edition``
        .``count distinct Event``
        .``count all``.``then``.``get the data``

[ country2
  |> Seq.map (fun i -> i.Edition, i.count)
  |> Seq.sortBy fst

  country2
  |> Seq.map (fun i -> i.Edition, i.Event)
  |> Seq.sortBy fst ]
|> Chart.Line

// Olympics.``by country``.Czechoslovakia.``by city``.``Tokyo (1964)``.data
Olympics.``by country``.Czechoslovakia.``by city``.``Moscow (1980)``.data
|> Seq.map (fun i -> i.Discipline)
|> Seq.iter (printfn "%A")




// TODO: vs. World Bank data
// TODO: Compare two countries (US vs. Russia, .. other competitors), with some nice boxplot-like chartm