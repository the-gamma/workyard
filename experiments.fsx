#load "packages/FsLab/FsLab.fsx"
#r @"../rest-provider/bin/TheGamma.RestProvider.dll"
open XPlot.GoogleCharts
open TheGamma

type Olympics = RestProvider<"http://localhost:10042/olympics", Timeout=3601>

let allData = Olympics.data |> Array.ofSeq

// ----------------------------------------------------------------------------
// Best individual medalists
// ----------------------------------------------------------------------------

allData
|> Seq.groupBy (fun c -> c.Athlete)
|> Seq.map (fun (a, items) ->
    items |> Seq.filter (fun i -> i.Medal = "Gold") |> Seq.length,
    items |> Seq.filter (fun i -> i.Medal = "Silver") |> Seq.length,
    items |> Seq.filter (fun i -> i.Medal = "Bronze") |> Seq.length,
    a,
    (items |> Seq.head).NOC)
|> Seq.sortDescending
|> Seq.take 20
|> Seq.iter (printfn "%A")

Seq.concat
  [ Olympics.``by athlete``.``United States``.``PHELPS, Michael``.data
    Olympics.``by athlete``.``United States``.``PHELPS Michael``.data
    //Olympics.``by athlete``.Czechoslovakia.``CASLAVSKA, Vera``.data
     ]
|> Seq.iter (fun i -> printfn "%s (%d) - %s, %s" i.City i.Edition i.Sport i.Event)

// ----------------------------------------------------------------------------
// Countries with most medals
// ----------------------------------------------------------------------------

allData
|> Seq.countBy (fun c -> c.NOC)
|> Seq.sortByDescending snd
|> Seq.take 10 
|> Seq.iter (printfn "%A")

let data = 
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

data 
|> Seq.map (fun teams ->
  Seq.concat teams
  |> Seq.groupBy (fun c -> c.Edition)
  |> Seq.map (fun (k, vs) -> k, vs |> Seq.map (fun i -> i.Discipline, i.Event, i.Event_gender) |> Seq.distinct |> Seq.length )
  |> realign )
|> Chart.Line
|> Chart.WithLabels ["USA"; "Russia"; "UK"; "France"; "Germany" ]
|> Chart.WithLegend(true)

// Note #1: WW2
// Note #2: Boycots

// ----------------------------------------------------------------------------
// With or without team events
// ----------------------------------------------------------------------------

let country = 
  Seq.concat
    [ Olympics.``by country``.``Czech Republic``.data
      Olympics.``by country``.Czechoslovakia.data
      Olympics.``by country``.Slovakia.data ]

[ country
  |> Seq.groupBy (fun c -> c.Edition)
  |> Seq.map (fun (k, vs) -> k, vs |> Seq.map (fun i -> i.Discipline, i.Event, i.Event_gender) |> Seq.distinct |> Seq.length )
  |> Seq.sortBy fst

  country
  |> Seq.groupBy (fun c -> c.Edition)
  |> Seq.map (fun (k, vs) -> k, vs |> Seq.length )
  |> Seq.sortBy fst ]
|> Chart.Line

// Olympics.``by country``.Czechoslovakia.``by city``.``Tokyo (1964)``.data
Olympics.``by country``.Czechoslovakia.``by city``.``Moscow (1980)``.data
|> Seq.map (fun i -> i.Discipline)
|> Seq.iter (printfn "%A")