#load "packages/fslab/fslab.fsx"
open Deedle

let niceName (k:string) = 
  let names = k.Split('_')
  names 
  |> Array.mapi (fun i n ->
    if i = 0 then n.[0].ToString().ToUpper() + n.Substring(1)
    elif i = names.Length - 1 then
      if n = "hours" || n = "minutes" then "(" + n + ")" else n
    else n )
  |> String.concat " "

do
  let df = Frame.ReadCsv(__SOURCE_DIRECTORY__ + "/turing/raw/views-by-id.csv")
  let cleaned = 
    df 
    |> Frame.filterRows (fun _ row -> 
        not (row.GetAs<string>("video_title").Contains("OLD LINK")) &&
        row.GetAs<string>("video_title") <> row.GetAs<string>("video_id") &&
        row?watch_time_minutes > 30.)
    |> Frame.mapColKeys niceName

  cleaned.SaveCsv(__SOURCE_DIRECTORY__ + "/turing/views-by-id.csv")

do
  let df = Frame.ReadCsv(__SOURCE_DIRECTORY__ + "/turing/raw/watch-time-by-date.csv",inferTypes=false)
  let cleaned = df |> Frame.mapColKeys niceName
  cleaned.SaveCsv(__SOURCE_DIRECTORY__ + "/turing/watch-time-by-date.csv")

do
  let df = Frame.ReadCsv(__SOURCE_DIRECTORY__ + "/turing/raw/subscribers-by-date.csv",inferTypes=false)
  let cleaned = df |> Frame.mapColKeys niceName
  cleaned.SaveCsv(__SOURCE_DIRECTORY__ + "/turing/subscribers-by-date.csv")

let sub = Frame.ReadCsv(__SOURCE_DIRECTORY__ + "/turing/subscribers-by-date.csv") |> Frame.indexRowsDate "date"
let watch = Frame.ReadCsv(__SOURCE_DIRECTORY__ + "/turing/watch-time-by-date.csv") |> Frame.indexRowsDate "date" 

open FSharp.Charting

let cleaned = Frame.ReadCsv(__SOURCE_DIRECTORY__ + "/turing/views-by-id.csv")

cleaned.Rows
|> Series.values
|> Seq.map (fun os -> 
    os.GetAs<System.DateTime>("Video created"),
    os.GetAs<float>("Views") )    
|> Chart.Point

Chart.Rows
  [ sub?subscribers 
    //|> Stats.expandingSum
    |> Stats.movingMean 40
    //|> Series.filter (fun k v -> k.Day % 7 = 1)
    |> Series.observations
    |> Chart.Line

    cleaned.Columns.[["Views"; "Video created"]] 
    |> Frame.dropSparseRows
    |> Frame.rows
    |> Series.values
    |> Seq.map (fun os -> 
        os.GetAs<System.DateTime>("Video created"),
        os.GetAs<float>("Views") )    
    |> Chart.Point

    watch?views 
    //|> Stats.expandingSum
    |> Stats.movingMean 40
    //|> Series.filter (fun k v -> k.Day % 5 = 0)
    //|> Series.map (fun k v -> if k > System.DateTime(2016,10,1) then 0. else v)
    |> Chart.Line ]

Chart.Combine
  [ sub?subscribers |> Stats.expandingSum |> Chart.Line 
    watch?views |> Stats.expandingSum |> Chart.Line  ]

Chart.Line(watch?average_percentage_viewed)