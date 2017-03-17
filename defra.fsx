#r "System.Xml.Linq"
#load "packages/FsLab/FsLab.fsx"
open System
open System.Globalization
open FSharp.Data

type Year = XmlProvider<"https://uk-air.defra.gov.uk/data/atom-dls/auto/2017/atom.en.xml">

let removePrefix prefix (s:string) = 
  if s.StartsWith(prefix) then s.Substring(prefix.Length) else s
let removeAfter substr (s:string) = 
  let i = s.LastIndexOf(substr:string)
  if i > 0 then s.Substring(0, i) else s
let removeBefore substr (s:string) = 
  let i = s.IndexOf(substr:string)
  if i > 0 then s.Substring(i+substr.Length) else s

let y = 2017
let year = Year.Load(sprintf "https://uk-air.defra.gov.uk/data/atom-dls/auto/%d/atom.en.xml" y)

let getPollutantsId (url:string) = 
  if url.StartsWith("http://dd.eionet.europa.eu/vocabulary/aq/pollutant/") then
    url.Substring("http://dd.eionet.europa.eu/vocabulary/aq/pollutant/".Length) |> int
  else failwithf "Invalid pollutant: %s" url

let pollutants = 
  [ for entry in year.Entries do
    for link in entry.Links do 
    if link.Href.Contains "aq/pollutant" then 
      let id = link.Href.Substring(link.Href.LastIndexOf('/') + 1) |> int
      yield link.Href, (id, link.Title) ] |> dict

type Data = XmlProvider<"http://uk-air.defra.gov.uk/data/atom-dls/observations/auto/GB_FixedObservations_2017_BIR1.xml">
//type Data = XmlProvider<"https://uk-air.defra.gov.uk/data/atom-dls/observations/auto/GB_FixedObservations_2017_ABD.xml">

type Station =
  { Coordinates : float * float
    Code : string
    Name : string
    DataSource : string }

let stations = 
  [ for entry in year.Entries ->
      let name = entry.Summary.Value |> removePrefix "GB Fixed Observations for " |> removeAfter " ("
      let code = entry.Summary.Value |> removeBefore " (" |> removeAfter ") "
      let coords = match entry.Polygon.Split(' ') |> List.ofArray with log::lat::_ -> (float log, float lat) | _ -> failwith "Could not parse coordinates"
      { Coordinates = coords; Code = code; Name = name; DataSource = entry.Id } ]

let processStation station =
  let file = sprintf "%s/defra/%d_%s.csv" __SOURCE_DIRECTORY__ y station.Code
  if not (IO.File.Exists(file)) then
    printfn "Downloading: %s" station.Name
    let dataUrl = sprintf "http://uk-air.defra.gov.uk/data/atom-dls/observations/auto/GB_FixedObservations_%d_%s.xml" y station.Code
    let data = Data.Load(dataUrl)
    let observations = data.FeatureMembers |> Seq.choose (fun mem -> mem.OmObservation) |> Seq.filter (fun obs -> obs.ObservedProperty.Href.IsSome)
        
    let save = CsvFile.Parse("Time,Pollutant,Value")
    let rows = ResizeArray<_>()

    for obs in observations do
      let polid = getPollutantsId obs.ObservedProperty.Href.Value
      let block = obs.Result.DataArray.Encoding.TextEncoding.BlockSeparator
      let tok = obs.Result.DataArray.Encoding.TextEncoding.TokenSeparator
      let vals = obs.Result.DataArray.Values
      let fields = obs.Result.DataArray.ElementType.DataRecord.Fields
      if fields.[0].Name <> "StartTime" || fields.[4].Name <> "Value" then failwith "Unexpected fields!"
      for block in vals.Split([| block |], StringSplitOptions.RemoveEmptyEntries) do
        let flds = block.Split([| tok |], StringSplitOptions.None) 
        (DateTime.Parse(flds.[0]), float flds.[4]) |> ignore
        rows.Add(CsvRow(save, [| flds.[0].Trim(); string polid; flds.[4].Trim() |]))
    save.Append(rows).Save(file)

stations |> Seq.iter processStation