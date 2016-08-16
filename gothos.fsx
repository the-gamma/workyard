#load "packages/FsLab/FsLab.fsx"
open FSharp.Data

// medals from the history
type Medals = CsvProvider<const(__SOURCE_DIRECTORY__ + "/guardian/medals-expanded.csv")>
let usedCodes = [ for r in Medals.GetSample().Rows -> r.Team ] |> set

// codes that appear in the olympic data set
type Codes = HtmlProvider<const(__SOURCE_DIRECTORY__ + "/gothos/countrycodes.html")>
let codes = 
  [ yield "Serbia", "SRB"
    yield "Kosovo", "KOS"
    for r in Codes.GetSample().Tables.``3-Digit Country Codes``.Rows do
      let n = r.Country.Trim('*') 
      if usedCodes.Contains r.Code then yield n, r.Code ]

// coordinates known from gothos
type Gothos = CsvProvider<const(__SOURCE_DIRECTORY__ + "/gothos/country_centroids_all.tsv")>
let gotCoords = [ for s in Gothos.GetSample().Rows -> s.SHORT_NAME.ToLower(), (s.LAT, s.LONG) ]

// manual corrections for those we cannot match
let oc = codes |> List.map fst |> set
let gc = gotCoords |> List.map fst |> set

// not trying to cause any wars here, just picking the
// geographically nearest place that is in the csv file...
let renames = 
  [ "australasia", "australia"
    "bohemia", "czech republic"
    "british west indies", "jamaica"
    "czechoslovakia", "czech republic"
    "east germany", "germany"
    "independent olympic participants", "bosnia and herzegovina"
    "korea, north (pdr of korea)", "north korea"
    "korea, south", "south korea"
    "mixed team (1896 to 1904)", "united kingdom"
    "netherlands antilles", "saint martin"
    "russian empire", "russia"
    "russian federation", "russia"
    "serbia and montenegro", "serbia"
    "soviet union", "russia"
    "taiwan (chinese taipei)", "taiwan"
    "the bahamas", "bahamas"
    "unified team", "russia"
    "unified team of germany", "germany"
    "united kingdom (great britain)", "united kingdom"
    "virgin islands", "british virgin islands"
    "west germany", "germany"
    "yugoslavia", "bosnia and herzegovina"
    "individual olympic athletes", "kuwait" (* true for Rio *) ] |> dict

// produce json file with locations
type Locations = JsonProvider<"""{"country":"USA", "coordinates":[1.2,2.0]}""">

let gotLookup = dict gotCoords
let nameLookup = dict [ for n, c in codes -> c, n ]

let locs = 
  [| for code in usedCodes ->
      let name = nameLookup.[code]
      let lname = name.ToLower()
      let lname = if renames.ContainsKey lname then renames.[lname] else lname
      match gotLookup.TryGetValue(lname) with
      | true, (lo, la) -> Locations.Root(name, [| lo; la |]).JsonValue
      | _ -> failwithf "Failed to find: %s" name |] |> JsonValue.Array

System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/gothos/locations.json", locs.ToString())
