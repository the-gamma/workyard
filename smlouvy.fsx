#r "System.Xml.Linq.dll"
#load "packages/FsLab/FsLab.fsx"
open FSharp.Data
open System.IO

// --------------------------------------------------------------------------------------
// Loading and pre-processing data
// --------------------------------------------------------------------------------------

type Record = 
  { DatumUzavreni : System.DateTime
    DatumPublikace : System.DateTime
    Hodnota : decimal
    ChybiHodnota : bool
    SubjektNazev : string
    SubjektUtvar : string
    Schavlil : string
    Predmet : string
    Odkaz : string
    Platne : bool
    Prijemci : string
    PrijemciIco : string }

let cache = __SOURCE_DIRECTORY__ + "/smlouvy"
let (</>) a b = Path.Combine(a, b)

type Index = XmlProvider<const(__SOURCE_DIRECTORY__ + "/index.xml")>
type Dump = XmlProvider<const(__SOURCE_DIRECTORY__ + "/smlouva.xml")>

let updateCache() = async {
  use wc = new System.Net.WebClient()
  let! index = Index.AsyncLoad("https://data.smlouvy.gov.cz")
  for m in index.Dumps do
    let fn suffix = sprintf "%04d-%02d%s.xml" m.Rok m.Mesic suffix
    if File.Exists(cache </> fn "-partial") then File.Delete(fn "-partial")
    if not (File.Exists(cache </> fn "")) then 
      let target = cache </> fn (if m.DokoncenyMesic then "" else "-partial")
      do! wc.AsyncDownloadFile(System.Uri(m.Odkaz), target) }

let readFiles () = 
  let smlouvy = ResizeArray<_>(10000)
  for f in Directory.GetFiles(cache) do
    let dump = Dump.Load(f)
    for z in dump.Zaznams do
      let prijemci = 
        [ for s in z.Smlouva.SmluvniStranas do
            if s.Prijemce = Some 1 then yield s.Nazev ]
      let prijemciIco = 
        [ for s in z.Smlouva.SmluvniStranas do
            if s.Prijemce = Some 1 && s.Ico <> None then yield s.Ico.Value.String.Value ]
      let prijemci = if prijemci.IsEmpty then "Nezadáno" else String.concat ", " prijemci
      let prijemciIco = if prijemciIco.IsEmpty then "Nezadáno" else String.concat ", " prijemciIco

      let hodnota, hodnotaChybi = 
        match z.Smlouva.HodnotaBezDph, z.Smlouva.HodnotaVcetneDph with
        | _, Some h when h <> 0.0M -> h, false
        | Some h, _ when h <> 0.0M -> h * 1.2M, false
        | _ -> 0.0M, true

      { DatumPublikace = z.CasZverejneni
        DatumUzavreni = z.Smlouva.DatumUzavreni
        Hodnota = hodnota
        ChybiHodnota = hodnotaChybi
        SubjektNazev = z.Smlouva.Subjekt.Nazev
        SubjektUtvar = defaultArg z.Smlouva.Subjekt.Utvar ""
        Schavlil = defaultArg z.Smlouva.Schvalil "Nezadáno"
        Predmet = z.Smlouva.Predmet
        Odkaz = z.Odkaz
        Platne = z.PlatnyZaznam
        Prijemci = prijemci
        PrijemciIco = prijemciIco } |> smlouvy.Add
  smlouvy.ToArray()

updateCache() |> Async.RunSynchronously
let smlouvy = readFiles()

// --------------------------------------------------------------------------------------
// Faceted data service
// --------------------------------------------------------------------------------------

let facets : list<string * Facet<Medals.Row>> = 
  [ // Single-choice 
    yield "game", Filter("game", false, fun r -> Some(r.Games, makeThingSchema "City" r.Games))
    yield "medal", Filter("medal", false, fun r -> Some(r.Medal, noSchema))
    yield "gender", Filter("gender", false, fun r -> Some(r.Gender, noSchema))
    yield "team", Filter("team", false, fun r -> Some(r.Team, makeThingSchema "Country" r.Team))
    yield "discipline", Filter("discipline", false, fun r -> Some(r.Discipline, makeThingSchema "SportsEvent" r.Sport))

    // Multi-choice
    yield "games", Filter("game", true, fun r -> Some(r.Games, makeThingSchema "City" r.Games))
    yield "medals", Filter("medal", true, fun r -> Some(r.Medal, noSchema))
    yield "teams", Filter("teams", true, fun r -> Some(r.Team, makeThingSchema "Country" r.Team))
    yield "disciplines", Filter("disciplines", true, fun r -> Some(r.Discipline, makeThingSchema "SportsEvent" r.Sport))

    // Multi-level facet with/without multi-choice
    let athleteChoice multi =  
      [ for (KeyValue(k,v)) in nocs -> 
          k, v, makeThingSchema "Country" v, Filter("athlete", multi, fun (r:Medals.Row) -> 
            if r.Team = v then Some(r.Athlete, makeThingSchema "Person" r.Athlete) else None) ]
    let sportChoice multi = 
      [ for (KeyValue(k,v)) in sports -> 
          k, v, makeThingSchema "SportsEvent" v, Filter("event", multi, fun (r:Medals.Row) ->  
            if r.Sport = v then Some(r.Event, makeThingSchema "SportsEvent" r.Event) else None) ]

    yield "athlete", Choice("country", athleteChoice false)
    yield "athletes", Choice("country", athleteChoice true)
    yield "sport", Choice ("sport", sportChoice false)
    yield "sports", Choice ("sport", sportChoice true) ]


// ----------------------------------------------------------------------------
// Create faceted service
// ----------------------------------------------------------------------------
    
let app =
  createFacetApp 
    (medals.Rows |> Seq.map (fun r ->
        Medals.Row
          ( r.Games, r.Year, r.Sport, r.Discipline, r.Athlete, countries.[r.Team],
            r.Gender, r.Event, r.Medal, r.Gold, r.Silver, r.Bronze) ) |> Array.ofSeq )
    (medals.Headers.Value |> Array.map (function
        | ("Year" | "Gold" | "Silver" | "Bronze") as h -> h, "int"
        | h -> h, "string")) facets
