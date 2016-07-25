#load "htmlcharrefs.fs"
#load "htmlparser.fs"
open FSharp.Data

// --------------------------------------------------------------------------------------
// HTML extracation
// --------------------------------------------------------------------------------------

type PathAttribute = 
  | Value of string
  | Set of string list

type PathPart = 
  | PathElement of (string * PathAttribute) list * string 
  | PathText
  | PathCData 
  | PathComment 
  | PathAnyElements

let printPath p = 
  p |> Seq.map (function
    | PathText -> "#"
    | PathCData -> "^"
    | PathComment -> "$"
    | PathAnyElements -> "*"
    | PathElement([], n) -> n
    | PathElement(attrs, n) ->
        let astr = 
          [ for a, v in attrs -> 
              match v with 
              | Value v -> sprintf "%s=%s" a v
              | Set v -> sprintf "%s=%s" a (String.concat " " v) ] 
        sprintf "%s[%s]" n (String.concat "," astr)) |> String.concat "."

type System.Collections.Generic.IEnumerable<'a> with
  member x.head() = x |> Seq.head

type HtmlNode with
  member n.innerText() = 
    let rec loop = function
      | HtmlNode.HtmlText s -> s
      | HtmlNode.HtmlElement(_, _, els) -> String.concat "" (List.map loop els)
      | _ -> ""
    loop n

  member n.text() = 
    match n with
    | HtmlNode.HtmlText s -> s
    | _ -> failwith "Node is not text"

  member n.attr(s) = 
    match n with
    | HtmlNode.HtmlElement(_, attrs, _) ->    
        attrs |> Seq.pick (function (HtmlAttribute(a, v)) when a = s -> Some v | _ -> None) 
    | _ -> failwith "Node is not an element"

[<System.Runtime.CompilerServices.Extension>]
type EnumerableExtensions =
  [<System.Runtime.CompilerServices.Extension>]
  static member innerText(nodes:seq<HtmlNode>) = 
    nodes |> Seq.map (fun n -> n.innerText()) |> String.concat ""
  [<System.Runtime.CompilerServices.Extension>]
  static member text(nodes:seq<HtmlNode>) = 
    nodes |> Seq.map (function HtmlText t -> t | _ -> failwith "Not a text node") |> String.concat ""
      

let splitSetAttribute (s:string) = 
  s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq

let attributePath k v =
  if k = "class" then k, PathAttribute.Set(splitSetAttribute v)
  else k, PathAttribute.Value(v)

let dropCommonPrefix paths = 
  let rec loop prefix paths = 
    let prePaths = paths |> List.choose (function p::ps -> Some(p, ps) | _ -> None)
    if List.length prePaths <> List.length paths then 
      let last = List.head prefix
      List.rev prefix, paths |> List.map (fun p -> last::p)
    else
      let firstSame = prePaths |> Seq.map (fst >> Some) |> Seq.reduce (fun a b -> if a = b then a else None)
      match firstSame with
      | Some first -> loop (first::prefix) (List.map snd prePaths)
      | None ->
          let last = List.head prefix
          List.rev prefix, paths |> List.map (fun p -> last::p)
  loop [] paths

let rec dropPrefix path prefix = 
  match path, prefix with
  | path, [_] -> path
  | pt::path, pr::prefix when pt = pr -> dropPrefix path prefix
  | _ -> failwith "Not a prefix"

let rec matchPath path node = seq {
  match path, node with
  | PathAnyElements::path, HtmlElement(_, _, nodes) ->
      yield! matchPath path node
      for node in nodes do
        yield! matchPath (PathAnyElements::path) node
  | PathText::[], HtmlText _ 
  | PathCData::[], HtmlCData _
  | PathComment::[], HtmlComment _ -> yield node
  | PathElement(pathAttrs, pathName)::path, HtmlElement(elName, elAttrs, nodes) ->
      let elAttrs = Map.ofSeq [ for HtmlAttribute(k, v) in elAttrs -> k, v]
      let attrMatch = pathAttrs |> Seq.forall (function
        | k, PathAttribute.Value(v) -> elAttrs.TryFind k = Some v
        | k, PathAttribute.Set(vs) ->  
            match elAttrs.TryFind k with
            | Some attr -> 
                let attr = attr |> splitSetAttribute |> set
                vs |> Seq.forall (attr.Contains)
            | _ -> false)
      if pathName = elName && attrMatch then
        match path with
        | [] -> yield node
        | path ->
            for node in nodes do
              yield! matchPath path node       
  | _ -> () }

let rec unionPathsPrefixes p1 p2 = 
  match p1, p2 with 
  | _ when p1 = p2 -> p1, ([], [])
  | e1::p1, e2::p2 when e1 = e2 ->
      let unioned, rest = unionPathsPrefixes p1 p2
      e1::unioned, rest
  | PathElement(a1, n1)::p1, PathElement(a2, n2)::p2 when n1 = n2 ->
      let a2lookup = Map.ofSeq a2
      let attrs = 
        a1 |> List.choose (fun (k, a) ->
          match a, a2lookup.TryFind k with
          | PathAttribute.Set(v1s), Some(PathAttribute.Set(v2s)) ->
              let vs = Set.intersect (set v1s) (set v2s)
              if Set.isEmpty vs then None
              else Some(k, PathAttribute.Set(List.ofSeq vs))
          | PathAttribute.Value(v1), Some(PathAttribute.Value(v2)) when v1 = v2 ->  
              Some(k, PathAttribute.Value v1)
          | _ -> 
              // In theory, we can interset Value & Set, but this never
              // happens because we now use set only for "class"
              None) 
      let unioned, rest = unionPathsPrefixes p1 p2 
      PathElement(attrs, n1)::unioned, rest
  | _ -> [], (p1, p2)

let unionPaths p1 p2 = 
  match unionPathsPrefixes p1 p2 with
  | u, ([], []) -> Some u
  | _ -> None

let rec collectPaths path f node = seq {
  let path, children = 
    match node with
    | HtmlNode.HtmlCData _ -> PathCData::path, []
    | HtmlNode.HtmlComment _ -> PathComment::path, []
    | HtmlNode.HtmlText s -> PathText::path, []
    | HtmlNode.HtmlElement(elName, attrs, elements) -> 
      PathElement([ for HtmlAttribute(k, v) in attrs -> attributePath k v], elName)::path, elements
  if f node then yield List.rev path
  for child in children do yield! collectPaths path f child }

let splitLast n path = 
  let p = path |> Array.ofList
  List.ofArray (p.[.. p.Length-n-1]), List.ofArray(p.[p.Length-n-1 ..])

let firstTextPath text root = 
  root |> collectPaths [] (function (HtmlText(s)) -> s.ToLower().Trim() = text | _ -> false) 

let downloadNodes (url:string) =  
  let html = 
    if System.Uri(url).IsFile then
      System.IO.File.ReadAllText(url)
    else
      let wc = new System.Net.WebClient()
      wc.DownloadString url
  match HtmlDocument.Parse(html) with
  | HtmlDocument(_, results::_) -> results
  | _ -> failwith "Invalid document"

// --------------------------------------------------------------------------------------
// Get all olympic games, sports and events
// --------------------------------------------------------------------------------------

module Olympic = 
  let results = downloadNodes "https://www.olympic.org/olympic-results"
  let sports = downloadNodes "https://www.olympic.org/ajaxscript/getdisciplinebygame/{1C3D47A1-714D-458C-8DCD-3C3BBE5B5509}"
  let events = downloadNodes "https://www.olympic.org/ajaxscript/geteventbydiscipline/{08EFF41D-CA1A-4719-9388-65A147443C5F}"

  // Paths to extract events from the results page
  let g1 = results |> firstTextPath "sochi 2014" |> Seq.nth 2
  let g2 = results |> firstTextPath "athens 1896" |> Seq.nth 1
  let gamesPath, gamesText = unionPaths g1 g2 |> Option.get |> splitLast 1

  let s1 = sports |> firstTextPath "archery" |> Seq.head
  let s2 = sports |> firstTextPath "fencing" |> Seq.head
  let sportsPath, sportsText = unionPaths s1 s2 |> Option.get |> splitLast 1

  let e1 = events |> firstTextPath "marathon women" |> Seq.head
  let e2 = events |> firstTextPath "triple jump men" |> Seq.head
  let eventsPath, eventsText = unionPaths s1 s2 |> Option.get |> splitLast 1

  let allGames = 
    [ for opt in matchPath gamesPath results ->
        (matchPath gamesText opt).text(),
        opt.attr("data-url") ]

  let allSports sports = 
    [ for opt in matchPath sportsPath sports ->
        (matchPath sportsText opt).text(),
        "/" + opt.attr("data-url") ]

  let allEvents events = 
    [ for opt in matchPath eventsPath events ->
        (matchPath eventsText opt).text(),
        opt.attr("value") ]

  let allEventLinks = 
    [ for games, gamesLink in allGames do
        printfn "*** %s ***" games
        let sports = downloadNodes ("https://www.olympic.org" + gamesLink)
        for sport, sportLink in allSports sports do
          let events = downloadNodes ("https://www.olympic.org" + sportLink)
          for event, eventLink in allEvents events do
            yield games, sport, event, eventLink ]

  let data = @"C:\Tomas\Public\thegamma\workyard\data"
  let failed = ResizeArray<_>()
  let work = async { 
    let wc = new System.Net.WebClient()
    try
      for _, _, _, f in allEventLinks do
        let file = data + "/" + f.TrimStart('/').Replace('/', '_') + ".html"
        let fails = failed |> Seq.exists ((=) f)
        if not (System.IO.File.Exists(file)) && not fails then
          try
            wc.DownloadFile("https://www.olympic.org" + f, file)
          with e ->
            printfn "Failed: %s" f
            failed.Add(f)
          do! Async.Sleep(1000)
        else printfn "Skipping: %s" f
    with e ->
      printfn "Download failed: %A" e }

  let cts = new System.Threading.CancellationTokenSource()      
  Async.Start(work, cts.Token)
  cts.Cancel()

  failed |> List.ofSeq
  failed |> Seq.length

  allEventLinks |> Seq.length

// --------------------------------------------------------------------------------------
// Parse data in table format
// --------------------------------------------------------------------------------------

module Table =
  let sample = downloadNodes "https://www.olympic.org/london-2012/athletics/100m-men"

  let roundBody, roundName = sample |> firstTextPath "final" |> Seq.head |> splitLast 3
  let p1 = sample |> firstTextPath "usain bolt" |> Seq.head
  let n1 = sample |> firstTextPath "jam" |> Seq.head
  let r1 = sample |> firstTextPath "9.63" |> Seq.head
  let m1 = sample |> firstTextPath "g" |> Seq.head
  let p2 = sample |> firstTextPath "derrick atkins" |> Seq.head
  let n2 = sample |> firstTextPath "bah" |> Seq.head
  let r2 = sample |> firstTextPath "10.08" |> Seq.head
  let m2 = sample |> firstTextPath "12." |> Seq.head
  let pp = unionPaths p1 p2 |> Option.get
  let nn = unionPaths n1 n2 |> Option.get
  let rr = unionPaths r1 r2 |> Option.get
  let mm, _ = dropCommonPrefix [m1; m2]

  let personPath, [ namePath; nationPath; resultPath; medalPath ] = dropCommonPrefix [ pp; nn; rr; mm ]
  let subPersonPath = dropPrefix personPath roundBody

  let parseTableResults data = 
    [ for r in matchPath roundBody data do
        let round = (matchPath roundName r).text() 
        let pers = matchPath subPersonPath r 
        for p in pers do 
          let medal, name, nation, result = 
            (matchPath medalPath p).innerText(), (matchPath namePath p).text(), 
            (matchPath nationPath p).text(), (matchPath resultPath p).text()
          if name <> "" then
            yield round, medal, name.Trim(), nation.Trim(), result.Trim() ]

// --------------------------------------------------------------------------------------
// Parse data in playoff format - this does not work
// --------------------------------------------------------------------------------------

module Playoff = 
  let sample = downloadNodes "https://www.olympic.org/london-2012/football/football-women"

  let p1 = sample |> firstTextPath "united states of america" |> Seq.nth 0
  let p2 = sample |> firstTextPath "united states of america" |> Seq.nth 1
  let p3 = sample |> firstTextPath "united states of america" |> Seq.nth 2

  let prefix, (r1, r2) = unionPathsPrefixes p1 p3
  let suffix, _ = unionPathsPrefixes (List.rev r1) (List.rev r2)
  let directNamePath = prefix @ PathAnyElements :: (List.rev suffix) 
  let boxPath, namePath = directNamePath |> splitLast 5

  let medalPath = 
    matchPath boxPath sample 
    |> Seq.pick (fun p ->
      let path = p |> firstTextPath "g" 
      if Seq.length path > 0 then Some(Seq.head path) else None)

  let parsePlayoffResults data = 
    [ for p in matchPath boxPath data do
         let medal = (matchPath medalPath p).text()
         let name = (matchPath namePath p).text() 
         if medal <> ""  then 
          yield medal, name ]

// --------------------------------------------------------------------------------------
// Parse downloaded data
// --------------------------------------------------------------------------------------

System.IO.Directory.GetFiles(@"C:\Tomas\Public\thegamma\workyard\data") 
|> Seq.filter (fun f -> f.Contains "london-2012")
|> Seq.iter (fun f ->
  let data = downloadNodes f
  match Playoff.parsePlayoffResults data, Table.parseTableResults data with
  | ((_::_) as p), _ -> () //printfn "PLAYOFF %s" (System.IO.Path.GetFileNameWithoutExtension f)
  | _, ((_::_) as p) -> () //printfn "TABLE %s" (System.IO.Path.GetFileNameWithoutExtension f) 
  | _ -> printfn "NOTHING: %s" (System.IO.Path.GetFileNameWithoutExtension f))
  
// --------------------------------------------------------------------------------------
// Downloading athlete list from BBC
// --------------------------------------------------------------------------------------

module BbcAthletes = 
  let sample = downloadNodes "http://www.bbc.co.uk/sport/olympics/2012/medals/athletes.inc?page=1"

  let p1 = firstTextPath "phelps michael" sample |> Seq.head
  let p2 = firstTextPath "franklin missy" sample |> Seq.head
  let athlets, athletName = unionPaths p1 p2 |> Option.get |> splitLast 2

  let a1 = matchPath athlets sample |> Seq.nth 0
  let a2 = matchPath athlets sample |> Seq.nth 3
  let countryPath = unionPaths (firstTextPath "united states" a1 |> Seq.head) (firstTextPath "jamaica" a2 |> Seq.head) |> Option.get

  let getAthletes page = 
    [ for a in matchPath athlets page ->
        let url = (matchPath [PathElement([], "th"); PathElement([], "a")] a |> Seq.head).attr("href")
        let urlParts = url.Split([| '/'; '#' |])
        urlParts.[urlParts.Length-2],
        (matchPath (countryPath |> splitLast 1 |> fst) a).head().attr("data-country"),
        (matchPath countryPath a).text().Trim(),
        (matchPath athletName a).text().Trim() ]

  let pages = 
    [ for i in 1 .. 18 ->
        printfn "%d" i
        downloadNodes ("http://www.bbc.co.uk/sport/olympics/2012/medals/athletes.inc?page=" + string i) ]

  let allAthletes = 
    [ for page in pages do
        yield! getAthletes page ]

// --------------------------------------------------------------------------------------
// Downloading all data
// --------------------------------------------------------------------------------------

let dir = __SOURCE_DIRECTORY__ + "/bbc"
let download () = async {
  let wc = new System.Net.WebClient()
  let count = ref 0 
  for guid, _, _, _ in  BbcAthletes.allAthletes do
    incr count
    let file = dir + "/" + guid
    printfn "%d/%d" count.Value (BbcAthletes.allAthletes |> Seq.length)
    if not (System.IO.File.Exists(file)) then
      wc.DownloadFile
        ( sprintf "http://www.bbc.co.uk/sport/olympics/2012/medals/athletes/%s.inc" guid,
          file ) }

let cts = new System.Threading.CancellationTokenSource()      
Async.Start(download (), cts.Token)
cts.Cancel()

// --------------------------------------------------------------------------------------
// Fuzzy matching of discipline names
// --------------------------------------------------------------------------------------

#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

type Medals = CsvProvider<const(__SOURCE_DIRECTORY__ + "/guardian/medals-1896-2008.csv")>

let known = 
  [ for r in Medals.GetSample().Rows ->  
      r.Sport, r.Discipline, r.Event ] 
  |> Seq.distinct 
  |> List.ofSeq

let wordRegex = System.Text.RegularExpressions.Regex("\\w+")
let numRegex =  System.Text.RegularExpressions.Regex("[0-9]+")

let specialWords = 
  [ "+", "+"; "-", "-"; "kg", "kg"; "free", "free"; "gre", "gre";
    "c1", "c-1"; "c2", "c-2"; "c4", "c-4";
    "k1", "k-1"; "k2", "k-2"; "k4", "k-4" ]

let collectWords words = 
  seq {
    for (s:string) in words do
      let s = s.ToLower()
      if s.Contains("+") then yield "+"
      if s.Contains("-") then yield "-"
      if s.Contains("kg") then yield "kg"
      if s.Contains("free") then yield "free"
      if s.Contains("gre") then yield "gre"
      if s.Contains("c1") then yield "+"
      if s.Contains("-") then yield "-"
      for m in wordRegex.Matches(s) do yield m.Value
      for m in numRegex.Matches(s) do yield m.Value }

let fuzzyMatch words1 words2 = 
  let words1 = words1 |> collectWords |> set
  let words2 = words2 |> collectWords |> set
  (Set.intersect words1 words2) |> Seq.sumBy (fun s -> s.Length)

let findDiscipline words = 
  known 
  |> Seq.sortBy (fun (s2, d2, e2) -> -fuzzyMatch words [s2; d2; e2])
  |> Seq.head

let normalizeName (s:string) = 
  try
    let c = s.ToCharArray()    
    let mutable i = 0
    while not (System.Char.IsUpper(c.[i])) do i <- i + 1 // skip 'le ', 'van der ' and such
    while i < s.Length && System.Char.IsUpper(c.[i]) do i <- i + 1
    System.String(c.[ .. i-1]).ToUpper() + if i = s.Length then "" else "," + System.String(c.[i ..])
  with e ->
    failwithf "Name: %s" s

// --------------------------------------------------------------------------------------
// Extracting medals from BBC
// --------------------------------------------------------------------------------------

let sample1 = downloadNodes "http://www.bbc.co.uk/sport/olympics/2012/medals/athletes/949dabea-d339-485f-bdc4-952a83a21971.inc"
let sample2 = downloadNodes "http://www.bbc.co.uk/sport/olympics/2012/medals/athletes/6f68366c-beec-4077-8eb2-657be78abbcb.inc"

let p1 = firstTextPath "men's 4 x 200m freestyle relay" sample1 |> Seq.head
let p2 = firstTextPath "men's team" sample2 |> Seq.head
let n1 = firstTextPath "1" sample1 |> Seq.head
let n2 = firstTextPath "1" sample2 |> Seq.head
let s1 = firstTextPath "swimming" sample1 |> Seq.head
let s2 = firstTextPath "gymnastics - artistic" sample2 |> Seq.head
let nn = unionPaths n1 n2 |> Option.get
let pp = unionPaths p1 p2 |> Option.get
let ss = unionPaths s1 s2 |> Option.get
let prefix, [ discName; sportName; medals ] = dropCommonPrefix [ pp; ss; nn ]

let medalLookup = dict [ "100", "Gold"; "010", "Silver"; "001", "Bronze" ]

let extractMedals file = 
  [ for p in matchPath prefix file -> 
      medalLookup.[(matchPath medals p).innerText().Replace(" ", "")],
      (matchPath discName p).text().Trim(),
      (matchPath sportName p).text().Trim() ]

let newRows = 
 [| let i = ref 0
    for guid, ccode, cname, name in BbcAthletes.allAthletes do
      incr i
      if i.Value % 10 = 0 then printfn "%d/1782" i.Value;
      for medal, disc, sport in extractMedals (downloadNodes (dir + "/" + guid)) do
        let g, gg, disc = 
          if disc.StartsWith("Men's ") then "M", "Men", disc.Substring("Men's ".Length)
          elif disc.StartsWith("Women's ") then "W", "Women", disc.Substring("Women's ".Length)
          else "X", "Unknown", disc
        let sport, disc, event = findDiscipline [sport; disc]
        yield Medals.Row("London", 2012, sport, disc, normalizeName name, ccode, gg, event, g, medal) |]

Medals
  //.Parse("City,Edition,Sport,Discipline,Athlete,NOC,Gender,Event,Event_gender,Medal")
  .GetSample()
  .Append(newRows).Save(__SOURCE_DIRECTORY__ + "/guardian/medals-merged.csv")

// --------------------------------------------------------------------------------------
//
// --------------------------------------------------------------------------------------

let merged = Medals.Load(__SOURCE_DIRECTORY__ + "/guardian/medals-merged.csv")
let expanded = CsvFile.Parse("City,Edition,Sport,Discipline,Athlete,NOC,Gender,Event,Medal,Gold,Silver,Bronze")

let nrows = 
  [| for r in merged.Rows ->
      let gold = if r.Medal = "Gold" then "1" else "0"
      let silver = if r.Medal = "Silver" then "1" else "0"
      let bronze = if r.Medal = "Bronze" then "1" else "0"
      let gender = match r.Event_gender with "M" -> " men" | "W" -> " women" | _ -> ""
      CsvRow(expanded, [|r.City; string r.Edition; r.Sport; r.Discipline; r.Athlete; 
        r.NOC; r.Gender; r.Event + gender; r.Medal; gold; silver; bronze |]) |]

expanded.Append(nrows).Save(__SOURCE_DIRECTORY__ + "/guardian/medals-expanded.csv")