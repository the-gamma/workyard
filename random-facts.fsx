#load @"C:\Temp\fslabj8\packages\FsLab\FsLab.fsx"
open FSharp.Data
open System

type MemberQuery = JsonProvider<"""
  [ { "name":"United Kingdom", "returns":{"kind":"nested", "endpoint":"/country"}, "trace":["country", "UK"],
      "parameters":[ {"name":"count", "type":null}, {"name":"another", "type":null} ],
      "documentation":"can be a plain string" }, 
    { "name":"United Kingdom", "returns":{"kind":"nested", "endpoint":"/country"},
      "documentation":{"endpoint":"/or-a-url-to-call"} }, 
    { "name":"Population", "returns":{"kind":"primitive", "type":null, "endpoint":"/data"}, "trace":["indicator", "POP"], 
      "documentation":{"title":"A", "details":"B"}} ] """>

type TypeInfo = JsonProvider<"""
  [ "float",
    { "name":"record", "fields":[ {"name":"A","type":null} ] },
    { "name":"seq", "params":[ null ] } ]
  """, SampleIsList=true>

let weight (endpoint:string) = 
  if endpoint.StartsWith("/pick") then 0.1
  //elif endpoint.EndsWith("/pivot/data") then 200.0
  elif endpoint.EndsWith("series/!/!") then 0.001
  elif endpoint.Contains("/and-pick") then 0.05
  //elif endpoint.StartsWith("/pivot/inject") then 100.0
  else 1.0

let rnd = Random()

let weightedRandom f (options:'a[]) = 
  let weighted = options |> Array.map (fun o -> f o, o)
  let total = weighted |> Array.sumBy fst
  let choice = rnd.NextDouble() * total
  weighted |> Array.fold (fun (res, sum) (w, item) ->
    match res with
    | Some r -> Some r, 0.0
    | None when sum < choice && sum + w >= choice -> Some item, 0.0
    | _ -> None, sum + w) (None, 0.0) |> fst |> Option.get

let rec weightedRandomOrder f (options:'a[]) = seq {
  if options.Length > 0 then
    let opt = weightedRandom f options
    yield opt
    yield! weightedRandomOrder f (options |> Array.filter (fun o -> o <> opt)) }

let parseCookieString (s:string) = 
  s.Split([|'&'|], StringSplitOptions.RemoveEmptyEntries) |> Array.choose (fun s -> 
    match s.Split('=') |> List.ofSeq with 
    | [] -> None
    | [ s ] -> Some (s, "")
    | k::v::_ -> Some(k, v) )

let (++) (a:string) (b:string) = a.TrimEnd('/') + "/" + b.TrimStart('/')

let rec random count source cookies url trace = seq {
  let members = MemberQuery.Parse(Http.RequestString(source ++ url, cookies=parseCookieString cookies))
  for m in members |> weightedRandomOrder (fun m -> weight m.Returns.Endpoint) do    
    if m.Returns.Kind = "nested" && m.Parameters.Length = 0 then
      printfn "%s (%s)" m.Name m.Returns.Endpoint
      yield! random (count + 1) source cookies m.Returns.Endpoint (m.Trace::trace)
    elif m.Returns.Kind = "primitive" then
      let trace = m.Trace::trace |> List.rev |> Seq.collect id |> String.concat "&"
      yield 
        Http.RequestString
          ( source ++ m.Returns.Endpoint, httpMethod="POST", body=TextRequest trace, 
            cookies=parseCookieString cookies )  }       

let source = "http://localhost:10042/pivot"
let cookies = "source=http://localhost:10042/olympics"

for r in MemberQuery.Parse(Http.RequestString(source, cookies=parseCookieString cookies)) do
  printfn "%s (%s)" r.Name r.Returns.Endpoint
  
for r in MemberQuery.Parse(Http.RequestString(source + "/pivot/inject-1/", cookies=parseCookieString cookies)) do
  printfn "%s (%s)" r.Name r.Returns.Endpoint

let en = (random 0 source cookies "" []).GetEnumerator()

en.MoveNext()
printfn "%A" (if en.Current.Length > 1000 then en.Current.Substring(0, 1000) else en.Current)


