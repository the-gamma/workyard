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

let firstElementPath tag attr attrval root = 
  root |> collectPaths [] (function 
    | HtmlElement(t,attrs,_) when t = tag && Seq.contains (HtmlAttribute(attr, attrval)) attrs -> true
    | _ -> false) 

let downloadNodes (url:string) =  
  let html = 
    if System.Uri(url).IsFile then
      System.IO.File.ReadAllText(url)
    else
      let wc = new System.Net.WebClient(Encoding = System.Text.UTF8Encoding.UTF8)
      wc.DownloadString url
  match HtmlDocument.Parse(html) with
  | HtmlDocument(_, results::_) -> results
  | _ -> failwith "Invalid document"

let unionAll ps = Seq.reduce (fun a b -> unionPaths a b |> Option.get) ps

// --------------------------------------------------------------------------------------
// Get all Turing people
// --------------------------------------------------------------------------------------

// Leadership
let leadership = downloadNodes "https://www.turing.ac.uk/people/leadership/"

let l1 = leadership |> firstTextPath "professor andrew blake" |> Seq.head
let l2 = leadership |> firstTextPath "professor jared tanner, university of oxford" |> Seq.head
let lpath, lname = unionPaths l1 l2 |> Option.get |> splitLast 1
let lrole, lperson = lpath |> splitLast 3

let lrname = 
  unionPaths
    (matchPath lrole leadership |> Seq.item 0 |> firstTextPath "research director" |> Seq.head)
    (matchPath lrole leadership |> Seq.item 1 |> firstTextPath "ceo" |> Seq.head) 
  |> Option.get

let lds = 
  [ for r in matchPath lrole leadership do  
      let role = (matchPath lrname r).text()
      for p in matchPath lperson r do
        let name = (matchPath lname p).text().Split(',')
        yield name.[0], "The Alan Turing Institute", "Leadership", role ]


// Faculty fellows
let fellows = downloadNodes "https://www.turing.ac.uk/faculty-fellows/"

let f1 = fellows |> firstTextPath "professor frank wood" |> Seq.head
let f2 = fellows |> firstTextPath "dr sebastian vollmer" |> Seq.head
let f3 = fellows |> firstTextPath "professor jon crowcroft" |> Seq.head
let fpath, fname = unionAll [f1; f2; f3] |> splitLast 4
let funi, _ = fname |> splitLast 2

let ffs = 
  [ for f in matchPath fpath fellows ->
      let name = (matchPath fname f).text()
      let uni = (matchPath funi f).innerText().Substring(name.Length)
      name, uni, "Faculty Fellow", "Faculty Fellow" ]
  
ffs |> Seq.length
ffs |> Seq.iter (printfn "%A")

// Research fellows
let research = downloadNodes "https://www.turing.ac.uk/research-fellows/"

let r1 = research |> firstTextPath "mihai cucuringu" |> Seq.head
let r2 = research |> firstTextPath "stephen law" |> Seq.head
let r3 = research |> firstTextPath "adria gascon" |> Seq.head
let rpath, rname = unionAll [r1; r2; r3] |> splitLast 4
let runi, _ = rname |> splitLast 2

let rfs = 
  [ for r in matchPath rpath research ->
      let name = (matchPath rname r).text()
      let uni = (matchPath runi r).innerText().Substring(name.Length)
      if name.EndsWith(" (Visiting Researcher)") then 
        name.Substring(0, name.Length - " (Visiting Researcher)".Length), uni, "Research Fellow", "Visiting Researcher"
      else 
        name, uni, "Research Fellow", "Research Fellow" ]

rfs |> Seq.length
rfs |> Seq.iter (printfn "%A")


// Phd students
let phds = downloadNodes "https://www.turing.ac.uk/doctoral-students/"

let p1 = phds |> firstTextPath "merve alanyali" |> Seq.head
let p2 = phds |> firstTextPath "luca melis" |> Seq.head
let p3 = phds |> firstTextPath "hasiba afzalzada" |> Seq.head
let ppath, pname = unionAll [p1; p2; p3] |> splitLast 4
let puni, _ = pname |> splitLast 2

let dss = 
  [ for p in matchPath ppath phds ->
      let name = (matchPath pname p).text()
      let uni = (matchPath puni p).innerText().Substring(name.Length)
      name, uni, "Doctoral Student", "Doctoral Student" ]

dss |> Seq.length
dss |> Seq.iter (printfn "%A")

#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

let people = CsvFile.Load(__SOURCE_DIRECTORY__ + "/turing/rse.csv")
let rows = 
  lds @ ffs @ rfs @ dss 
  |> Seq.map (fun (n,u,r,p) -> CsvRow(people, [| n; u; r; p|]) )

System.IO.File.Delete(__SOURCE_DIRECTORY__ + "/turing/people.csv")
people.Append(rows).Save(__SOURCE_DIRECTORY__ + "/turing/people.csv")


// --------------------------------------------------------------------------------------
// Get all Turing publications
// --------------------------------------------------------------------------------------

let pubs = downloadNodes "https://www.turing.ac.uk/publications/"

let b1 = pubs |> firstTextPath "facilitating the ethical use of health data for the benefit of society: electronic health records, consent and the duty of easy rescue" |> Seq.head
let b2 = pubs |> firstTextPath "what is data ethics?" |> Seq.head
let bpath, bname = unionAll [b1; b2] |> splitLast 3
let brest, _ = bname |> splitLast 2

let pubscsv = CsvFile.Parse("Name,Details")
let prows = 
  [ for p in matchPath bpath pubs ->
      let name = (matchPath bname p).text()
      let rest = (matchPath brest p).innerText().Substring(name.Length)
      CsvRow(pubscsv, [| name; rest |]) ]

System.IO.File.Delete(__SOURCE_DIRECTORY__ + "/turing/pubs.csv")
pubscsv.Append(prows).Save(__SOURCE_DIRECTORY__ + "/turing/pubs.csv")
