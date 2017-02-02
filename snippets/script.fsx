#r "System.Xml.Linq.dll"
#r "../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#load "../packages/FSharp.Azure.StorageTypeProvider/StorageTypeProvider.fsx"
#load "config.fs"
open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Microsoft.WindowsAzure.Storage
open Newtonsoft.Json

// --------------------------------------------------------------------------------------
// Data we store about snippets
// --------------------------------------------------------------------------------------

type Snippet = 
  { id : int
    likes : int
    posted : DateTime
    title : string
    description : string
    author : string
    twitter : string
    code : string 
    compiled : string
    config : string
    hidden : bool 
    version : string }

// --------------------------------------------------------------------------------------
// Reading & writing blobs in Azure storage
// --------------------------------------------------------------------------------------

let createCloudBlobClient() = 
  let account = CloudStorageAccount.Parse(Config.TheGammaSnippetsStorage)
  account.CreateCloudBlobClient()

let serializer = JsonSerializer.Create()

let toJson value = 
  let sb = System.Text.StringBuilder()
  use tw = new System.IO.StringWriter(sb)
  serializer.Serialize(tw, value)
  sb.ToString() 

let fromJson<'R> str : 'R = 
  use tr = new System.IO.StringReader(str)
  serializer.Deserialize(tr, typeof<'R>) :?> 'R

let readSnippets source =
  let container = createCloudBlobClient().GetContainerReference(source)
  if container.Exists() then
    let blob = container.GetBlockBlobReference("snippets.json")
    if blob.Exists() then 
      let json = blob.DownloadText(System.Text.Encoding.UTF8) 
      json, json |> fromJson<Snippet[]> |> List.ofArray
    else failwith "Blob 'snippets.json' does not exist."
  else failwithf "container '%s' not found" source

let writeSnippets source (json:string) = 
  let container = createCloudBlobClient().GetContainerReference(source)
  if container.Exists() then
    let blob = container.GetBlockBlobReference("snippets.json")
    blob.UploadText(json, System.Text.Encoding.UTF8)
  else failwithf "container '%s' not found" source

let initSnippets source = 
  let container = createCloudBlobClient().GetContainerReference(source)
  container.CreateIfNotExists() |> ignore
  let blob = container.GetBlockBlobReference("snippets.json")
  blob.UploadText("[]", System.Text.Encoding.UTF8)
  

// --------------------------------------------------------------------------------------
// Keeping current snippets using an agent
// --------------------------------------------------------------------------------------

do 
  let _, snips = readSnippets "olympics"
  for s in snips do 
    printfn "(%d) %s" s.id s.title

  // Fix source & remove some snippets
  let snips = 
    snips 
    |> Seq.filter (fun s -> s.id <> 21 && s.id <> 20 && s.id <> 17)
    |> Array.ofSeq

  let json = snips |> toJson
  writeSnippets "olympics" json

do
  let _, snips = readSnippets "thegamma"
  let json = snips |> Seq.map (fun s -> { s with twitter = s.twitter.TrimStart('@') }) |> Array.ofSeq |> toJson
  writeSnippets "thegamma" json

do
  let _, snips = readSnippets "thegamma"
  let testSnip = 
    { id = 23; likes = 0; posted = DateTime.Now; title = "Top medalists at Rio 2016"
      author = "Tomas Petricek"; twitter = "tomaspetricek"; 
      description = "Top medalists from Rio 2016 based on the number of gold medals. The snippet uses the pivot type provier to aggregate the data and shows a simple bar chart with the results."
      compiled = ""; hidden = false; version = "0.0.6"; config = ""; code = """let data =
  olympics
    .'filter data'.'Games is'.'Rio (2016)'.then
    .'group data'.'by Athlete'.'sum Gold'.then
    .'sort data'.'by Gold descending'.then
    .paging.take(8)
    .'get series'.'with key Athlete'.'and value Gold'
  
chart.column(data).legend(position="none")
  .set(fontName="Roboto", fontSize=14)
  .set(colors=["#F4C300"], title="Top medalists (by number of gold medals) at Rio 2016")""" }

  let snips = Seq.append snips [ testSnip ] |> Array.ofSeq    
  let json = snips |> toJson
  writeSnippets "thegamma" json
