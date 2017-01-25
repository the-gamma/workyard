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
    source : string
    hidden : bool }

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

let readSnippets () =
  let container = createCloudBlobClient().GetContainerReference("olympics")
  if container.Exists() then
    let blob = container.GetBlockBlobReference("snippets.json")
    if blob.Exists() then 
      let json = blob.DownloadText(System.Text.Encoding.UTF8) 
      json, json |> fromJson<Snippet[]> |> List.ofArray
    else failwith "Blob 'snippets.json' does not exist."
  else failwith "container 'olympics' not found" 

let writeSnippets (json:string) = 
  let container = createCloudBlobClient().GetContainerReference("olympics")
  if container.Exists() then
    let blob = container.GetBlockBlobReference("snippets.json")
    blob.UploadText(json, System.Text.Encoding.UTF8)
  else failwith "container 'olympics' not found" 

// --------------------------------------------------------------------------------------
// Keeping current snippets using an agent
// --------------------------------------------------------------------------------------

let _, snips = readSnippets()
for s in snips do 
  printfn "(%d) %s [%s]" s.id s.title s.source

(*
let snips = 
  snips 
  |> Seq.map (fun s -> { s with source = "olympics" })
  |> Seq.filter (fun s -> s.id <> 21 && s.id <> 20 && s.id <> 17)
  |> Array.ofSeq
*)
    
let json = snips |> toJson
writeSnippets json
