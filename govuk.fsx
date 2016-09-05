#load "packages/FsLab/FsLab.fsx"
#load "htmlcharrefs.fs"
#load "htmlparser.fs"
open FSharp.Data

// --------------------------------------------------------------------------------------
// 
// --------------------------------------------------------------------------------------

type List = JsonProvider<"""{"help": "url","success": true,"result": ["foo","bar"] }""">
type Data = JsonProvider<"https://data.gov.uk/api/3/action/package_show?id=mot-active-vts">

let pkgNames = List.Load("https://data.gov.uk/api/3/action/package_list")
//for pkg in pkgNames.Result do

let pkgs = 
  pkgNames.Result 
  |> Array.filter (fun name -> name.Contains("centre-for-environment-fisheries-aquaculture-science-cefas") |> not)
  |> Array.map (fun name ->
    lazy Data.Load("https://data.gov.uk/api/3/action/package_show?id=" + name))

for pkg in pkgs do
  printfn "%s" pkg.Value.Result.Organization.Title

  try
    let data = pkg.Value
    printfn " - %s" data.Result.Title
    if data.Result.Resources |> Seq.exists (fun r -> r.Format.ToUpper() = "CSV") then
      printfn "\n%s\n%s" data.Result.Title (data.Result.Title |> String.map (fun _ -> '='))
      printfn "%s" data.Result.Notes
      printfn "\n"
      for res in data.Result.Resources do
        printfn " - %s: %s" res.Format res.Url
  with e -> printfn "Error: %s" e.Message

pkgNames.Result |> Seq.take 5000 |> Seq.iter (printfn "%s")
pkgNames.Result |> Seq.length