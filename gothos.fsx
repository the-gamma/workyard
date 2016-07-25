#load "packages/FsLab/FsLab.fsx"
open FSharp.Data

type Codes = HtmlProvider<const(__SOURCE_DIRECTORY__ + "/gothos/countrycodes.html")>
let oc = set ("serbia":: [ for r in Codes.GetSample().Tables.``3-Digit Country Codes``.Rows -> r.Country.TrimEnd('*').ToLower() ])

type Google = HtmlProvider<"https://developers.google.com/public-data/docs/canonical/countries_csv">
let gg = set [ for s in Google.GetSample().Tables.``Countries.csv``.Rows -> s.name.ToLower() ]

type Gothos = CsvProvider<const(__SOURCE_DIRECTORY__ + "/gothos/country_centroids_all.tsv")>
let gc = set [ for s in Gothos.GetSample().Rows -> s.SHORT_NAME.ToLower() ]

Set.intersect gc oc |> Seq.length

oc - gc |> Seq.length
oc - gg |> Seq.length
oc - (Set.union gc gg) |> Seq.length

oc - gg |> Seq.iter (printfn "%s")
oc - gc |> Seq.iter (printfn "%s")
