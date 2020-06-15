// Learn more about F# at http://fsharp.org

open System
open FSharp.Data

/// Return a list of school LEA codes (string of integers) for use in subsequent functions
let fetchCodes =
    HtmlDocument.Load("https://adesrc.arkansas.gov/").CssSelect("span.hidden-sm-inline")
    |> List.map (fun span ->
        span.InnerText().Replace(" (","").Replace(")","")
    )
    |> List.filter (fun code ->
        match System.Int32.TryParse code with
        | true,_ -> true
        | _ -> false
    )

let extractByClass name node =
    HtmlNode.descendants false (HtmlNode.hasClass name) node
    |> Seq.map HtmlNode.innerText

/// Takes the first page and returns a pair of schoolname and address
let parseSchoolBasic (firstpage: HtmlNode) =
    let banner = HtmlNode.descendants false (HtmlNode.hasClass "report-card-mp") firstpage |> Seq.head
    let schoolname = extractByClass "hdr-left" banner |> Seq.fold (+) ""
    let schooladdr = extractByClass "schoolAddress" banner |> Seq.fold (+) ""
    (schoolname, schooladdr)

/// Takes the div.school node and returns a paired list of attributes and values
/// Only applies to page 1
let parseSchool (school: HtmlNode) =
    let parameters = extractByClass "left-span" school
    let values = extractByClass "right-span" school
    Seq.zip parameters values

/// Extract information from the overview section
/// Only applies to page 1
let parseDemographics (demography: HtmlNode) =
    let splitter (s:string) =
        s.Replace(" ","").Split(',')
    let races =
        HtmlNode.descendants false (HtmlNode.hasClass "pie") demography
        |> Seq.head
        |> HtmlNode.innerText
        |> splitter
        |> Array.zip [|"Indian";"Asian";"Black";"Pacific Islander";"Hispanic";"White";"Mixed"|]
        |> Array.toSeq
    let qualifiers = extractByClass "col-left" demography
    let percentages = extractByClass "col-right" demography
    Seq.append races <| Seq.zip qualifiers percentages

let parseSchoolCombined(page: HtmlNode) =
    let (name, address) = parseSchoolBasic page
    let schoolnode = page.CssSelect "div.school" |> List.head
    let studentnode = page.CssSelect "div.student" |> List.head
    let schoolmeta = Seq.map (fun (c, v) -> [name;address;c;v]) (parseSchool schoolnode)
    let demographics = Seq.map (fun (c,v) -> [name;address;c;v]) (parseDemographics studentnode)
    Seq.append schoolmeta demographics

let tryAdd (s: string) (t: string) =
    let first = System.Double.TryParse s
    let second = System.Double.TryParse t
    match (first, second) with
    | ((true,x),(true,y)) -> string (x+y)
    | _ -> s

/// Extract 'year' column groupings
let getYears (page: HtmlNode) =
    let headers = page.CssSelect "table.report-card-section-table > thead > tr"
    match headers with
    | [_] | _::_ ->
        headers
        |> List.head
        |> HtmlNode.elements
        |> List.map HtmlNode.innerText
        |> List.tail
    | [] -> // selection breaks down in deep nesting for some reason. Descendants seems to work if you skip enough rows....
        page.Descendants ["tr"]
        |> Seq.skip 2
        |> Seq.head
        |> HtmlNode.elements
        |> List.map HtmlNode.innerText
        |> List.tail

/// For pages 2-14. Should return sequence of metric, category, year, rank1...rank5
/// should ignore columns which are just a sum of two other columns
/// should ignore state average. This is accessible elsewhere
let parseAchievement (page: HtmlNode) =
    let years = getYears page
    let mutable metric = "default"
    let mutable collection = []
    page.CssSelect "table.report-card-section-table > tr" /// There is an index out of range in here somewhere...
    |> List.iter (fun row ->
        match row.HasClass("data-section") with
        | true -> metric <- row.InnerText()
        | false -> 
            let mutable tds = row.Elements()
            let category = List.head tds |> HtmlNode.innerText
            if List.length tds < 17 then
                tds <- List.append tds (List.replicate 16 (HtmlNode.Parse("<p></p>").Head)) /// a terrible hack to prevent out of bounds indexing :(
            let y1src = tds.[2..6] |> List.map HtmlNode.innerText
            let year1data = List.append [y1src.Head; (tryAdd y1src.[1] y1src.[2])] y1src.[3..] |> List.append [metric;category;years.[0]]
            let year2data = tds.[8..11] |> List.map HtmlNode.innerText |> List.append [metric;category;years.[1]]
            let year3data = tds.[13..16] |> List.map HtmlNode.innerText |> List.append [metric;category;years.[2]]
            collection <- List.append collection [year1data; year2data; year3data]
    )
    List.toSeq collection

/// Take the NAEP pages and parse to a sequence of lists (metric,category,year,rank1..4)
let parseNAEP (page: HtmlNode) =
    let years = getYears page
    let mutable metric = "default"
    let mutable collection = []
    page.CssSelect "table.report-card-section-table > tr" /// just assuming the same problem exists
    |> List.iter (fun row ->
        match row.HasClass("data-section") with
        | true -> metric <- row.InnerText()
        | false -> 
            let mutable tds = row.Elements()
            let category = List.head tds |> HtmlNode.innerText
            let year1data = tds.[1..4] |> List.map HtmlNode.innerText |> List.append [metric;category;years.[0]]
            let year2data = tds.[6..9] |> List.map HtmlNode.innerText |> List.append [metric;category;years.[1]]
            collection <- List.append collection [year1data; year2data]
    )
    List.toSeq collection

let parseSpecial (page: HtmlNode) =
    let years = getYears page
    let mutable metric = "default"
    let mutable collection = []
    page.Descendants ["tr"] |> Seq.skip 4 |> List.ofSeq // seriously. tbody is not selectable
    |> List.iter (fun row ->
        let mutable tds = HtmlNode.elements row
        let category = List.head tds |> HtmlNode.innerText
        if tds.Length = 10 then
            let first = tds.[1..3] |> List.map HtmlNode.innerText |> List.append [metric;category;years.[0]]
            let second = tds.[4..6] |> List.map HtmlNode.innerText |> List.append [metric;category;years.[1]]
            let third = tds.[7..9] |> List.map HtmlNode.innerText |> List.append [metric;category;years.[2]]
            collection <- List.append collection [first;second;third]
        )
    List.toSeq collection

/// Take a LEA code and get the report card page. Return the page itself
let fetchReportCard code =
    Http.RequestString
        ("https://adesrc.arkansas.gov/ReportCard/View",
        httpMethod="GET",
        query = ["lea",code; "schoolYear","2017"])
    |> HtmlDocument.Parse // Each sub-report is on a page

type Destination =
    | School
    | Achievement
    | Special
    | NAEP

let writeOut (dest: Destination) (results: string list seq) =
    let homedir = System.Environment.GetEnvironmentVariable("HOME")
    IO.Directory.CreateDirectory (System.IO.Path.Combine [|homedir; "results"|]) |> ignore
    let makestream name = System.IO.Path.Combine [|homedir; "results"; name+".csv"|] |> IO.File.AppendText
    let writeresults (writer: IO.StreamWriter)  = results |> Seq.map (String.concat ",") |> Seq.iter writer.WriteLine
    let write name =
        use s = makestream name
        writeresults s
        s.Close()
    match dest with
    | School -> write "school"
    | Achievement -> write "achievement"
    | Special -> write "special"
    | NAEP -> write "NAEP"

/// Add a school first because I forgot to make this part of the other functions :(
let prependSchool (schoolname: string) (lea: string) (results: string list seq)=
    results |> Seq.map (List.append [schoolname; lea])


/// main logic for looping over an HTML page and appending to CSVs
let extract (lea: string) (doc: HtmlDocument) =
    let pages = doc.Descendants (HtmlNode.hasClass "page-wrapper") // list of each page
    let schoolname =  doc.CssSelect("head > title").Head.InnerText()
    Seq.zip (seq {0..(Seq.length pages - 1)}) pages // pages with indices
    |> Seq.iter (
        function
        |(i,p) when i = 0 -> parseSchoolCombined p |> writeOut School
        |(i,p) when i > 0 && i < 14 -> parseAchievement p |> prependSchool schoolname lea |> writeOut Achievement
        |(i,p) when i = 15 || i = 16 -> parseNAEP p |> prependSchool schoolname lea |> writeOut NAEP
        |(i,p) when i > 16 -> parseSpecial p |> prependSchool schoolname lea |> writeOut Special
        | _ -> ()
    )


/// basically just main
let fetchAll (cooldown:int) =
    let queue = 
        try fetchCodes
        with
        | _ -> 
            printfn "Could not fetch LEA codes"
            exit 1
    List.iter (fun lea ->
        printfn "fetching %s..." lea
        try
            fetchReportCard(lea)
            |> extract lea
        with
        | _ -> printfn "Could not fetch & extract %s" lea
        ) queue
    System.Threading.Thread.Sleep(cooldown)

// let testout =
//     let content = HtmlDocument.Parse(IO.File.ReadAllText("/home/sam/Source/fs/SchoolScrape/got.html"))
//         // ["6040700";"6040702";"6040704";"1701000"]
//         // |> List.head
//         // |> fetchReportCard
//     extract "6040700" content
//     // let pages = content.Descendants (HtmlNode.hasClass "page-wrapper")
//     // Seq.length pages |> printfn "Number of pages: %d" // list of each page
//     // let schoolname =  content.CssSelect("head > title").Head.InnerText()
//     // printfn "Schoolname is %A" schoolname
//     // // let firstpage = Seq.head pages |> parseSchoolCombined
//     // // printfn "Results for firstpage:\n\n"
//     // // Seq.iter (fun x -> printfn "%A" x) firstpage
//     // let thirdpage = Seq.take 3 pages |> Seq.last |> parseAchievement
//     // printfn "Results for achievement:\n\n"
//     // Seq.iter (fun x -> printfn "%A" x) thirdpage
//     // let page = Seq.take 18 pages |> Seq.last
//     // let years = getYears page
//     // let mutable metric = "default"
//     // let mutable collection = []
//     // let rows = page.Descendants ["tr"] |> Seq.skip 4 |> List.ofSeq 
//     // printfn "%d" rows.Length
//     // let page20 = Seq.take 21 pages |> Seq.last
//     // printfn "%A" (page20.Descendants ["tr"] |> Seq.skip 2 |> Seq.head)

/// MAIN ///
[<EntryPoint>]
let main argv =
    printfn "starting download"
    if argv.Length >= 1 then
        fetchAll (int (Array.head argv))
    else
        fetchAll 15000
    printfn "done"
    0 // return an integer exit code

