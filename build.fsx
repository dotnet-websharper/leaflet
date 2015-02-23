#load "tools/includes.fsx"
open IntelliFactory.Build

let bt =
    BuildTool().PackageId("WebSharper.Leaflet", "3.0-alpha")
        .References(fun r -> [r.Assembly "System.Web"])

let main =
    bt.WebSharper.Extension("WebSharper.Leaflet")
        .SourcesFromProject()

let test =
    bt.WebSharper.HtmlWebsite("WebSharper.Leaflet.Tests")
        .SourcesFromProject()
        .References(fun r -> [r.Project main])

bt.Solution [
    main
    test

    bt.NuGet.CreatePackage()
        .Configure(fun c ->
            { c with
                Title = Some "WebSharper.Leaflet-0.7.2"
                LicenseUrl = Some "http://websharper.com/licensing"
                ProjectUrl = Some "https://github.com/intellifactory/websharper.leaflet"
                Description = "WebSharper Extensions for Leaflet 0.7.2"
                RequiresLicenseAcceptance = true })
        .Add(main)

]
|> bt.Dispatch
