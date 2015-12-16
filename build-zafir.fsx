#load "tools/includes.fsx"
open IntelliFactory.Build

let bt =
    BuildTool().PackageId("Zafir.Leaflet")
        .VersionFrom("Zafir")
        .WithFSharpVersion(FSharpVersion.FSharp30)
        .WithFramework(fun fw -> fw.Net40)
        .References(fun r -> [r.Assembly "System.Web"])

let main =
    bt.Zafir.Extension("WebSharper.Leaflet")
        .SourcesFromProject()

let test =
    bt.Zafir.HtmlWebsite("WebSharper.Leaflet.Tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project main
                r.NuGet("Zafir.Html").Reference()
            ])

bt.Solution [
    main
//    test

    bt.NuGet.CreatePackage()
        .Configure(fun c ->
            { c with
                Title = Some "Zafir.Leaflet-0.7.2"
                LicenseUrl = Some "http://websharper.com/licensing"
                ProjectUrl = Some "https://github.com/intellifactory/websharper.leaflet"
                Description = "Zafir Extensions for Leaflet 0.7.2"
                RequiresLicenseAcceptance = true })
        .Add(main)

]
|> bt.Dispatch
