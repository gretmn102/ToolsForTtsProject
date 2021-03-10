module ToolForTtsProject
open FsharpMyExtension
open FsharpMyExtension.Either

type ObjectState =
    {
        Autoraise : bool
        ColorDiffuse : FSharpJsonType.JsonF
        CustomImage : FSharpJsonType.JsonF
        Description : string
        GMNotes : string
        GUID : string
        Grid : bool
        GridProjection : bool
        Hands : bool
        HideWhenFaceDown : bool
        IgnoreFoW : bool
        Locked : bool
        LuaScript : string
        LuaScriptState : string
        Name : string
        Nickname : string
        Snap : bool
        Sticky : bool
        Tooltip : bool
        Transform : FSharpJsonType.JsonF
        XmlUI : string
    }
type Root =
    {
        CustomUIAssets : FSharpJsonType.JsonF []
        Date : string
        DecalPallet : FSharpJsonType.JsonF []
        GameMode : string
        Gravity : double
        Grid : FSharpJsonType.JsonF
        Hands : FSharpJsonType.JsonF
        Lighting : FSharpJsonType.JsonF
        LuaScript : string
        LuaScriptState : string
        MusicPlayer : FSharpJsonType.JsonF
        Note : string
        ObjectStates : ObjectState list
        PlayArea : double
        Rules : string
        SaveName : string
        Sky : string
        SkyURL : string
        SnapPoints : FSharpJsonType.JsonF []
        TabStates : FSharpJsonType.JsonF
        Table : string
        TableURL : string
        Turns : FSharpJsonType.JsonF
        VersionNumber : string
        XmlUI : string
    }

type FileName = string
type Script = string

let testTypes () =
    let path = @"C:\Users\User\Documents\My Games\Tabletop Simulator\Saves\TS_Save_1089.json"
    let exp : FSharpJsonType.JsonF = FSharpJsonType.Serialize.desf path
    let act =
        let x : Root = FSharpJsonType.Serialize.desf path
        FSharpJsonType.Serialize.ser x
        |> FSharpJsonType.Serialize.des
        |> fun (x : FSharpJsonType.JsonF) -> x
    Json.serf "output\\exp.json" exp
    Json.serf "output\\act.json" act
    ()

type Guid = string
type Content = string
type Ext = Xml | Lua

type SourceFileName = { Name:string option; Guid:Guid; Ext:Ext; }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module SourceFileName =
    let sprint (x:SourceFileName) =
        x.Name
        |> Option.map (sprintf "%s.")
        |> Option.defaultValue ""
        |> fun s -> sprintf "%s%s.%A" s x.Guid x.Ext

    module Parser =
        type GlobalOrOther =
            | Global of Ext
            | Other of SourceFileName

        open FParsec
        type 'a Parser = Parser<'a, unit>
        let pext =
            let pext =
                (pstringCI "xml" >>% Xml) <|> (pstringCI "lua" >>% Lua)
            pext .>>? eof
        let pother : _ Parser =
            pipe2
                (many1Satisfy ((<>) '.') .>> pchar '.')
                ((pext |>> fun ext -> None, ext)
                 <|> pipe2
                        (many1Satisfy ((<>) '.') .>> pchar '.')
                        pext
                        (fun x ext -> Some x, ext))
                (fun first (second, ext) ->
                    match second with
                    | Some guid ->
                        { Name = Some first; Guid = guid; Ext = ext }
                    | None ->
                        { Name = None; Guid = first; Ext = ext }
                    |> Other
                )
        let pglobal : _ Parser =
            pstringCI "global" .>>? pchar '.'
            >>? pext
            |>> fun ext -> Global ext
            // |>> fun ext ->
            //     { Name = Some "global"; Guid = ""; Ext = ext }

        open FsharpMyExtension.Either
        let start str =
            match run ((pglobal <|> pother) .>> eof) str with
            | Success(x, _, _) -> Right x
            | Failure(str, _, _) -> Left str

type Project =
    {
        Global:{| LuaContent:Content option; XmlContent:Content option |}
        Xml:Map<Guid, SourceFileName * Content>
        Lua:Map<Guid, SourceFileName * Content>
    }
let emptyProject =
    {
        Global = {| LuaContent = None; XmlContent = None |}
        Xml = Map.empty
        Lua = Map.empty
    }

let preLoadProject xs =
    let error x y =
        sprintf "%s и %s have the same id"
            (SourceFileName.sprint x)
            (SourceFileName.sprint y)

    let rec f (acc: Project) = function
        | ((x : SourceFileName.Parser.GlobalOrOther), content:Content)::xs ->
            match x with
            | SourceFileName.Parser.Other x ->
                match x.Ext with
                | Lua ->
                    let m = acc.Lua
                    match Map.tryFind x.Guid m with
                    | None ->
                        let m = Map.add x.Guid (x, content) m
                        f { acc with Lua = m } xs
                    | Some (y, _) -> Left(error y x)
                | Xml ->
                    let m = acc.Xml
                    match Map.tryFind x.Guid m with
                    | None ->
                        let m = Map.add x.Guid (x, content) m
                        f { acc with Xml = m } xs
                    | Some (y, _) -> Left(error y x)
            | SourceFileName.Parser.Global ext ->
                match ext with
                | Xml -> {| acc.Global with XmlContent = Some content |}
                | Lua -> {| acc.Global with LuaContent = Some content |}
                |> fun x -> f { acc with Global = x } xs
        | [] -> Right acc
    f emptyProject xs

let loadProject dir =
    let files = System.IO.Directory.GetFiles dir

    files
    |> Array.choose (fun path ->
        System.IO.Path.GetFileName path
        |> SourceFileName.Parser.start
        |> Either.map (
            fun x -> x, System.IO.File.ReadAllText path
        )
        |> Option.ofEither
    )
    |> Array.toList // OPTIMIZE: превратить в `seq`
    |> preLoadProject

open Newtonsoft.Json.Linq
let substitute (root:JToken) (project:Project) =
    match root.Type with
    | JTokenType.Object ->
        let root = root :?> JObject
        match project.Global.LuaContent with
        | Some x ->
            root.["LuaScript"] <- JValue(x)
        | None -> ()

        match project.Global.XmlContent with
        | Some x ->
            root.["XmlUI"] <- JValue(x)
        | None -> ()

        root.["ObjectStates"]
        |> Seq.iter (fun x ->
            match Map.tryFind (x.["GUID"].Value<string>()) project.Lua with
            | Some(srcFileName, content) ->
                x.["LuaScript"] <- JValue(content)
            | None -> ()

            match Map.tryFind (x.["GUID"].Value<string>()) project.Xml with
            | Some(srcFileName, content) ->
                x.["XmlUI"] <- JValue(content)
            | None -> ()
        )
    | _ -> failwithf "something wrong with json"

let extract (root:Root) (project:Project) : Project =
    let f name xmlUI luaScript project =
        let project =
            if System.String.IsNullOrEmpty xmlUI then
                project
            else
                let m = project.Xml
                match Map.tryFind name m with
                | Some (srcFileName, _) ->
                    { project with
                        Xml = Map.add name (srcFileName, xmlUI) m
                    }
                | None ->
                    let srcFileName = { Name = None; Guid = name; Ext = Xml }
                    { project with
                        Xml = Map.add name (srcFileName, xmlUI) m
                    }

        if System.String.IsNullOrEmpty luaScript then
            project
        else
            let m = project.Lua
            match Map.tryFind name m with
            | Some (srcFileName, _) ->
                { project with
                    Lua = Map.add name (srcFileName, luaScript) m
                }
            | None ->
                let srcFileName = { Name = None; Guid = name; Ext = Lua }
                { project with
                    Lua = Map.add name (srcFileName, luaScript) m
                }
    let project =
        let f content =
            if System.String.IsNullOrEmpty content then
                None
            else
                Some content
        { project with
            Global = {| LuaContent = f root.LuaScript; XmlContent = f root.XmlUI |}
        }

    root.ObjectStates
    |> List.fold (fun st x -> f x.GUID x.XmlUI x.LuaScript st) project

[<EntryPoint>]
let main argv =
    let printHelp () =
        [
            "--help — эта справка"
            "--extract — вынимает все luaScript и XmlUI и сохраняет в отдельные файлы с перезаписью существующих"
            "--substitute — считывает все файлы скриптов и XmlUI и записывает в save"
        ]
        |> String.concat "\n"
        |> printfn "%s"
    let getSave () =
        let savesPaths = System.IO.Directory.GetFiles(System.Environment.CurrentDirectory, "*.json")
        match savesPaths with
        | [| savePath |] ->
            try
                let root = FSharpJsonType.Serialize.desf savePath
                Right(savePath, root)
            with e ->
                Left(e.Message)
        | [||] -> Left "any save file with extension .json not found"
        | xs -> Left (sprintf "found greater than one save files:\n%A" xs)

    match argv with
    | [| "--help" |] ->
        printHelp ()
    | [| "--extract" |] ->
        getSave()
        |> Either.either
            (printfn "%s")
            (fun (_, root : Root) ->
                let dir = System.Environment.CurrentDirectory

                match loadProject dir with
                | Left err ->
                    printfn "%s" err
                | Right proj ->
                    let proj = extract root proj
                    seq {
                        match proj.Global.LuaContent with
                        | Some content ->
                            "global.lua", content
                        | None -> ()
                        match proj.Global.XmlContent with
                        | Some content ->
                            "global.xml", content
                        | None -> ()
                        yield!
                            proj.Lua
                            |> Seq.map (fun (KeyValue(k, (srcFileName, content))) ->
                                SourceFileName.sprint srcFileName, content
                            )
                    }
                    |> Seq.iter (fun (fileName, script) ->
                        System.IO.File.WriteAllText(fileName, script)
                    )
            )
    | [| "--substitute" |] ->
        getSave()
        |> Either.either
            (printfn "%s")
            (fun (savePath, (root : JToken)) ->
                let dir = System.Environment.CurrentDirectory

                match loadProject dir with
                | Left err ->
                    printfn "%s" err
                | Right m ->
                    substitute root m

                    use file = System.IO.File.CreateText(savePath)
                    use st = new Newtonsoft.Json.JsonTextWriter(file)
                    st.Formatting <- Newtonsoft.Json.Formatting.Indented
                    let ser = Newtonsoft.Json.JsonSerializer()
                    ser.Serialize(st, root)
            )
    | x -> printHelp ()
    0