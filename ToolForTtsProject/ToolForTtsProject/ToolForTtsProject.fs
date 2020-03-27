module ToolForTtsProject
open FsharpMyExtension

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

let extract (root:Root) : (FileName * Script) list =
    let f name xmlUI luaScript =
        let x =
            if System.String.IsNullOrEmpty xmlUI then
                None
            else
                (sprintf "%s.xml" name, xmlUI)
                |> Some
        let y =
            if System.String.IsNullOrEmpty luaScript then
                None
            else
                (sprintf "%s.lua" name, luaScript)
                |> Some
        match x, y with
        | None, None -> []
        | Some x, None -> [x]
        | None, Some y -> [y]
        | Some x, Some y -> [x; y]
    let xs =
        root.ObjectStates
        |> List.collect (fun x -> f x.GUID x.XmlUI x.LuaScript)
    f "global" root.XmlUI root.LuaScript @ xs

let substitute (root:Root) (m:Map<FileName, Script>) =
    let root =
        match Map.tryFind "global.lua" m with
        | Some x ->
            { root with LuaScript = x }
        | None -> root
    let root =
        match Map.tryFind "global.xml" m with
        | Some x ->
            { root with XmlUI = x }
        | None -> root
    root.ObjectStates
    |> List.map (fun x ->
        let x =
            match Map.tryFind (sprintf "%s.lua" x.GUID) m with
            | Some y -> { x with LuaScript = y }
            | None -> x
        let x =
            match Map.tryFind (sprintf "%s.xml" x.GUID) m with
            | Some y -> { x with XmlUI = y }
            | None -> x
        x
    )
    |> fun xs -> { root with ObjectStates = xs }

let test () =
    let path = @"C:\Users\User\Documents\My Games\Tabletop Simulator\Saves\TS_Save_1089.json"
    let x = FSharpJsonType.Serialize.desf path : Root
    let y = extract x |> Map.ofList |> substitute x
    x = y

let substitute2 (root:FSharpJsonType.JsonF) (m:Map<FileName, Script>) =
    let f2 root fileName fieldName m =
        let get =
            FSharpJsonType.JsonF.getScalar
            >> function FSharpJsonType.JsonFType.StringTyp x -> x
        match root with
        | FSharpJsonType.JsonF.Obj x ->
            match Map.tryFind (fileName (get x.["GUID"]) ) m with
            | Some y ->
                let v =
                    FSharpJsonType.JsonFType.StringTyp y
                    |> FSharpJsonType.JsonF.Scalar
                Map.add fieldName v x
                |> FSharpJsonType.JsonF.Obj
            | None -> x |> FSharpJsonType.JsonF.Obj
    let f root fileName fieldName m =
        match Map.tryFind fileName m with
        | Some x ->
            match root with
            | FSharpJsonType.JsonF.Obj o ->
                let v =
                    FSharpJsonType.JsonFType.StringTyp x
                    |> FSharpJsonType.JsonF.Scalar
                Map.add fieldName v o
                |> FSharpJsonType.JsonF.Obj
        | None -> root
    let root =
        f root "global.lua" "LuaScript" m
    let root =
        f root "global.xml" "XmlUI" m
    match root with
    | FSharpJsonType.JsonF.Obj o ->
        match o.["ObjectStates"] with
        | FSharpJsonType.JsonF.Sequence xs ->
            xs
            |> Array.map (fun x ->
                let x =
                    f2 x (sprintf "%s.lua") "LuaScript" m
                let x =
                    f2 x (sprintf "%s.xml") "XmlUI" m
                x
            )
            |> fun xs ->
                Map.add "ObjectStates" (FSharpJsonType.JsonF.Sequence xs) o

open FsharpMyExtension.Either
[<EntryPoint>]
let main argv =
    let printHelp () =
        [
            "--help — эта справка"
            "--extract — вынимает все luaScript и XmlUI и сохраняет в отдельные файлы"
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
                extract root
                |> List.iter (fun (fileName, script) ->
                    System.IO.File.WriteAllText(fileName, script)
                )
            )
    | [| "--substitute" |] ->
        getSave()
        |> Either.either
            (printfn "%s")
            (fun (savePath, (root : FSharpJsonType.JsonF)) ->
                let m =
                    // let path = @"c:\Users\User\Documents\My Games\Tabletop Simulator\Saves\Temp"
                    // System.IO.Directory.GetFiles(path)
                    System.IO.Directory.GetFiles(System.Environment.CurrentDirectory)
                    |> Array.filter (
                        System.IO.Path.GetExtension
                        >> fun x -> x = ".xml" || x = ".lua")
                    |> Array.map (fun path ->
                        System.IO.Path.GetFileName path, System.IO.File.ReadAllText path
                    )
                    |> Map.ofArray
                substitute2 root m
                |> FSharpJsonType.Serialize.serf savePath
            )
    | x -> printHelp ()
    0