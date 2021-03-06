open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

// #load "..\..\ToolForTtsProject\ToolForTtsProject\ToolForTtsProject.fs"
open ToolForTtsProject
[<Tests>]
let preLoadProjectTest =
    testList "preLoadProject tests" [
        testCase "preLoadProject/base" <| fun _ ->
            let mocks =
                [
                    "global.lua"

                    "id1.xml"
                    "id1.lua"

                    "some_name.id2.lua"
                ]
                |> List.map (fun fileName ->
                    let fileTyp =
                        SourceFileName.Parser.start fileName
                        |> Either.getOrDef' (failwithf "%A")
                    fileTyp, sprintf "content of %s" fileName)

            let act =
                mocks
                |> preLoadProject
            let exp =
                Right
                  { Global = {| LuaContent = Some "content of global.lua"
                                XmlContent = None |}
                    Xml = Map [("id1", ({ Name = None
                                          Guid = "id1"
                                          Ext = Xml }, "content of id1.xml"))]
                    Lua =
                         Map
                           [("id1", ({ Name = None
                                       Guid = "id1"
                                       Ext = Lua }, "content of id1.lua"));
                            ("id2", ({ Name = Some "some_name"
                                       Guid = "id2"
                                       Ext = Lua }, "content of some_name.id2.lua"))] }

            Assert.Equal("", exp, act)

        testCase "preLoadProject/same id" <| fun _ ->
            let mocks =
                [
                    "id1.lua"
                    "same_id.id1.lua"
                ]
                |> List.map (fun fileName ->
                    let fileTyp =
                        SourceFileName.Parser.start fileName
                        |> Either.getOrDef' (failwithf "%A")
                    fileTyp, sprintf "content of %s" fileName)

            let act =
                mocks
                |> preLoadProject
            let exp =
                Left "id1.Lua и same_id.id1.Lua have the same id"

            Assert.Equal("", exp, act)
    ]

[<EntryPoint>]
let main arg =
    defaultMainThisAssembly arg
