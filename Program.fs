// Learn more about F# at http://fsharp.org

open System
open System.IO
open FSharp.Reflection
open FSharp.Control.Tasks.V2
open NJsonSchema
open NJsonSchema.CodeGeneration.TypeScript

[<Literal>]
let TYPE_TEMPLATE = """
export interface {{TypeName}} {
{{Properties}}
}
"""

[<Literal>]
let TYPE_TEMPLATE_UNION = """
export type {{TypeName}} = {{Properties}}
"""

type Basic = {
    name: string
}

type Complicated = {
    name: string
    date: int
}

type DiscriminatedUnionType =
    | Basic of Basic
    | Complicated of Complicated

let write path content = File.WriteAllText (path, content)

let join (separator: string) (values: #seq<string>) = String.Join(separator, values)

let schemaToTypeScript (schema: JsonSchema) =
    let generator = TypeScriptGenerator schema
    generator.Settings.TypeStyle <- TypeScriptTypeStyle.Interface
    generator.GenerateFile ()

let replaceNames (``type``: Type) (replace: string) (payload: string) =
    payload.Replace (sprintf "interface %s" ``type``.Name, sprintf "interface %s" replace)

let (|Regular|DiscriminatedUnion|) ``type`` =
    if FSharpType.IsUnion ``type`` 
    then DiscriminatedUnion
    else Regular

let rec generateStr ``type`` =
    match ``type`` with
    | Regular -> generateStrRegularType ``type``
    | DiscriminatedUnion -> generateStrDiscriminatedUnionType ``type`` 
and generateStrRegularType ``type`` =
    JsonSchema.FromType ``type``
    |> schemaToTypeScript
and generateStrDiscriminatedUnionType ``type`` =
    let cases =
        FSharpType.GetUnionCases ``type``
        |> Array.map (
            fun case ->
                let field = case.GetFields().[0] 
                let typeName = sprintf "%s_%s" ``type``.Name case.Name
                let typeDefinition =
                    generateStr field.PropertyType
                    |> replaceNames field.PropertyType typeName
                typeName, case.Name, typeDefinition
        )

    let typeDefinitions = 
        cases 
        |> Array.map (fun (_, _, typeDefinition) -> typeDefinition)

    let properties =
        cases
        |> Array.map (
            fun (typeName, propName, _) ->
                sprintf "    %s?: %s;" propName typeName
        )
        |> join "\n"

    // let properties =
    //     cases
    //     |> Array.map (
    //         fun (typeName, _, _) -> typeName
    //     )
    //     |> join " | "

    [
        yield! typeDefinitions
        yield 
            TYPE_TEMPLATE
            // TYPE_TEMPLATE_UNION
                .Replace(
                    "{{TypeName}}",
                    ``type``.Name
                )
                .Replace(
                    "{{Properties}}",
                    properties
                )
    ]
    |> join "\n"

let generate types =
    types
    |> List.map generateStr
    |> join "\n\n"
    |> write "./out.ts"

[<EntryPoint>]
let main argv =
    generate [
        typeof<Basic>
        typeof<Complicated>
        typeof<DiscriminatedUnionType>
    ]
    0 // return an integer exit code
