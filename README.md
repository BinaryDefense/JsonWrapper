# BinaryDefense.JsonWrapper

## What

A plugin for [Myriad](https://github.com/MoiraeSoftware/myriad) for generating statically typed lossless wrappers around JToken given a schema.

## Why

When serializing JSON directly into known record types, this can cause the underlying dataset to be lost. Given a JSON object:

```json
{
    "Name" : "James Kirk",
    "Age"  : 32
}
```

and a [record type](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records):

```fsharp

type Person = {
    Name : string
    Age : uint32
}
```

serializing is fine. However, if the underlying json adds more information in it's payload such as: 

```json
{
    "Name" : "James Kirk",
    "Age"  : 32,
    "UniformColor": "Gold"
}
```

If that type is then serialized and saved somewhere, our record type will lose the `UniformColor` field, causing this to be a lossy conversion.

This Myriad plugin will instead generate [a class](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/classes) given a record type as a schema.

## How

Add `BinaryDefense.Myriad.Plugins.JsonWrapper` to your project. 

Given our record above as a schema, we need to add the attribute `[<Generator.JsonWrapper>]` to it.

```fsharp
[<Generator.JsonWrapper>]
type Person = {
    Name : string
    Age : uint32
}
```

Additionally, an entry to the `fsproj` file must be added:

```xml
<Compile Include="MyGeneratedFile.fs">
    <MyriadFile>MyTypes.fs</MyriadFile>
    <MyriadNameSpace>GeneratedNamespace</MyriadNameSpace>
</Compile>
```

`MyTypes.fs` is where the `Person` record type lives. `MyGeneratedFile.fs` will be the file generated with the output. `GeneratedNamespace` will be the namespace the generated code lives in. On build, Myriad [will generate the code](https://github.com/MoiraeSoftware/myriad#msbuild-usage.)  An example of it's output:

```fsharp
type Person(jtoken: JToken, serializer: JsonSerializer) =
    member this.Name
        with get () =
            let selectedToken = jtoken.["Name"]
            selectedToken.ToObject<string> serializer
        and set (newValue: string) =
            jtoken.["Name"] <- JToken.FromObject(newValue, serializer)

    member this.Age
        with get () =
            let selectedToken = jtoken.["Age"]
            selectedToken.ToObject<uint32> serializer
        and set (newValue: uint32) =
            jtoken.["Age"] <- JToken.FromObject(newValue, serializer)

    override this.GetHashCode () = jtoken.GetHashCode()

    override this.Equals(objToCompare: obj) =
        match objToCompare with
        | :? IHaveJToken as jTokenToCompare -> JToken.DeepEquals(jTokenToCompare.InnerData, jtoken)
        | _ -> false

    ///This allows the class to be pattern matched against
    member this.Deconstruct(Name: outref<int>, Age: outref<string>) =
        Name <- this.Name
        Age <- this.Age

    interface IHaveJToken with
        override this.InnerData = jtoken
```

When using this type, you'll need to also add [custom converters](https://www.newtonsoft.com/json/help/html/CustomJsonConverter.htm) to the JsonSerializer you are using throughout your application.  

```fsharp
let converters = Converters.recommendedConverters

let serializationSettings =
    let s = JsonSerializerSettings()
    scrubDefaultDUConverter s.Converters
    for c in converters do s.Converters.Add c
    s

let jsonSettings = serializationSettings
let jsonSerializer =JsonSerializer.CreateDefault looseSettings

```

This will be using the [IHaveJTokenConverter](src/BinaryDefense.JsonWrapper.Core/BinaryDefense.JsonWrapper.Core.fs) to ensure the serialization is lossless.


Additional reading on this topic: 

- [Event Sourcing Versioning](https://leanpub.com/esversioning/read#leanpub-auto-wrapper)
- [Serialization is lossy](https://web.archive.org/web/20160913235956/http://kellabyte.com/2013/05/02/serialization-is-lossy/)

### Features

- Lossless
- Override backing field name
- Enforce Required fields
- Destructuring
- Structural equals

---

## Builds

macOS/Linux | Windows
--- | ---
[![Travis Badge](https://travis-ci.org/BinaryDefense/BinaryDefense.JsonWrapper.svg?branch=master)](https://travis-ci.org/BinaryDefense/BinaryDefense.JsonWrapper) | [![Build status](https://ci.appveyor.com/api/projects/status/github/BinaryDefense/BinaryDefense.JsonWrapper?svg=true)](https://ci.appveyor.com/project/BinaryDefense/BinaryDefense.JsonWrapper)
[![Build History](https://buildstats.info/travisci/chart/BinaryDefense/BinaryDefense.JsonWrapper)](https://travis-ci.org/BinaryDefense/BinaryDefense.JsonWrapper/builds) | [![Build History](https://buildstats.info/appveyor/chart/BinaryDefense/BinaryDefense.JsonWrapper)](https://ci.appveyor.com/project/BinaryDefense/BinaryDefense.JsonWrapper)  

## NuGet 

Package | Stable | Prerelease
--- | --- | ---
BinaryDefense.JsonWrapper.Core | [![NuGet Badge](https://buildstats.info/nuget/BinaryDefense.JsonWrapper)](https://www.nuget.org/packages/BinaryDefense.JsonWrapper.Core/) | [![NuGet Badge](https://buildstats.info/nuget/BinaryDefense.JsonWrapper.Core?includePreReleases=true)](https://www.nuget.org/packages/BinaryDefense.JsonWrapper.Core/)
BinaryDefense.Myriad.Plugins.JsonWrapper | [![NuGet Badge](https://buildstats.info/nuget/BinaryDefense.Myriad.Plugins.JsonWrapper)](https://www.nuget.org/packages/BinaryDefense.Myriad.Plugins.JsonWrapper/) | [![NuGet Badge](https://buildstats.info/nuget/BinaryDefense.Myriad.Plugins.JsonWrapper?includePreReleases=true)](https://www.nuget.org/packages/BinaryDefense.Myriad.Plugins.JsonWrapper/)

---

### Developing

Make sure the following **requirements** are installed on your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

or

- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers)


---

### Environment Variables

- `CONFIGURATION` will set the [configuration](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x#options) of the dotnet commands.  If not set, it will default to Release.
  - `CONFIGURATION=Debug ./build.sh` will result in `-c` additions to commands such as in `dotnet build -c Debug`
- `GITHUB_TOKEN` will be used to upload release notes and Nuget packages to GitHub.
  - Be sure to set this before releasing
- `DISABLE_COVERAGE` Will disable running code coverage metrics.  AltCover can have [severe performance degradation](https://github.com/SteveGilham/altcover/issues/57) so it's worth disabling when looking to do a quicker feedback loop.
  - `DISABLE_COVERAGE=1 ./build.sh`


---

### Building


```sh
> build.cmd <optional buildtarget> // on windows
$ ./build.sh  <optional buildtarget>// on unix
```

The bin of your library should look similar to:

```
$ tree src/MyCoolNewLib/bin/
src/MyCoolNewLib/bin/
└── Debug
    ├── net461
    │   ├── FSharp.Core.dll
    │   ├── MyCoolNewLib.dll
    │   ├── MyCoolNewLib.pdb
    │   ├── MyCoolNewLib.xml
    └── netstandard2.1
        ├── MyCoolNewLib.deps.json
        ├── MyCoolNewLib.dll
        ├── MyCoolNewLib.pdb
        └── MyCoolNewLib.xml

```

---

### Build Targets

- `Clean` - Cleans artifact and temp directories.
- `DotnetRestore` - Runs [dotnet restore](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-restore?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- [`DotnetBuild`](#Building) - Runs [dotnet build](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- `DotnetTest` - Runs [dotnet test](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-test?tabs=netcore21) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- `GenerateCoverageReport` - Code coverage is run during `DotnetTest` and this generates a report via [ReportGenerator](https://github.com/danielpalme/ReportGenerator).
- `WatchTests` - Runs [dotnet watch](https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0) with the test projects. Useful for rapid feedback loops.
- `GenerateAssemblyInfo` - Generates [AssemblyInfo](https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.applicationservices.assemblyinfo?view=netframework-4.8) for libraries.
- `DotnetPack` - Runs [dotnet pack](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-pack). This includes running [Source Link](https://github.com/dotnet/sourcelink).
- `SourceLinkTest` - Runs a Source Link test tool to verify Source Links were properly generated.
- `PublishToNuGet` - Publishes the NuGet packages generated in `DotnetPack` to NuGet via [paket push](https://fsprojects.github.io/Paket/paket-push.html).
- `GitRelease` - Creates a commit message with the [Release Notes](https://fake.build/apidocs/v5/fake-core-releasenotes.html) and a git tag via the version in the `Release Notes`.
- `GitHubRelease` - Publishes a [GitHub Release](https://help.github.com/en/articles/creating-releases) with the Release Notes and any NuGet packages.
- `FormatCode` - Runs [Fantomas](https://github.com/fsprojects/fantomas) on the solution file.
- `BuildDocs` - Generates Documentation from `docsSrc` and the [XML Documentation Comments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/xmldoc/) from your libraries in `src`.
- `WatchDocs` - Generates documentation and starts a webserver locally.  It will rebuild and hot reload if it detects any changes made to `docsSrc` files, libraries in `src`, or the `docsTool` itself.
- `ReleaseDocs` - Will stage, commit, and push docs generated in the `BuildDocs` target.
- [`Release`](#Releasing) - Task that runs all release type tasks such as `PublishToNuGet`, `GitRelease`, `ReleaseDocs`, and `GitHubRelease`. Make sure to read [Releasing](#Releasing) to setup your environment correctly for releases.
---


### Releasing

- [Start a git repo with a remote](https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/)

```sh
git add .
git commit -m "Scaffold"
git remote add origin https://github.com/user/MyCoolNewLib.git
git push -u origin master
```

- [Create your NuGeT API key](https://docs.microsoft.com/en-us/nuget/nuget-org/publish-a-package#create-api-keys)
    - [Add your NuGet API key to paket](https://fsprojects.github.io/Paket/paket-config.html#Adding-a-NuGet-API-key)

    ```sh
    paket config add-token "https://www.nuget.org" 4003d786-cc37-4004-bfdf-c4f3e8ef9b3a
    ```

    - or set the environment variable `NUGET_TOKEN` to your key


- [Create a GitHub OAuth Token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
  - You can then set the environment variable `GITHUB_TOKEN` to upload release notes and artifacts to github
  - Otherwise it will fallback to username/password

- Then update the `CHANGELOG.md` with an "Unreleased" section containing release notes for this version, in [KeepAChangelog](https://keepachangelog.com/en/1.1.0/) format.

NOTE: Its highly recommend to add a link to the Pull Request next to the release note that it affects. The reason for this is when the `RELEASE` target is run, it will add these new notes into the body of git commit. GitHub will notice the links and will update the Pull Request with what commit referenced it saying ["added a commit that referenced this pull request"](https://github.com/TheAngryByrd/MiniScaffold/pull/179#ref-commit-837ad59). Since the build script automates the commit message, it will say "Bump Version to x.y.z". The benefit of this is when users goto a Pull Request, it will be clear when and which version those code changes released. Also when reading the `CHANGELOG`, if someone is curious about how or why those changes were made, they can easily discover the work and discussions.

Here's an example of adding an "Unreleased" section to a `CHANGELOG.md` with a `0.1.0` section already released.

```markdown
## [Unreleased]

### Added
- Does cool stuff!

### Fixed
- Fixes that silly oversight

## [0.1.0] - 2017-03-17
First release

### Added
- This release already has lots of features

[Unreleased]: https://github.com/user/MyCoolNewLib.git/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/user/MyCoolNewLib.git/releases/tag/v0.1.0
```

- You can then use the `Release` target, specifying the version number either in the `RELEASE_VERSION` environment
  variable, or else as a parameter after the target name.  This will:
  - update `CHANGELOG.md`, moving changes from the `Unreleased` section into a new `0.2.0` section
    - if there were any prerelease versions of 0.2.0 in the changelog, it will also collect their changes into the final 0.2.0 entry
  - make a commit bumping the version:  `Bump version to 0.2.0` and adds the new changelog section to the commit's body
  - publish the package to NuGet
  - push a git tag
  - create a GitHub release for that git tag

macOS/Linux Parameter:

```sh
./build.sh Release 0.2.0
```

macOS/Linux Environment Variable:

```sh
RELEASE_VERSION=0.2.0 ./build.sh Release
```


