// --------------------------------------------------------------------------------------
// A simple FAKE build script that:
//  1) Hosts Suave server locally & reloads web part that is defined in 'app.fsx'
//  2) Deploys the web application to Azure web sites when called with 'build deploy'
// --------------------------------------------------------------------------------------

#r "packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/Suave/lib/net40/Suave.dll"

// https://github.com/fsharp/FAKE/issues/739
#I "packages/FAKE/tools/"
#r "packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.OpenCoverHelper
open Fake.ProcessHelper

open System
open System.IO
open System.Net
open Suave
open Suave.Web
open Microsoft.FSharp.Compiler.Interactive.Shell

// Properties
let buildDir = "./build/"
let testDir = "./test/"
let coverageDir = buildDir + "coverage/"

// Pattern specifying assemblies to be tested using xunit
let testAssemblies = buildDir @@ "*Test*.dll"

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir]
)

Target "BuildApp" (fun _ ->
   !! "**/*.fsproj"
     |> MSBuildRelease buildDir "Build"
     |> Log "AppBuild-Output: "
)


// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    !! testAssemblies 
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            })
)

// see http://cdroulers.com/blog/2015/05/06/net-code-coverage-with-opencover-and-bamboo/
Target "RunUnitTests" (fun _ ->
    let assembliesToTest = (" ", (!! (buildDir + "/*Test*.dll"))) |> System.String.Join
    CreateDir coverageDir
    CreateDir testDir
    let nCoverDir = coverageDir + "ncover/"
    CreateDir nCoverDir
    trace "Run OpenCover with NUnit"
    OpenCover
        (fun p -> { p with 
                            ExePath = "./packages/OpenCover/tools/OpenCover.Console.exe"
                            TestRunnerExePath = "./packages/NUnit.Runners/tools/nunit-console-x86.exe"
                            Output = coverageDir + "results.xml"
                            Register = RegisterUser
                            Filter = "+[*]* -[*.Tests*]*"
                  })
        ("/nologo /noshadow /framework=net-4.5.1 /result=" + testDir + "nunit-results.xml /output=" + testDir + "nunit-output.txt " + assembliesToTest)

    trace "Generate OpenCover report"
    Shell.Exec(@".\packages\ReportGenerator\tools\ReportGenerator.exe ", args = coverageDir + "results.xml " + coverageDir + "html") |> ignore

(*
    trace "Generate NCover output"
    let xslDoc = new System.Xml.Xsl.XslCompiledTransform()
    xslDoc.Load "opencover-to-ncover.xslt"
    xslDoc.Transform(coverageDir + "results.xml", nCoverDir + "results.xml")
*)    
)

// --------------------------------------------------------------------------------------
// The following uses FileSystemWatcher to look for changes in 'app.fsx'. When
// the file changes, we run `#load "app.fsx"` using the F# Interactive service
// and then get the `App.app` value (top-level value defined using `let app = ...`).
// The loaded WebPart is then hosted at localhost:8083.
// --------------------------------------------------------------------------------------
let appFsx = __SOURCE_DIRECTORY__ @@ "DustFsNews/News.fsx"
let appDir = System.IO.Path.GetDirectoryName(appFsx)

let sbOut = Text.StringBuilder()
let sbErr = Text.StringBuilder()

let fsiSession =
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)
  let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
  let argv = Array.append [|"/fake/fsi.exe"; "--quiet"; "--noninteractive"; "-d:DO_NOT_START_SERVER"|] [||]
  FsiEvaluationSession.Create(fsiConfig, argv, inStream, outStream, errStream)

let reportFsiError (e:exn) =
  traceError "Reloading script failed."
  traceError (sprintf "Message: %s\nError: %s" e.Message (sbErr.ToString().Trim()))
  sbErr.Clear() |> ignore

let reloadScript () =
  try
    traceImportant "Reloading script..."
    fsiSession.EvalInteraction(sprintf "#load @\"%s\"" appFsx)
    fsiSession.EvalInteraction("open News")
    match fsiSession.EvalExpression("app") with
    | Some app -> Some(app.ReflectionValue :?> WebPart)
    | None -> failwith "Couldn't get 'app' value"
  with e -> reportFsiError e; None

// --------------------------------------------------------------------------------------
// Suave server that redirects all request to currently loaded version
// --------------------------------------------------------------------------------------

let currentApp = ref (fun _ -> async { return None })

let serverConfig =
  { defaultConfig with
      homeFolder = Some appDir
      //logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Debug
      bindings = [ HttpBinding.create HTTP IPAddress.Loopback 8083us] }

let reloadAppServer () =
  reloadScript() |> Option.iter (fun app ->
    currentApp.Value <- app
    traceImportant "New version of script loaded!" )

Target "run" (fun _ ->
  let app ctx = currentApp.Value ctx
  let _, server = startWebServerAsync serverConfig app

  // Start Suave to host it on localhost
  reloadAppServer()
  Async.Start(server)
  // Open web browser with the loaded file
  System.Diagnostics.Process.Start("http://localhost:8083") |> ignore

  // Watch for changes & reload when script changes
  use watcher = !! (appDir @@ "*.*") |> WatchChanges (fun _ -> reloadAppServer())
  traceImportant "Waiting for edits. Press any key to stop."
  //System.Console.ReadLine() |> ignore
  System.Threading.Thread.Sleep(System.Threading.Timeout.Infinite)
)

// --------------------------------------------------------------------------------------
// Targets for running build script in background (for Atom)
// --------------------------------------------------------------------------------------

open System.Diagnostics

let runningFileLog = __SOURCE_DIRECTORY__ @@ "build.log"
let runningFile = __SOURCE_DIRECTORY__ @@ "build.running"

Target "spawn" (fun _ ->
  if File.Exists(runningFile) then
    failwith "The build is already running!"

  let ps =
    ProcessStartInfo
      ( WorkingDirectory = __SOURCE_DIRECTORY__,
        FileName = __SOURCE_DIRECTORY__  @@ "packages/FAKE/tools/FAKE.exe",
        Arguments = "run --fsiargs build.fsx",
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false )
  use fs = new FileStream(runningFileLog, FileMode.Create, FileAccess.ReadWrite, FileShare.Read)
  use sw = new StreamWriter(fs)
  let p = Process.Start(ps)
  p.ErrorDataReceived.Add(fun data -> printfn "%s" data.Data; sw.WriteLine(data.Data); sw.Flush())
  p.OutputDataReceived.Add(fun data -> printfn "%s" data.Data; sw.WriteLine(data.Data); sw.Flush())
  p.EnableRaisingEvents <- true
  p.BeginOutputReadLine()
  p.BeginErrorReadLine()

  File.WriteAllText(runningFile, string p.Id)
  while File.Exists(runningFile) do
    System.Threading.Thread.Sleep(500)
  p.Kill()
)

Target "attach" (fun _ ->
  if not (File.Exists(runningFile)) then
    failwith "The build is not running!"
  use fs = new FileStream(runningFileLog, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
  use sr = new StreamReader(fs)
  while File.Exists(runningFile) do
    let msg = sr.ReadLine()
    if not (String.IsNullOrEmpty(msg)) then
      printfn "%s" msg
    else System.Threading.Thread.Sleep(500)
)

Target "stop" (fun _ ->
  if not (File.Exists(runningFile)) then
    failwith "The build is not running!"
  File.Delete(runningFile)
)

// --------------------------------------------------------------------------------------
// Minimal Azure deploy script - just overwrite old files with new ones
// --------------------------------------------------------------------------------------

Target "deploy" (fun _ ->
  let sourceDirectory = System.IO.Path.GetDirectoryName(appFsx)
  let wwwrootDirectory = __SOURCE_DIRECTORY__ @@ "../wwwroot"
  CleanDir wwwrootDirectory
  CopyRecursive sourceDirectory wwwrootDirectory false |> ignore
)


// Dependencies
"BuildApp"
  ==> "RunTests"    
  ==> "run"

RunTargetOrDefault "run"
