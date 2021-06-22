# FsharpWorks.Prelude

A library of standard useful function

This service uses [Fake](https://fake.build/) as a build system (an analogy of Cake, Nuke, Shake et. al.)

## Building a project

A full build is described in [Build.fs](./Build/Build.fs) and can be triggered as

Before running the build for the very first time one should probably ask Dotnet to restore tools:

```bash
$ dotnet tool restore
```

After that the project can be built as:

```bash
$ dotnet run build
```

### Build targets

All the build targets can be invoked via `dotnet run <target-name>` command.

Some of the targets are:

`dotnet run format` - formats F# code in all the projects within `./src` folder

`dotnet run build` - Builds the solution

`dotnet run tests` - Runs tests across the solution

`dotnet run publish` - Publishes the service into `./publish` folder for binary distribution

`dotner run githubrelease` - Creates a release on GitHub. Can only be used in a context of GH Actions.

