[![Build status: Windows](https://img.shields.io/appveyor/ci/pouar/yadfa/master.svg?style=plastic&logo=appveyor&label=appveyor)](https://ci.appveyor.com/project/pouar/yadfa/branch/master)
[![Build status: Linux](https://img.shields.io/gitlab/pipeline/pouar/yadfa/master.svg?logo=gitlab&style=plastic)](https://gitlab.com/pouar/yadfa/pipelines)

A diaperfur/babyfur themed text adventure/rpg game I'm working on, you play it by typing in Lisp functions, similar to those text adventure games you play by typing in commands, but in Lisp form

Right now, most of the core of the game is done, but is missing a lot of content, and I might need help thinking up content for the game.

So far this has only been tested on Linux and Windows 10, but should be portable enough. I got it to successfully run in SBCL, CCL, and ECL, but I couldn't generate a working image in ECL yet.

Install instructions
====================

for Linux users
--------------------------


I've added a flatpak repo for Linux users, you can add it with

```
flatpak remote-add --if-not-exists yadfa https://gitlab.com/pouar/yadfa/raw/master/flatpak/yadfa.flatpakrepo
```

then install the game with

```
flatpak install yadfa net.pouar.yadfa
```

you can update the game with (it will also be updated when you run `flatpak update` which updates all packages installed with flatpak)

```
flatpak update net.pouar.yadfa
```

you can uninstall the game with 

```
flatpak uninstall net.pouar.yadfa
```

for Windows users
--------------------------

I've also added a chocolatey repo for Windows users. You can add the repo from PowerShell with

```
choco source add -n yadfa -s https://api.bintray.com/nuget/pouar/yadfa-chocolatey
```

then install the game with

```
choco install yadfa
```

you can upgrade the game with (it will also be upgraded when you run `choco upgrade all` which upgrades all applications installed with chocolatey)

```
choco upgrade yadfa
```
or uninstall it with

```
choco uninstall yadfa
```
