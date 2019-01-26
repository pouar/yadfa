[![Build status: Windows](https://img.shields.io/appveyor/ci/pouar/yadfa/master.svg?style=plastic&logo=appveyor&label=appveyor)](https://ci.appveyor.com/project/pouar/yadfa/branch/master)
[![Build status: Linux](https://img.shields.io/gitlab/pipeline/pouar/yadfa/master.svg?logo=gitlab&style=plastic)](https://gitlab.com/pouar/yadfa/pipelines)
[![Download For Win64](https://img.shields.io/badge/dynamic/json.svg?label=download&url=https%3A%2F%2Fapi.bintray.com%2Fpackages%2Fpouar%2Fyadfa-generic%2Fwin64%2Fversions%2F_latest&query=name&colorB=blue&style=plastic&logo=windows)](https://bintray.com/pouar/yadfa-generic/win64/_latestVersion)

A diaperfur/babyfur themed text adventure/rpg game I'm working on, you play it by typing in Lisp functions, similar to those text adventure games you play by typing in commands, but in Lisp form

Right now, most of the core of the game is done, but is missing a lot of content, and I might need help thinking up content for the game.

So far this has only been tested on Linux and Windows 10, but should be portable enough. I got it to successfully run in SBCL, CCL, and ECL, but I couldn't generate a working image in ECL yet.

I've added a flatpak repo, you can add it with

```
flatpak remote-add --if-not-exists yadfa https://cgit.pouar.net/yadfa.git/plain/flatpak/yadfa.flatpakrepo
```

then install the game with

```
flatpak install yadfa net.pouar.yadfa
```
