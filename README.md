[![Build status](https://img.shields.io/appveyor/ci/pouar/yadfa/master.svg?style=plastic&logo=appveyor&label=appveyor)](https://ci.appveyor.com/project/pouar/yadfa/branch/master)
[![Download For Win64](https://img.shields.io/bintray/v/pouar/yadfa-generic/win64.svg?style=plastic&logo=windows&label=download)](https://bintray.com/pouar/yadfa-generic/win64/_latestVersion)

A diaperfur/babyfur themed text adventure/rpg game I'm working on, you play it by typing in Lisp functions, similar to those text adventure games you play by typing in commands, but in Lisp form

Right now, most of the core of the game is done, but is missing a lot of content, and I might need help thinking up content for the game.

So far this has only been tested on Linux and Windows 10, but should be portable enough. I got it to successfully run in SBCL, CCL, and ECL, but I couldn't generate a working image in ECL yet.

I've added a flatpak repo on bintray, you can add it with

```
flatpak remote-add --if-not-exists yadfa https://gitlab.com/pouar/yadfa/raw/master/.yadfa.flatpakrepo
```

then install the game with

```
flatpak install yadfa net.pouar.yadfa
```
