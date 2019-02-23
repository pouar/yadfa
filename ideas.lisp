;;;; This file mostly contains ideas for in game content, mostly in the form of quotes.

;;;comments are included in this thing for people who are out of the loop
'(The-Turnip
    ;; Didn't mean for this to be so long, but Windows had so many faults that it was hard not to. This is just the tip of the iceberg.
     "A new Linux distro has been announced to make it more like Windows since Linux is `too hard'. It has plenty of new Windows-like features.

First up is the new package format, an SQL relational database. Based on Windows design principals, those make the perfect package format.

The structure is so damn easy and transparent we had to invent an XML to SQL converter to use it.

We also simplified the part that determines the directory layout. Instead of storing the files in a directory tree that represents the root directory to figure out where everything goes. You now give each and every file its own sql table where you enter all the details about all those individual files, in that table you give it a reference to an external key which is stored in a separate table called the directory table, where you list every single directory known to man (or at least the package file).

As for the package database itself, we'll just do what Windows does and dump the contents of ram to disk because it's faster. Also, the new instalation system is self updating, but can't update itself while it's running, so you need to find some way to run it without running it.

How do you use this new easy to use installer to install packages you ask? with this simple and easy to use C function of course:
```C
LinInstInstallPackageEx(LPTSTR lpPackageName, LPTSTR lpPackagePath, BOOL sync, BOOL upgrade, DWORD reserved, LPSECURITY_ATTRIBUTES lpSecurityAttributes, LPMYSTERY lpMysteryPointer);
```
here is the descriptions of the parameters

lpPackageName: name of package in repo to install. Set this to NULL when installing from a file

lpPackagePath: path to package file to install, set this to NULL when installing from a repo

sync: whether to update the package database first

upgrade: whether to upgrade all the packages first

reserved: reserved, must be 0

lpSecurityAttributes: security attributes to set, you probably don't have any, so set this to NULL

lpMysteryPointer: No idea what this does, so just set this to NULL

here's an example of the function in action
```C
LinInstInstallPackageEx(\"gimp\", NULL, 0, 0, 0, NULL, NULL);
```
Yep, all those NULLs and 0s are totally necessary

We even revamped the shell to behave more like cmd.exe since Windows is clearly easier to use, and therefore making the cli like Windows will make it easier

We changed `#' to behave like `:' on some systems but not on other systems

The shell now reads the entire file, executes the first command, reads the entire file again, executes the next command, reads the entire file again, executes the next command, etc.

You're supposed to get to the previous command in the history by hitting down instead of up occasionally right?

Command prompt windows are now limited to 80 columns in width, because nobody uses anything longer than that right?

Instead of putting our executables in one standard location, we decided to spread them out across different directories. so instead of typing `somecommand', you're now typing `/c/Program Files (x86)/My Awesome Program/Some Command'. Isn't that way better?

All that crap and more."

     "A new college has opened up, teaching you highly advanced programming skills such as using scanf for input and using Microsoft products"

     ;; this used to be a thing
     ;; https://torrentfreak.com/and-when-even-the-death-penalty-doesnt-deter-copying-what-then-110807
     ;; wouldn't be surprised if they did try to bring this back if they thought they could get away with it
     "The copyright industry has brought back the breaking wheel as punishment for piracy. Many human rights groups question the ethics of this punishment while the copyright industry's supporters, mostly just parroting the copyright industry, say they need to be tough on piracy and that it's their own fault for pirating stuff so it's totally justified."
     "MIT has released a new windowing system called X. It follows the Unix philosophy of `doing one thing and doing it well'. And by that we mean it will manage your I/O ports and PCI Devices, do power management, be its own print server, be an a.out/COFF/ELF interpreter, and more. They made X network transparent, by putting the X server on the local machine, and the window manager on the remote machine. That's gotta be efficient right? The default installation brings the best desktop environment Unix has to offer, a single window with a command prompt. They even added Xprint support to glxgears so you can see how many pages your printer can print per second."

     "Election Results: Our new president is Dwayne Elizondo Mountain Dew Herbert Camacho, I mean Donald Trump."

     ;; mostly based off of these
     ;; https://www.reddit.com/r/flightsim/comments/7ywh2f
     ;; https://www.techdirt.com/articles/20180221/11392039278
     ;; https://www.reddit.com/r/flightsim/comments/78h2ak
     ;; they tried this before with the PMDG MD11, back when the head of FSLabs used to be the head developer at PMDG,
     ;; another group of flight sim addon developers, but that time it deleted all your textures instead of installing
     ;; a password harvester, and it mistook a lot of paying customers for pirates then too
     ;; https://www.avsim.com/forums/topic/454030-pmdg-md11-deletes-entire-texture-folder
     "Database Leak: We had to do a password reset on all of our subscribers. Details on how that happened:
\\begin{flashback}
\\begin{README}
Good news from Pegasus Sim Lavatories. As part of this new update, We've included a password harvester as \"DRM\" that will run on everyone's ^[^?^[^?every pirate's ^[^?^[^?^[^?this one file sharer's system. It transfers their password database to a secure system securely (Sending it over plain HTTP in base64 to a Windows Server with RDP exposed to the world is considered \"secure\" right?). We promise it won't trigger on our paying customers' systems this time (Not that we actually fixed that). Enjoy our new update!!!^Xh^?Just a normal update, nothing to see here
\\end{README}
Looks safe to me
*tries to apply the update, \"real time\" antivirus goes off*
Oh who cares what you think, you're just some antivirus that I rely on
*disables the antivirus and applies it anyway*
\\end{flashback}
also, maybe hacking Chromium to act as our password database wasn't such a good idea"
     "Someone invented a time machine and brought an old Lisper from olden times into the future. Here's what he had to say about the wonders of modern technology `What the fuck happened?' He was amazed with today's computer software that is more complex, less customizable and extendable, harder to debug, and lags all the time. But at least hardware got a lot faster.")

'"You see a bunch of people with Windows and Mac laptops, mostly refugees from GNOME. One half were scared back to Windows and the other half wanted a real Mac."

;;this one takes place at Freddy Fazbear's Pizza
'"Freddy Fazbear jumps up!!! *LOUD SCREECH!!!*. Ok, apparently jumpscares don't work very well in text based games."

;;;A plist containing several locations that appeared in a dream I had. They looked pretty cool so I might put them in the game
'(:candle-carnival "An amusement park located in the sky, has a giant pool, various rides, and is powered by monkies"
     :sky-base "A giant base located in orbit, has a main structure with various platforms attached to it, similar to the Comet Observatory in Super Mario Galaxy, but bigger with the platforms spread farther appart. Has a special gravity simulator that pulls you back up if you fall off, kinda like a trampoline. You can use this to move between various platforms"
     :star-city "A giant city in space. All the buildings and parks are on floating platforms. You use a space ship or jetpack to travel between the different platforms")
'("Good news, Our Brainwashing^[^?Regression therapy service is now sponsered by the Music And Film Association of America (aka MAFIAA) and so we're now bundling a few new services with it, so we need you to put up these `motivational posters' in the regression therapy room"
     "*looks through the propaganda posters* `Sharing Is Piracy' `Drm Is Your Friend And Only Affects Pirates' `The Raspberry Pi Is An Illegal Streaming Device Filled With Malware' Doesn't this cross a line?"
     "But... Money"
     "Good point *puts up the posters*")
'("Ok, these people have been brainwashing people for the MAFIAA. We need a way to debrainwash them. Any ideas?"
    "Well, it turns out that the concept of brainwashing is total bullshit and doesn't really work, but that doesn't really matter as long as the victim is stupid enough. As for fixing this, maybe we could convince them that we can talk to plants. It worked when they belived those Brawndo commercials")
