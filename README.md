# cactus.purge

## Introduction

This package commemorates "The Great Cactus Purge of 2020" in which data 
scientists across the globe obliterated the cactus operator (`%<>%`) from
their code, in an attempt to make their function arguments and returns
more explicit. 

It was a significant and critical time for epidemiology data scientists, 
performing many late night data processing tasks, waiting for the inputs
to arrive, if they ever did, coping with the inevitable changes in date
format, column headers, and general interpretation of data, or the
absence of data. 

This package was designed to provide therapy to those scientists concerned
with both the real-time modelling, and _the purge_. It is, also, some might 
say, the pivotal moment where R at last reveals its true nature as a 
potential retro-gaming platform. 

## Requirements

* The package has been developed on Windows; it should also work nicely on 
  Mac OS.

* On Linux, audio support is tricky. 
  You may have some luck with `sudo apt-get install portaudio19-dev` but
  so far it looks sketchy. Sometimes it works for a while then 
  crashes, or sometimes the audio driver chatters onto stdout, which
  makes a mess on the game screen and disrupts cursor control. 
  
  Linux users might need to turn sound off on the front page, to get a 
  pleasant (but sadly quiet) gaming experience. I suggest in this sad case,
  you make your own atmospheric sound effects as you play, and have a look
  in the `inst/audio` folder for hints of what you're missing.

  Feedback or suggestions to address this terrible problem are welcome.
  I'm not sure if WINE gives any hope?

## Execution

```
install.packages("devtools")   # if necessary
devtools::install_github("weshinsley/cactus.purge")
```

* Open a terminal or a Windows Command Prompt, of size 80 x 30 characters or 
  more. On Mac/Linux I believe you just drag the size out. On Windows, right
  click on the top of the Command Prompt, Proprties, Layout tab, and set
  the window size there. If you have a snazzy themed terminal, you may 
  want to also somehow set the background colour to black.

* From your terminal/command prompt: `Rscript -e "cactus.purge::launch()"`

* Note that you can't play Cactus Purge within RStudio; it must be run from
  a terminal-like environment - including within the command-line `R` itself 
  if you like.

* On Windows, the first time you run Cactus Purge, a registry key may
  need to be set to allow coloured output in your Command Prompt window.
  The game will do this for you if you ask. IF you prefer to do it yourself, 
  this will work from the command-line,
```
REG ADD HKCU\CONSOLE /f /v VirtualTerminalLevel /t REG_DWORD /d 1
```
  or, run `regedit`, and go to `Computer\HKEY_CURRENT_USER\Console` and
  set the `VirtualTerminalLevel` to `1`.

## If 'RScript' is not recognised....

* You are probably on windows, and need `Rscript` to be in your path. 
  Click Start, and begin typing `env` - you should see `Edit the System
  Environment Variables`, Click `Environment Variables`, and look for 
  `Path` in the top window. `Edit`, `New`, `Browse`, and find a folder
  somewhat similar to `This PC: C:\Program Files\R\R-4.2.2\bin`, then 
  click `OK` until everything goes away. Open a new Command Prompt and 
  try again.

## Performance

* Terminals are not that fast, surprisingly. You would think writing 
  characters to a console would be speedy. Additionally, it seems that
  R sometimes incrementally slows down while playing - not clear why
  at this stage. Restart to renew fast and slick frame rates.

## License
[MIT](https://choosealicense.com/licenses/mit/)
