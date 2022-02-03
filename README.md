# cactus.purge

## Introduction

Most of this package has been written while waiting for data processing tasks
to complete; data tasks that have been necessary to do mostly daily for the
past year or so. A need therefore arose for relaxing therapeutic energy
outlet, and no other suitable R package was found.

## Execution

* Install the package with `devtools::install_github("weshinsley/cactus.purge")`
* Open a terminal or a Windows Command Prompt, of size 80 x 30 characters or more.
On Mac/Linux I believe you just drag the size out. On Windows, right click on the
top of the Command Prompt, Proprties, Layout tab, and set Windows Size there.
* From your terminal/command prompt: `Rscript -e "cactus.purge::launch()"`

### If 'RScript' is not recognised....
* You'll need `Rscript` to be in your path. On Windows,
click Start, and start to type `env` - and you should see `Edit the System
Environment Variables`, Click `Environment Variables`, and look for `Path` in the
top window. `Edit`, `New`, `Browse`, and find a folder similar to 
`This PC: C:\Program Files\R\R-4.1.2\bin`, then click `OK` until everything 
goes away. Open a new Command Prompt and try again.

## License
[MIT](https://choosealicense.com/licenses/mit/)
