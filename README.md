This is the repository containing the data and full analysis code used
for our
[publication](https://ejnmmires.springeropen.com/articles/10.1186/s13550-017-0304-1):

> Matheson GJ, Plavén-Sigray P, Forsberg A, Varrone A, Farde L,
> Cervenka S. Assessment of simplified ratio-based approaches for
> quantification of PET \[<sup>11</sup>C\]PBR28 data. EJNMMI research.
> 2017 Dec 1;7(1):58.

The analysis begins after time activity curves have been extracted from
the dynamic PET images, as this is a reanalysis of a data set which was
[previously published
on](https://link.springer.com/article/10.1007%2Fs00259-015-3149-8). The
data is supplied in the RawData folder, and the analysis notebook is
provided in the R folder.

Software Requirements
---------------------

### Programs

The analysis is performed using R. For those who have not used R before,
you should start by downloading [R](https://www.r-project.org/), and
then [RStudio](https://www.r-project.org/). If you wish to compile the
report as a pdf, you will also need [MiKTeX](https://miktex.org/).

### Packages

You will then need several packages for the analysis: these can be
installed in R from CRAN: an archive of R packages. This can be
performed using the following syntax:

    install.packages("packagename")

Packages are loaded using the following syntax:

    library(packagename)

You should download the packages which are loaded at the start of the
analysis before running the analysis.

Several packages and/or versions of packages used in the analysis are
not available from CRAN and are instead hosted on GitHub. These can be
installed directly using the *devtools* package using the following
syntax:

    library(devtools)
    install_github("username/packagename")

Downloading Everything
----------------------

The best way to download everything is first to download Git if you
don't already have it, and then to clone the repository using the
terminal (or Git Bash on Windows):

    git clone https://github.com/mathesong/PBR28_RatioMethods.git

At this point, you will have downloaded the repository to your computer.
If you have not used Git before, I recommend that you follow [this short
tutorial](https://try.github.io/levels/1/challenges/1) to get you
started.

Reproducing the Analysis
------------------------

### Viewing the Code and Figures

To simply view the code and figures, just click on the R folder within
GitHub. I have rendered everything in markdown as the README file, so
you can view everything without even cloning the repository.

### Running the Code

You should run all code from the R folder. You can set your R
environment working directory there using the File Explorer in RStudio,
and then clicking More &gt; Set as Working Directory.

#### Running code blocks step-by-step

Open R/Modelling\_and\_Analysis.Rmd in RStudio, and click the play
buttons in the top right of each code block to step through the code.

#### Rendering the analysis report documents

Either click on Knit and select the desired output format in RStudio
with R/Modelling\_and\_Analysis.Rmd. Otherwise, run the R/renderFiles.R
script.

Questions, Issues, Suggestions, etc
-----------------------------------

Please either contact me at granville.matheson\[at\]ki.se, or create an
issue on GitHub.
