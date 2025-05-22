## Using multi-objective reinforcement learning to inform reservoir operations for mitigation of saltwater intrusion

This repository contains data and code for the first chapter of my dissertation. The project is mainly implemented using R code, and packages are managed using `renv`. The first main section involves Bayesian inference, and is implemented using the `cmdstanr` package, that requires installation of a back-end software interface.

## Getting Started

### Prerequisites

To clone and run this project, you need:

-   R (version \>= 4.0.0)

-   RStudio (recommended)

-   cmdstanr (for Bayesian modeling)

-   git (for cloning the repository)

### Installation Steps

1.  **Clone the repository in a terminal (RStudio or other)**

```{bash}
git clone https://github.com/EthanHeidtman/Chapter1.git <YourDesiredFolder>
cd YourDesiredFolder
```

2.  **Open the project in RStudio:**

-   Double click the `.Rproj` file or use this line in the terminal:

```{bash}
rstudio repo_name.Rproj
```

3.  **Install `renv` if it isn't already (in the RStudio Console):**

```{r}
install.packages('renv')
```

4.  **Restore the Project's environment (in the RStudio Console):**

```{r}
renv::restore()
```

-   This will restore the project's environment, all of the packages and dependencies that are specified in the `renv.lock` file

5.  **Install the `cmdstanr` package (in the RStudio Console):**

```{r}
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

-   You will also need to install the back-end software CmdStan (in the RStudio console):

```{r}
cmdstanr::install_cmdstan()
```

-   Make sure to add the CmdStan path to your environment if not already configured

### Testing the Installation

To verify proper setup and initialization, run the following in RStudio's console

```{r}
library(cmdstanr)
cmdstanr::cmdstan_path()
```

-   You should see the path to the CmdStan installation. If not, refer to the [cmdstanr installation guide](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) for troubleshooting and further reference

## Running the Project

For your convenience, you can run the entire project with a single script, once the environment is restored and CmdStan is set up. Data are preprocessed and tidal fitting are already performed (see `Scripts/R/TidyingData.R`, `Scripts/R/TidalFitting.R`, and `Scripts/R/FinalHourlyData.R`), so `Scripts/MasterScript.R` runs the entire project from that point, starting with the creation of salt prediction models.

Simply run the following command in the RStudio Console:

```{r}
source(Scripts/MasterScript.R)
```

This will execute all necessary steps in the correct order and produce all figures and outputs.

## Troubleshooting

If you encounter issues related to missing packages, ensure that `renv` has correctly restored the environment. Try this in the RStudio Console:

```{r}
renv::diagnostics()
renv::restore()
```

For `cmdstanr` issues, check the installation using (in the RStudio Console):

```{r}
cmdstanr::cmdstan_version()
```
