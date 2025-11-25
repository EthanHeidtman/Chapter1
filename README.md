## Using multi-objective reinforcement learning to inform reservoir operations for mitigation of saltwater intrusion

This repository contains data and code for the first chapter of my dissertation. The project is mainly implemented using R code, and packages are managed using `renv`. 

## Getting Started

### Prerequisites

To clone and run this project, you need:

-   R (version \>= 4.0.0)

-   RStudio (recommended)

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

## Running the Project

For your convenience, you can run the entire project with a single script, once the environment is restored. Data are preprocessed and tidal fitting are already performed (see `Scripts/R/TidyingData.R`, `Scripts/R/TidalFitting.R`, and `Scripts/R/FinalHourlyData.R`), so `Scripts/MasterScript.R` runs the entire project from that point, starting with the creation of salt prediction models.

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
