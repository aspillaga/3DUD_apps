
# Shiny apps to visualize 3D-UDs

In this repository, we provide three shiny apps that allow exploring the 3D-UDs obtained for the tagged common dentex (*Dentex dentex*) individuals referred in the main manuscript. Shiny apps can be directly run in any web browser using the http://www.shinyapps.io platform, or downloaded and locally executed from the GitHub repository using RStudio and the `shiny` package for `R`. This second option is more stable but takes longer as data has to be downloaded.


## App #1: Individual 3D-UDs of common dentex

Shiny app to visualize and compare the 3D-UDs of the common dentex individuals tagged in the Medes Islands MPA (NW Mediterranean Sea).

**URL:** https://aspillaga.shinyapps.io/3DUD_individuals/

**Download and run from GitHub:**

```{r 3DUD_individuals, eval = FALSE}
shiny::runGitHub(repo = "aspillaga/3DUD_apps", subdir = "3DUD_individuals")
```


## App #2: Method comparison

Shiny app to compare the 3D-UDs of common dentex individuals obtained by applying the new method incorporating the topography, by applying the method but using a null topography, and by applying a 3D kernel to the original telemetry data after calculating centres of activity (CoA).

In can be directly run from the following URL:

**URL:** https://aspillaga.shinyapps.io/3DUD_methods/

**Download and run from GitHub:**

```{r 3DUD_methods, eval = FALSE}
shiny::runGitHub(repo = "aspillaga/3DUD_apps", subdir = "3DUD_methods")
```


# App #3: Spawning

Shiny app to visualize the weekly and day-night 3D-UDs of common dentex individuals and explore their behaviour before and during the spawning season.

**URL:** https://aspillaga.shinyapps.io/3DUD_weeks_spawning/

**Download and run from GitHub:**

```{r 3DUD_spawning, eval = FALSE}
shiny::runGitHub(repo = "aspillaga/3DUD_apps", subdir = "3DUD_weeks_spawning")
```

