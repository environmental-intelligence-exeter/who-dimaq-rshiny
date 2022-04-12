# Set pkgs
pkgs = c(
  "shiny",
  "bslib",
  "dplyr",
  "tidyr",
  "viridis",
  "sf",
  "DT",
  "shinyWidgets",
  "shinycssloaders",
  "plotly",
  "threejs",
  "leaflet",
  "leafgl",
  "raster"
)

# Install pkgs not yet installed
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

# Load pkgs
invisible(lapply(pkgs, library, character.only = TRUE))

# Load data
grid_prediction = readRDS("data/grid_prediction.RDS")
excceed = readRDS("data/exceed.rds")
concentration = readRDS("data/concentrations.rds")
who_world_map = readRDS("data/who_world_map.rds")
ground_monitors = readRDS(("data/ground_monitor.rds"))
countries = as.list(unique(grid_prediction$CountryName))



