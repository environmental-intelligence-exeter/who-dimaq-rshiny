# Set pkgs
pkgs = c(
  "shiny",
  "bslib",
  "dplyr",
  "viridis",
  "sf",
  "DT",
  "shinyWidgets",
  "shinycssloaders",
  "plotly",
  "threejs"
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
excceed = readRDS("data/exceed_10_15_25_35.RDS")
concentration = readRDS("data/concentrations_exposure.RDS")
countries = as.list(unique(grid_prediction$CountryName))

ggplot(data = concentration %>% filter(byvar == "Algeria") %>% filter(UrbanRural == "Overall"), aes(Year,Mean))   +
  geom_point() +
  geom_line(aes(y=LowerCI), color = "red", linetype = "dashed") +
  geom_line(aes(y=UpperCI), color = "red", linetype = "dashed") +
  geom_line(aes(y=LowerPI), color = "blue", linetype = "dashed") +
  geom_line(aes(y=UpperPI), color = "blue", linetype = "dashed") +
  geom_smooth(method=lm, se=TRUE)
