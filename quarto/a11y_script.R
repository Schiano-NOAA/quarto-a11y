# Script to add accessibility to quarto docs from asar fxns
setwd(here::here())

# render quarto doc if not already done

# Load in functions
source("a11y_fxns.R")

# Run on our quarto produced tex file
add_alttext(
  x = "quarto_a11y.tex",
  alttext_csv_dir = getwd(),
  compile = FALSE
)
add_tagging(
  x = "quarto_a11y.tex",
  compile = TRUE
)
