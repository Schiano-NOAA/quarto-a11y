# asar accessibility functions
#Add alternative text into latex
add_alttext <- function(
    x = list.files(getwd())[grep(".tex", list.files(getwd()))],
    dir = getwd(),
    alttext_csv_dir = getwd(),
    compile = TRUE,
    rename = NULL) {
  # Read latex file
  if (!file.exists(file.path(dir, x))) cli::cli_abort("File {dir}/{x} does not exist!")
  tex_file <- readLines(file.path(dir, x))
  
  # Check: count instances of pattern
  # sub_count <- length(
  #   grep("1",
  #        stringr::str_count(
  #          tex_file,
  #          pattern = stringr::coll("\\pandocbounded{\\includegraphics["))
  #        )
  #   )
  
  # Check if alt text csv is where indicated
  if (!file.exists(file.path(alttext_csv_dir, "captions_alt_text.csv"))) cli::cli_abort("'captions_alt_text.csv' not found in {alttext_csv_dir}.")
  
  # Identify lines with figures
  # check if any lines have figures added
  if (!any(grepl("fig-([a-z]+|[a-z]+_[a-z]+|[a-z]+_[a-z]+_[a-z]+)-1.pdf", tex_file))) cli::cli_abort("No images/figures present in file.")
  # this approach allows us to not mistake the replacement for other figures
  # For render to pdf
  fig_lines <- grep("fig-([a-z]+|[a-z]+_[a-z]+|[a-z]+.[a-z]+)-1.pdf", tex_file) # -plot
  # for html render or external images
  fig_lines <- c(
    fig_lines,
    grep("fig-([a-z]+|[a-z]+_[a-z]+)-1.png", tex_file)
  )
  
  # If not using tagpdf (and maybe lua) - the following code was previously used
  # create check to see if there are any instances where the suffix is not plot-1
  # Replace instances of macro in the tex file
  # replace_macro <- gsub(
  #   "\\pandocbounded",
  #   "\\pdftooltip",
  #   tex_file
  # )
  
  # Replace pandocbounded with pdftooltip so alt text can be added
  # No longer using tooltip - pandocbounded will work fine with the next adjustments
  # tex_file[fig_lines] <- lapply(
  #   tex_file[fig_lines],
  #   function(line) {
  #     gsub("\\pandocbounded", "\\pdftooltip", line)
  #   }
  # )
  
  # Check instance of pandocbounded to find plots that were not made from R chunks
  addl_figs <- setdiff(grep("\\pandocbounded", tex_file)[-1], fig_lines)
  
  # Add alt text to custom images
  # read in alt text csv file to match with labels
  alttext <- utils::read.csv(file.path(alttext_csv_dir, "captions_alt_text.csv"))
  if (length(addl_figs) > 0) {
    for (i in addl_figs) {
      # Find line label
      line <- tex_file[i]
      # Find line following target to extract label
      matches <- grep("\\label", tex_file)
      label_line <- matches[matches > i][1]
      line_label <- stringr::str_extract(tex_file[label_line], "\\\\label\\{([^}]*)\\}") |>
        stringr::str_remove_all("^\\\\label\\{|\\}$")
      # Match label name to label in csv and extract alttext
      alttext_i <- alttext |>
        dplyr::filter(label == line_label) |>
        dplyr::pull(alt_text)
      if (is.na(label_line)) {
        alttext_i <- ""
        cli::cli_alert_warning("No alternative text found for {line_label}.")
      }
      # Add selected alttext onto end of the line
      tex_file[i] <- gsub(
        "keepaspectratio",
        paste0("keepaspectratio,alt={'", alttext_i, "'}"),
        tex_file[i]
      )
      # tex_file[i] <- paste(tex_file[i], "{", alttext_i, "}", sep = "")
    }
  }
  
  # Insert alt text for figures
  # Call alt text in list with names
  # Extract figures produced from quarto
  quarto_folder <- list.files(dir, pattern = "_files") 
  pdf_folder <- list.files(
    file.path(dir, quarto_folder),
    pattern = "figure-pdf"
  )
  obj_files <- list.files(file.path(quarto_folder, pdf_folder))
  obj_files <- obj_files[grep(".pdf", obj_files)] # only select pdf if this is run multiple times
  
  # read all files in obj_files and put into list
  # this code is not very efficient because this was quickly adapted from 
  # another method we take alternative text from plots made from another package
  alt_text_list <- list()
  for (i in 1:length(obj_files)) {
    # extract name to add into the list for placement
    rda_name <- stringr::str_replace(obj_files[i], "-1.pdf", "")
    # if name is >1 word then replace the _ with - to follow naming convention for
    # figures in tex file
    if (grepl("_", rda_name)) rda_name <- stringr::str_replace(rda_name, "_", "-")
    # convert to name in tex file to find where the line is located
    tex_name <- glue::glue("{rda_name}-1.png") # replacing pdf - img ext are changed in next step
    # extract alt. text with figure
    alt_text <- alttext$alt_text[[which(alttext$label == rda_name)]]
    # place obj into list
    alt_text_list[[tex_name]] <- alt_text
    # remove rda file to declutter
    # rm(rda)
  }
  
  # Convert all pdf images to png if render was to pdf
  # extract all files from render folder
  img_path <- file.path(dir, gsub(".tex", "_files/figure-pdf", x))
  if (dir.exists(img_path)) {
    imgs <- list.files(img_path)
    for (i in 1:length(imgs)) {
      img_file <- imgs[i]
      if (grepl(".png", img_file)) next
      img_file_con <- gsub(".pdf", ".png", img_file)
      if (!file.exists(file.path(img_path, img_file_con))) {
        pdftools::pdf_convert(
          file.path(img_path, img_file),
          format = "png",
          dpi = 300,
          filenames = file.path(img_path, img_file_con)
        ) |>
          suppressWarnings() |>
          suppressMessages()
      }
      # Replace names in the tex file
      tex_file <- gsub(img_file, img_file_con, tex_file)
    }
  }
  
  # Find where figure is located and append the alt. text
  # TODO: make checks so only adds to images that don't already have alt text included in them
  for (i in seq_along(alt_text_list)) {
    fig_line <- grep(names(alt_text_list[i]), tex_file) # BUG: if renamed and tagging was run first, this will not work
    # Check that line we are adding the alt text to is for correct fig
    if (!grepl(names(alt_text_list[i]), tex_file[fig_line])) {
      cli::cli_alert_warning("Non-matching object name to tex file line.")
      next
    }
    # Check that selected tex_line contains a marked figure - aka correct placement
    file_name <- stringr::str_remove(x, ".tex")
    if (!grepl(glue::glue("{file_name}_files/figure-pdf/fig-"), tex_file[fig_line])) {
      cli::cli_alert_warning("Improper line for appendment.")
      cli::cli_alert_warning("Skipped adding alternative text for {names(alt_text_list[i])}", wrap = TRUE)
      next
    }
    tex_file[fig_line] <- gsub(
      "keepaspectratio",
      paste0("keepaspectratio,alt={'", alt_text_list[[i]], "'}"),
      tex_file[fig_line]
    )
  }
  
  # Save overwrite tex file
  write(unlist(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
  # utils::capture.output(cat(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)), append = FALSE)
  cli::cli_alert_success("______Alternative text added to tex file.______")
  # Render the .tex file after edits
  if (compile) {
    cli::cli_alert_info("______Compiling in progress - This can take a while...______")
    # test if this can be done when skeleton is in different folder than the wd
    tinytex::lualatex(file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
    cli::cli_alert_success("______Compiling finished______")
  }
}

#-------------------------------------------------------------------------------

# Add tagging structure to latex documents produced from quarto
add_tagging <- function(
    x = list.files(getwd())[grep(".tex", list.files(getwd()))],
    dir = getwd(),
    compile = TRUE,
    rename = NULL) {
  if (length(x) == 0) {
    cli::cli_abort(c(
      message = ".tex file not found.",
      "i" = "`x` entered as an empty character",
      "i" = "`dir` entered as {dir}"
    ))
  }
  # Read latex file
  tex_file <- readLines(fs::path(dir, x))
  
  # Identify line where the new accessibility content should be added after
  line_after <- grep("\\PassOptionsToPackage\\{dvipsnames\\,svgnames\\,x11names\\}\\{xcolor\\}", tex_file)
  # Accessibility additions before /documentclass
  line_to_add <- "\\input{accessibility.tex}"
  # Add line into file
  tex_file <- append(line_to_add, tex_file, after = line_after)
  # Add in . from default quarto issue
  # \hypersetup{linkcolor=}
  issue_color <- grep("\\hypersetup\\{linkcolor=\\}", tex_file)
  if (length(issue_color) > 1) {
    cli::cli_alert_warning("Failed to solve ~ ! LaTeX Error: Unknown color ''.")
  } else {
    tex_file[issue_color] <- gsub(
      "(linkcolor=)",
      "\\1.",
      tex_file[issue_color]
    )
  }
  # Export file
  write(tex_file, file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
  
  # Add accessibility.tex to directory
  accessibility <- paste0(
    "\\DocumentMetadata{%", "\n",
    "  ", "lang = en-us,", "\n",
    "  ", "pdfversion = 2.0,", "\n",
    "  ", "pdfstandard = ua-2,", "\n",
    "  ", "tagging = on,", "\n",
    "  ", "tagging-setup = {math/setup=mathml-SE}", "\n",
    "}", "\n",
    "\\tagpdfsetup{activate, tabsorder=structure}", "\n",
    "% Use the following to fix bug in November 2023 download of LaTeX", "\n",
    "% \\ExplSyntaxOn", "\n",
    "% \\cs_generate_variant:Nn__tag_prop_gput:Nnn{cnx}", "\n",
    "% \\ExplSyntaxOff", "\n",
    "%", "\n"
  )
  
  # Save accessibility partial
  utils::capture.output(cat(accessibility), file = file.path(dir, "accessibility.tex"), append = FALSE)
  cli::cli_alert_success("______Tagging structure added to tex file.______")
  if (compile) {
    cli::cli_alert_info("______Compiling in progress - This can take a while...______")
    # test if this can be done when skeleton is in different folder than the wd
    tinytex::lualatex(file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
    cli::cli_alert_success("______Compiling finished______")
  }
}
