clevRvisShiny <- function() {
    appDir <- system.file("shiny-app", package = "clevRvis")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `clevRvis`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
