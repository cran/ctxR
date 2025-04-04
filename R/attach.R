.onAttach <- function(...) {

  tos <- paste0(
    cli::col_green(cli::symbol$info),
    ' ',
    "CCTE's Terms of Service: ",
    cli::col_blue(cli::style_italic(
      cli::style_hyperlink('<https://www.epa.gov/comptox-tools/computational-toxicology-and-exposure-apis>', 'https://www.epa.gov/comptox-tools/computational-toxicology-and-exposure-apis')
    ))
  )
  cite <- paste0(
    cli::col_green(cli::symbol$info),
    ' ',
    'Please cite ', cli::col_blue('ctxR'), ' if you use it! Use `citation(\'ctxR\')` for details.'
  )

  rlang::inform(
    paste0(tos, '\n', cite),
    class = 'packageStartupMessage'
  )

  .getKeyIntoPkgEnv(silent = FALSE)
  bootstrap_ctxR()

}

.onLoad <- function(...) {
  .getKeyIntoPkgEnv(silent = TRUE)
  bootstrap_ctxR()
}


bootstrap_ctxR <- function() {
  set_ctxR_option(
    'ctx' = structure(
      list(

    ),
    class = 'ctx_credentials'
  ),
  'display_api_key' = FALSE
  )
}

.pkgenv <- new.env(parent = emptyenv())

.defaultFile <- function() {
  if (getRversion() >= "4.0.0") {
    ctxRdir <- tools::R_user_dir("ctxR")
    if (dir.exists(ctxRdir)) {
      fname <- file.path(ctxRdir, "api.dcf")
      if (file.exists(fname)) {
        return(fname)
      }
    }
  }
  return("")
}

.getKeyIntoPkgEnv <- function(silent = TRUE) {
  .pkgenv[["api"]] <- ""
  fname <- .defaultFile()
  if (fname != "") {
    res <- read.dcf(fname)
    if (!is.na(match("key", colnames(res)))) {
      .pkgenv[["api"]] <- res[[1, "key"]]
      if (!silent) packageStartupMessage("Setting API key from config file.")
    } else {
      if (!silent) packageStartupMessage("API key file found but no api entry.")
    }
  } else if ((ev <- Sys.getenv("CTX_API_KEY")) != "") {
    .pkgenv[["api"]] <- ev
    if (!silent) packageStartupMessage("Setting API key from environment variable.")
  } else {
    if (!silent) packageStartupMessage(paste("No config file or environment variable",
                                             "found: API access unlikely."))
  }
}
