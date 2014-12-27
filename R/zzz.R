# Mainly setting options and startup messages


.onAttach <- function(libname, pkgname) {
    #packageStartupMessage("Make sure you correctly set the timezone with Sys.setnev(TZ = 'UTC')")
}


.onLoad <- function(libname, pkgname) {
    # OPTIONS
    op <- options()
    op.cronr <- list(
        lubridate.fasttime = TRUE,
        digits.secs = 3
    )
    options(op.cronr)
    #on.exit(options(op), add = TRUE)

    ### Setting up the environment
    #Sys.setenv(TZ = "UTC")

    invisible()
}
