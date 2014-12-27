



### Period helpers

# Expand a shorthand abbreviation like "- 1m" to a list with a sign, value and unit
expand_rel_time <- function(abbr) {
    abbr <- gsub(" ", "", abbr, fixed = TRUE) # remove spaces
    sign <- ""
    if (isTRUE(grepl("^[+-]", abbr))) {
        sign <- substr(abbr, 1, 1)
        abbr <- substring(abbr, 2)
    }

    match_value <- regexpr("^[0-9]+[.]?[0-9]*", abbr)
    if (length(regmatches(abbr, match_value)) > 0) {
        value <- as.integer(regmatches(abbr, match_value)[[1]])
        unit  <- paste(regmatches(abbr, match_value, invert = T)[[1]], collapse = "")
    } else {
        unit  <- abbr
        value <- 1
    }

    # We cannot use tolower(unit) since the same letter "m" is used for both minutes and Months
    unit_expanded <- ""
    if (unit == "m")                     {
        unit_expanded <- "minutes"
    } else if (unit %in% c("h", "H"))    {
        unit_expanded <- "hours"
    } else if (unit %in% c("s", "S"))    {
        unit_expanded <- "seconds"
    } else if (unit %in% c("ms", "MS"))  {
        unit_expanded <- "milliseconds"
    } else if (unit %in% c("d", "D"))    {
        unit_expanded <- "days"
    } else if (unit == "M")              {
        unit_expanded <- "months"
    } else if (unit %in% c("y", "Y"))    {
        unit_expanded <- "years"
    } else if (unit %in% c("w", "W"))    {
        unit_expanded <- "weeks"
    } else if (unit %in% c("ns", "NS"))  {
        unit_expanded <- "nanoseconds"
    } else
        warning(paste0("time unit \"", unit, "\" not supported"))

    return(
        list(
            sign = sign,
            value = value,
            unit = unit,
            unit_expanded = unit_expanded,
            expr_expanded = paste0(sign, value, " ", unit_expanded)
        )
    )
}


# Extended version of strftime which supports %s and %N like GNU date
strftime_extended <- function(x, format = "", ...) {
    sec <- unclass(as.POSIXct(x))
    msec <- "000000000"
    x_str <- as.character(x)

    parts <- strsplit(x_str, ".", fixed = T)[[1]]
    if (length(parts) > 1) {
        sec  <- as.integer(x)
        msec <- parts[2]
    }
    format <- gsub("%s", sec, format)
    format <- gsub("%N", msec, format)
    return(strftime(x, format, ...))
}


# Round a timestamp in whole units
round_timestamp <- function(abbr, now) {
    Ret <- expand_rel_time(abbr)
    unit <- Ret$unit
    value <- Ret$value

    # Create a hash of units with positions in the datetime string
    Pos <- c("y" = 1, "Y" = 1, "M" = 2, "d" = 3, "D" = 3, "h" = 4, "H" = 4, "m" = 5,
             "s" = 6, "S" = 6, "ms" = 7, "MS" = 7, "ns" = 7, "NS" = 7)

    if (!unit %in% c(names(Pos), "w", "W"))
        warning(paste0("time unit ", unit, " not supported"))

    if (unit %in% c("w", "W")) {
        week_day <- as.integer(strftime(now, "%w"))
        if (week_day > 0)
            now <- now - week_day * 86400
        unit <- "d"
    }

    # Compute the time spec as required by mktime (YYYY MM DD HH MM SS) - also suitable for use in split()
    now_spec <- strftime_extended(now, "%Y %m %d %H %M %S %N")

    # Parse datespec - "year Month day hour mins secs nanosecs"
    Now <- strsplit(now_spec, " ")[[1]]

    # Create an ordered array of units with the starting value for each unit
    First <- c("1970", "01", "01", "00", "00", "00", "000")
    # Create an ordered array of units with the max value for each unit
    Max   <- c(0, 12, 31, 12, 60, 60, 1000)

    # Quantize the trailing units after unit
    anchor <- Pos[unit]

    for (i in anchor + 1:length(Now))
        Now[i] <- First[i]
    if (unit %in% c("ms", "MS"))
        Now[7] <- floor(as.integer(Now[7]) / 1000000)

    # If a value is provided, then round also this time unit (except from days which cannot be consistently rounded) in whole multiples of the value
    if (value > 1 && !unit %in% c("d", "D") && (Max[anchor] %% value) == 0) {
        now_anchor   <- as.integer(Now[anchor])
        First_anchor <- as.integer(First[anchor])
        Now[anchor]  <- floor((now_anchor - First_anchor) / value) * value + First_anchor
    }

    # Handle milliseconds
    if (unit %in% c("ms", "MS"))
        Now[7] <- sprintf("%03d", as.integer(Now[7]))  # force 3 decimal places prepended with 0 if needed

    # Now rebuild the timestamp
    now_spec <- Now[1]
    for (i in 2:length(Now))
        now_spec <- paste0(now_spec, " ", Now[i])
    return (strptime(paste0(substring(now_spec, 1, 19), ".", substring(now_spec, 21)),
                     "%Y %m %d %H %M %OS"))
}


# Wrapper around systime_shift() after first expanding the abbreviated expression (e.g. -1h), and handling edge cases
#' @importFrom lubridate seconds minutes hours days weeks years
compute_relative <- function(abbr, now = Sys.time()) {
    Abbr  <- expand_rel_time(abbr)
    unit  <- Abbr$unit
    value <- Abbr$value
    unit_expanded <- Abbr$unit_expanded
    sign  <- Abbr$sign

    choices <- c("hours", "seconds", "minutes", "days", "weeks", "months",
                 "quarters", "years", "milliseconds", "nanoseconds")
    i <- pmatch(unit_expanded, choices, nomatch = 0L, duplicates.ok = TRUE)
    if (all(i == 0L))
        stop(gettextf("'arg' should be one of %s (or a prefix)",
                      paste(dQuote(choices), collapse = ", ")), domain = NA)
    i <- i[i > 0L]
    unit <- choices[i]

    diff_now <- switch (
        unit,
        "seconds"      = seconds(value),
        "minutes"      = minutes(value),
        "hours"        = hours(value),
        "days"         = days(value),
        "weeks"        = weeks(value),
        "months"       = months(value),
        "quarters"     = months(3 * value),
        "years"        = years(value),
        "milliseconds" = 0.001 * value,
        "nanoseconds"  = 0.000000001 * value
    )

    ret <- if (sign == "-") (now - diff_now) else (now + diff_now)

    # consider rounding in 3 digits before returning the result
    return(ret)
}



