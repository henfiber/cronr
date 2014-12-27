# Main functions
# Some Reference: http://howardhinnant.github.io/date_algorithms.html



# Helper to sleep until specific timestamp
# Consider using the `naptime` package instead of Sys.sleep()
sleep_until <- function(target) {
    target <- as.POSIXct(target, origin = "1970-01-01", tz = "GMT")
    diff <- target - Sys.time()
    if (diff <= 0)
        return(0)  			# target has already passed
    repeat {
        if (diff <= .0001)
            return(1)
        # sleep in smaller intervals on long idle periods to make for timer drifts (on high system load)
        if (diff > 10) {
            Sys.sleep(diff / 2.0)
        } else {
            Sys.sleep(diff)
        }
        diff <- target - Sys.time()
    }
    return(1)
}




# Compute the immediate next timestamp when given an expression with an interval and an offset
compute_next <- function(expr, tstamp) {
    period <- expr; poffset <- ""
    parts <- strsplit(expr, ":")[[1]]
    if (length(parts) > 1) {
        period <- parts[1]
        poffset <- parts[2]
    }
    tnext <- round_timestamp(period, tstamp)
    if (nchar(poffset, allowNA = T) > 0) {
        # Calculate the module of the offset with the period (if the offset is smaller than the period, then that will equal the offset)
        tnext <- compute_relative(paste0("+", poffset), tnext)
        offset_mod <- unclass(as.POSIXct(tnext)) - unclass(as.POSIXct(round_timestamp(period, tnext))) + 0
        # Now calculate it with the right offset (offset_mod)
        if (offset_mod > 0)
            tnext <- compute_relative(paste0("+", offset_mod, "s"), round_timestamp(period, tstamp))
    }
    if ((tnext + 0) <= (tstamp + 1))   # make sure a string timestamp like "1491234567.000" is converted to numeric or else bad things happen
        tnext <- compute_relative(paste0("+", period), tnext)

    return(tnext)
}




#' Task Scheduler
#'
#' Run tasks in a schedule
#'
#' @param Schedules  An ordered vector with the schedules for each task in shorthand period:offset form
#' @param TasksInfo  Ordered Structure with extra information which will be passed to handler
#' @param method     Whether a function (callback) or a program should be called
#' @param handler    The name of the function which handles the task execution
#' @param ...        Extra arguments to be passed to the handler
#'
#' @examples
#' # Define a handler
#' my_handler <- function(task_id, timestamp, tasks_info) {
#'   # Do something with this "task_id" using the information in "tasks_info"
#'   message("Running ", task_id, " at ", unclass(Sys.time()), " - scheduled: ",
#'           timestamp, " (", as.POSIXct(timestamp, origin = "1970-01-01") , ")")
#' }
#' # Define the tasks info structure
#' my_tasks  <- data.frame(schedules = c("2s", "1m:3s", "1h:5s"),
#'                         my_jobs    = c("job01", "job02",  "job03"),
#'                         other_info = c("...",   "...",     "..."),
#'                         stringsAsFactors = F)
#' # create the schedules vector
#' schedules <- my_tasks$schedules
#' # Start the scheduler
#' crond(schedules, my_tasks, "callback", "my_handler")
#'
#' @export
crond <- function(Schedules, TasksInfo, method = "callback", handler = "dummy_task_handler", ...) {
    task_handler <- match.fun(handler)

    # Initialize Queue
    TasksQueue <- liqueueR::PriorityQueue$new()
    now <- Sys.time()

    # Start: Find the next time instance for all definitions and add them to the queue
    for (task_id in 1:length(Schedules)) {
        expr <- Schedules[task_id]
        tnext <- compute_next(expr, now)
        PushLow(TasksQueue, task_id, as.numeric(as.POSIXct(tnext)))
    }

    if (method == "debug") {
        cat(unlist(TasksQueue$data), unlist(TasksQueue$priorities))
    }

    repeat {
        TasksNext <- c()
        n <- TasksQueue$size()

        if(n > 0) {
            Data <- PopLow(TasksQueue)
            tnext <- Data$prio
            TasksNext[length(TasksNext) + 1] <- Data$id
        }
        while(TasksQueue$size() > 1 && TasksQueue$priorities[1] == tnext) {
            Data <- PopLow(TasksQueue)
            TasksNext[length(TasksNext) + 1] <- Data$id
        }

        # Get some sleep and check whether the time has already passed
        got_rest <- sleep_until(tnext)

        if(! got_rest) {
            if (++cnt_unrest > 3) {
                warning(paste0("Multiple consecutive past timestamps coming in scheduler. ",
                        "That indicates a bug, an unsupported time utility or invalid input. ",
                        "Getting some sleep.."))
                Sys.sleep(cnt_unrest - 2)
            }
        } else {
            cnt_unrest <- 0
        }

        # Needed timestamps and some checks
        now <- Sys.time()
        now_scheduled <- tnext

        # Now take some action
        for (i in 1:length(TasksNext)) {
            task_id <- TasksNext[i]
            expr <- Schedules[task_id]
            tnext <- compute_next(expr, now)
            PushLow(TasksQueue, task_id, as.numeric(as.POSIXct(tnext)))
            if (method == "callback") {
                # @handler(task_id, now_scheduled, TasksInfo)
                task_handler(task_id, now_scheduled, TasksInfo, ...)
            } else if (method == "program") {
                system(sprintf("%s %s %s %s", handler, task_id, now_scheduled, TasksInfo))
            } else if (method == "debug") {
                message(paste0("Running task ", task_id, " at: ", now, " - next is: ", tnext))
            }
        }
    }
}





