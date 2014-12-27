# Some sample handlers

dummy_task_handler <- function(task_id, timestamp, tasks_info) {
    # Do something with this "task_id" using the information in "tasks_info"
    message("Running ", task_id, " at ", unclass(Sys.time()), " - scheduled: ",
            timestamp, " (", as.POSIXct(timestamp, origin = "1970-01-01") , ")")
}
