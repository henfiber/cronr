library('testthat')
library('cronr')

test_crond <- function() {
    Schedules <- c("2s", "3s", "15s", "1h:43m")
    crond(Schedules, "")
}


example_readme <- function() {
    my_handler <- function(task_id, timestamp, tasks_info) {
        # Do something with this "task_id" using the information in "tasks_info"
        message("Running ", task_id, " at ", unclass(Sys.time()), " - scheduled: ",
                timestamp, " (", as.POSIXct(timestamp, origin = "1970-01-01") , ")")
    }

    my_tasks  <- data.frame(schedules  = c("2s", "1m:3s", "1h:5s"),
                            my_jobs    = c("job01", "job02",  "job03"),
                            other_info = c("...",   "...",     "..."),
                            stringsAsFactors = F)
    schedules <- my_tasks$schedules
    crond(schedules, my_tasks, "callback", "my_handler")
}

#test_crond()
#example_readme()
