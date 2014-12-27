
# A Cron-like second-precision Scheduler for R

This is a scheduling package for running one-off or recurring tasks in second-precision schedules. It may be useful either in maintenance tasks, or in batch processing pipelines where new data are made available periodically. For example, you may need to retrieve new records written in a database every 30 seconds and process them in batches.



# Installation

Development version from GitHub

```{R}
install.packages("devtools")
devtools::install_github("henfiber/cronr")
```

Then load it as usually

```{R}
library('cronr')
```

# Usage

Here is how to use the main scheduling function `crond`.

Firtly, define a "task handler" function which will be responsible for executing your tasks:

```{r}
my_handler <- function(task_id, timestamp, tasks_info) {
	# Do something with this "task_id" using the information in "tasks_info"
	message("Running ", task_id, " at ", unclass(Sys.time()), " - scheduled: ",
			timestamp, " (", as.POSIXct(timestamp, origin = "1970-01-01") , ")")
}
```

Then define some tasks in a data frame or other structure. The scheduling definitions are expressed in a "period:offset" notation, meaning that a task will run every "period" units, waiting "offset" units before running the task. Units are specified in shorthand form, e.g. "1m" for "1 minute", "2h" for 2 hours etc.

```{r}
my_tasks  <- data.frame(schedules  = c("2s", "1m:3s", "1h:5s"),
						my_jobs    = c("job01", "job02",  "job03"),
						other_info = c("...",   "...",     "..."),
						stringsAsFactors = F)
schedules <- my_tasks$schedules
```


Then start the scheduler using the `crond` function

```{r}
crond(schedules, my_tasks, "callback", "my_handler")
```

The scheduler will run indefinitely in the background until you interrupt the R session.



# License

GPLv2 or newer

Copyright 2014 Ilias Kotinas




