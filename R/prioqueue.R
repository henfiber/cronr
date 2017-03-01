### Priority Queue functions
# Currently tested with the liqueueR package
#
#
# In a scheduler we want to always retrieve the element with the lowest "priority",
# i.e. the task with the lowest timestamp
# That's why we insert elements with a negative timestamp as a priority



# PushLow
# Push an element into a priority Queue with a negative sign in its priority so that it comes first, rather than last, and vice versa
PushLow <- function(Q, id, prio) {
    Q$push(id, 0 - prio)
}

# Remove and return the lowest priority element of a queue
# return a list containing its id and the negative of the stored priority (to reverse the effect of PushLow)
PopLow <- function(Q) {
    n <- Q$size()
    if (n == 0)
        return(FALSE)
    prio <- 0 - Q$priorities[1]
    id <- Q$pop()
    return( list(id = id, prio = prio) )
}

