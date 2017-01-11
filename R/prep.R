assume_direction_pos_class <- function(x, class, pos_class, neg_class, direction,
                                       na.rm) {
    # Check classes
    if(na.rm) uc <- unique(stats::na.omit(class)) else uc <- unique(class)
    luc <- length(uc)
    if (luc != 2) stop(paste("Expecting two classes, got", luc))
    if (na.rm) x <- stats::na.omit(x)
    if (na.rm) class <- stats::na.omit(class)

    if (is.null(direction) & !is.null(pos_class)) {
        if (mean(stats::na.omit(x[class != pos_class])) < mean(stats::na.omit(x[class == pos_class]))) {
            message("Assuming the positive class has higher x values")
            direction <- ">"
        } else {
            message("Assuming the positive class has lower x values")
            direction <- "<"
        }
    }
    if (is.null(direction) & is.null(pos_class)) direction <- ">"
    if (!is.null(direction) & is.null(pos_class)) {
        if (direction == ">" | direction == ">=") {
            if (mean(stats::na.omit(x[class == uc[1]])) > mean(stats::na.omit(x[class == uc[2]]))) {
                message(paste("Assuming", uc[1], "as the positive class"))
                message("Assuming the positive class has higher x values")
                pos_class <- uc[1]
            } else {
                message(paste("Assuming", uc[2], "as the positive class"))
                message("Assuming the positive class has higher x values")
                pos_class <- uc[2]
            }
        } else {
            if (mean(stats::na.omit(x[class == uc[1]])) < mean(stats::na.omit(x[class == uc[2]]))) {
                message(paste("Assuming", uc[1], "as the positive class"))
                message("Assuming the positive class has lower x values")
                pos_class <- uc[1]
            } else {
                message(paste("Assuming", uc[2], "as the positive class"))
                message("Assuming the positive class has lower x values")
                pos_class <- uc[2]
            }
        }
    }
    if (!any(pos_class == class)) stop("Positive class not found in data")
    if (is.null(neg_class)) {
        neg_class <- unique(class)
        neg_class <- neg_class[neg_class != pos_class]
    }
    return(list(direction = direction, pos_class = pos_class, neg_class = neg_class))
}


inf_to_candidate_cuts <- function(candidate_cuts, direction) {
    if (direction == ">" | direction == ">=") {
        candidate_cuts <- unique(c(-Inf, candidate_cuts))
    } else {
        candidate_cuts <- unique(c(candidate_cuts, Inf))
    }
    return(candidate_cuts)
}





