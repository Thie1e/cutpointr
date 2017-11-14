assume_direction_pos_class <- function(x, class, pos_class, neg_class, direction,
                                       na.rm, uc) {

    # Handle NAs
    if (na.rm) {
        na_indx <- is.na(x)
        na_indc <- is.na(class)
        complete_ind <- !(na_indx + na_indc)
        x <- x[complete_ind]
        class <- class[complete_ind]
    }

    if (is.null(direction) & !is.null(pos_class)) {
        if (stats::median(x[class != pos_class]) < stats::median(x[class == pos_class])) {
            message("Assuming the positive class has higher x values")
            direction <- ">="
        } else {
            message("Assuming the positive class has lower x values")
            direction <- "<="
        }
    }
    if (is.null(direction) & is.null(pos_class)) direction <- ">="
    if (!is.null(direction) & is.null(pos_class)) {
        if (direction == ">" | direction == ">=") {
            if (stats::median(x[class == uc[1]]) > stats::median(x[class == uc[2]])) {
                message(paste("Assuming", uc[1], "as the positive class"))
                message("Assuming the positive class has higher x values")
                pos_class <- uc[1]
            } else {
                message(paste("Assuming", uc[2], "as the positive class"))
                message("Assuming the positive class has higher x values")
                pos_class <- uc[2]
            }
        } else {
            if (stats::median(x[class == uc[1]]) < stats::median(x[class == uc[2]])) {
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


