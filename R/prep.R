assume_direction_pos_class <- function(x, class, pos_class, neg_class, direction,
                                       na.rm) {

    # Check classes
    if (na.rm) uc <- unique(stats::na.omit(class)) else uc <- unique(class)
    luc <- length(uc)
    if (luc != 2) stop(paste("Expecting two classes, got", luc))

    # Handle NAs
    if (na.rm) {
        na_indx <- is.na(x)
        na_indc <- is.na(class)
        complete_ind <- rowSums(cbind(na_indx, na_indc)) == 0
        x <- x[complete_ind]
        class <- class[complete_ind]
    } else {
        if (anyNA(c(x, class))) stop("NAs in x or class but na.rm = FALSE")
    }

    # If Inf and -Inf mean gives NaN
    if (any(x == Inf) & any(x == -Inf)) {
        message("Removing -Inf from mean for assuming pos_class and direction")
        class <- class[x != -Inf]
        x <- x[x != -Inf]
    }

    # If after removing NAs and Inf only <= 1 class is left (simple fallback)
    if (length(unique(class)) != 2) {
        warning("After removing NA / Inf only one class left. Guessing pos_class and direction")
        message("Assuming the positive class has higher x values")
        message(paste("Assuming", uc[1], "as the positive class"))
        return(list(direction = ">=", pos_class = uc[1], neg_class = uc[2]))
    }

    if (is.null(direction) & !is.null(pos_class)) {
        if (mean(x[class != pos_class]) < mean(x[class == pos_class])) {
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
            if (mean(x[class == uc[1]]) > mean(x[class == uc[2]])) {
                message(paste("Assuming", uc[1], "as the positive class"))
                message("Assuming the positive class has higher x values")
                pos_class <- uc[1]
            } else {
                message(paste("Assuming", uc[2], "as the positive class"))
                message("Assuming the positive class has higher x values")
                pos_class <- uc[2]
            }
        } else {
            if (mean(x[class == uc[1]]) < mean(x[class == uc[2]])) {
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


