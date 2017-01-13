assume_direction_pos_class <- function(x, class, pos_class, neg_class, direction,
                                       na.rm) {
    # Check classes
    if(na.rm) uc <- unique(stats::na.omit(class)) else uc <- unique(class)
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

    if (is.null(direction) & !is.null(pos_class)) {
        if (mean(x[class != pos_class]) < mean(x[class == pos_class])) {
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

microbenchmark::microbenchmark(
    suppressMessages(
    assume_direction_pos_class(iris$Petal.Width, iris$Species == "versicolor", na.rm = T,
                               direction = NULL, pos_class = NULL, neg_class = NULL)
    )
)
#     min      lq     mean   median      uq     max neval
# 311.478 322.643 344.9638 334.9095 351.626 641.243   100


inf_to_candidate_cuts <- function(candidate_cuts, direction) {
    if (direction == ">" | direction == ">=") {
        candidate_cuts <- unique(c(-Inf, candidate_cuts))
    } else {
        candidate_cuts <- unique(c(candidate_cuts, Inf))
    }
    return(candidate_cuts)
}





