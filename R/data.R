#' Suicide attempts and DSI sum scores of 532 subjects
#'
#' Various personality and clinical psychological characteristics were assessed
#' as part of an online-study preventing suicide. To identify persons at risk
#' for attempting suicide, various demographic and clinical characteristics
#' were assessed. Depressive Symptom Inventory - Suicidality Subscale
#' (DSA-SS) sum scores and past suicide attempts from 532 subjects are
#' included as a demonstration set to calculate optimal cutpoints. Two
#' additional demographic variables (age, gender) are also included to test
#' for group differences.
#'
#' @format A data frame with 532 rows and 4 variables:
#' \describe{
#'   \item{age}{(numeric) Age of participants in years}
#'   \item{gender}{(factor) Gender}
#'   \item{dsi}{(numeric) Sum-score (0 = low suicidality, 12 = high suicidality)}
#'   \item{suicide}{(factor) Past suicide attempt (no = no attempt, yes = at least one attempt)}
#' }
#' @source von Glischinski, M., Teisman, T., Prinz, S., Gebauer, J., and Hirschfeld, G. (2017). Depressive Symptom Inventory- Suicidality Subscale: Optimal cut points for clinical and non-clinical samples. Clinical Psychology & Psychotherapy
"suicide"


#' Nodal involvement and acid phosphatase levels in 53 prostate cancer patients
#'
#' Prostatic acid phosphatase (PAP) emerged as the first clinically
#' useful tumor marker in the 1940s and 1950s. This data set contains the
#' serum levels of acid phosphatase of 53 patients that were confirmed to
#' have prostate cancer and whether the neighboring lymph nodes were involved.
#'
#' @format A data frame with 53 rows and 2 variables:
#' \describe{
#'   \item{acid_phosphatase}{(numeric) Blood serum level of acid phosphatase}
#'   \item{nodal_inolvement}{(logical) Whether neighboring lymph nodes were involved}
#' }
#' @source Le CT (2006). A solution for the most basic optimization problem associated with an ROC curve. Statistical methods in medical research 15: 571–584
"prostate_nodal"