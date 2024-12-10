#' A reconstructed dataset from the REMAP-CAP clinical trial by Angus et al. (2020)
#'
#' @format A data frame with 379 rows and 6 variables:
#' \describe{
#'   \item{group}{Type of treatment received}
#'   \item{deathTime}{Time to death in days}
#'   \item{dischargeTime}{Time to discharge in days}
#'   \item{deathStatus}{If patient died within 21 days of follow-up (1=Yes; 0=No)}
#'   \item{dischargeStatus}{If patient was discharged within 21 days of follow-up (1=Yes; 0=No)}
#'   \item{score}{Number of organ support-free days (if patient died, assigned value of -1)}
#' }
#'
#' @references
#' Angus, Derek C., et al. "Effect of hydrocortisone on mortality and organ support in patients with severe COVID-19: the REMAP-CAP COVID-19 corticosteroid domain randomized clinical trial." Jama 324.13 (2020): 1317-1329.
#'
"remapcap"

#' Synthetic data showing modified Rankin Scale and neurological improvement
#'
#' @format A data frame with 2000 rows and 3 variables:
#' \describe{
#'   \item{treat}{Type of treatment received}
#'   \item{mRS}{modified Rankin Score at 90 days (0= no disability, 6 = death)}
#'   \item{early_neuro_improvement}{Change in neurological function at 24 hours}
#' }
#'
"neuro_improvement"
