


#' Compute risk prediction
#'
#' @param object a list with minimal model parameters
#' @param new_data data frame to compute predictions on (unscaled)
#' @param pred_horizon time to compute predictions for
#'
#' @return numeric array with dimensions nrow(new_data) x length(pred_horizon)
#' @export
#'
#' @examples
#' new_data <- data.frame(
#' "treatment" = "placebo",
#' "sex" = "female",
#' "age_yrs" = 50,
#' "bmi" = 21,
#' "glucose_fasting_mgdl" = 120,
#' "hba1c_percent" = 6.2,
#' "chol_trig_mgdl" = 180
#' )
#'
#' new_data$treatment <- factor(new_data$treatment,
#'                              levels = c("lifestyle", "metformin", "placebo"))
#'
#' new_data$sex <- factor(new_data$sex, levels = c("male", "female"))
#'
#' cph_predict()
#'
cph_predict <- function(object, new_data, pred_horizon = 3){

  # point to standard deviations
  sds <- object$standard_devs

  new_data_scaled <- within(new_data, {

    glucose_fasting_mgdl = glucose_fasting_mgdl / sds$glucose_fasting_mgdl

    age_yrs        = age_yrs        / sds$age_yrs
    bmi            = bmi            / sds$bmi
    hba1c_percent  = hba1c_percent  / sds$hba1c_percent
    chol_trig_mgdl = chol_trig_mgdl / sds$chol_trig_mgdl

  })

  xmat <- model.matrix(as.formula(object$xmat_formula),
                       data = new_data_scaled)[, -1L]

  lp <- xmat %*% object$betas

  lp_means <- sum(object$lp_means * object$betas)

  bhaz <- object$bhaz %>%
    filter(time <= pred_horizon) %>%
    slice(n()) %>%
    pull(hazard)

  1 - exp(exp(lp-lp_means) * -bhaz)

}
