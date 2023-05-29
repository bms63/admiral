#' Adds a Parameter Computed from the Analysis Value of Other Parameters
#'
#' Adds a parameter computed from the analysis value of other parameters. It is
#' expected that the analysis value of the new parameter is defined by an
#' expression using the analysis values of other parameters. For example mean
#' arterial pressure (MAP) can be derived from systolic (SYSBP) and diastolic
#' blood pressure (DIABP) with the formula
#' \deqn{MAP = \frac{SYSBP + 2 DIABP}{3}}{MAP = (SYSBP + 2 DIABP) / 3}
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the `by_vars` parameter, `PARAMCD`, and `AVAL`
#'   are expected.
#'
#'   The variable specified by `by_vars` and `PARAMCD` must be a unique key of
#'   the input dataset after restricting it by the filter condition (`filter`
#'   parameter) and to the parameters specified by `parameters`.
#'
#' @param dataset_add Additional dataset
#'
#'   The variables specified by the `by_vars` parameter are expected.
#'
#'   The variable specified by `by_vars` and `PARAMCD` must be a unique key of
#'   the additional dataset after restricting it to the parameters specified by
#'   `parameters`.
#'
#'   If the argument is specified, the observations of the additional dataset
#'   are considered in addition to the observations from the input dataset
#'
#' @param filter Filter condition
#'
#'   The specified condition is applied to the input dataset before deriving the
#'   new parameter, i.e., only observations fulfilling the condition are taken
#'   into account.
#'
#'   *Permitted Values:* a condition
#'
#' @param parameters Required parameter codes
#'
#'   It is expected that all parameter codes (`PARAMCD`) which are required to
#'   derive the new parameter are specified for this parameter or the
#'   `constant_parameters` parameter.  If an SDTM dataset is used, temporary parameter codes can be derived
#'   by specifying a list of expressions. The name of the element defines the
#'   temporary parameter code and the expression the condition for selecting the
#'   records. For example ` parameters = exprs("NEWS1013" = RSTESTCD %in% c("NEWS103", "NEWS101"))`
#'
#'   *Permitted Values:* A character vector of `PARAMCD` values
#'
#' @param by_vars Grouping variables
#'
#'   For each group defined by `by_vars` an observation is added to the output
#'   dataset. Only variables specified in `by_vars` will be populated
#'   in the newly created records.
#'
#'   *Permitted Values:* list of variables
#'
#' @param constant_parameters Required constant parameter codes
#'
#'   It is expected that all the parameter codes (`PARAMCD`) which are required
#'   to derive the new parameter and are measured only once are specified here.
#'   For example if BMI should be derived and height is measured only once while
#'   weight is measured at each visit. Height could be specified in the
#'   `constant_parameters` parameter. (Refer to Example 2)
#'
#'   *Permitted Values:* A character vector of `PARAMCD` values
#'
#' @param constant_by_vars By variables for constant parameters
#'
#'   The constant parameters (parameters that are measured only once) are merged
#'   to the other parameters using the specified variables. (Refer to Example 2)
#'
#'   *Permitted Values:* list of variables
#'
#' @param analysis_value Definition of the analysis value
#'
#'   An expression defining the analysis value of the new parameter is
#'   expected. The analysis values of the parameters specified by `parameters`
#'   can be accessed using `analysis_value_name.<parameter code>`, e.g., `AVAL.SYSBP`.
#'
#'   *Permitted Values:* An unquoted expression
#'
#' @param analysis_value_name Definition of name of the analysis variable
#'
#'   An expression defining the analysis value name of the new parameter is
#'   expected. Default is `AVAL`
#'
#'   *Permitted Values:* An unquoted variable name
#'
#' @param set_values_to Variables to be set
#'
#'   The specified variables are set to the specified values for the new
#'   observations. For example `exprs(PARAMCD = "MAP")` defines the parameter
#'   code for the new parameter.
#'
#'   *Permitted Values:* List of variable-value pairs
#'
#' @details For each group (with respect to the variables specified for the
#'   `by_vars` parameter) an observation is added to the output dataset if the
#'   filtered input dataset contains exactly one observation for each parameter
#'   code specified for `parameters`.
#'
#'   For the new observations `analysis_value_name` is set to the value specified by
#'   `analysis_value` and the variables specified for `set_values_to` are set to
#'   the provided values. The values of the other variables of the input dataset
#'   are set to `NA`.
#'
#'
#' @return The input dataset with the new parameter added. Note, a variable will only
#'    be populated in the new parameter rows if it is specified in `by_vars`.
#'
#' @family der_prm_bds_findings
#'
#' @keywords der_prm_bds_findings
#'
#' @export
#'
#' @examples
#' library(tibble)
#'
#' # Example 1: Derive MAP
#' advs <- tribble(
#'   ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
#'   "01-701-1015", "DIABP", "Diastolic Blood Pressure (mmHg)", 51, "mmHg", "BASELINE",
#'   "01-701-1015", "DIABP", "Diastolic Blood Pressure (mmHg)", 50, "mmHg", "WEEK 2",
#'   "01-701-1015", "SYSBP", "Systolic Blood Pressure (mmHg)", 121, "mmHg", "BASELINE",
#'   "01-701-1015", "SYSBP", "Systolic Blood Pressure (mmHg)", 121, "mmHg", "WEEK 2",
#'   "01-701-1028", "DIABP", "Diastolic Blood Pressure (mmHg)", 79, "mmHg", "BASELINE",
#'   "01-701-1028", "DIABP", "Diastolic Blood Pressure (mmHg)", 80, "mmHg", "WEEK 2",
#'   "01-701-1028", "SYSBP", "Systolic Blood Pressure (mmHg)", 130, "mmHg", "BASELINE",
#'   "01-701-1028", "SYSBP", "Systolic Blood Pressure (mmHg)", 132, "mmHg", "WEEK 2"
#' )
#'
#' derive_param_computed(
#'   advs,
#'   by_vars = exprs(USUBJID, VISIT),
#'   parameters = c("SYSBP", "DIABP"),
#'   analysis_value = (AVAL.SYSBP + 2 * AVAL.DIABP) / 3,
#'   set_values_to = exprs(
#'     PARAMCD = "MAP",
#'     PARAM = "Mean Arterial Pressure (mmHg)",
#'     AVALU = "mmHg"
#'   )
#' )
#'
#' # Example 2: Derive BMI where height is measured only once
#' advs <- tribble(
#'   ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
#'   "01-701-1015", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
#'   "01-701-1015", "WEIGHT", "Weight (kg)", 54.0, "kg", "SCREENING",
#'   "01-701-1015", "WEIGHT", "Weight (kg)", 54.4, "kg", "BASELINE",
#'   "01-701-1015", "WEIGHT", "Weight (kg)", 53.1, "kg", "WEEK 2",
#'   "01-701-1028", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
#'   "01-701-1028", "WEIGHT", "Weight (kg)", 78.5, "kg", "SCREENING",
#'   "01-701-1028", "WEIGHT", "Weight (kg)", 80.3, "kg", "BASELINE",
#'   "01-701-1028", "WEIGHT", "Weight (kg)", 80.7, "kg", "WEEK 2"
#' )
#'
#' derive_param_computed(
#'   advs,
#'   by_vars = exprs(USUBJID, VISIT),
#'   parameters = "WEIGHT",
#'   analysis_value = AVAL.WEIGHT / (AVAL.HEIGHT / 100)^2,
#'   set_values_to = exprs(
#'     PARAMCD = "BMI",
#'     PARAM = "Body Mass Index (kg/m^2)",
#'     AVALU = "kg/m^2"
#'   ),
#'   constant_parameters = c("HEIGHT"),
#'   constant_by_vars = exprs(USUBJID)
#' )
#'
#' # Example 3: Derive BMI where height is measured only once
#' adnews2 <- tribble(
#'   ~USUBJID, ~PARAM, ~PARAMCD, ~PARAMN, ~RSORRES, ~AVAL,
#'   "ABC-13-1305", "NEWS1-Respirations", "NEWS101", 1, "0", 0,
#'   "ABC-13-1305", "NEWS1-Oxygen Saturation SpO2 Scale 1", "NEWS102", 2, "0", 0,
#'   "ABC-13-1305", "NEWS1-Oxygen Saturation SpO2 Scale 2", "NEWS103", 3, "3", 3,
#'   "ABC-13-1305", "NEWS1-Air or Oxygen", "NEWS104", 4, " ", NA,
#'   "ABC-13-1305", "NEWS1-Air or Oxygen: Device", "NEWS104A", 5, " ", NA,
#'   "ABC-13-1305", "NEWS1-Systolic Blood Pressure", "NEWS105", 6, "0", 0,
#'   "ABC-13-1305", "NEWS1-Pulse", "NEWS106", 7, "0", 0,
#'   "ABC-13-1305", "NEWS1-Consciousness", "NEWS107", 8, "1", 1,
#'   "ABC-13-1305", "NEWS1-Temperature", "NEWS108", 9, "0", 0,
#'   "ABC-13-1305", "NEWS1-NEWS Total", "NEWS109", 10, "4", 0,
#'   "ABC-13-1305", "NEWS1-Monitoring Frequency (HOURS)", "NEWS110", 11, "0", 0,
#'   "ABC-13-1305", "NEWS1-Escalation of Care", "NEWS111", 12, "0", 0,
#'   "ABC-13-1305", "NEWS1-Total Score - Analysis", "NEWS1TS", 13, " ", 4,
#' )
#'
#' adnews <- derive_param_computed(
#'   adnews2,
#'   by_vars = exprs(USUBJID),
#'   parameters = c("NEWS101", "NEWS103", "NEWS1TS"),
#'   analysis_value = (case_when(
#'     AVAL.NEWS1TS <= 4 & (RSORRES.NEWS101 == "3" |
#'       RSORRES.NEWS103 == "3") ~ "Low-Medium",
#'     AVAL.NEWS1TS <= 4 ~ "Low",
#'     AVAL.NEWS1TS > 4 & AVAL.NEWS1TS <= 6 ~ "Medium",
#'     AVAL.NEWS1TS >= 7 ~ "High",
#'     TRUE ~ NA_character_
#'   )),
#'   analysis_value_name = AVALC,
#'   set_values_to = exprs(
#'     PARAMCD = "NEWS1TRG",
#'     PARAM = "NEWS1-Trigger - Analysis",
#'     PARAMN = 14
#'   )
#' )
derive_param_computed <- function(dataset,
                                  dataset_add = NULL,
                                  by_vars,
                                  parameters,
                                  analysis_value,
                                  analysis_value_name = AVAL,
                                  set_values_to,
                                  filter = NULL,
                                  constant_by_vars = NULL,
                                  constant_parameters = NULL) {
  assert_vars(by_vars)
  assert_vars(constant_by_vars, optional = TRUE)
  assert_data_frame(dataset, required_vars = exprs(!!!by_vars, PARAMCD))
  filter <- assert_filter_cond(enexpr(filter), optional = TRUE)

  # Bind datasets to use the appending of them during the pivoting
  dataset_bind <- bind_rows(dataset, dataset_add)
  param_custom <- c()
  # Iterate over each element in the 'parameters' list
  for (i in seq_along(parameters)) {
    # If element of the list is a logical expression for new PARAMCD then
    if (is.null(names(parameters[i])) || names(parameters[i]) == "") {
      param_custom <- c(param_custom, parameters[[i]])
    } else {
      # If element of the list is PARAMCD that already exist
      expression <- parameters[[i]]
      dataset_bind <- dataset_bind %>%
        mutate(
          PARAMCD = if_else(!!enexpr(expression), names(parameters[i]), PARAMCD)
        )
      param_custom <- c(param_custom, names(parameters[i]))
    }
  }

  param_custom <- as.character(unlist(param_custom))


  params_available <- unique(dataset_bind$PARAMCD)
  assert_character_vector(param_custom, values = params_available)
  assert_character_vector(constant_parameters, values = params_available, optional = TRUE)
  assert_varval_list(set_values_to)
  if (!is.null(set_values_to$PARAMCD)) {
    assert_param_does_not_exist(dataset, set_values_to$PARAMCD)
  }

  # select observations and variables required for new observations
  data_filtered <- dataset_bind %>%
    filter_if(filter)

  data_parameters <- data_filtered %>%
    filter(PARAMCD %in% param_custom)

  if (nrow(data_parameters) == 0L) {
    warn(
      paste0(
        "The input dataset does not contain any observations fullfiling the filter condition (",
        expr_label(filter),
        ") for the parameter codes (PARAMCD) ",
        enumerate(param_custom),
        "\nNo new observations were added."
      )
    )
    return(dataset)
  }

  params_available <- unique(data_filtered$PARAMCD)
  params_missing <- setdiff(c(param_custom, constant_parameters), params_available)
  if (length(params_missing) > 0) {
    warn(
      paste0(
        "The input dataset does not contain any observations fullfiling the filter condition (",
        expr_label(filter),
        ") for the parameter codes (PARAMCD) ",
        enumerate(params_missing),
        "\nNo new observations were added."
      )
    )
    return(dataset)
  }

  signal_duplicate_records(
    data_parameters,
    by_vars = exprs(!!!by_vars, PARAMCD),
    msg = paste(
      "The filtered input dataset contains duplicate records with respect to",
      enumerate(c(vars2chr(by_vars), "PARAMCD")),
      "\nPlease ensure that the variables specified for `by_vars` and `PARAMCD`",
      "are a unique key of the input data set restricted by the condition",
      "specified for `filter` and to the parameters specified for `parameters`."
    )
  )

  # horizontalize data, Variables for PARAMCD = "PARAMx" -> VARIABLE.PARAMx
  hori_data <- data_parameters %>%
    pivot_wider(
      names_from = PARAMCD,
      values_from = setdiff(names(.), by_vars),
      names_glue = "{.value}.{PARAMCD}",
      values_fill = NA
    )

  if (!is.null(constant_parameters)) {
    data_const_parameters <- data_filtered %>%
      filter(PARAMCD %in% constant_parameters)

    hori_const_data <- data_const_parameters %>%
      pivot_wider(
        names_from = PARAMCD,
        values_from = setdiff(names(.), constant_by_vars),
        names_glue = "{.value}.{PARAMCD}",
        values_fill = NA
      )

    hori_data <- inner_join(hori_data, hori_const_data, by = vars2chr(constant_by_vars))
  }

  pivoted_vars <- setdiff(names(hori_data), names(data_parameters))

  # add analysis value (AVAL) and parameter variables, e.g., PARAMCD
  hori_data <- hori_data %>%
    process_set_values_to(exprs(!!enexpr(analysis_value_name) := !!enexpr(analysis_value))) %>%
    process_set_values_to(set_values_to) %>%
    select(-pivoted_vars) %>%
    # keep only observations where analysis values are available or eq to NaN
    filter(!is.na(!!enexpr(analysis_value_name)) | is.nan(!!enexpr(analysis_value_name)))

  bind_rows(dataset, hori_data)
}
