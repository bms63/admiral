## Test 1: new observations are derived correctly ----
test_that("derive_param_computed Test 1: new observations are derived correctly", {
  input <- tibble::tribble(
    ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
    "01-701-1015", "DIABP", "Diastolic Blood Pressure (mmHg)", 51, "mmHg", "BASELINE",
    "01-701-1015", "DIABP", "Diastolic Blood Pressure (mmHg)", 50, "mmHg", "WEEK 2",
    "01-701-1015", "SYSBP", "Systolic Blood Pressure (mmHg)", 121, "mmHg", "BASELINE",
    "01-701-1015", "SYSBP", "Systolic Blood Pressure (mmHg)", 121, "mmHg", "WEEK 2",
    "01-701-1028", "DIABP", "Diastolic Blood Pressure (mmHg)", 79, "mmHg", "BASELINE",
    "01-701-1028", "DIABP", "Diastolic Blood Pressure (mmHg)", 80, "mmHg", "WEEK 2",
    "01-701-1028", "SYSBP", "Systolic Blood Pressure (mmHg)", 130, "mmHg", "BASELINE",
    "01-701-1028", "SYSBP", "Systolic Blood Pressure (mmHg)", 132, "mmHg", "WEEK 2"
  )

  new_obs <-
    inner_join(input %>% filter(PARAMCD == "DIABP") %>% select(USUBJID, VISIT, AVAL),
      input %>% filter(PARAMCD == "SYSBP") %>% select(USUBJID, VISIT, AVAL),
      by = c("USUBJID", "VISIT"),
      suffix = c(".DIABP", ".SYSBP")
    ) %>%
    mutate(
      AVAL = (2 * AVAL.DIABP + AVAL.SYSBP) / 3,
      PARAMCD = "MAP",
      PARAM = "Mean arterial pressure (mmHg)",
      AVALU = "mmHg"
    ) %>%
    select(-AVAL.DIABP, -AVAL.SYSBP)
  expected_output <- bind_rows(input, new_obs)

  expect_dfs_equal(
    derive_param_computed(
      input,
      parameters = c("SYSBP", "DIABP"),
      by_vars = exprs(USUBJID, VISIT),
      analysis_value = (AVAL.SYSBP + 2 * AVAL.DIABP) / 3,
      set_values_to = exprs(
        PARAMCD = "MAP",
        PARAM = "Mean arterial pressure (mmHg)",
        AVALU = "mmHg"
      )
    ),
    expected_output,
    keys = c("USUBJID", "PARAMCD", "VISIT")
  )
})

## Test 2: new observations are derived correctly with constant parameters ----
test_that("derive_param_computed Test 2: new observations are derived correctly with constant parameters", {
  input <- tibble::tribble(
    ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
    "01-701-1015", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
    "01-701-1015", "WEIGHT", "Weight (kg)", 54.0, "kg", "SCREENING",
    "01-701-1015", "WEIGHT", "Weight (kg)", 54.4, "kg", "BASELINE",
    "01-701-1015", "WEIGHT", "Weight (kg)", 53.1, "kg", "WEEK 2",
    "01-701-1028", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
    "01-701-1028", "WEIGHT", "Weight (kg)", 78.5, "kg", "SCREENING",
    "01-701-1028", "WEIGHT", "Weight (kg)", 80.3, "kg", "BASELINE",
    "01-701-1028", "WEIGHT", "Weight (kg)", 80.7, "kg", "WEEK 2"
  )

  new_obs <-
    inner_join(input %>% filter(PARAMCD == "HEIGHT") %>% select(USUBJID, AVAL),
      input %>% filter(PARAMCD == "WEIGHT") %>% select(USUBJID, VISIT, AVAL),
      by = c("USUBJID"),
      suffix = c(".HEIGHT", ".WEIGHT")
    ) %>%
    mutate(
      AVAL = AVAL.WEIGHT / (AVAL.HEIGHT / 100)^2,
      PARAMCD = "BMI",
      PARAM = "Body Mass Index (kg/m2)",
      AVALU = "kg/m2"
    ) %>%
    select(-AVAL.HEIGHT, -AVAL.WEIGHT)
  expected_output <- bind_rows(input, new_obs)

  expect_dfs_equal(
    derive_param_computed(
      input,
      parameters = c("WEIGHT"),
      by_vars = exprs(USUBJID, VISIT),
      constant_parameters = c("HEIGHT"),
      constant_by_vars = exprs(USUBJID),
      analysis_value = AVAL.WEIGHT / (AVAL.HEIGHT / 100)^2,
      set_values_to = exprs(
        PARAMCD = "BMI",
        PARAM = "Body Mass Index (kg/m2)",
        AVALU = "kg/m2"
      )
    ),
    expected_output,
    keys = c("USUBJID", "PARAMCD", "VISIT")
  )
})

## Test 3: no new observations are added if filtered dataset is empty ----
test_that("derive_param_computed Test 3: no new observations are added if filtered dataset is empty", {
  input <- tibble::tribble(
    ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
    "01-701-1015", "DIABP", "Diastolic Blood Pressure (mmHg)", 51, "mmHg", "BASELINE",
    "01-701-1015", "DIABP", "Diastolic Blood Pressure (mmHg)", 50, "mmHg", "WEEK 2",
    "01-701-1015", "SYSBP", "Systolic Blood Pressure (mmHg)", 121, "mmHg", "BASELINE",
    "01-701-1015", "SYSBP", "Systolic Blood Pressure (mmHg)", 121, "mmHg", "WEEK 2",
    "01-701-1028", "DIABP", "Diastolic Blood Pressure (mmHg)", 79, "mmHg", "BASELINE",
    "01-701-1028", "DIABP", "Diastolic Blood Pressure (mmHg)", 80, "mmHg", "WEEK 2",
    "01-701-1028", "SYSBP", "Systolic Blood Pressure (mmHg)", 130, "mmHg", "BASELINE",
    "01-701-1028", "SYSBP", "Systolic Blood Pressure (mmHg)", 132, "mmHg", "WEEK 2"
  )

  expect_warning(
    derive_param_computed(
      input,
      filter = VISIT == "WEEK 24",
      parameters = c("SYSBP", "DIABP"),
      by_vars = exprs(USUBJID, VISIT),
      analysis_value = (AVAL.SYSBP + 2 * AVAL.DIABP) / 3,
      set_values_to = exprs(
        PARAMCD = "MAP",
        PARAM = "Mean arterial pressure (mmHg)",
        AVALU = "mmHg"
      )
    ) %>%
      expect_dfs_equal(input,
        keys = c("USUBJID", "PARAMCD", "VISIT")
      ),
    "The input dataset does not contain any observations fullfiling the filter condition .*"
  )
})

## Test 4: no new observations are added if a parameter is missing ----
test_that("derive_param_computed Test 4: no new observations are added if a parameter is missing", {
  input <- tibble::tribble(
    ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
    "01-701-1015", "DIABP", "Diastolic Blood Pressure (mmHg)", 51, "mmHg", "BASELINE",
    "01-701-1015", "DIABP", "Diastolic Blood Pressure (mmHg)", 50, "mmHg", "WEEK 2",
    "01-701-1015", "SYSBP", "Systolic Blood Pressure (mmHg)", 121, "mmHg", "BASELINE",
    "01-701-1015", "SYSBP", "Systolic Blood Pressure (mmHg)", 121, "mmHg", "WEEK 2",
    "01-701-1028", "DIABP", "Diastolic Blood Pressure (mmHg)", 79, "mmHg", "BASELINE",
    "01-701-1028", "DIABP", "Diastolic Blood Pressure (mmHg)", 80, "mmHg", "WEEK 2",
    "01-701-1028", "SYSBP", "Systolic Blood Pressure (mmHg)", 130, "mmHg", "BASELINE",
    "01-701-1028", "SYSBP", "Systolic Blood Pressure (mmHg)", 132, "mmHg", "WEEK 2"
  )

  expect_warning(
    derive_param_computed(
      input,
      filter = PARAMCD == "DIABP",
      parameters = c("SYSBP", "DIABP"),
      by_vars = exprs(USUBJID, VISIT),
      analysis_value = (AVAL.SYSBP + 2 * AVAL.DIABP) / 3,
      set_values_to = exprs(
        PARAMCD = "MAP",
        PARAM = "Mean arterial pressure (mmHg)",
        AVALU = "mmHg"
      )
    )
    %>%
      expect_dfs_equal(input,
        keys = c("USUBJID", "PARAMCD", "VISIT")
      ),
    "The input dataset does not contain any observations fullfiling the filter condition .*"
  )
})


## Test 5: Usage of multiple variables from the source dataset in analysis_value expression ----
test_that("derive_param_computed Test 5: Usage of multiple variables from the source dataset in analysis_value expression", {
  input <- tibble::tribble(
    ~USUBJID, ~PARAM, ~PARAMCD, ~PARAMN, ~RSORRES, ~AVAL,
    "ABC-13-1305", "NEWS1-Respirations", "NEWS101", 1, "0", 0,
    "ABC-13-1305", "NEWS1-Oxygen Saturation SpO2 Scale 1", "NEWS102", 2, "0", 0,
    "ABC-13-1305", "NEWS1-Oxygen Saturation SpO2 Scale 2", "NEWS103", 3, "3", 3,
    "ABC-13-1305", "NEWS1-Air or Oxygen", "NEWS104", 4, " ", NA,
    "ABC-13-1305", "NEWS1-Air or Oxygen: Device", "NEWS104A", 5, " ", NA,
    "ABC-13-1305", "NEWS1-Systolic Blood Pressure", "NEWS105", 6, "0", 0,
    "ABC-13-1305", "NEWS1-Pulse", "NEWS106", 7, "0", 0,
    "ABC-13-1305", "NEWS1-Consciousness", "NEWS107", 8, "1", 1,
    "ABC-13-1305", "NEWS1-Temperature", "NEWS108", 9, "0", 0,
    "ABC-13-1305", "NEWS1-NEWS Total", "NEWS109", 10, "4", 0,
    "ABC-13-1305", "NEWS1-Monitoring Frequency (HOURS)", "NEWS110", 11, "0", 0,
    "ABC-13-1305", "NEWS1-Escalation of Care", "NEWS111", 12, "0", 0,
    "ABC-13-1305", "NEWS1-Total Score - Analysis", "NEWS1TS", 13, " ", 4,
  )


  new_obs <- inner_join(input %>% filter(PARAMCD == "NEWS103") %>% select(USUBJID, RSORRES),
    input %>% filter(PARAMCD == "NEWS101") %>% select(USUBJID, RSORRES),
    by = c("USUBJID"),
    suffix = c(".NEWS103", ".NEWS101")
  ) %>%
    inner_join(input %>% filter(PARAMCD == "NEWS1TS") %>% select(USUBJID, AVAL),
      by = c("USUBJID")
    ) %>%
    mutate(
      AVALC = case_when(
        AVAL <= 4 & (RSORRES.NEWS101 == "3" |
          RSORRES.NEWS103 == "3") ~ "Low-Medium",
        AVAL <= 4 ~ "Low",
        AVAL > 4 & AVAL <= 6 ~ "Medium",
        AVAL >= 7 ~ "High",
        TRUE ~ NA_character_
      ),
      PARAMCD = "NEWS1TRG",
      PARAM = "NEWS1-Trigger - Analysis",
      PARAMN = 14
    ) %>%
    select(-RSORRES.NEWS103, -RSORRES.NEWS101, -AVAL)
  expected_output <- bind_rows(input, new_obs)

  expect_dfs_equal(
    derive_param_computed(
      input,
      by_vars = exprs(USUBJID),
      parameters = c("NEWS101", "NEWS103", "NEWS1TS"),
      analysis_value = (case_when(
        AVAL.NEWS1TS <= 4 & (RSORRES.NEWS101 == "3" |
          RSORRES.NEWS103 == "3") ~ "Low-Medium",
        AVAL.NEWS1TS <= 4 ~ "Low",
        AVAL.NEWS1TS > 4 & AVAL.NEWS1TS <= 6 ~ "Medium",
        AVAL.NEWS1TS >= 7 ~ "High",
        TRUE ~ NA_character_
      )),
      analysis_value_name = AVALC,
      set_values_to = exprs(
        PARAMCD = "NEWS1TRG",
        PARAM = "NEWS1-Trigger - Analysis",
        PARAMN = 14
      )
    ),
    expected_output,
    keys = c("USUBJID", "PARAMCD")
  )
})




## Test 6: Usage of expressions in param and usage of dataset_add input parameter ----
test_that("derive_param_computed Test 6: Usage of expressions in param and usage of dataset_add input parameter", {
  adnews <- tibble::tribble(
    ~USUBJID, ~PARAM, ~PARAMCD, ~PARAMN, ~AVAL, ~RSORRES,
    "ABC-13-1305", "NEWS1-Total Score - Analysis", "NEWS1TS", 13, 4, "4",
  )


  rs <- tibble::tribble(
    ~USUBJID, ~RSTEST, ~RSTESTCD, ~RSORRES,
    "ABC-13-1305", "NEWS1-Oxygen Saturation SpO2 Scale 1", "NEWS102", "0",
    "ABC-13-1305", "NEWS1-Oxygen Saturation SpO2 Scale 2", "NEWS103", "3",
    "ABC-13-1305", "NEWS1-Air or Oxygen", "NEWS104", " ",
    "ABC-13-1305", "NEWS1-Air or Oxygen: Device", "NEWS104A", " ",
    "ABC-13-1305", "NEWS1-Systolic Blood Pressure", "NEWS105", "0",
    "ABC-13-1305", "NEWS1-Pulse", "NEWS106", "0",
    "ABC-13-1305", "NEWS1-Consciousness", "NEWS107", "1",
    "ABC-13-1305", "NEWS1-Temperature", "NEWS108", "0",
    "ABC-13-1305", "NEWS1-NEWS Total", "NEWS109", "4",
    "ABC-13-1305", "NEWS1-Monitoring Frequency (HOURS)", "NEWS110", "0",
    "ABC-13-1305", "NEWS1-Escalation of Care", "NEWS111", "0",
  )

  expected_output <- bind_rows(
    adnews,
    tibble::tribble(
      ~USUBJID, ~PARAM, ~PARAMCD, ~PARAMN, ~AVAL, ~RSORRES, ~AVALC,
      "ABC-13-1305", "NEWS1-Trigger - Analysis", "NEWS1TRG", 14, NA, NA, "Low-Medium",
    )
  )

  expect_dfs_equal(
    derive_param_computed(
      adnews,
      dataset_add = rs,
      by_vars = exprs(USUBJID),
      parameters = exprs(
        "NEWS1013" = RSTESTCD %in% c("NEWS103", "NEWS101"),
        "NEWS1TS"
      ),
      analysis_value = (case_when(
        AVAL.NEWS1TS <= 4 & (RSORRES.NEWS1013 == "3") ~ "Low-Medium",
        AVAL.NEWS1TS <= 4 ~ "Low",
        AVAL.NEWS1TS > 4 & AVAL.NEWS1TS <= 6 ~ "Medium",
        AVAL.NEWS1TS >= 7 ~ "High",
        TRUE ~ NA_character_
      )),
      analysis_value_name = AVALC,
      set_values_to = exprs(
        PARAMCD = "NEWS1TRG",
        PARAM = "NEWS1-Trigger - Analysis",
        PARAMN = 14
      )
    ),
    expected_output,
    keys = c("USUBJID", "PARAMCD")
  )
})
