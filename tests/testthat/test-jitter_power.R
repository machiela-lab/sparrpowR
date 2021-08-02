context("jitter_power")

###########################
# spatial_jitter testthat #
###########################

test_that("jitter_power throws error with invalid arguments", {
  
  expect_error(
    jitter_power(obs_data = NULL,
                 sim_total = 2,
                 samp_control = "MVN",
                 s_control = 0.01, 
                 alpha = 0.01, 
                 resolution = 10, 
                 edge = "uniform")
  )
  
  expect_error(
    jitter_power(obs_data = unique(chorley),
                 sim_total = NULL,
                 samp_control = "MVN",
                 s_control = 0.01, 
                 alpha = 0.01,
                 resolution = 10, 
                 edge = "uniform")
  )
  
  expect_error(
    jitter_power(obs_data = unique(chorley),
                 sim_total = 2,
                 samp_control =  NULL,
                 s_control = 0.01, 
                 alpha = 0.01,
                 resolution = 10, 
                 edge = "uniform")
  )
  
  expect_error(
    jitter_power(obs_data = unique(chorley),
                 sim_total = 2,
                 samp_control = "MVN",
                 s_control = NULL, 
                 alpha = 0.01, 
                 resolution = 10, 
                 edge = "uniform")
  )
}
)


test_that("jitter_power works", {  
  
  expect_named(jitter_power(obs_data = unique(chorley),
                            sim_total = 2,
                            samp_control = "MVN",
                            s_control = 0.01, 
                            alpha = 0.07, 
                            resolution = 100, 
                            p_correct = "FDR")
  ) 
}
)

test_that("parallel processing with future package functions properly", {
  expect_named(jitter_power(obs_data = unique(chorley),
                            sim_total = 2,
                            samp_control = "MVN",
                            s_control = 0.01,
                            parallel = TRUE,
                            n_core = 2,
                            verbose = FALSE)
  )
}
)
