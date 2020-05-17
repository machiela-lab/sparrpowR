context("jitter_power")

###########################
# spatial_jitter testthat #
###########################

test_that("jitter_power throws error with invalid arguments", {
  
  expect_error(
    jitter_power(obs_data = NULL,
                 sim_total = 2,
                 samp_control = "MVN",
                 s_control = .01, 
                 upper_tail = 0.995,
                 lower_tail = 0.005, 
                 resolution = 10, 
                 edge = "uniform", 
                 cascon = FALSE)
  )
  
  expect_error(
    jitter_power(obs_data = unique(chorley),
                 sim_total = NULL,
                 samp_control = "MVN",
                 s_control = .01, 
                 upper_tail = 0.995,
                 lower_tail = 0.005, 
                 resolution = 10, 
                 edge = "uniform", 
                 cascon = FALSE)
  )
  
  expect_error(
    jitter_power(obs_data = unique(chorley),
                 sim_total = 2,
                 samp_control =  NULL,
                 s_control = .01, 
                 upper_tail = 0.995,
                 lower_tail = 0.005, 
                 resolution = 10, 
                 edge = "uniform", 
                 cascon = FALSE)
  )
  
  expect_error(
    jitter_power(obs_data = unique(chorley),
                 sim_total = 2,
                 samp_control = "MVN",
                 s_control = NULL, 
                 upper_tail = 0.995,
                 lower_tail = 0.005, 
                 resolution = 10, 
                 edge = "uniform", 
                 cascon = FALSE)
  )
}
)


test_that("jitter_power works", {  
  expect_named(jitter_power(obs_data = unique(chorley),
                            sim_total = 2,
                            samp_control = "MVN",
                            s_control = .01, 
                            upper_tail = 0.995,
                            lower_tail = 0.005, 
                            resolution = 10, 
                            edge = "uniform", 
                            cascon = FALSE)
  ) 
  
}
)  
