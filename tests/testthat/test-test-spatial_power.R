context("spatial_power")

##########################
# spatial_power testthat #
##########################

test_that("spatial_power throws error with invalid arguments", {
  
  expect_error(spatial_power(x_case = NULL,
                             x_control = c(0.25),
                             y_case = c(0.75),
                             y_control = c(0.75),
                             n_case = 10,
                             n_control = 50,
                             r_case = 0.1,
                             r_control = 0.1,
                             s_case = 0.05,
                             s_control = 0.1,
                             l_case = 200,
                             l_control = 100,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             npc_control = 100,
                             e_control = 0,
                             sim_total = 2,
                             cascon = FALSE, 
                             verbose = FALSE)
  ) 
  
  expect_error(spatial_power(x_case = c(0.25),
                             x_control = NULL,
                             y_case = c(0.75),
                             y_control = c(0.75),
                             n_case = 10,
                             n_control = 50,
                             r_case = 0.1,
                             r_control = 0.1,
                             s_case = 0.05,
                             s_control = 0.1,
                             l_case = 200,
                             l_control = 100,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             npc_control = 100,
                             e_control = 0,
                             sim_total = 2,
                             cascon = FALSE, 
                             verbose = FALSE)
  ) 
  
  expect_error(spatial_power(x_case = c(0.25),
                             x_control = c(0.25),
                             y_case = NULL,
                             y_control = c(0.75),
                             n_case = 10,
                             n_control = 50,
                             r_case = 0.1,
                             r_control = 0.1,
                             s_case = 0.05,
                             s_control = 0.1,
                             l_case = 200,
                             l_control = 100,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             npc_control = 100,
                             e_control = 0,
                             sim_total = 2,
                             cascon = FALSE, 
                             verbose = FALSE)
  ) 
  
  expect_error(spatial_power(x_case = c(0.25),
                             x_control = c(0.25),
                             y_case = c(0.75),
                             y_control = NULL,
                             n_case = 10,
                             n_control = 50,
                             r_case = 0.1,
                             r_control = 0.1,
                             s_case = 0.05,
                             s_control = 0.1,
                             l_case = 200,
                             l_control = 100,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             npc_control = 100,
                             e_control = 0,
                             sim_total = 2,
                             cascon = FALSE, 
                             verbose = FALSE)
  ) 
  
  expect_error(spatial_power(x_case = c(0.25),
                             x_control = c(0.25),
                             y_case = c(0.75),
                             y_control = c(0.75),
                             n_case = NULL,
                             n_control = 50,
                             r_case = 0.1,
                             r_control = 0.1,
                             s_case = 0.05,
                             s_control = 0.1,
                             l_case = 200,
                             l_control = 100,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             npc_control = 100,
                             e_control = 0,
                             sim_total = 2,
                             cascon = FALSE, 
                             verbose = FALSE)
  ) 
  
  expect_error(spatial_power(x_case = c(0.25),
                             x_control = c(0.25),
                             y_case = c(0.75),
                             y_control = c(0.75),
                             n_case = 10,
                             n_control = NULL,
                             r_case = 0.1,
                             r_control = 0.1,
                             s_case = 0.05,
                             s_control = 0.1,
                             l_case = 200,
                             l_control = 100,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             npc_control = 100,
                             e_control = 0,
                             sim_total = 2,
                             cascon = FALSE, 
                             verbose = FALSE)
  ) 
  
  expect_error(spatial_power(x_case = c(0.25),
                             x_control = c(0.25),
                             y_case = c(0.75),
                             y_control = c(0.75),
                             n_case = 10,
                             n_control = 50,
                             r_case = 0.1,
                             r_control = 0.1,
                             s_case = 0.05,
                             s_control = 0.1,
                             l_case = 200,
                             l_control = 100,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             npc_control = 100,
                             e_control = 0,
                             sim_total = NULL,
                             cascon = FALSE, 
                             verbose = FALSE)
  )   
  
}
) 


test_that("spatial_power works", {  
  expect_named(spatial_power(x_case = c(0.25),
                             x_control = c(0.25),
                             y_case = c(0.75),
                             y_control = c(0.75),
                             n_case = 10,
                             n_control = 50,
                             r_case = 0.1,
                             r_control = 0.1,
                             s_case = 0.05,
                             s_control = 0.1,
                             l_case = 200,
                             l_control = 100,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             npc_control = 100,
                             e_control = 0,
                             sim_total = 2,
                             cascon = FALSE, 
                             verbose = FALSE)
  ) 
  
}
)
