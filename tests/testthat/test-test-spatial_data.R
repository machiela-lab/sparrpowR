context("spatial_data")

#########################
# spatial_data testthat #
#########################

test_that("spatial_data throws error with invalid arguments", {
  
  expect_error(
    spatial_data(x_case = NULL,
                 y_case = c(0.5),
                 x_control = c(0.5),
                 y_control = c(0.5),
                 n_case = 100,
                 n_control = 700,
                 r_case = 0.1,
                 s_case = 0.05,
                 l_case = 200,
                 r_control = 0.1,
                 s_control = 0.005,
                 l_control = 100,
                 e_control = 0,
                 samp_case = "MVN", 
                 samp_control = "MVN",
                 sim_total = 1,
                 npc_control = 10,
                 win = spatstat::disc(radius = 0.5, centre = c(0.5,0.5)))
  )
  
  expect_error(
    spatial_data(x_case = c(0.5),
                 y_case = NULL,
                 x_control = c(0.5),
                 y_control = c(0.5),
                 n_case = 100,
                 n_control = 700,
                 r_case = 0.1,
                 s_case = 0.05,
                 l_case = 200,
                 r_control = 0.1,
                 s_control = 0.005,
                 l_control = 100,
                 e_control = 0,
                 samp_case = "MVN", 
                 samp_control = "MVN",
                 sim_total = 1,
                 npc_control = 10,
                 win = spatstat::disc(radius = 0.5, centre = c(0.5,0.5)))
  )
  
  expect_error(
    spatial_data(x_case = c(0.5),
                 y_case = c(0.5),
                 x_control = NULL,
                 y_control = c(0.5),
                 n_case = 100,
                 n_control = 700,
                 r_case = 0.1,
                 s_case = 0.05,
                 l_case = 200,
                 r_control = 0.1,
                 s_control = 0.005,
                 l_control = 100,
                 e_control = 0,
                 samp_case = "MVN", 
                 samp_control = "MVN",
                 sim_total = 1,
                 npc_control = 10,
                 win = spatstat::disc(radius = 0.5, centre = c(0.5,0.5)))
  )
  
  expect_error(
    spatial_data(x_case = c(0.5),
                 y_case = c(0.5),
                 x_control = c(0.5),
                 y_control = NULL,
                 n_case = 100,
                 n_control = 700,
                 r_case = 0.1,
                 s_case = 0.05,
                 l_case = 200,
                 r_control = 0.1,
                 s_control = 0.005,
                 l_control = 100,
                 e_control = 0,
                 samp_case = "MVN", 
                 samp_control = "MVN",
                 sim_total = 1,
                 npc_control = 10,
                 win = spatstat::disc(radius = 0.5, centre = c(0.5,0.5)))
  )
  
  expect_error(
    spatial_data(x_case = c(0.5),
                 y_case = c(0.5),
                 x_control = c(0.5),
                 y_control = c(0.5),
                 n_case = 100,
                 n_control = 700,
                 r_case = 0.1,
                 s_case = 0.05,
                 l_case = 200,
                 r_control = 0.1,
                 s_control = 0.005,
                 l_control = 100,
                 e_control = 0,
                 samp_case = "MVN", 
                 samp_control = "MVN",
                 sim_total = NULL,
                 npc_control = 10,
                 win = spatstat::disc(radius = 0.5, centre = c(0.5,0.5)))
  )
}
)   

test_that("spatial_data works", {  
  expect_silent(spatial_data(x_case = c(0.5),
                             y_case = c(0.5),
                             x_control = c(0.5),
                             y_control = c(0.5),
                             n_case = 100,
                             n_control = 700,
                             r_case = 0.1,
                             s_case = 0.05,
                             l_case = 200,
                             r_control = 0.1,
                             s_control = 0.005,
                             l_control = 100,
                             e_control = 0,
                             samp_case = "MVN", 
                             samp_control = "MVN",
                             sim_total = 1,
                             npc_control = 10,
                             win = spatstat::disc(radius = 0.5, centre = c(0.5,0.5)))
  ) 
  
}
)  
