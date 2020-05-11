library(testthat)


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

##########################
# spatial_plots testthat #
##########################

# Generate spatial_power_output
spatial_power_output<- spatial_power(x_case = c(0.25),
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
                          verbose = FALSE
) 


## spatial_plots throws error with invalid arguments

test_that("spatial_plots throws error with invalid arguments", {
  
expect_error(spatial_plots(input = NULL, 
              p_thresh = 0.8, 
              plot_pts = TRUE, 
              chars = c(4,5), 
              sizes = c(0.5,0.5),
              cols = c("grey0", "grey80", "grey100", "red", "blue"))
)

expect_error(spatial_plots(input = spatial_power_output,
                           p_thresh = 0.8, 
                           plot_pts = T, 
                           chars = c(4,5),
                           sizes = c(0.5,0.5), 
                           cols = NULL)
)

}
)

## spatial_plots works

test_that("spatial_plots works", {
  skip_on_cran()
  expect_output(spatial_plots(input = spatial_power_output, 
                              p_thresh = 0.8, 
                              plot_pts = T,
                              chars = c(4,5), 
                              sizes = c(0.5,0.5), 
                              cols = c("grey0", "grey80", "grey100", "red", "blue")),
                NA)
  
}
)
