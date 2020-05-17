context("spatial_plots")

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