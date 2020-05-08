# spatial_jitter Example

sim_power <- jitter_power(obs_data = unique(chorley),
                          sim_total = 2,
                          samp_control = "MVN",
                          s_control = .01, # default = 1
                          upper_tail = 0.995, # default = 0.975
                          lower_tail = 0.005, # default = 0.025
                          resolution = 10, # default = 128
                          edge = "uniform", # default = "uniform"
                          cascon = FALSE # default = FALSE
)

spatial_plots(input = sim_power, # use output of SRR simulation
              p_thresh = 0.8, # default = 0.8
              #plot_text = T, # default = FALSE in case resolution >> 10
              plot_pts = T, # default = TRUE 
              chars = c(4,5), # case, control
              sizes = c(0.5,0.5), # case, control
              cols = c("#0000ff", "#00ff00", "#ff0000", "#a020f0", "#ffa500") #c("blue", "green", "red", "purple", "orange") # insufficient, sufficient, text, case, control
)


# spatial_data Example
rand_pts <- spatial_data(x_case = c(0.5),
                         y_case = c(0.5),
                         x_control = c(0.5),
                         y_control = c(0.5),
                         n_case = 100,
                         n_control = 700,
                         r_case = 0.1,
                         s_case = 0.05,
                         l_case = 200,
                         r_control = 0.1,
                         s_control = 0.05,
                         l_control = 100,
                         e_control = 0,
                         samp_case = "MVN", 
                         samp_control = "MVN",
                         sim_total = 1,
                         npc_control = 10,
                         win = spatstat::disc(radius = 0.5, centre = c(0.5,0.5))
)

spatial_plots(input = rand_pts, # use output of data simulation
              n_sim = 1, # default = 4 simulations
              chars = c(4,5), # case, control
              sizes = c(0.5,0.5), # case, control
              cols = c("blue", "green", "red", "purple", "orange") # insufficient, sufficient, text, case, control
) 


# spatial_power Example
sim_power<- spatial_power(x_case = c(0.25),
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

spatial_plots(input = sim_power, # use output of SRR simulation
              p_thresh = 0.8, # default = 0.8
              #plot_text = T, # default = FALSE in case resolution >> 10
              plot_pts = T, # default = TRUE 
              chars = c(4,5), # case, control
              sizes = c(0.5,0.5), # case, control
              cols = c("grey0", "grey80", "grey100", "red", "blue")
              
)

