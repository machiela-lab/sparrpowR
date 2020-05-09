# spatial_jitter Example

jitter_power(obs_data = unique(chorley),
             sim_total = 2,
             samp_control = "MVN",
             s_control = .01, 
             upper_tail = 0.995,
             lower_tail = 0.005, 
             resolution = 10, 
             edge = "uniform", 
             cascon = FALSE
)


# spatial_data Example
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
             s_control = 0.05,
             l_control = 100,
             e_control = 0,
             samp_case = "MVN", 
             samp_control = "MVN",
             sim_total = 1,
             npc_control = 10,
             win = spatstat::disc(radius = 0.5, centre = c(0.5,0.5))
)


# spatial_power Example
spatial_power(x_case = c(0.25),
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


#spatial_plots Example

donttest{spatial_plots(input = sparrpowR_object, 
                        p_thresh = 0.8, 
                        #plot_text = T,
                        plot_pts = T, 
                        chars = c(4,5),
                        sizes = c(0.5,0.5),
                        cols = c("grey0", "grey80", "grey100", "red", "blue")
                        
)
}
