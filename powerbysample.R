power_by_sample <- function(candidate_set, min_sample_size, max_sample_size, step, parameters, targetSNR, nameplot) {
  counter <-1
  designpower <-list()
  for(samplesize in seq(min_sample_size, max_sample_size, by = step)) {
    optdesign <- gen_design(candidateset = candidate_set, model = parameters, trials = samplesize)
    designpower[[counter]] <- eval_design(design = optdesign, alpha = 0.2, effectsize = targetSNR, detailedoutput = TRUE, conservative = TRUE)
    counter <- counter + 1
  }
  joined <- do.call(rbind, designpower)
  filtered <- dplyr::filter(joined, type == "effect.power")
  
  return( ggplot(filtered) + ggtitle("Power vs. Sample Size")+
            
            theme(text = element_text(size = 16))+
            
            theme(plot.background = element_rect(color = "black", linewidth = 1))+
            
            labs(title = nameplot)+
            labs(subtitle = "Power vs. Sample Size")+
            
            geom_line(aes(x = trials, y = power, color = parameter), linewidth = 1) +
            
            scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
            
            scale_x_continuous(breaks = seq(min_sample_size, max_sample_size, step))
  )
  
  
}
