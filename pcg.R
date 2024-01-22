PowerCurveGenerator <- function(design, model_input, name_of_plot){
  snr_min      = 0 
  snr_max      = 2.5 
  snr_interval = 0.1 
  
  for (i in seq(snr_min, snr_max, by = snr_interval)) {
    
    Powertable  = eval_design(design = design, model = model_input, alpha = 0.2, effectsize = i, detailedoutput = TRUE, conservative = TRUE)
    
    if (i == snr_min) {
      result = data.frame(Parameter = rep(NA, times = nrow(Powertable)))
      result[, "Parameter"] = Powertable$parameter
    }
    result[, paste("SNR", i, sep = "_")] = Powertable$power
  }
  
  rows_to_keep = seq(2, which(result$Parameter == "(Intercept)")[2] - 1)
  
  SNRtable = result[rows_to_keep, ] 
  SNRinteractions = SNRtable[grep(":", SNRtable$Parameter), ]
  SNReffects = SNRtable[grep(":", SNRtable$Parameter, invert = TRUE), ]
  
  
  list_minmax <- list()
  list_minmax$MinEffect = SNReffects %>% slice_min(SNR_1) %>% 
    slice_head(n = 1) %>% mutate(Parameter = "Min Main Effect")
  list_minmax$MaxEffect = SNReffects %>% slice_max(SNR_1) %>% 
    slice_head(n = 1) %>% mutate(Parameter = "Max Main Effect")
  list_minmax$MinInteraction = SNRinteractions %>% slice_min(SNR_1) %>% 
    slice_head(n = 1) %>% mutate(Parameter = "Min 2F Interaction")
  list_minmax$MaxInteraction = SNRinteractions %>% slice_max(SNR_1) %>% 
    slice_head(n = 1) %>% mutate(Parameter = "Max 2F Interaction")
  
  MinMax = bind_rows(list_minmax)
  rm(list_minmax)
  
  PowerCurve <-
    MinMax %>%
    pivot_longer(
      cols = -Parameter
      , names_to = "SNR"
      , values_to = "Power"
    ) %>%
    mutate(
      SNR = SNR %>% str_split(pattern = fixed("SNR_"), n = 2, simplify = TRUE) %>% 
        as_tibble(.name_repair = "universal") %>% pull(2) %>% as.numeric()
    )
  
  ?element_text
  PowerCurvePlot = ggplot(PowerCurve, aes(x = SNR, y = Power, colour = Parameter, linetype = Parameter)) +
    theme(text = element_text(size = 20))+
    theme(plot.background = element_rect(color = "black", linewidth = 1))+
    geom_line(linewidth = 1.5) +
    labs(x = "Signal-to-Noise Ratio (SNR)") + 
    labs(y = "Power") + 
    labs(title = "Power Curves") +
    labs(subtitle = name_of_plot) +
    scale_colour_brewer(palette = "Paired") + 
    theme(legend.position = "right")
  
  
  list(MinMax, PowerCurvePlot)}
