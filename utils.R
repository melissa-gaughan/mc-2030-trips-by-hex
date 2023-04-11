## interval options need to be monotonically increasing
## either lower bound or upper bound needs to be further adjusted, here we adjust upper bound in list
#MG interval options are set to 10-500, incrementing by 10
#added im values_df to pass in full dataframe to calculate bins
calculateBucket <- function(min_val,max_val, values_df,  max_bin=10,interval=10,interval_options=seq(10,500,10),center=100,floor_at=NULL,ceil_at=NULL){
  # min_val= minVal
  # max_val = maxVal
  # interval_options=seq(10,500,10)
  
  
  min_val_round <- plyr::round_any(min_val,interval,f=floor)
  max_val_round <- plyr::round_any(max_val,interval,f=ceiling)
  
  #this function is only getting passed the min and max vals. It doesn't know the distribution and can't account for outliers. 
  #pass in the full vector of values? 
  
  if (min_val_round == min_val){
    min_val_round <- min_val_round - interval # reduce by one group as the cut function takes (x1,x2]
  }
  
  if ((min_val_round == 0) & (max_val_round == 0)){
    delta <- max_val_round - min_val_round
    num_bin <- c(1)
    res_idex <- 1
    interval_plot <- 0
    breaks <- c(-interval,0,0,interval)
    
  }else if (min_val * max_val >= 0){ #changed this to not reference rounded values as it was causing issues with situations where 1 was getting rounded down to 0
    ## when both are negative/positive/invovles 0
    # delta <- max_val_round - min_val_round
    # 
    # num_bin <- ceiling(delta / interval_options)
    # res_idx <- which(num_bin < max_bin)[1]
    # interval_plot <- interval_options[res_idx]
    # 
    # # the range (max_val_round - min_val_round) needs to be adjusted to be a multiple of interval
    # # then we do a shift to get the adjusted upper bound: adjusted range + min
    # breaks <- seq(min_val_round,
    #               plyr::round_any(max_val_round - min_val_round,interval,f=ceiling) + min_val_round,
    #               interval_plot)
    # 
    
    pos_bins <- values_df %>% 
      
      mutate(bin = ntile(value, n= ((max_bin)-2))) %>% 
      group_by(bin) %>% 
      summarise(bin_min = min(value, na.rm=T), 
                bin_max = max(value, na.rm=T))
    
    pos_bins_vec <- unique(as.vector(c(pos_bins$bin_min, pos_bins$bin_max)))
    
    
    breaks <- c( 0, pos_bins_vec)
    # MG: so this is assigning based on the total range of the data, not on the number of observations in the plot.
    #Use ntile to bucket records by index?
  }else{
    ## when one is positive and one is negative
    ## need to make sure 0 is always in the breaks
    delta_positive <- max_val_round
    delta_negative <- abs(min_val_round)
    
    num_bin <- ceiling(delta_positive / interval_options) + ceiling(delta_negative / interval_options)
    res_idx <- which(num_bin < max_bin)[1]
    interval_plot <- interval_options[res_idx]
    
    neg_bins <- values_df %>% 
      filter(value<0) %>% 
      mutate(bin = ntile(value, n= ((max_bin/2)-2))) %>% # even split of bins around 0 assumes normal distribution around 0. possibly change
      group_by(bin) %>% 
      summarise(bin_min = min(value, na.rm=T), 
                bin_max = max(value, na.rm=T))
    
    neg_bins_vec <- unique(as.vector(c(neg_bins$bin_min, neg_bins$bin_max)))
    
    
    pos_bins <- values_df %>% 
      filter(value>0) %>% 
      mutate(bin = ntile(value, n= ((max_bin/2)-2))) %>% 
      group_by(bin) %>% 
      summarise(bin_min = min(value, na.rm=T), 
                bin_max = max(value, na.rm=T))
    
    pos_bins_vec <- unique(as.vector(c(pos_bins$bin_min, pos_bins$bin_max)))
    
    
    breaks <- c(neg_bins_vec, 0, pos_bins_vec)
    
    # breaks <- c(seq(plyr::round_any(min_val_round,interval_plot,f=floor), 0, interval_plot), 
    #             seq(0,plyr::round_any(max_val_round,interval_plot,f=ceiling), interval_plot)[-1] # remove 0 so no duplicate 0
  #  )
  }
  
  breaks_adjusted_for_label <- breaks
  if (!is.null(floor_at)){
    if (min(breaks) < floor_at){
      breaks_adjusted_for_label <- c(max(floor_at,breaks_adjusted_for_label[1]), # e.g., floor the break to start from 0 for the original value
                                     breaks_adjusted_for_label[breaks_adjusted_for_label > floor_at])
    }
  }
  
  if (!is.null(ceil_at)){
    if (max(breaks) > ceil_at){
      breaks_adjusted_for_label <- c(breaks_adjusted_for_label[breaks_adjusted_for_label < ceil_at], # e.g., ceil the break to end by 100 for the original value
                                     min(ceil_at,breaks_adjusted_for_label[length(breaks_adjusted_for_label)]))
    }
  }
  
  return(list(num_bin=num_bin[res_idx],
              interval=interval_plot,
              breaks=breaks,
              breaks_label=sapply(1:length(breaks_adjusted_for_label), 
                                  function(x) if (x>1) paste0(round(breaks_adjusted_for_label[x-1], 2),      
                                                              ' to ',
                                                             round(breaks_adjusted_for_label[x], 2)) else '')[-1]))
}

#what is color bucket? no default value. It's in the app actually. 
# color_bucket <- calculateBucket(minVal,maxVal,
#                                 interval=interval,interval_options = seq(interval,500,interval),center=center,floor_at= -1 * as.numeric(input$select_format))
inferColor <- function(color_bucket,color_below='#e34a33',color_above='#2166ac',color_center='white',interval=10,center=100){
  # if (center > 0){
  #   breaks <- breaks[breaks>=-center] # original value should be floored at 0
  # }
  
  # color_below='#e34a33'
  # color_above='#31a354'
  # color_center='white'
  # interval=10
  # center=0
  # breaks <- breaks
  
  breaks <- color_bucket$breaks
  
  breaks_above <- breaks[breaks>=0]
  breaks_below <- breaks[breaks<=0]
  
  max_bin_oneside <- max(length(breaks[breaks<=0])-1, length(breaks[breaks>=0])-1)
  
  if (abs(sum(sign(breaks))) >= length(breaks)-1){ # -1 to take 0 into consideration
    
    # +1 so that the group around 0 gets slightly different color than white
    # then remove the white color
    color_val <- if (length(breaks_below)<2) { #changed from ==0 to account for the fact that breaks below will ALWAYS have a zero in it (len ==1)
      colorRampPalette(colors = c(color_center, color_above), space = "Lab")(max_bin_oneside+1)[-1]
    }else{
      colorRampPalette(colors = c(color_below, color_center), space = "Lab")(max_bin_oneside+1)[-(max_bin_oneside+1)]
    }
    
    return(data.frame(metric_color_group = color_val,
                      metric_color_label = color_bucket$breaks_label,
                      stringsAsFactors = F))
    
  }else{
    if (length(breaks[breaks==0])>1){
      ## when there are two 0 in breaks (only one region), we want to create a unique color group 100-100 that's white
      ## also max_bin_oneside gets two 0 counted, need to take 1 out
      ## in the end we end up with 3 colors: slightly above, at center, slightly below
      color_val <- c(colorRampPalette(colors = c(color_below, color_center), space = "Lab")(max_bin_oneside), 
                     colorRampPalette(colors = c(color_center, color_above), space = "Lab")(max_bin_oneside)[-1])
    }else{
      # create symmetric colors with max_bin_oneside+1 (considering white color) then take white out
      # then for each side take the effective ones (drop the ones not needed)
      
      # negative: (length(breaks_below)-1) is number of effective color groups for negative ones; if it's 1 we want to take the last color
      color_val_negative <- colorRampPalette(colors = c(color_below, color_center), space = "Lab")(max_bin_oneside+1)[-(max_bin_oneside+1)][(max_bin_oneside - (length(breaks_below)-1) +1):max_bin_oneside]
      # positive: (length(breaks_above)-1) is number of effective color groups for positive ones; if it's 1 we want to take the first color
      color_val_positive <- colorRampPalette(colors = c(color_center, color_above), space = "Lab")(max_bin_oneside+1)[-1][1:(length(breaks_above)-1)]
      
      color_val <- c(color_val_negative, color_val_positive)
    }
    
    return(data.frame(metric_color_group = color_val,
                      metric_color_label = as.factor(color_bucket$breaks_label),
                      stringsAsFactors = F))
  }
}
