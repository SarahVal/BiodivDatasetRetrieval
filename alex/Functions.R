


## 1. Counts of each relevance category
###################################################

count_by_relevance <- function(df) {
  
  df_counts_relevance <- as.data.frame(table(df$dataset_relevance))
  df_counts_relevance <- df_counts_relevance[order(-df_counts_relevance$Freq),]
  colnames(df_counts_relevance) <- c("dataset relevance", "N")

  return(df_counts_relevance)
  
}


## 2. Queries performance
###################################################



## Create tables that count the number of datasets of each query belonging to each relevance category


compute_df_n_relevance_queries <- function(data) {
  
  
  # Cretae variable queries (levels of factor variable id_query)
  
  queries <- levels(data$id_query)
  queries <- queries[-1] # I eliminate the first level that creates which is " " (don't know why because there are no blank entries)
  
  # Cretae vectors to store the number of rows matching conditions in the loop
  
  n_publications <- c(numeric(length(queries)))
  n_high <- c(numeric(length(queries)))
  n_moderate <- c(numeric(length(queries)))
  n_low <- c(numeric(length(queries)))
  n_non_relevant <- c(numeric(length(queries)))
  n_no_dataset <- c(numeric(length(queries)))
  
  
  # loop through the queries to count the number of entries matching the different levels of the dataset_relevance variable
  
  for (i in 1:length(queries)) {
    
    n_publications[i] <- length(which(data$id_query == queries[i]))
    
    n_high[i] <- length(which(data$id_query == queries[i] & data$dataset_relevance == "H"))
    
    n_moderate[i] <- length(which(data$id_query == queries[i] & data$dataset_relevance == "M"))
    
    n_low[i] <- length(which(data$id_query == queries[i] & data$dataset_relevance == "L"))
    
    n_negligible <- length(which(data$id_query == queries[i] & data$dataset_relevance == "X"))
    
    n_non_valid <- length(which(data$id_query == queries[i] & data$valid_yn == "no"))
    
    n_non_relevant[i] <- n_negligible + n_non_valid
    
    n_no_dataset[i] <- length(which(data$id_query == queries[i] & data$dataset_relevance == "No dataset"))
    
  }
  
  
  df_queries_counts <- data.frame(queries,
                                  n_publications,
                                  n_high,
                                  n_moderate,
                                  n_low,
                                  n_non_relevant,
                                  n_no_dataset
  )
  
  df_queries_counts_ord <- df_queries_counts[order(df_queries_counts$queries),]
  
  return(df_queries_counts_ord)
  
}



# Plot raw data of queries and counts of dataset relevance



plot_queries_relevance_raw <- function(df) {
  
  df_queries_counts_ord_npubl <-df[order(df$n_publications),]
  
  df_queries_counts_ord_npubl <- melt(df_queries_counts_ord_npubl, id ="queries")
  
  df_queries_counts_ord_npubl <- df_queries_counts_ord_npubl[df_queries_counts_ord_npubl$value > 0,]
  
  df_queries_counts_ord_npubl$variable <- factor(df_queries_counts_ord_npubl$variable, 
                                                 levels = c("n_publications", 
                                                            "n_high", "n_moderate", 
                                                            "n_low", "n_non_relevant", 
                                                            "n_no_dataset"))
  
  plot_queries_relevance_counts <- ggplot(df_queries_counts_ord_npubl, 
                                          aes(x=queries, y=value, group = variable, 
                                              color = variable,shape=variable )) +
    geom_segment( aes(x=queries ,xend=queries, y=0, yend=max(value)), color="grey") +
    geom_point(size=4, alpha = 0.8) +
    coord_flip() +
    # theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="top"
    ) +
    xlab("Query ID")+
    ylab("publications counts") +
    scale_color_manual(values = c("gray60", "red","purple2", "dodgerblue", "black", "black"))+
    scale_shape_manual(values = c(15, 19, 19, 19, 1,13))+
    my.theme
  
  
  return(plot_queries_relevance_counts)
  
}








plot_relevance_queries <- function(levels_relevance) {
  
  
  df_queries_counts_ord_npubl[df_queries_counts_ord_npubl$variable %in% levels_relevance,]
  
  # if (levels_relevance)
  
}

levels_relevance <- c("n_high", "n_moderate")













## Compute index1 (sum of puntuations according to relevance datasets) and order, 
# ad dit as new variable of the dataframe, and order it in descending order
# according to the index specified (either 1 or 2)



compute_index12 <- function(df_counts, order_based_index) {
  
  
  
  ### Index1
  
  index1 <- numeric(nrow(df_counts))
  index2 <- numeric(nrow(df_counts))
  
  
  
  df_counts <- cbind(df_counts, index1, index2)
  
  for (i in 1:nrow(df_counts)) {
    
    puntuation_high <- df_counts[i, "n_high"]*5
    
    puntuation_high2 <- df_counts[i, "n_high"]*5/df_counts[i, "n_publications"]
    
    puntuation_moderate <- df_counts[i, "n_moderate"]*2
    
    puntuation_moderate2 <- df_counts[i, "n_moderate"]*2/df_counts[i, "n_publications"]
    
    puntuation_low <- df_counts[i, "n_low"]/2
    
    puntuation_low2 <- (df_counts[i, "n_low"]/2)/df_counts[i, "n_publications"]
    
    
    df_counts[i,"index1"] <- puntuation_high + puntuation_moderate + puntuation_low
    
    df_counts[i,"index2"] <- puntuation_high2 + puntuation_moderate2 + puntuation_low2
  }
  
  
  
  if (order_based_index == "1") {
    
    df_counts_ord <- df_counts[order(-df_counts$index1),]
    
   
  } else if (order_based_index == "2") {
    
    df_counts_ord <- df_counts[order(-df_counts$index2),]
    
  }

  
  
  
  return(df_counts_ord)
  

  
}



plot_queries_index <- function(df) {
  
  ## Plot for index1
  
  plot_queries_index1 <- ggplot(df, aes(x=queries, y=index1, group = n_publications, color = n_publications) ) +
    geom_segment( aes(x=queries ,xend=queries, y=0, yend=max(index1)), color="grey") +
    geom_point(size=4, alpha = 0.8) +
    coord_flip() +
    # theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="right"
    ) +
    xlab("Query ID")+
    ylab("index 1") +
    scale_fill_gradient(low = "blue", high = "red")+
    my.theme
  
  
  ## Plot for index2
  
  plot_queries_index2 <- ggplot(df, aes(x=queries, y=index2, group = n_publications, color = n_publications) ) +
    geom_segment( aes(x=queries ,xend=queries, y=0, yend=max(index2)), color="grey") +
    geom_point(size=4, alpha = 0.8) +
    coord_flip() +
    # theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="right"
    ) +
    xlab("Query ID")+
    ylab("index 2") +
    scale_fill_gradient(low = "blue", high = "red")+
    my.theme
  
  
  ## Merge both plots
  
  plot <- ggarrange(plot_queries_index1, plot_queries_index2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
  
  return(plot)
  
}




## 3. Temporal duration and frequency
###################################################




# Calculate the number of publications without temporal duration information
# These are going to be those whose value is "no" in the dataset, which is converted
# to NAs and then the NAs are counted

count_not.reported_temporal.duration <- function(df) {
  
  dataset_temp_duration <- subset(df, 
                                  temporal_duration_y > 0 | temporal_duration_y == "no",
                                  select = c(temporal_duration_y,id_query))
  
  dataset_temp_duration$temporal_duration_y <- as.numeric(dataset_temp_duration$temporal_duration_y)
  
  
  nNa <- length(dataset_temp_duration[is.na(dataset_temp_duration)])
  
  return(nNa)
  
}


# Obtain dataframe with counts of each duration

count_durations <- function(df, order_by) {
  
  dataset_temp_duration <- subset(df, 
                                  temporal_duration_y > 0 | temporal_duration_y == "no",
                                  select = c(temporal_duration_y,id_query))
  
  dataset_temp_duration$temporal_duration_y <- as.numeric(dataset_temp_duration$temporal_duration_y)
  
  dataset_temp_duration <- dataset_temp_duration[order(dataset_temp_duration$temporal_duration_y),]
  
  dataframe_duration_counts <- as.data.frame(table(dataset_temp_duration$temporal_duration_y))
  
  colnames(dataframe_duration_counts) <- c("temporal_duration","counts")
  
  if(order_by == "temporal_duration"){
    
    dataframe_duration_counts <- dataframe_duration_counts[order(
      dataframe_duration_counts$temporal_duration),]
    
  } else if(order_by == "counts") {
    
    dataframe_duration_counts <- dataframe_duration_counts[order(
      -dataframe_duration_counts$counts),]
  }
  
  
  return(dataframe_duration_counts)
  
}







# Plot counts of each duration


plot_duration_counts <- function(df, counts_Na) {
  
  plot <- ggdotchart(df, x = "temporal_duration", y = "counts",
                                   size = 5, 
                                   add = "segment",
                                   xlab = "Temporal duration (years)",
                                   ylab = "N articles",
                                   sorting = "none",
                                   add.params = list(color = "lightgray", size = 1.3),
                                   position = position_dodge(0.45),
                                   ggtheme = theme_pubclean(),
                                   title = "Temporal duration in retrieved datasets")+
    geom_label(
      label= paste(counts_Na, "not reported"), 
      x=20,
      y=max(df$counts)-2,
      label.padding = unit(0.55, "lines"), # Rectangle size around label
      label.size = 0.15,
      color = "black",
      fill="white"
    )+
    theme(axis.text.x = element_text(angle = 0, hjust=0.95,vjust=0.2))+
    my.theme
  
  return(plot)
  
}



# Plot duration in each relevance category

plot_duration_relevance <- function(df, counts_Na) {
  
  df$temporal_duration_y <- as.numeric(df$temporal_duration_y)
  
  df$dataset_relevance <- as.factor(df$dataset_relevance)
  
  df$dataset_relevance <- ordered(df$dataset_relevance,
                                                    levels = c("X", "L", "M", "H"))
  
  
  plot_rel_duration <- ggplot(subset(df, !is.na(df$dataset_relevance)), 
                              aes(x = dataset_relevance, y = temporal_duration_y))+
    xlab("dataset relevance")+
    ylab("duration (years)")+
    geom_boxplot()+
    geom_jitter(shape=16, position=position_jitter(0.2), size =3, alpha = 0.5) +
    geom_label(
      label= paste(counts_Na, "not reported"), 
      x = "X",
      y=30,
      label.padding = unit(0.55, "lines"), # Rectangle size around label
      label.size = 0.15,
      color = "black",
      fill="white"
    )+
    theme_bw()+
    my.theme 
  
  return(plot_rel_duration)
}



## 4. Spatial range
###################################################


# count publications where spatial range is not reported

count_not.reported_spatial_range <- function(df) {
  
  dataset1 <- df[df$dataset_relevance != "cant access",]
  
  dataset1$dataset_relevance <- as.factor(dataset1$dataset_relevance)
  
  spatial_range_km2_vec <- dataset1$spatial_range_km2[dataset1$spatial_range_km2 != ""]
  
  spatial_range_km2_vec <- spatial_range_km2_vec[!is.na(spatial_range_km2_vec)]
  
  n_not_reported <- length(which(spatial_range_km2_vec == "no"))
  
  return(n_not_reported)
  
}



# Plot spatial ranges counts

plot_spat.range_counts <- function(df) {
  
  dataset1 <- df[df$dataset_relevance != "cant access",]
  
  dataset1$dataset_relevance <- as.factor(dataset1$dataset_relevance)
  
  spatial_range_km2_vec <- dataset1$spatial_range_km2[dataset1$spatial_range_km2 != ""]
  
  spatial_range_km2_vec <- spatial_range_km2_vec[!is.na(spatial_range_km2_vec)]
  
  spatial_range_km2_vec <- as.numeric(spatial_range_km2_vec) 
  
  
  max(spatial_range_km2_vec)
  
  
  cuts_range_km2_vec <- cut(spatial_range_km2_vec, breaks = c(0,5000, 15000, 
                                                              max(spatial_range_km2_vec, na.rm = TRUE)), 
                            include.lowest = TRUE)
  
  df_cuts_range_km2_vec <- as.data.frame(table(cuts_range_km2_vec))
  
  #By default, the argument right is set to TRUE, so the intervals are opened on the left and closed on the right (x, y].
  
  df_cuts_range_km2_vec$Freq <- as.numeric(df_cuts_range_km2_vec$Freq)
  
  ######################################################################
  
  colnames(df_cuts_range_km2_vec) = c("ranges", "counts")
  
  df_cuts_range_km2_vec$ranges <- as.factor(df_cuts_range_km2_vec$ranges)
  
  plot <- ggdotchart(df_cuts_range_km2_vec, x = "ranges", y = "counts",
                              size = 5, 
                              add = "segment",
                              xlab = "Spatial range (km2)",
                              ylab = "N articles",
                              sorting = "none",
                              add.params = list(color = "lightgray", size = 1.3),
                              position = position_dodge(0.45),
                              ggtheme = theme_pubclean(),
                              title = "Spatial range in retrieved datasets")+
    scale_x_discrete(labels = c("=< 5.000", "(5.000-15.0000]", "> 15.000"))+
    theme(axis.text.x = element_text(angle = 0, hjust=0.95,vjust=0.2))+
    my.theme
  
  return(plot)
  
  
}





## 5. Plot temporal duration, spatial range, relevance
###################################################


plot_spat_temp_relevance <- function(df) {
  
  
  dataset1 <- df[df$dataset_relevance != "cant access",]
  
  dataset1$dataset_relevance <- as.factor(dataset1$dataset_relevance)
  
  
  
  dataset_filt <- dataset1[,c("spatial_range_km2","temporal_duration_y", "dataset_relevance")]
  
  dataset_filt <- dataset_filt[
    dataset_filt$spatial_range_km2 != "" & 
      dataset1$temporal_duration_y != "" & 
      dataset_filt$dataset_relevance != "No dataset" & 
      dataset_filt$dataset_relevance != "" ,]
  
  
  
  no_spatial.range <- length(which(dataset_filt$spatial_range_km2 == "no"))
  no_temp.duration <- length(which(dataset_filt$temporal_duration_y == "no"))
  
  
  dataset_filt$spatial_range_km2 <- as.numeric(dataset_filt$spatial_range_km2)
  
  dataset_filt$spatial_range_km2 <- cut(dataset_filt$spatial_range_km2, breaks = c(0,5000, 10000, 15000,                                                      max(dataset_filt$spatial_range_km2, na.rm = TRUE)), 
                                        include.lowest = TRUE)
  
  dataset_filt$temporal_duration_y <- as.numeric(dataset_filt$temporal_duration_y)
  
  
  
  
  
  plot <- ggplot(na.omit(dataset_filt), 
                 aes(x = spatial_range_km2, y = temporal_duration_y))+
    xlab("spatial range (km2)")+
    ylab("duration (years)")+
    geom_boxplot()+
    geom_jitter(aes(colour = dataset_relevance),shape=16, position=position_jitter(0.2), size =4.5, alpha = 0.5) +
    geom_label(
      label= paste(no_spatial.range+no_temp.duration, "with no data:", no_spatial.range, "(spatial range),", no_temp.duration, "(temp. duration)"), 
      x = "(5e+03,1e+04]",
      y=max(na.omit(dataset_filt$temporal_duration_y)),
      label.padding = unit(0.55, "lines"), # Rectangle size around label
      label.size = 0.15,
      color = "black",
      fill="white"
    )+
    scale_color_manual(values = c("red","dodgerblue","purple2"))+
    theme_bw()+
    my.theme

  
  return(plot)
  
}





## 6. EBV data types and format



compute_df_data.type <- function(df) {
  

  
  df$data_type <- as.character(df$data_type)
  
  length(which(startsWith(df$data_type, "abundance") == TRUE))
  
  
  
  N_presence.only <-length(which(startsWith(df$data_type, "presence only") == TRUE |
                                 startsWith(df$data_type, "presence-only") == TRUE |
                                 endsWith(df$data_type, "presence only") == TRUE |
                                 endsWith(df$data_type, "presence-only") == TRUE))
  
  N_presence.absence <-length(which(startsWith(df$data_type, "presence-absence") == TRUE |
                                    startsWith(df$data_type, "presence-absence") == TRUE))
  
  N_abundance <-length(which(startsWith(df$data_type, "abundance") == TRUE |
                             startsWith(df$data_type, "abundance") == TRUE |
                             endsWith(df$data_type, "abundance") == TRUE |
                             endsWith(df$data_type, "abundance") == TRUE |
                             startsWith(df$data_type, "density") == TRUE |
                             endsWith(df$data_type, "density") == TRUE))
  
  N_EBV_genetic <- length(which(startsWith(df$data_type, "EBV genetic analysis") == TRUE |
                                endsWith(df$data_type, "EBV genetic analysis") == TRUE))
  
  
  
  N_genetic_analyses <- length(which(startsWith(df$data_type, "genetic analyses") == TRUE |
                                     endsWith(df$data_type, "genetic analyses") == TRUE))
  
  N_distribution <- length(which(startsWith(df$data_type, "distribution") == TRUE |
                                 endsWith(df$data_type, "distribution") == TRUE))
  
  
  N_other <-length(which(startsWith(df$data_type, "other") == TRUE |
                         endsWith(df$data_type, "other") == TRUE))
  
  data_type_col <- c("presence.only",
                     "presence.absence",
                     "abundance",
                     "EBV_genetic",
                     "genetic_analyses",
                     "distribution",
                     "other")
  
  N_articles <- c(N_presence.only,
                  N_presence.absence,
                  N_abundance,
                  N_EBV_genetic,
                  N_genetic_analyses,
                  N_distribution,
                  N_other)
  
  df_data_type <- data.frame(data_type_col,N_articles)
  
  
  # Calculate percentage of each one
  
  df_data_type$percentage <- (df_data_type$N_articles)*100/sum(df_data_type$N_articles)
  
  
  return(df_data_type)
  
  
  
}




plot_data.type_counts <- function(df) {
  
  plot <- ggdotchart(df, x = "data_type_col", y = "N_articles",
                               size = 5, 
                               add = "segment",
                               xlab = "Data type",
                               ylab = "N articles",
                               sorting = "none",
                               add.params = list(color = "lightgray", size = 1.3),
                               position = position_dodge(0.45),
                               ggtheme = theme_pubclean(),
                               title = "Data type in retrieved datasets")+
    theme(axis.text.x = element_text(angle = 0, hjust=0.95,vjust=0.2))+
    scale_x_discrete(guide = guide_axis(n.dodge=2))+
    my.theme
  
  plot
  
}



plot_data.type_format <- function(df) {
  
  
  
  dataset_fil_format <- df[df$dataset_location != "",]
  
  location <- dataset_fil_format$dataset_location
  format <- dataset_fil_format$dataset_format
  
  df <- data.frame(location, format)
  
  df1 <- df %>%               
    separate_rows(format, sep=",") 
  
  df2 <- df1 %>%               
    separate_rows(location, sep=",") 
  
  df2$location <- trimws(df2$location)
  
  df2$location <- as.factor(df2$location)
  
  df2$format <- as.factor(df2$format)
  
  df3 <- as.data.frame(table(df2$format, df2$location))
  
  colnames(df3) = c("format", "location", "counts")
  
  
  
  plot <- ggplot(df3, aes(x=location, y=counts, group = format, color = format) ) +
    geom_segment( aes(x=location ,xend=location, y=0, yend=max(counts)), color="grey") +
    geom_point(size=4, alpha = 0.8) +
    coord_flip() +
    # theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="top"
    ) +
    xlab("Dataset location")+
    ylab("N publications") +
    my.theme
  
  
  return(plot)
  
  
}





## 8. Datasets accessibility and relevance per year

# Raw plot with all years


plot_relevance_year <- function(df) {
  
  
  df$year1 <- format(as.POSIXct(df$publication_date, format = "%m/%d/%Y"), format ="%Y")
  
  dataset_valid <- df[df$valid_yn == "yes",]
  
  dataset_valid1 <- dataset_valid[is.na(dataset_valid$year) == FALSE, ]
  
  dataset_valid1$dataset_relevance <- ordered(dataset_valid1$dataset_relevance,levels = c("", "No dataset", "cant access", "X", "L", "M", "H"))
  
  plot <- ggplot(dataset_valid1, aes(x=year1, y=dataset_relevance)) + geom_point()+
    geom_bin2d() +
    xlab("year") +
    ylab("dataset accessibility and relevance") +
    ggtitle("dataset accessibility, relevance, and publication year")+
    scale_fill_continuous(type = "viridis") +
    theme_bw()+
    scale_x_discrete(guide = guide_axis(n.dodge=2))+
    my.theme
  
  return(plot)
  
}


# Plot with temporal range years

compute_df_relevance_year.range <- function(df) {
  
  df$year1 <- format(as.POSIXct(df$publication_date, format = "%m/%d/%Y"), format ="%Y")
  
  dataset_valid <- df[df$valid_yn == "yes",]
  
  dataset_valid1 <- dataset_valid[is.na(dataset_valid$year) == FALSE, ]
  
  dataset_valid1$dataset_relevance <- ordered(dataset_valid1$dataset_relevance,levels = c("", "No dataset", "cant access", "X", "L", "M", "H"))
  
  
  
  dataset_valid1$year1.cut <- cut(as.numeric(dataset_valid1$year1), 
                                  breaks = c(0,1989, 2000, 2010,
                                max(dataset_valid1$year1, na.rm = TRUE)), 
                                  include.lowest = TRUE)
  
  dataset_valid1[dataset_valid1$dataset_relevance]
  
  
  df_counts_relevance_temporal.range <- as.data.frame(table(
    dataset_valid1$dataset_relevance, dataset_valid1$year1.cut))
  
  colnames(df_counts_relevance_temporal.range) = c(
    "dataset relevance", "publication year", "Publication counts")
  
  
  return(df_counts_relevance_temporal.range)
  
}





plot_relevance_year.range <- function(df) {
  
  df$year1 <- format(as.POSIXct(df$publication_date, format = "%m/%d/%Y"), format ="%Y")
  
  dataset_valid <- df[df$valid_yn == "yes",]
  
  dataset_valid1 <- dataset_valid[is.na(dataset_valid$year) == FALSE, ]
  
  dataset_valid1$dataset_relevance <- ordered(dataset_valid1$dataset_relevance,levels = c("", "No dataset", "cant access", "X", "L", "M", "H"))
  
  
  
  dataset_valid1$year1.cut <- cut(as.numeric(dataset_valid1$year1), 
                                  breaks = c(0,1989, 2000, 2010,
                                             max(dataset_valid1$year1, na.rm = TRUE)), 
                                  include.lowest = TRUE)
  
  dataset_valid1[dataset_valid1$dataset_relevance]
  
  
  plot <- ggplot(dataset_valid1, aes(x = year1.cut, fill = forcats::fct_rev(dataset_relevance))) +
    geom_bar()+
    xlab("year") +
    ylab("dataset accessibility and relevance") +
    ggtitle("dataset accessibility, relevance, and publication year")+
    scale_fill_manual(values = c("red","dodgerblue","purple2","darkgrey","yellow","floralwhite"))+
    scale_x_discrete(labels = c("<= 1990", "(1990-2000]", "(2000-2010]", "> 2010"))+
    theme_bw()+
    my.theme
  
  return(plot)
  
  
}




# Plot location geospatial-temporal information

compute_df_location_info <- function(df) {
  
  df_loc_info <- df[,c("dataset_location", "spatial_range_position","temporal_range_position", "temporal_duration_position")]
  
  df_loc_info <- df_loc_info[df_loc_info$dataset_location != "",]
  df_loc_info <- df_loc_info[df_loc_info$dataset_location != "no",]
  
  df_loc_info1 <- df_loc_info %>%               
    separate_rows(dataset_location, sep=",") 
  
  df_loc_info1 <- df_loc_info1[-is.na(df_loc_info1$dataset_location),]
  
  df_loc_info2 <- df_loc_info1 %>%               
    separate_rows(spatial_range_position, sep=",") 
  
  df_loc_info3 <- df_loc_info2 %>%               
    separate_rows(temporal_range_position, sep=",") 
  
  df_loc_info4 <- df_loc_info3 %>%               
    separate_rows(temporal_duration_position, sep=",") 
  
  df_loc_info4$dataset_location<- trimws(df_loc_info4$dataset_location)
  
  df_loc_info4$dataset_location <- as.factor(df_loc_info4$dataset_location)
  
  df_loc_info4$spatial_range_position <- trimws(df_loc_info4$spatial_range_position)
  df_loc_info4$temporal_range_position <- trimws(df_loc_info4$temporal_range_position)
  df_loc_info4$temporal_duration_position <- trimws(df_loc_info4$temporal_duration_position)
  
  return(df_loc_info4)
  

  
}


plot_location_info <- function(df, variable, colname) {
  
  
  df1 <- df[variable != "",]

  df2 <- df1[, c("dataset_location", colname)]
  
  plot <- ggplot(na.omit(df2), aes(x= df2[,2], fill = dataset_location)) + 
    geom_bar(aes(y = (..count..))) +
    geom_text(stat='count', aes(label=..count..),position = position_stack(vjust = 0.5))+
    theme_bw()+
    my.theme+
    theme(axis.text.x = element_text(angle = 0, hjust=0.95,vjust=0.2, size = 9))+
    ylab("N retrieved articles") +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
  
  return(plot)
  
  
}



################################

# Get list of keywords present in a string


get_keywords <- function(input_string, dataset_types) {
  
  vec_keyword_present <- c()
  vec_keyword <- c()
  
  for (i in 1:nrow(dataset_types)) {
    
    if(str_detect(string = input_string, pattern = dataset_types$keywords[i]) == TRUE){
      
      vec_keyword <- dataset_types$keywords[i]
      
    }
    
    vec_keyword_present[i] <- vec_keyword[!is.na(vec_keyword)]
    
  }
  
  return(vec_keyword_present)
  
}















