visualize_airport_delays <- function() {
    
    delay_data <- flights %>% 
        group_by(dest) %>% 
        summarize(Avg = mean(arr_delay, na.rm = TRUE)) %>%
        left_join(airports, by = c("dest" = "faa")) %>%
        mutate(corrdinates = paste("lat = ", lat, ", lon = ", lon, sep = " ")) %>%
        filter(!is.na(name),
               !is.nan(Avg))
    
    
    
    plot <- ggplot(delay_data, aes(x = lat, y = lon, colour = Avg, alpha = 0.2, size = Avg)) +
        geom_point() +
        # theme(axis.text.x=element_text(angle=60,hjust=1),
        #       legend.position="none") +
        geom_jitter() +
        ggtitle("Average delay to airports") +
        ylab("Longitude") +
        xlab("Latitude")
    
    return(plot)
        
}
visualize_airport_delays()
