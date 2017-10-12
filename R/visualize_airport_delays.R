visualize_airport_delays <- function() {
    
    delay_data <- flights %>% 
        group_by(dest) %>% 
        summarize(Avg = mean(arr_delay, na.rm = TRUE)) %>%
        left_join(airports, by = c("dest" = "faa")) %>%
        filter(!is.na(name),
               !is.nan(Avg))
    
    
    
    plot <- ggplot(delay_data, aes(x = lat, y = lon, colour = Avg, alpha = 0.2)) +
        geom_point() +
        # theme(legend.position="none") +
        ggtitle("Average delay to airports") +
        ylab("Longitude") +
        xlab("Latitude")
    
    return(plot)
        
}
visualize_airport_delays()
