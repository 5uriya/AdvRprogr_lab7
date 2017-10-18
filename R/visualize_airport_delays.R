#' @title Visualize Airport Delays
#' @name visualize_airport_delays
#' @return A plot of airport delays by longitude and latitude
#' @description Visualize Airport Delays
#' @export

visualize_airport_delays <- function() {
    
    delay_data <- flights %>% 
        group_by(dest) %>% 
        summarize(Avg = mean(arr_delay, na.rm = TRUE)) %>%
        left_join(airports, by = c("dest" = "faa")) %>%
        filter(!is.na(name),
               !is.nan(Avg))
    
    
    
    plot <- ggplot(delay_data, aes(y = lat, x = lon, colour = Avg, alpha = 0.2, size = 3)) +
        geom_point() +
        # theme(legend.position="none") +
        ggtitle("Average delay to airports") +
        ylab("Longitude") +
        xlab("Latitude")

    return(plot)
        
}
visualize_airport_delays()
