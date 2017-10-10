visualize_airport_delays <- function() {
    
    delay_data <- flights %>% 
        group_by(dest) %>% 
        summarize(Avg = mean(arr_delay, na.rm = TRUE)) %>%
        left_join(airports, by = c("dest" = "faa")) %>%
        mutate(corrdinates = paste("lat = ", lat, ", lon = ", lon, sep = " ")) %>%
        filter(!is.na(name),
               !is.nan(Avg))
    
    
    
    plot <- ggplot(delay_data, aes(x = dest, y = Avg, labels = corrdinates)) +
        geom_point(aes(alpha = .2)) +
        theme(axis.text.x=element_text(angle=60,hjust=1),
              legend.position="none") +
        geom_jitter() +
        ggtitle("Average delay to airports") +
        ylab("Average delay in minutes") +
        xlab("Airports")
        
    
}


#
# joined <- airports %>% mutate(origin = faa) %>% semi_join(flights)
# dep_delay <- dplyr::select(flights, dep_delay, origin ) %>% mutate(faa = origin) %>% full_join(airports)
# arr_delay <- dplyr::select(flights, arr_delay, dest ) %>% mutate(faa = dest) %>% full_join(airports)
#
# group_dep <- dep_delay %>% group_by(name) %>% mean(dep_delay)
#
# grouped_dep <- dplyr::group_by(dep_delay, faa, name)
# summarise(grouped_dep, mean=mean(dep_delay))
#
# #group_dep_d <- filter(flights, dep_delay >0)  %>% dplyr::group_by(origin) %>% summarise(mean(dep_delay))
#
# group_delays <- flights %>% dplyr::group_by(dest) %>% summarise(summarize(Avg = mean(arr_delay,na.rm = TRUE))
#
# joined <-   group_arr_d %>% mutate(faa = dest) %>% left_join(airports)
#
#
#
# dplyr::group_by(iris, Species)
# dataset %>% group_by(name) %>% mutate(mean())
#
# dplyr::tbl_df(airports)
# dplyr::tbl_df(flights)
# summary(dep_delay)
# summary(group_dep)
