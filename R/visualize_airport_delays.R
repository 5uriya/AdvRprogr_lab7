visualize_airport_delays <- function() {
    
    group_delays <-
        flights %>% dplyr::group_by(dest) %>% summarize(Avg = mean(arr_delay, na.rm = TRUE))
    
    joined <-
        group_delays %>% mutate(faa = dest) %>% left_join(airports)
    
    coord <- joined %>%
        group_by(dest) %>%
        summarize(Coordinates = paste0("lat = ", lat[1], ", lon = ", lon[1], collapse = " "))
    
    final_data <- dplyr::left_join(joined, coord, by = "dest")
    plot <-
        ggplot(final_data, aes(x = dest, y = Avg, label = Coordinates))
    
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
