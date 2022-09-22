##
## Plotting functions for
## R Shiny App: Team Honest Bangers
##

get_plot_dat <- function(df, filt_var, filt_val) {

  filt_df <- df %>% filter(.data[[filt_var]] %in% filt_val)
  return(filt_df)
  
}


get_num_song_plot <- function(df, pal) {
  
  g <- ggplot(df, aes(x = year_chr, fill = friend)) + 
    geom_bar(position = "stack", color = I("black")) + 
    xlab(NULL) + ylab("Number of songs") + 
    scale_fill_manual(values = all_of(pal)) +
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    theme_bw() + theme(legend.title = element_blank())
  g_int <- ggplotly(g, tooltip = c("count", "friend"))
  return(g_int)
  
}


get_song_length_plot <- function(df, pal) {
  
  df <- df %>% mutate(length = stri_sub_replace(str_pad(song_length_lostColon, 3, pad = "0"), 2, 1, value = ":"))

  y_max <- df %>% 
    pull(song_length) %>% 
    max()
  
  g <- ggplot(df, aes(x = friend, y = song_length/60)) + 
    geom_quasirandom(aes(fill = friend, label = song_title, label2 = artist, label3 = length),
                     width = 0.25, alpha = 0.8, shape = 21, size = 1.75, color = "grey30") +
    geom_boxplot(color = "grey30", outlier.shape = NA, width = 0.2,
                 position = position_nudge(x = -0.4)) +
    scale_fill_manual(values = all_of(pal)) + 
    scale_x_discrete(limits = rev) +
    scale_y_continuous(breaks = seq(0, ceiling(all_of(y_max)/60), 1)) + 
    ylab("Song (Minutes)") + xlab("Friend") +
    coord_flip() +
    theme_bw() + theme(legend.position = "none")
  
  g_int <- ggplotly(g, tooltip = c("song_title", "artist", "length")) %>%
    plotly::layout(boxgap = 0.8)
  
  g_int$x$data[length(g_int$x$data)] <- lapply(g_int$x$data[length(g_int$x$data)], FUN = function(x){
    x$marker = list(opacity = 0)
    return(x)
  })
  
  g_int$x$data[[length(g_int$x$data)]]$y <- g_int$x$data[[length(g_int$x$data)]]$y - 0.35
  
  return(g_int)
  
}
