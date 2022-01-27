library(shiny)

enableBookmarking(store = "url")

# sample_df <- data.frame(x = c(0,1,0),
#                         y = c(1,0,-1))
# 
# sample_df %>%
#   ggplot(aes(x,y, color = y)) +
#   geom_point() +
#   scale_colour_gradient2(low = "darkred", high = "darkgreen",
#                          midpoint = 0) +
#   labs(
#     color = ""
#   ) +
#   theme(
#     legend.direction = "vertical"
#   )
