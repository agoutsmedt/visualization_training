library(ggplot2)
library(dplyr)

# Sample data
data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c( 27,23, 25, 22)
)

# Bar chart
bar_plot <- ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity",show.legend = F) +
  theme_pubclean() +
  scale_fill_jco()+
  labs(title = "Bar Chart: Differences are clear",y=NULL)

# Pie chart (using coord_polar)
pie_plot <- ggplot(data, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +scale_fill_jco()+
  labs(title = "Pie Chart: Harder to distinguish differences")

# Display both plots (e.g., in RMarkdown or with patchwork)
 pie_plot+bar_plot

?labs
 
