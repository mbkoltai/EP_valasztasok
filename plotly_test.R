### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# PLOTLY test
# Create a larger sample dataset with more data points
set.seed(42)
data <- data.frame(
  category = rep(letters[1:4], each = 100),
  x = rnorm(400),
  y = rnorm(400),
  label = sample(letters, 400, replace = TRUE)
)

# Create a faceted ggplot with many data points
p <- ggplot(data, aes(x = x, y = y, color = category, label = label)) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~category) +
  ggtitle("Complex Faceted Plot Example") +
  theme_minimal()

# Convert to an interactive plot using ggplotly
interactive_plot <- ggplotly(p, tooltip = c("x", "y", "label", "category"))

# Optionally optimize rendering with WebGL (useful for large datasets)
interactive_plot <- interactive_plot %>% toWebGL()

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(interactive_plot, "complex_faceted_plot.html")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


p_orsz <- l_orszagos$eredmenyek %>%
  ggplot(aes(y=EV_csoport,x=value)) + 
  facet_wrap(~valt_tipus,scales = "free_x",nrow=2) +
  geom_bar(aes(fill=LISTA),stat="identity",position="stack",color="black",width=9/10) + # 
  labs(fill="Pártlisták",caption=l_ellenzek_2019_2024$captions) +
  geom_text(data=l_orszagos$text,aes(label=signif(value,ifelse(grepl("szav",valt_tipus),4,3))),
            hjust=-0.1,size=6) +
  scale_x_continuous(expand = expansion(mult=c(0.01/2,0.08))) +
  scale_fill_manual(values=l_orszagos$szinek) +
  xlab("") + ylab("")  + theme_bw() + val_theme # + theme()

# Convert to an interactive plot using ggplotly
interactive_plot <- ggplotly(p_orsz, tooltip = c("x", "y", "label", "category"))

# Optionally optimize rendering with WebGL (useful for large datasets)
# interactive_plot <- interactive_plot %>% toWebGL()

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(interactive_plot, "PLOTS/interactive/FIDESZ_ellenzek_Mihaz_EP_2019_2024_orsz.html")
