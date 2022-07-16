# Counting up things
plot_counts_aesfill <- function(df, y, fill,  ylab, xlab, title){
ggplot(df, aes(y = forcats::fct_infreq(y), fill = fill)) + 
    geom_bar() + 
    theme_minimal() + 
    labs(y = ylab, x = xlab, title = title) + 
    scale_fill_grey() + 
    theme(text= element_text(size=14, family="Times"), 
          axis.text = element_text(size=14),
          axis.text.x = element_text(size = 10),
          plot.title = element_text(hjust = .5, vjust = 0, size = 16), 
          strip.background  = element_blank())
}


# In this version I am just changing the function to reflect the new datafrmae column names I updated. The previous one is for version 1 of the paper.
plot.theme.levels <- function(df, plot_title, ylim, colors, xlab){
  ggplot(df) + 
    geom_bar(aes(x = reorder(reference, -percent), y = percent, fill = cit_type), 
             position = "dodge", stat = "identity") + 
    scale_fill_manual(values = colors) +
    labs(x = xlab, y = "Percent of references", fill = "", title = plot_title) + 
    theme_minimal() + 
    theme(text= element_text(size=14, family="Times"), 
          plot.title = element_text(hjust = .5, vjust = 0, size = 14)) +
    coord_flip()
}

