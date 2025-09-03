
##### load the required libraries #####
library(tidyverse)
library(hrbrthemes)
library(ggrepel)
library(scales)

##### choose the loan file #####
loans <- read_csv(choose.files())

##### create the plot #####
ggplot(loans, aes(x = levered_return, y = upb,
                  color = performing,
                  label = loan_name)) +
  geom_point(aes(size = equity), alpha = 0.6)+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  scale_color_manual(values = c("N" = '#23408F', "Y" = 'gray50'))+ # color NPLs blue, performing loans gray
  scale_x_continuous(labels = function(x) { # function to add S + or S - depending on label value
    ifelse(x >= 0, paste0('S + ', label_percent(accuracy = 1)(x)),
                         paste0('-S - ', label_percent(accuracy = 1)(abs(x))))
    },
                     limits = c(-0.10, 0.30), # limits of how long the x axis should be
                     breaks = seq(-0.10, 0.30, by = 0.05), # where the x axis labels should go
                     expand = expansion(mult = c(0,0))) + # expand the edges slightly
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), # format the y axis in millions
                     limits = c(0, 122000000), # limits of how long the y axis should be
                     breaks = seq(10000000, 110000000, by = 10000000), # where the y axis labels should go
                     expand = expansion(mult = c(0,0))) + # expand the edges slightly
  scale_size(labels = function(x) paste0(comma(x/1000000), "M"), # format the legend for bubble size as millions
             range = c(5, 12))+ # force the size range of the bubbles to be larger than the default
  geom_text_repel(size = 4.5, # repel the labels and increase the size
                  seed = 123,
                  box.padding = 1,
                  show.legend = FALSE, 
                  max.overlaps = 12) +
  ylab("GAAP Loan Balance") + # y axis label
  xlab("Levered Return excl. Fees") + # x axis label
  labs(#title = 'Levered Return by Loan',
       size = "Equity",
       color = "Performing (Y/N)") + 
  theme_bw() + 
  theme(axis.title.x = element_text(size = 12, color = 'gray35', face = 'plain', family = 'sans'), # change color and size of x axis title
        axis.text.x = element_text(size = 12, color = 'gray35', family = 'sans'), # change color and size of x axis labels
        axis.title.y = element_text(size = 12, color = 'gray35', face = 'plain', family = 'sans'), # change color and size of y axis title
        axis.text.y = element_text(size = 12, color = 'gray35', family = 'sans'), # change color and size of y axis labels
        plot.title = element_text(color = 'gray35', face = 'plain', family = 'sans'),
        panel.border = element_blank(), # remove the plot border
        axis.line.x = element_line(color = 'gray35'), # change color of x axis line
        axis.line.y = element_line(color = 'gray35'), # change color of y axis line
        panel.grid.major = element_line(color = "gray95", size = 0.5),  # Adjust grid lines color
        panel.grid.minor = element_line(color = "gray95", size = 0.25), # Adjust grid lines color
        panel.grid.minor.y = element_blank(), # remove minor grid lines
        panel.grid.minor.x = element_blank(), # remove minor grid lines
        axis.ticks = element_line(color = 'gray90', size = 0.5), # change color of tick lines
        legend.title = element_text(hjust = 0.5), # center justify the legends
        legend.text = element_text(hjust = 0.5) # center justify the legends
        )
  

##### save the plot #####
ggsave("C:/Users/depet/Downloads/loans.png",
       width = 16,
       height = 9,
       dpi = 300)

