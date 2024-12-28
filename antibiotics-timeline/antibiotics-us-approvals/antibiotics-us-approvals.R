library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)
library(data.table)
library(readxl)

# Data source: Coussens, N. P., Molinaro, A. L., Culbertson, K. J., Peryea, T., Zahoránszky-Köhalmi, G., Hall, M. D., & Daines, D. A. (2018). Better living through chemistry: Addressing emerging antibiotic resistance. Experimental Biology and Medicine, 243(6), 538–553. https://doi.org/10.1177/1535370218755659

# !!! Download spreadsheet and replace this with path to file
file_path <- ""

ab_drugs <- read_excel(paste0(file_path, "Antibiotics_approvals_US.xlsx"),
                       sheet=1,
                       col_types=c("text", "text", "text", "numeric", 
                                   "numeric", "text", 
                                   "text", "text"))

colnames(ab_drugs) <- c("Type", "Class", "Drug", "Year", "MCS_cluster", "ISO_type", "Molecular_weight", "Source")

# Arrange by year
ab_drugs <- ab_drugs %>%
  arrange(Year) 

# Give drugs an ID number to put them in order
setDT(ab_drugs)[, id := .GRP, by = Class]

# Label first antibiotic for each class
ab_drugs$First <- +(!duplicated(ab_drugs$Class))


ab_drugs <- as_tibble(ab_drugs)

# Select colors
group.colors <- c("Beta lactams" = "#38AABA", 
                  "MLS family" = "#FDE12D", 
                  "Other antibiotics" ="#9649CB")

ggplot(data=ab_drugs, aes(x=Year, 
                          y=id, 
                          label=Class)) +
  geom_point(aes(fill=Type), color="black", size=2.5, pch=21, stroke=0.8) +
  # Show names
  geom_text(hjust=1, nudge_x=-2.5, data=filter(ab_drugs,First==1), size=3.5) + 
  theme_classic() +
  scale_fill_manual(values=group.colors) +
  theme(text = element_text(family = "Lato"), # Set Lato for all text elements
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 12), # Adjust x-axis label size
        axis.ticks.y=element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12), # Set subtitle size
        plot.caption = element_text(size = 12),  # Set caption size
        legend.title = element_text(size = 14),  # Adjust legend title size
        legend.text = element_text(size = 12),   # Adjust legend categories size
        axis.title.x.top = element_text(vjust = 1.5), # Adjust x-axis title position at the top
        axis.text.x.top = element_text(vjust = 1.5),  # Adjust x-axis text position at the top
        axis.ticks.x.top = element_line(),           # Add ticks to the top x-axis
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
  labs(title="Timeline of the development of antibiotic drugs",
       subtitle="The year when each antibiotic drug class was first approved in the United States.\nAdditional dots on each row show subsequent antibiotics of the same class.",
       x="",
       y="",
       fill="Type",
       caption="Source: Coussens et al. (2018). Better living through chemistry: Addressing emerging antibiotic resistance.") +
  coord_cartesian(xlim=c(1930,2024)) +
  scale_y_reverse() + # Flip y-axis
  scale_x_continuous(breaks= seq(1930,2020,by=10), 
                     labels = seq(1930,2020,by=10),
                     position = "bottom") # Position x-axis at the bottom


ggsave(paste0(file_path, "antibiotics_timeline_us_approvals.svg"),height=8,width=10)
ggsave(paste0(file_path, "antibiotics_timeline_us_approvals.png"),height=8,width=10)

