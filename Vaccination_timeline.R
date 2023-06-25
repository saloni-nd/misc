library(tidyverse)
library(scales)
library(data.table)

# Import xlsx spreadsheet
file_path <- ""

vax <- read_csv(paste0(file_path, "Vaccine timeline.csv"), skip = 0)

# Arrange by year
vax <- vax %>%
          arrange(Year, Name) 

# Give vaccines an ID number - all vaccines for the same disease get the same ID
setDT(vax)[, id := .GRP, by = Name]

vax <- as.tibble(vax)

# Label first vaccine for each disease
vax$First <- +(!duplicated(vax$Name))

# Select colors
group.colors <- c(Bacteria = "#38AABA", 
                  Virus = "#BC8E5A", 
                  Parasite ="#970046")

ggplot(data=vax, aes(x=Year,y=id, label=Name)) +
  geom_point(aes(color=Organism), size=2, stroke=1) +
  geom_point(colour="black", pch=21, size=2.5) +
  # Show text only for first vaccine
  geom_text(hjust=1, nudge_x=-4, data=filter(vax,First==1), size=3) + 
  scale_color_manual(values=group.colors) +
  theme_classic() +
  theme(axis.text.y=element_blank(),
        plot.title = element_text(size = 20)) +
  labs(title="Timeline of vaccines",
       subtitle="The year when each vaccine was approved for the first time.\nSubsequent vaccines for the same disease are shown on the same row.",
       x="",
       y="",
       color="Target organism") +
  coord_cartesian(xlim=c(1770,2025))
  
ggsave(paste0(file_path, "Vaccine_timeline.svg"))
  
