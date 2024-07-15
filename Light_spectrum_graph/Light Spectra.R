librarian::shelf(plyr, dplyr, readr, tidyverse, janitor, expss,
                 rstudioapi,  readxl, cowplot, ggpubr, 
                 photobiology, photobiologyWavebands, ggspectra,sysfonts,showtext,
                 quiet = T,
                 update_all = FALSE)

##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))




# Importing files
spec_data <- read_delim("Datafile/uMOL_EO1.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)%>%
  as_tibble()%>%
  dplyr::rename(
    wave = "Wavelength(nm)",
    ppfd = "PFD(umol m-2 s-1)"
  )

colnames(spec_data)



# # If importing multiple files to calculate average
# # Importing files
# mydir="raw_light_data/spectra"                                                            #Mention the directory (as "mydir") in the W.Dir. containing the datafiles
# file_list= list.files(path=mydir, pattern="*.csv", full.names=TRUE)               #List all the .csv files in the specified "mydir" and assign it to "myfiles". 
# file_list                                                                         #Shows all the files present 
# 
# 
# # Importing files
# data_master <- ldply(file_list, read.table, sep = ",", fill=T, header = T)%>%     #collectively apply "import function for all the files in the specified directory
#   as_tibble()
# 
# 
# colnames(data_master)[1:3] <- c("wave", "ppfd", "max_int") 
# 
# 
# # calculate normalized ppfd
# data_master$nppfd <- data_master$ppfd/data_master$max_int
# 
# 
# 
# data_mean <- aggregate(nppfd~wave, data_master, mean) #make a dataset with mean values of four reps










# Calculate the proportion of red, blue and green in the the PAR region of the spectrum
perc_data <-spec_data[-c(1:20, 321:401), ] %>%
  mutate(group = case_when(
    wave >= 400 & wave <= 499 ~ "blue",
    wave >= 500 & wave <= 599 ~ "green",
    wave >= 600 & wave <= 699 ~ "red"
  )) %>%
  group_by(group) %>%
  summarise(cum_sum = sum(ppfd)) %>%
  mutate(perc = paste0(round((cum_sum / sum(cum_sum)) * 100), "%")) %>%
  select(-c(2))%>%
  bind_cols(data.frame(x = c(450, 550, 650)))%>%
  print()






spec_plot_b <- ggplot(spec_data) + 
  geom_polygon(aes(x= wave, y=ppfd), size = 10, fill = NA)+
  annotate("rect", xmin = 600, xmax = 700,ymin = 8.5, ymax = 9, fill="#d76f7f")+
  annotate("rect", xmin = 500, xmax = 600,ymin = 8.5, ymax = 9, fill="#8ec689")+
  annotate("rect", xmin = 400, xmax = 500,ymin = 8.5, ymax = 9, fill="#73a0c7")+
  geom_text(data = perc_data,
            mapping = aes(x = x,
                          y = 8.75,
                          label = perc),
            check_overlap = T,
            size = 3.2,
            hjust = "centre",
            family = "Source Sans Pro")+
  stat_color(aes(x= wave, y=ppfd, fill = wave), geom = "line") + 
  scale_color_identity()+
  scale_y_continuous(limits = c(0,9))+
  theme_bw()+
  theme(text=element_text(family= "Source Sans Pro"))+
  labs(x= "Wavelength (nm)",
       y= expression(PPFD~(mu~mols~m^-2~s^-1)))

spec_plot_b



showtext_auto()
ggsave("spec_plot_PS1000.pdf", spec_plot_b, 
       width = 12, height = 9, units = "cm")





###### Collage
fig_ncer_spec <-  ggarrange(spec_plot_a, spec_plot_b,
                            ncol = 2, 
                            align = "hv",
                            labels = "AUTO")

fig_ncer_spec



ggsave("combined_spec.pdf", fig_ncer_spec, 
       width = 20, height = 9, units = "cm")
