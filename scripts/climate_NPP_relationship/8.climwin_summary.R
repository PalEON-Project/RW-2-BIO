# script to read in all the climwin results and do an initial comparison
# 
# read in the tree, site, and taxon x site climwin best fit models

site.climwin <- read.csv("out/climwin/climwin_site_summary.csv")
taxon.climwin <- read.csv("out/climwin/climwin_taxon_summary.csv")
tree.climwin <- read.csv("out/climwin/AGBI_tree_level_climwin_summary.csv")

# 
month.start.df <- data.frame(WindowOpen = 0:18, 
           WindowOpen.name = c("October","September", "August", "July", "June", "May", "April", "March", "February", "January", 
                                "prev_December", "prev_November", "prev_October", "prev_September", "prev_August", "prev_July", "prev_June", "prev_May", "prev_April"))
month.end.df <- data.frame(WindowClose = 0:18, 
                             WindowClose.name = c("October","September", "August", "July", "June", "May", "April", "March", "February", "January", 
                                                  "prev_December", "prev_November", "prev_October", "prev_September", "prev_August", "prev_July", "prev_June", "prev_May", "prev_April"))

# define the scales
site.climwin$scale <- "Site"
taxon.climwin$scale <- "Taxon"
tree.climwin$scale <- "Tree"

# for site and tree assign a taxon columns
site.climwin <- site.climwin %>% mutate(taxon = "all") %>% select(Site, taxon, Climate, WindowOpen, WindowClose, Month.start, Month.end, scale )
tree.climwin <- tree.climwin %>% mutate(taxon = "all") %>% select(Site, taxon, Climate, WindowOpen, WindowClose, Month.start, Month.end, scale )

# combine all of the dataframes together
all.climwin <- rbind(site.climwin, taxon.climwin, tree.climwin) %>% left_join(.,month.start.df) %>% left_join(., month.end.df)

ggplot(all.climwin, aes(x = interaction(Site, taxon), ymin = Month.start, ymax = Month.end, color = taxon))+geom_errorbar(width = 0.1, positon = position_dodge())+facet_wrap(~Climate)

# create an interaction term for plotting:
all.climwin$Scale_taxon <- interaction(all.climwin$scale, all.climwin$taxon)
all.climwin$Scale_taxon <- factor(all.climwin$Scale_taxon, levels = unique(all.climwin$Scale_taxon))

# make sure plotting happens in the order of months
all.climwin$WindowOpen.name <- factor(all.climwin$WindowOpen.name, levels = rev(c("October","September", "August", "July", "June", "May", "April", "March", "February", "January", 
                                                                                "prev_December", "prev_November", "prev_October", "prev_September", "prev_August", "prev_July", "prev_June", "prev_May", "prev_April")))
all.climwin$WindowClose.name <- factor(all.climwin$WindowClose.name, levels = rev(c("October","September", "August", "July", "June", "May", "April", "March", "February", "January", 
                                                                              "prev_December", "prev_November", "prev_October", "prev_September", "prev_August", "prev_July", "prev_June", "prev_May", "prev_April")))

site
for(i in 1:length(site)){
ggplot()+
  geom_segment(data = all.climwin %>% filter(Site %in% site[i]), aes(x = Scale_taxon, y = WindowClose.name, yend = WindowOpen.name, color = taxon), size = 5)+
  geom_point(data = all.climwin %>% filter(Site %in% site[i] & (WindowOpen - WindowClose) == 0), aes(x = Scale_taxon, y = WindowClose.name, color = taxon), size = 3)+
  facet_wrap(~Climate)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+ylab("Months before October")+coord_flip()
ggsave(height = 5, width = 10, units = "in", paste0("out/climwin/summary_scale_figures/summary_climwin_scale_figures_", site[i], ".png"))
}
       