df = data.frame(dysglycemia = runif(1000,min = 0,max=100))

df %>% 
  ggplot(data=.,aes(x=dysglycemia)) +
  geom_histogram(bins=100) +
  theme_bw() +
  xlab("Unified Glycemia Indicator") +
  geom_vline(aes(color= "RPG",xintercept=100-6.5)) +
  geom_vline(aes(color="FPG",xintercept=100-9.3)) +
  geom_vline(aes(color="A1C",xintercept = 100-19.7)) +
  scale_color_manual(name = "Cutoff",
                     values = c(RPG = "darkgreen",
                                FPG = "blue",
                                A1C = "red")) +
  theme(legend.position = "bottom")


figA = df %>% 
  mutate(status = case_when(dysglycemia > (100-6.5) ~ "High",
                            TRUE ~ "Low")) %>% 
  ggplot(data=.,aes(x=dysglycemia,fill=status)) +
  geom_histogram(bins=100) +
  theme_bw() +
  xlab("Unified Glycemia Indicator") +
  scale_fill_manual(values=c(High = "red",
                             Low = "darkgreen"))

figB = df %>% 
  mutate(status = case_when(dysglycemia > (100-9.3) ~ "High",
                            TRUE ~ "Low")) %>% 
  ggplot(data=.,aes(x=dysglycemia,fill=status)) +
  geom_histogram(bins=100) +
  theme_bw() +
  xlab("Unified Glycemia Indicator") +
  scale_fill_manual(values=c(High = "red",
                             Low = "darkgreen"))

figC = df %>% 
  mutate(status = case_when(dysglycemia > (100-19.7) ~ "High",
                            TRUE ~ "Low")) %>% 
  ggplot(data=.,aes(x=dysglycemia,fill=status)) +
  geom_histogram(bins=100) +
  theme_bw() +
  xlab("Unified Glycemia Indicator") +
  scale_fill_manual(values=c(High = "red",
                             Low = "darkgreen"))


require(ggpubr)

ggarrange(figA,figB,figC,nrow=3,ncol=1,
          labels = c("RPG","FPG","A1C")) %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_dysglycemia gold standard.png"),
         width=8,height=10)
