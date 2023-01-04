# CPUE for Atka for each year 
yrs <- seq(2000,2022,by=2)
yrs <- yrs[-which(yrs %in% c(2008, 2020))]
spcode <- 21921
df_allyrs <- data.frame(year = yrs, cpue_kgkm2_eastern = NA, var_cpue_kgkm2_eastern = NA)

for (i in 1:length(yrs)){
  x <- make_top_cpue(YEAR = yrs[i],SRVY = "AI",cpue_raw = cpue_raw,topn = 100)
  xx <- x %>% filter(species_code==spcode & INPFC_AREA=="Eastern Aleutians")
  cpue_yr <- as.numeric(xx$wgted_mean_cpue_kgkm2) 
  cpue_yr_variance <- as.numeric(xx$var_cpue_kgkm2 ) 
  df_allyrs$cpue_kgkm2_eastern[i] <- cpue_yr
  df_allyrs$var_cpue_kgkm2_eastern[i] <- cpue_yr_variance
}

p1 <-  df_allyrs %>%
  ggplot(aes(x=year,y=cpue_kgkm2_eastern)) +
  geom_point(color="grey35",size=2) +
  geom_line(color="grey35", size=.75) +
  theme_classic() +
  xlab("Year") +
  ylab(bquote(Survey~CPUE~(kg / km^2)))


png(filename = "CPUE_Atka_trend.png",width = 6,height = 6,res = 200,units = 'in')
p1 
dev.off()

write.csv(df_allyrs,file = "survey_cpue_atka.csv",row.names = FALSE)
