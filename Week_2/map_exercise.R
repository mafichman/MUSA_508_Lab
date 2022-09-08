### This is code to help replicate the TOD assignment plots from the textbook
### you have to run the code in `Lab2_Indicators_ACS.RMD` first in order
### to have the `tracts16`, `selection1` and other objects created.

myData  <- rbind(selectCentroids, clip) %>%
  rbind(., selection1)

ggplot(myData)+
  geom_sf(data = st_union(tracts16))+
  geom_sf(aes(fill = q5(TotalPop))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(myData, "TotalPop"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Philadelphia; 2009") +
  facet_wrap(~Selection_Type)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
  
  
  # 1.3.1 Maps
  
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts16))+
    geom_sf(aes(fill = TOD)) +
    labs(title = "Time/Space Groups") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
  
  
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts16))+
    geom_sf(aes(fill = q5(MedRent.inf))) +
    geom_sf(data = buffer, fill = "transparent", color = "red")+
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "MedRent.inf"),
                      name = "Rent\n(Quintile Breaks)") +
    labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
  
  #1.4 Submarkets
  
# As a map
    
ggplot(allTracts.threeMarkets)+
  geom_sf(data = st_union(tracts16))+
  geom_sf(aes(fill = Submarket))+
  scale_fill_manual(values = c("orange", "green", "blue", "black"))+
  labs(title = "Three Submarkets As Tracts") +
  mapTheme()

# As a facetted plot

st_drop_geometry(allTracts.threeMarkets) %>%
  group_by(year, Submarket) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T)) %>%
  gather(Variable, Value, -year, -Submarket) %>%
  ggplot(aes(year, Value, fill = Submarket)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("orange", "green", "blue", "black"))+
  labs(title = "Indicator differences across submarkets") +
  plotTheme() + theme(legend.position="bottom")

