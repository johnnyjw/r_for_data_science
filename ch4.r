#exs p 79

#1 interesting thing...extract variable ctrl+alt+V
# use environment from ch 3
mutated <- not_cancelled %>% 
  group_by(dest, carrier) %>% 
  summarize(mean_del = mean(arr_delay)) %>%
  mutate(n_carr = n()) 
mutated%>% 
  filter(n_carr>1) %>% 
  arrange(dest, mean_del)

