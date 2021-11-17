test <- read_excel("Test.xlsx")
test_long <- pivot_longer(test, cols = -YOS, names_to = "Age", values_to = "TermRate") %>% 
  select(Age, YOS, TermRate) %>% 
  mutate(Age = as.numeric(Age)) %>% 
  arrange(Age, YOS)
