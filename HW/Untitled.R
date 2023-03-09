head(df)

df |> 
  filter(Year == '2010') |>
  select(c(1, 2, 6)) |>
  head()


head(q1)

q1 |> filter(Cause_Of_Death == 'Certain infectious and parasitic diseases') |>
  ggplot(aes(x=reorder(State,Crude_Mortality_Rate), y=Crude_Mortality_Rate)) +
  geom_col(fill='#f68060', alpha=.6, width=.4) +
  coord_flip() +
  labs(x='State', y='Crude Mortality Rate', title='main')

q1 |> filter(Cause_Of_Death == 'Certain infectious and parasitic diseases')
  

head(q2)

q2 |> filter(Cause_Of_Death == 'Certain Infectious And Parasitic Diseases' & State == 'AL') |>
  ggplot(aes(x=Year)) +
  geom_line(aes(y=Crude_Mortality_Rate, color='state')) +
  geom_line(data=q2.1.sub, aes(y=Nat_Avg, color='National Avg')) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 11)) + 
  labs(x='Year', y='Crude Mortality Rate', color="color") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p2 = g2.1.sub |> filter() 

q2.1.sub = q2.1 |> filter(Cause_Of_Death == 'Certain Infectious And Parasitic Diseases')


             
             
# calculate national average
q2.1 = df |> group_by(Cause_Of_Death, Year) |>
  summarise(Nat_Avg = sum(Deaths)/sum(Population)*100000)

q2.1 |> filter(Cause_Of_Death == 'Certain Infectious And Parasitic Diseases') |>
  ggplot(aes(x=Year, y=Nat_Avg, color="National Avg")) +
  geom_line()

