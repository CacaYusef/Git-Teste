if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,readxl, ggplot2, pracma, tidyr, esquisse, psych)
 
df <- read_excel(
  here::here("dados/1.xlsx"),
  sheet = "decision (subjects table)"
)

#2. Média grupo#####
group_average <- df %>%
  group_by(group, round) %>%
  summarise(average = mean(decision[variable == "contribution"], 
                           na.rm = TRUE), .groups = "drop") %>%
  mutate(group = as.factor(group))

group_average_plot <- ggplot(group_average, aes(x = round, y = average, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(values = c(
    "1" = "#794834",  "5" = "#bba06b",  "9" = "#373658", 
    "2" = "#BF926E", "6" = "#cd7729", "10" = "#1b0530",
    "3" = "#6C5F34","7" = "#d91900", "11" = "#34052b",
    "4" = "purple",    "8" = "#470819",  "12" = "purple"
  )) + scale_x_continuous(breaks = seq(min(group_average$round), max(group_average$round), by = 1)) +
 labs(
    title = "Contribuição média dos grupos por round", 
    x = "Rounds",
    y = "Contribuição",
    color = "Grupo"
  )  +
  theme_minimal()
print(group_average_plot)
#
#
#
# 3. Média jogadores ####
players_average <- df %>% 
  filter(variable == "contribution") %>% 
  group_by(round) %>%
  summarise(players_average = mean(decision, na.rm = TRUE))

players_average_plot <- ggplot(players_average, 
                               aes(x = round, y = players_average)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(min(group_average$round), max(group_average$round), by = 1)) +
labs(
  title = "Contribuição média dos jogadores por round", 
  x = "Rounds",
  y = "Contribuição",
  color = "Grupo"
)  +
  theme_minimal()
print(players_average_plot)
view(players_average)

#5. Importando outra data set####

df2 <- read_excel(
  here::here("dados/1 projet.xlsx"),
  sheet = "Public goods contributions")

df2_arrumada <- df2 %>% 
  pivot_longer(, cols = 2:18, names_to = "cities", values_to = "amount")

cities_plot <- ggplot(df2_arrumada, aes(x = Period, y = amount, color = cities, group = cities)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = seq(min(df2_arrumada$Period), max(df2_arrumada$Period), by = 1)) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 1)) +
  labs(
    title = "Contribuição média por cidade",
    x = "Período",
    y = "Quantidade",
    color = "Cidade"
  ) +
  theme_minimal()
print(cities_plot)

#6. Com e sem punição cidades ####

df2.1_com_punição <- read_excel(
  here::here("dados/1 projet.xlsx"),
  sheet = "Planilha1") %>% 
  pivot_longer(, cols = 2:17, names_to = "cities", values_to = "amount") %>%
  mutate(cenario = "Com punição")

df2.1_sem_punição <- read_excel(
  here::here("dados/1 projet.xlsx"),
  sheet = "Public goods contributions") %>% 
  pivot_longer(, cols = 2:18, names_to = "cities", values_to = "amount")  %>%
  mutate(cenario = "Sem punição")

df_comp <- bind_rows(df2.1_sem_punição, df2.1_com_punição)
 gráfico_6 <- df_comp %>%  group_by(cenario, Period) %>% 
    summarise(média = mean(amount)) 

cities_com_sem_punição_plot <- ggplot(gráfico_6, aes(x = Period, y = média, color = cenario, group = cenario)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_x_continuous(breaks = seq(min(gráfico_6$Period), max(gráfico_6$Period), by = 1)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 1)) +
  labs(
    title = "Contribuição média por cenário",
    x = "Período",
    y = "Quantidade",
    color = "Cenário"
  ) 
print(cities_com_sem_punição_plot)


#7. Comparação 10* round (com e sem puniçao)####

comparação_10_round <- gráfico_6 %>%
  filter(Period == 10 | Period == 1)

comparação_plot <- ggplot(comparação_10_round, aes(fill=cenario, y=média, x=Period)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Comparação dos cenários com e sem punição") +
  facet_wrap(~cenario) +
  xlab("Round") + 
  scale_x_continuous(breaks = seq(min(comparação_10_round$Period), max(comparação_10_round$Period), by = 9))+
  scale_y_continuous(breaks = round(seq(0, max(comparação_10_round$média), by = 3.869879), 1))
print(comparação_plot)

#8. Dados finais####

tabela <- df_comp %>%
  group_by(cenario, Period) %>%
  filter(Period == "10"| Period == 1 ) %>% 
  summarise(
    `Desvio Padrão` = sd(amount, na.rm = TRUE),
    Amplitude = max(amount, na.rm = TRUE) - min(amount, na.rm = TRUE),
    .groups = "drop"
  )
print(tabela) ## Tabela de desvio padrão e amplitude

t.test(
  amount ~ cenario,
  data = df_comp %>% filter(Period == 1)
) # T test para cenário sem/com punição para período 1

t.test(
  amount ~ cenario,
  data = df_comp %>% filter(Period == 10)
) # T test para cenário sem/com punição para período 10

















