# -----
library(tidyverse)
library(cowplot)
library(patchwork)
# -----
DATA_DIR="/home/magnolia/DataScience/Stellar_Class/data"
TRAIN_CSV="/train.csv"
TEST_CSV="/test.csv"

train=read_delim(file.path(DATA_DIR, TRAIN_CSV), delim = ',')
features<-colnames(train)
target<-train$class
# Histogram ----

alpha_p <- train %>% 
  ggplot(aes(x=alpha)) +
  geom_histogram(bins=round(sqrt(nrow(train)))/20, 
                 color='#482f36', fill='#c1a878') +
  theme_bw() +
  labs(title='Histogram: Alpha')

delta_p <- train %>% 
  ggplot(aes(x=delta)) +
  geom_histogram(bins=round(sqrt(nrow(train)))/20, 
                 color='#482f36', fill='#c1a878') +
  theme_bw() +
  labs(title='Histogram: Delta')

u_p <- train %>% 
  ggplot(aes(x=u)) +
  geom_histogram(bins=round(sqrt(nrow(train)))/20, 
                 color='#482f36', fill='#c1a878') +
  theme_bw() +
  labs(title='Histogram: U')

g_p <- train %>% 
  ggplot(aes(x=g)) +
  geom_histogram(bins=round(sqrt(nrow(train)))/20, 
                 color='#482f36', fill='#c1a878') +
  theme_bw() +
  labs(title='Histogram: G')

r_p <- train %>% 
  ggplot(aes(x=r)) +
  geom_histogram(bins=round(sqrt(nrow(train)))/20, 
                 color='#482f36', fill='#c1a878') +
  theme_bw() +
  labs(title='Histogram: R')

i_p <- train %>% 
  ggplot(aes(x=i)) +
  geom_histogram(bins=round(sqrt(nrow(train)))/20, 
                 color='#482f36', fill='#c1a878') +
  theme_bw() +
  labs(title='Histogram: I')

z_p <- train %>% 
  ggplot(aes(x=z)) +
  geom_histogram(bins=round(sqrt(nrow(train)))/20, 
                 color='#482f36', fill='#c1a878') +
  theme_bw() +
  labs(title='Histogram: Z')

redshift_p <- train %>% 
  ggplot(aes(x=redshift)) +
  geom_histogram(bins=round(sqrt(nrow(train)))/20, 
                 color='#482f36', fill='#c1a878') +
  theme_bw() +
  labs(title='Histogram: Red Shift')

alpha_p + delta_p + u_p + g_p + r_p + i_p + z_p + redshift_p

# Class, galaxy, Spectral -----
target_class_p <- train %>% 
  group_by(class) %>% 
  count() %>% 
  ggplot(aes(x=class, y=n)) +
  geom_bar(stat='identity', 
           fill=c('#b55e86', '#4d749a', '#3d2d4d'),
           color='black') +
  scale_y_continuous(labels=scales::comma) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Target: 'class' count",
       subtitle = "Galaxy: 377,480, QSO: 117,143, STAR: 82,724",
       x=NULL, y=NULL)


galaxy_map=c("Blue_Cloud"='#1d62e4', "Red_Sequence"='#e55d4d')

class_galaxy_pop_p <- train %>% 
  group_by(class, galaxy_population) %>% 
  count() %>% 
  ggplot(aes(x=class, y=n, fill=galaxy_population)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_manual(name='Galaxy Population',
                    labels=c('Blue Cloud', 'Red Sequence'),
                    values=galaxy_map) +
  scale_y_continuous(labels=scales::comma) +
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(0.9,0.9))


class_galaxy_pop_spec_p <- train %>% 
  group_by(class, galaxy_population, spectral_type) %>% 
  count() %>% 
  ggplot(aes(x = galaxy_population, y = n, fill = spectral_type)) +
  geom_bar(stat='identity', color='black') +
  facet_wrap(~ class) +
  scale_fill_manual(values = c(
    "A/F" = "#2be6f7",
    "G/K" = "#c297f2",
    "M"   = "#1b3766",
    "O/B" = "#e2e6f8"
  )) +
  labs(x = NULL, y = NULL, fill = "Spectral type") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

spec_type_n_galaxy_pop_fill_class_facet_p <- train %>% 
  group_by(class, galaxy_population, spectral_type) %>% 
  count() %>% 
  ggplot(aes(x = spectral_type, y = n, fill = galaxy_population)) +
  geom_bar(stat='identity', color='black') +
  facet_wrap(~ class) +
  scale_fill_manual(name='Galaxy Population',
                    labels=c('Blue Cloud', 'Red Sequence'),
                    values=galaxy_map) +
  labs(x = NULL, y = NULL, fill = "Spectral type") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())


# Box plot ----  
c_alpha_p <- train %>% 
  ggplot(aes(x=class, y=alpha)) +
  geom_boxplot()

c_delta_p <- train %>% 
  ggplot(aes(x=class, y=delta)) +
  geom_boxplot()

c_u_p <- train %>% 
  ggplot(aes(x=class, y=u)) +
  geom_boxplot()

c_g_p <- train %>% 
  ggplot(aes(x=class, y=g)) +
  geom_boxplot()

c_r_p <- train %>% 
  ggplot(aes(x=class, y=r)) +
  geom_boxplot()

c_i_p <- train %>% 
  ggplot(aes(x=class, y=i)) +
  geom_boxplot()

c_z_p <- train %>% 
  ggplot(aes(x=class, y=z)) +
  geom_boxplot()

c_redshift_p <- train %>% 
  ggplot(aes(x=class, y=redshift)) +
  geom_boxplot()

c_alpha_p + c_delta_p + c_u_p + c_g_p + c_r_p + c_i_p + c_z_p + c_redshift_p

# Violin plot ----

c_alpha_v_p <- train %>% 
  ggplot(aes(x = class, y = alpha)) +
  geom_violin()

c_delta_v_p <- train %>% 
  ggplot(aes(x=class, y=delta)) +
  geom_violin()

c_u_v_p <- train %>% 
  ggplot(aes(x=class, y=u)) +
  geom_violin()

c_g_v_p <- train %>% 
  ggplot(aes(x=class, y=g)) +
  geom_violin()

c_r_v_p <- train %>% 
  ggplot(aes(x=class, y=r)) +
  geom_violin()

c_i_v_p <- train %>% 
  ggplot(aes(x=class, y=i)) +
  geom_violin()

c_z_v_p <- train %>% 
  ggplot(aes(x=class, y=z)) +
  geom_violin()

c_redshift_v_p <- train %>% 
  ggplot(aes(x=class, y=redshift)) +
  geom_violin()


c_alpha_v_p + c_delta_v_p + c_u_v_p + c_g_v_p + c_r_v_p + c_i_v_p + c_z_v_p + c_redshift_v_p


train
