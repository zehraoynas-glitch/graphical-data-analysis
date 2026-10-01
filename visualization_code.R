#zehra oynaş 2220329027

###SİNAPLOT
install.packages("readr")
install.packages("dplyr") 
install.packages("tidyr")
install.packages("ggforce")
install.packages("plotly")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
             
data <- read_csv("C:/Users/zehra/Desktop/METABRIC_RNA_Mutation.csv")

# Genel istatistikler
df_clean <- data %>%
  select(age_at_diagnosis, cellularity) %>%
  drop_na()

df_clean %>%
  group_by(cellularity) %>%
  summarise(
    mean_age = mean(age_at_diagnosis),
    median_age = median(age_at_diagnosis),
    sd_age = sd(age_at_diagnosis)
  )
summary(df_clean$age_at_diagnosis)
sd(df_clean$age_at_diagnosis)  

#High ve Low
df_ttest <- df_clean %>% filter(cellularity %in% c("High", "Low"))
t.test(age_at_diagnosis ~ cellularity, data = df_ttest)

# High ve Moderate
df_ttest <- df_clean %>% filter(cellularity %in% c("High", "Moderate"))
t.test(age_at_diagnosis ~ cellularity, data = df_ttest)

#Low ve Moderate 
df_ttest <- df_clean %>% filter(cellularity %in% c("Low", "Moderate"))
t.test(age_at_diagnosis ~ cellularity, data = df_ttest)

#grafik çizimi
df <- data[, c("age_at_diagnosis", "cellularity")]

df$cellularity <- as.factor(df$cellularity)

df <- df[df$cellularity %in% c("High", "Moderate", "Low"), ]

p <- ggplot(df, aes(x = cellularity, y = age_at_diagnosis, color = cellularity)) +
  geom_sina(alpha = 0.7, size = 2) +
  labs(title = "Teşhis Yaşının Hücresel Yoğunluğa Göre Dağılımı",
       x = "Hücresel Yoğunluk",
       y = "Teşhis Yaşı") +
  theme_minimal()

print(p)


library(plotly)
# ggplot grafiğinin plotly hali
ggplotly(p)

#png için
ggsave("sinaplot.png", plot = p, width = 8, height = 6, dpi = 300)




###SANKEY DİAGRAM
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("networkD3")

library(readr)
library(dplyr)
library(tidyr)
library(networkD3)

# Veriyi ISO-8859-9 kodlamasıyla okutuyorum
df <- read_csv("C:/Users/zehra/Desktop/urban_wildlife_adaptation_english.csv", 
               locale = locale(encoding = "ISO-8859-9"))


# Hatalı olan türkçe karakterleri düzeltiyorum
df <- df %>%
  drop_na(Species, Location_Type) %>%
  mutate(
    Species = case_when(
      Species == "GÃ¼vercin" ~ "Güvercin",
      TRUE ~ Species
    ),
    Location_Type = case_when(
      Location_Type == "EndÃ¼striyel" ~ "Endüstriyel",
      TRUE ~ Location_Type
    )
  )

#genel analizler
# Çapraz tablo
table_species_location <- table(df$Species, df$Location_Type)
print(table_species_location)

# Ki-kare testi
chisq.test(table_species_location)

# Frekans tablosu (her hayvan türü, her lokasyon için kaç kez gözlemlenmiş, sıklıkları)
df_summary <- df %>%
  count(Species, Location_Type, name = "Freq")

# Düğümler
nodes <- data.frame(name = unique(c(df_summary$Species, df_summary$Location_Type)))

# ID atamaları
df_summary$source <- match(df_summary$Species, nodes$name) - 1
df_summary$target <- match(df_summary$Location_Type, nodes$name) - 1

sankeyNetwork(Links = df_summary,
              Nodes = nodes,
              Source = "source",
              Target = "target",
              Value = "Freq",
              NodeID = "name",
              fontSize = 14,
              nodeWidth = 40,
              width = 800,
              height = 600,
              colourScale = JS("d3.scaleOrdinal().range(['#c77cff', '#ffd92f', '#fc8d62', '#f781bf', '#a6cee3', '#66c2a5', '#b2df8a', '#cab2d6'])"))

#grafik interaktif, bu yüzden png formatında değil de html formatında oluşturuyorum
install.packages("htmlwidgets")
library(htmlwidgets)

saveWidget(
  sankeyNetwork(Links = df_summary,
                Nodes = nodes,
                Source = "source",
                Target = "target",
                Value = "Freq",
                NodeID = "name",
                fontSize = 14,
                nodeWidth = 40,
                width = 800,
                height = 600,
                colourScale = JS("d3.scaleOrdinal().range(['#c77cff', '#ffd92f', '#fc8d62', '#f781bf', '#a6cee3', '#66c2a5', '#b2df8a', '#cab2d6'])")
  ),
  "sankey.html"
)




###ALLUVİAL DİAGRAM
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggalluvial")
install.packages("alluvial")

library(readr)
library(dplyr) 
library(tidyr)  
library(ggplot2)  
library(ggalluvial) 
library(alluvial)

df <- read_csv("C:/Users/zehra/Desktop/Global Crude Petroleum Trade 1995-2021.csv")  


#genel analizler
#sadece 2019–2021 yılları için
df_filtered <- df %>%
  filter(Year %in% c(2019, 2020, 2021))

#tanımlayıcı istatistikler
summary(df_filtered$`Trade Value`)
mean(df_filtered$`Trade Value`, na.rm = TRUE)
median(df_filtered$`Trade Value`, na.rm = TRUE)
sd(df_filtered$`Trade Value`, na.rm = TRUE)

#Gruplara göre ortalama, medyan, SD
#year için
df_filtered %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(`Trade Value`, na.rm = TRUE),
    Median = median(`Trade Value`, na.rm = TRUE),
    SD = sd(`Trade Value`, na.rm = TRUE)
  )

#action için
df_filtered %>%
  group_by(Action) %>%
  summarise(
    Mean = mean(`Trade Value`, na.rm = TRUE),
    Median = median(`Trade Value`, na.rm = TRUE),
    SD = sd(`Trade Value`, na.rm = TRUE)
  )

#continent için
df_filtered %>%
  group_by(Continent) %>%
  summarise(
    Mean = mean(`Trade Value`, na.rm = TRUE),
    Median = median(`Trade Value`, na.rm = TRUE),
    SD = sd(`Trade Value`, na.rm = TRUE)
  )

#Export vs Import
t.test(`Trade Value` ~ Action, data = df_filtered, var.equal = FALSE)

#ANOVA
anova_model <- aov(`Trade Value` ~ Continent, data = df_filtered)
summary(anova_model)

#Çapraz tablolar
table(df_filtered$Year, df_filtered$Action)
table(df_filtered$Action, df_filtered$Continent)


#grafik çizimi
df_base <- df %>%
  filter(Year %in% c(2019, 2020, 2021),
         !Continent %in% c("Oceania", "Antarctica")) %>%
  mutate(
    Continent = trimws(Continent),
    Continent = recode(Continent,
                       "South America" = "S.America",
                       "North America" = "N.America")
  ) %>%
  group_by(Year, Action, Continent) %>%
  summarise(Freq = sum(`Trade Value`, na.rm = TRUE), .groups = "drop")

renkler <- ifelse(df_base$Action == "Export", "#a05195", "#ffd92f")

alluvial(df_base[, 1:3],
         freq = df_base$Freq,
         col = renkler,
         border = NA,
         alpha = 1,
         cex = 0.7,
         hide = df_base$Freq < quantile(df_base$Freq, 0.2))

#png
png("alluvial.png", width = 1000, height = 800)
alluvial(df_base[, 1:3],
         freq = df_base$Freq,
         col = renkler,
         border = NA,
         alpha = 1,
         cex = 0.7,
         hide = df_base$Freq < quantile(df_base$Freq, 0.2))
dev.off()



###CHOROPLETH MAP - TMAP
install.packages("sf")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tmap")
install.packages("stringi")

library(sf)        # mekansal veri
library(readr)     # veri yükleme
library(dplyr)     # veri temizleme
library(tidyr)     # varsa veri düzenleme
library(tmap)      # harita çizimi
library(stringi)

egitim <- read_csv("C:/Users/zehra/Desktop/ortegitim2023.csv",
                   locale = locale(encoding = "ISO-8859-9"))  


#genel analizler
mean(egitim$toplam)   
median(egitim$toplam)       
sd(egitim$toplam)           
summary(egitim$toplam)      
shapiro.test(egitim$toplam)

#grafik çizimi
egitim <- egitim %>%
  mutate(iladı = case_when(
    iladı == "ısparta" ~ "isparta",
    iladı == "ığdır" ~ "iğdır",
    TRUE ~ iladı
  ))



turkiye <- st_read("C:/Users/zehra/Desktop/tr-cities-utf8.json")

names(turkiye)

turkiye <- turkiye %>%
  mutate(iladı = tolower(name))


harita_verisi <- left_join(turkiye, egitim, by = "iladı")


tm_shape(harita_verisi) +
  tm_polygons("toplam",
              palette = "Purples",
              legend.show = FALSE) +  
  tm_layout(
    main.title = "İllere Göre Ortalama Eğitim Süresi - 2023",
    main.title.position = "center",
    main.title.size = 1.5
  )


tm_shape(harita_verisi) +
  tm_polygons("toplam",
              palette = "Purples",
              title = "2023 Ortalama Eğitim Süresi (Yıl)") +
  tm_layout(
    legend.only = TRUE,
    legend.position = c("center", "center")
  )

#png
my_map <- tm_shape(harita_verisi) +
  tm_polygons("toplam",
              palette = "Purples",
              legend.show = FALSE) +
  tm_layout(
    main.title = "İllere Göre Ortalama Eğitim Süresi - 2023",
    main.title.position = "center",
    main.title.size = 1.5
  )

tmap_save(tm = my_map, filename = "choropleth.png", width = 8, height = 6, dpi = 300)

