# 2. faza: Uvoz podatkov

#slovar imen držav
slovar <- c("Belgium" = "Belgija",
            "Germany (until 1990 former territory of the FRG)" = "Nemčija",
            "Bulgaria" = "Bolgarija",
            "Czechia" = "Češka",
            "Denmark" = "Danska",
            "Germany" = "Nemčija",
            "Estonia" = "Estonija",
            "Kosovo (under United Nations Security Council Resolution 124" = "Kosovo",
            "Ireland" = "Irska",
            "Greece" = "Grčija",
            "Spain" = "Španija",
            "France" = "Francija",
            "Croatia" = "Hrvaška",
            "Italy" = "Italija",
            "Cyprus" = "Ciper",
            "Latvia" = "Latvija",
            "Lithuania" = "Litva",
            "Luxembourg" = "Luksemburg",
            "Hungary" = "Madžarska",
            "Malta" = "Malta",
            "Netherlands" = "Nizozemska",
            "Switzerland" = "Švica",
            "Austria" = "Avstrija",
            "Poland" = "Poljska",
            "Portugal" = "Portugalska",
            "Romania" = "Romunija",
            "Slovenia" = "Slovenija",
            "Slovakia" = "Slovaška",
            "Finland" = "Finska",
            "Sweden" = "Švedska",
            "United Kingdom" = "Združeno kraljestvo (Velika Britanija)",
            "Iceland" = "Islandija",
            "Norway" = "Norveška",
            "North Macedonia" = "Severna Makedonija",
            "Serbia" = "Srbija",
            "Turkey" = "Turčija",
            "France (metropolitan)" = "Metropolitanska Francija",
            "Montenegro" = "Črna gora")


#tabela1 - brezposelni po stopnjah izobrazbe, spolu in kohezijskih regijah (v 1000 in v %)

stolpci1 <- c("leto", "kohezijska_regija", "spol", "izobrazba" ,"meritev", "podatek")
tabela1 <- read_csv("podatki/tabela1.csv",col_names = stolpci1, skip=3, locale=locale(encoding = "Windows-1250"))

tabela1$podatek <- as.numeric(as.character(tabela1$podatek))

tabela1 <- tabela1 %>% pivot_wider(names_from = meritev, values_from = podatek)%>%
  rename(Stevilo="Število (v 1000)")%>%
  rename(delez="Delež (v %)")%>%
  mutate(stevilo = Stevilo*1000)

tabela1 <- subset(tabela1, select = c("leto", "kohezijska_regija", "spol", "izobrazba" ,"stevilo", "delez")) 

#tabela2 - mere aktivnosti po statisticnih regijah (v %)

stolpci2 <- c("leto", "statisticna_regija", "meritev", "delez")
tabela2 <- read_csv("podatki/tabela2.csv",col_names = stolpci2, skip=3, locale=locale(encoding = "Windows-1250"))
tabela11 <- tabela2 %>% filter(meritev=="Stopnja brezposelnosti") %>%
select(c(-meritev))
#tabela3 - stopnja brezposelnosti v evropskih drzavah

tabela3 <- read_csv("podatki/tabela3.csv", locale = locale(encoding = "Windows-1250"), skip = 1,
            col_names = "stolpci3") %>% 
          separate(stolpci3, c("leto", "spol", "izobrazba", "starost", "meritev", "delez"), '","' ) %>%
  separate("leto", c("leto","drzava"), ',"') 

tabela3$delez <- gsub("\"", "",tabela3$delez)

tabela3$spol <- sub("Males", "moški", tabela3$spol)
tabela3$spol <- sub("Females", "ženske", tabela3$spol)

tabela3$izobrazba[tabela3$izobrazba == "Less than primary, primary and lower secondary education (levels 0-2)"] <- "Brez izobrazbe, nepopolna osnovnošolska ali osnovnošolska"
tabela3$izobrazba[tabela3$izobrazba == "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)"] <- "Nižja ali srednja poklicna"
tabela3$izobrazba[tabela3$izobrazba == "Tertiary education (levels 5-8)"] <- "Srednja strokovna, višješolska, visokošolska ali univerzitetna"

tabela3 <- subset(tabela3, select = c("leto", "drzava", "spol", "izobrazba", "delez")) 


tabela3$delez <- gsub(",", ".",tabela3$delez)
tabela3$delez <- as.numeric(as.character(tabela3$delez))

tabela3_imena <- tabela3 %>% mutate(drzava=slovar[drzava])
tabela3_imena$delez <- as.numeric(as.character(tabela3_imena$delez))
#tabele za Shiny
s1_shiny <- tabela3_imena %>% filter(spol== "moški") %>% select(-c(spol))
s2_shiny <- tabela3_imena %>% filter(spol== "ženske") %>% select(-c(spol))
