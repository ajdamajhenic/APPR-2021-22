# 3. faza: Vizualizacija podatkov

brezposelnost <- tabela1 %>%
                 group_by(leto, spol, izobrazba) %>%
                 summarize(brezposelnost = sum(delez, na.rm = TRUE))

graf1 <- ggplot(data=brezposelnost, aes(x=leto, y=brezposelnost, fill=izobrazba)) +
         geom_col(position = "dodge") +
         facet_wrap(.~spol,ncol=1) +
         scale_x_continuous(breaks = 1*2008:2021) +
         theme(axis.text.x = element_text(angle = 90)) +
         ylab('Delež brezposelnih') +
         xlab('Leto') +
         ggtitle('Spreminjanje deleža brezposelnih glede na stopnjo izobrazbe skozi leta')

graf1


povprecna_brezposelnost <- tabela3_imena %>%
                            group_by(leto, drzava,spol,izobrazba) %>%
                            summarize(povprecje=sum(delez, na.rm = TRUE)/2)%>% 
                            filter(drzava %in% c("Avstrija", "Slovenija", "Italija", "Madžarska", "Hrvaška"))

s1<- povprecna_brezposelnost %>% filter(spol=="moški")
s2<- povprecna_brezposelnost %>% filter(spol=="ženske")

graf2 <- ggplot(data=s1, aes(x=leto, y=povprecje, col=izobrazba)) +
          geom_point() +
          geom_line() +
          facet_wrap(.~drzava,ncol=3) +
          labs(col="drzava") +
          theme(axis.text.x = element_text(angle = 90)) +
          ylab('Povprečje brezposelnih') +
          xlab('Leto') +
          ggtitle('Primerjava deleža brezposelnih moških glede na stopnjo izobrazbe med državami') +
          labs(col="Izobrazba")

graf2

graf3 <- ggplot(data=s2, aes(x=leto, y=povprecje, col=izobrazba)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~drzava,ncol=3) +
  labs(col="drzava") +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab('Povprečje brezposelnih') +
  xlab('Leto') +
  ggtitle('Primerjava deleža brezposelnih žensk glede na stopnjo izobrazbe med državami') +
  labs(col="Izobrazba")

graf3

graf4 <- ggplot(data=s1, aes(x=leto, y=povprecje, col=drzava)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~izobrazba,ncol=3) +
  labs(col="drzava") +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab('Povprečje brezposelnih') +
  xlab('Leto') +
  ggtitle('Primerjava deleža brezposelnih moških glede na stopnjo izobrazbe med državami') +
  labs(col="Država")

graf4      

graf5 <- ggplot(data=s2, aes(x=leto, y=povprecje, col=drzava)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~izobrazba,ncol=3) +
  labs(col="drzava") +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab('Povprečje brezposelnih') +
  xlab('Leto') +
  ggtitle('Primerjava deleža brezposelnih žensk glede na stopnjo izobrazbe med državami') +
  labs(col="Država")

graf5 

#ZEMLJEVIDI

skupno_po_regijah <- tabela2 %>%
                      group_by(statisticna_regija,meritev) %>%
                      summarise(sestevek=sum(delez)/14)

m1 <- skupno_po_regijah %>% filter(meritev=="Stopnja brezposelnosti")
m2 <- skupno_po_regijah %>% filter(meritev=="Stopnja delovne aktivnosti")

Slovenija <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/SVN_adm_shp.zip",
                             "SVN_adm1", encoding = "UTF-8")  

#Slovenija$NAME_1[Slovenija$NAME_1 == "GoriĹˇka"] <- "Goriška"
#Slovenija$NAME_1[Slovenija$NAME_1 == "KoroĹˇka"] <- "Koroška"
#Slovenija$NAME_1[Slovenija$NAME_1 == "Notranjsko-kraĹˇka"] <- "Notranjsko-kraška"
#Slovenija$NAME_1[Slovenija$NAME_1 == "Obalno-kraĹˇka"] <- "Obalno-kraška"

Slovenija$NAME_1 <- Slovenija$NAME_1 %>%
  str_replace("Spodnjeposavska", "Posavska") %>%
  str_replace("Notranjsko-kraška", "Primorsko-notranjska")

zemljevid_slo <- tm_shape(merge(Slovenija, m1, by.x="NAME_1", by.y="statisticna_regija")) + 
  tm_polygons("sestevek",palette="Purples")+ 
  tm_style("grey") +
  tm_layout(main.title="Povprečna stopnja brezposelnosti",
            legend.height = 0.4,
            legend.width = 1,
            legend.title.size = 0.1,
            legend.text.size = 0.5,
            legend.position = c(0.75,0.1),
            legend.bg.alpha = 1) + 
  tm_text(text='NAME_1', size=0.6) 

zemljevid_slo  


zemljevid_slo2 <- tm_shape(merge(Slovenija, m2, by.x="NAME_1", by.y="statisticna_regija")) + 
  tm_polygons("sestevek",palette="Purples")+ 
  tm_style("grey") +
  tm_layout(main.title="Povprečna stopnja delovne aktivnosti",
            legend.height = 0.4,
            legend.width = 1,
            legend.title.size = 0.1,
            legend.text.size = 0.5,
            legend.position = c(0.75,0.1),
            legend.bg.alpha = 1) + 
  tm_text(text='NAME_1', size=0.6) 

zemljevid_slo2  

#uvozim zemljevid Evrope

zemljevid <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")
zemljevid <- zemljevid[zemljevid$CONTINENT == "Europe",]

skupno_po_drzavah <- tabela3 %>%
  group_by(drzava,izobrazba) %>%
  summarise(sestevek1=(sum(delez)/24))

iz1 <- skupno_po_drzavah %>%
  filter(izobrazba=="Brez izobrazbe, nepopolna osnovnošolska ali osnovnošolska")
iz2 <- skupno_po_drzavah %>%
  filter(izobrazba=="Nižja ali srednja poklicna")
iz3 <- skupno_po_drzavah %>%
  filter(izobrazba=="Srednja strokovna, višješolska, visokošolska ali univerzitetna")

zemljevid1 <- tm_shape(merge(zemljevid,
                             iz1,duplicateGeoms = TRUE,
                             by.x="SOVEREIGNT", by.y="drzava"), xlim=c(-25,32), ylim=c(32,72)) +
  tm_polygons(col= "sestevek1", title = "Delež brezposelnih") + 
  tm_layout(bg.color = "skyblue") + 
  tm_layout(main.title = "Delež brezposelnih  med ljudmi \n brez izobrazbe, z nepopolno osnovnošolsko \n ali osnovnošolsko izobrazbo", main.title.size = 0.5, legend.title.size = 2)

zemljevid1

zemljevid2 <- tm_shape(merge(zemljevid,
                             iz2,duplicateGeoms = TRUE,
                             by.x="SOVEREIGNT", by.y="drzava"), xlim=c(-25,32), ylim=c(32,72)) +
  tm_polygons(col= "sestevek1", title = "Delež brezposelnih") + 
  tm_layout(bg.color = "skyblue") + 
  tm_layout(main.title = "Delež brezposelnih med ljudmi z \n nižjo ali srednjo poklicno izobrazbo", main.title.size = 0.5, legend.title.size = 2)

zemljevid2

zemljevid3 <- tm_shape(merge(zemljevid,
                             iz3,duplicateGeoms = TRUE,
                             by.x="SOVEREIGNT", by.y="drzava"), xlim=c(-25,32), ylim=c(32,72)) +
  tm_polygons(col= "sestevek1", legend.position = c(0.1,0.75)) + 
  tm_layout(bg.color = "skyblue") + 
  tm_layout(main.title = "Delež brezposelnih med ljudmi s \n srednjo strokovno, višješolsko, visokošolsko \n ali univerzitetno izobrazbo", main.title.size = 0.5, legend.title.size = 2)

zemljevid3
