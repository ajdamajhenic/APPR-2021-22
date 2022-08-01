# 4. faza: Napredna analiza podatkov

# ==============================================================================
# Uporabljene funkcije pri napredni analizi:
# ==============================================================================

# funkcije uporabljene pri razvrščanju v skupine:

hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# narišemo diagram višin združevanja
diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "black"
    )+
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "black"
    )+
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "red", size = 2.5
    )+
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x, y = y, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue(h = c(180, 270)) +
    xlab("") +
    ylab("") +
    theme_classic() 
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}

obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  razdalje = dist(podatki)
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(42) # zato, da so rezultati ponovljivi
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic() 
}

#-------------------
# ==============================================================================
# RAZVRSTITEV V SKUPINE
# ==============================================================================

# 1) k-means

# koliko skupin naj izberem pri razdelitvi z metodo voditeljev:
is.na(skupno_po_drzavah) %>% table()

dim(skupno_po_drzavah)

skupno_po_drzavah[is.na(skupno_po_drzavah)] = 0

#razvrščanje po skupinah glede na stopnjo izobrazbe = "Brez izobrazbe, nepopolna osnovnošolska ali osnovnošolska"

#---- DIAGRAM OBRISI ----

brez.clust <- skupno_po_drzavah %>% filter(izobrazba == "Brez izobrazbe, nepopolna osnovnošolska ali osnovnošolska")
r.hc = brez.clust %>% obrisi(hc = TRUE, od=2, do=10)

r.hc.plt <- diagram.obrisi(r.hc)
r.hc.plt

#------- DENDROGRAM -----

X <- brez.clust[,-2]
drzave <- brez.clust[, 1] %>% unlist()
razdalje <- brez.clust[, -1] %>% dist()
dendrogram  <- dist(X) %>% hclust(method = "ward.D")
plot(dendrogram,
     labels = brez.clust$drzava,
     ylab = "višina",
     main = NULL)

#oba predlagata razporeditev v 4 skupine

#--- IZRAČUN IN IZRIS KOLENA ----
r = hc.kolena(dendrogram)
diagram.kolena(r)


drzave.x.y <-
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

#glede na prejšne predloge izberem 4 skupine
k = 4

skupine <- brez.clust %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()
diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)

#------------------------------

#razvrščanje po skupinah glede na stopnjo izobrazbe = "Nižja ali srednja poklicna"
#---- DIAGRAM OBRISI ----

nizja.clust <- skupno_po_drzavah %>% filter(izobrazba == "Nižja ali srednja poklicna")
r.hc = nizja.clust %>% obrisi(hc = TRUE, od=2, do=10)

r.hc.plt <- diagram.obrisi(r.hc)
r.hc.plt

#------- DENDROGRAM -----

X <- nizja.clust[,-2]
drzave <- nizja.clust[, 1] %>% unlist()
razdalje <- nizja.clust[, -1] %>% dist()
dendrogram  <- dist(X) %>% hclust(method = "ward.D")
plot(dendrogram,
     labels = nizja.clust$drzava,
     ylab = "višina",
     main = NULL)

#oba predlagata razporeditev v 5 skupin

#--- IZRAČUN IN IZRIS KOLENA ----
r = hc.kolena(dendrogram)
diagram.kolena(r)


drzave.x.y <-
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

#glede na prejšne predloge izberem 5 skupin
k = 5

skupine <- nizja.clust %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()
diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)

#------------------------------

#razvrščanje po skupinah glede na stopnjo izobrazbe = "Srednja strokovna, višješolska, visokošolska ali univerzitetna"
#---- DIAGRAM OBRISI ----

visja.clust <- skupno_po_drzavah %>% filter(izobrazba == "Srednja strokovna, višješolska, visokošolska ali univerzitetna")
r.hc = visja.clust %>% obrisi(hc = TRUE, od=2, do=10)

r.hc.plt <- diagram.obrisi(r.hc)
r.hc.plt

#------- DENDROGRAM -----

X <- visja.clust[,-2]
drzave <- visja.clust[, 1] %>% unlist()
razdalje <- visja.clust[, -1] %>% dist()
dendrogram  <- dist(X) %>% hclust(method = "ward.D")
plot(dendrogram,
     labels = visja.clust$drzava,
     ylab = "višina",
     main = NULL)

#eden predlaga razporeditev v 9 skupin, drugi pa v 3 ali 5 skupin

#--- IZRAČUN IN IZRIS KOLENA ----
r = hc.kolena(dendrogram)
diagram.kolena(r)


drzave.x.y <-
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

#glede na prejšne predloge izberem 3 skupine
k = 3

skupine <- visja.clust %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()
diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)

#-----------------------------------------

zamakni <- function(x, n){c(rep(NA, n), x)[1:length(x)]}
naredi.df.4 <- function(x){
  data.frame(real  = x,
             "T-1"  = zamakni(x, 1),
             "T-2" = zamakni(x, 2),
             "T-3" = zamakni(x, 3),
             "T-4" = zamakni(x, 4))
}


podatki1 <- brezposelnost %>% filter(spol=="Ženske", izobrazba == "Nižja ali srednja poklicna") %>% ungroup() %>% 
  select(-c(spol, izobrazba))

razsirjeni <- naredi.df.4(podatki1$brezposelnost)


ucenje = function(podatki, formula, algoritem) {
  switch(
    algoritem,
    lin.reg = lm(formula, data = podatki),
    log.reg = glm(formula, data = podatki, family = "binomial"),
    ng = ranger(formula, data = podatki)
  )
}

napovedi = function(podatki, model, algoritem) {
  switch(
    algoritem,
    lin.reg = predict(model, podatki),
    log.reg = ifelse(
      predict(model, podatki, type = "response") >= 0.5,
      1, -1
    ),
    ng = predict(model, podatki)$predictions
  )
}

napaka_regresije = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yn.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yn - yn.hat) ^ 2
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}

napaka_razvrscanja = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yd.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yd != yd.hat)
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}



# Razreži vektor x na k enako velikih kosov
razbitje = function(x, k) {
  
  # Razreži indekse vektorja na k intervalov
  razrez = cut(seq_along(x), k, labels = FALSE)
  
  # Razbij vektor na k seznamov
  # na osnovi razreza intervalov
  split(x, razrez)
}




pp.razbitje = function(n, k = 3, stratifikacija = NULL, seme = NULL) {
  
  # najprej nastavimo seme za naključna števila, če je podano
  if (!is.null(seme)) {
    set.seed(seme)
  }
  
  # če ne opravljamo stratifikacije, potem vrnemo navadno razbitje
  # funkcijo sample uporabimo zato, da naključno premešamo primere
  if (is.null(stratifikacija)) {
    return(razbitje(sample(1:n), k))
  }
  
  # če pa opravljamo stratifikacijo, razbitje izvedemo za vsako
  # vrednost spremenljive stratifikacija posebej in nato
  # podmnožice združimo v skupno razbitje
  r = NULL
  for (v in levels(stratifikacija)) {
    
    # Če smo pri prvi vrednosti vzpostavimo razbitje
    if (is.null(r)) {
      # opravimo razbitje samo za primere z vrednostjo v
      r = razbitje(sample(which(stratifikacija == v)), k)
    } else {
      # opravimo razbitje za vrednost v
      # in podmnožice združimo s trenutnim razbitjem
      r.v = razbitje(sample(which(stratifikacija == v)), k)
      for (i in 1:k) {
        r[[i]] = c(r[[i]], r.v[[i]])
      }
    }
  }
  r
}


precno.preverjanje = function(podatki, razbitje, formula, algoritem, razvrscanje) {
  
  # pripravimo vektor za napovedi
  if (razvrscanje) {
    pp.napovedi = factor(rep(1, nrow(podatki)), levels = c(-1,1))
  } else {
    pp.napovedi = rep(0, nrow(podatki))
  }
  
  # gremo čez vse podmnožice Si razbitja S
  for (i in 1:length(razbitje)) {
    # naučimo se modela na množici S \ Si
    model = podatki[ -razbitje[[i]], ] %>% ucenje(formula, algoritem)
    
    # naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = podatki[ razbitje[[i]], ] %>% napovedi(model, algoritem)
  }
  
  if (razvrscanje) {
    mean(pp.napovedi != podatki$yd)
  } else {
    mean((pp.napovedi - podatki$real) ^ 2)
  }
}

ucni <- razsirjeni[5:nrow(razsirjeni),]
#brez stratifikacije
pp.ucni = pp.razbitje(nrow(ucni))

precno.preverjanje(ucni, pp.ucni, real ~ ., "lin.reg", FALSE )

