# 3. faza: Vizualizacija podatkov


#############################################################################
# Najprej uvozimo podatke
#############################################################################

sportni_kart = read_csv("podatki/sportni_karton_urejen")


#############################################################################
# Sedaj sledijo grafi
#############################################################################


# Graf 0, ki prikazuje spreminjanje povprečne višine fantov skozi leta 2009-2021 za razrede 1-9

graf_0 = sportni_kart %>% filter(spol=="fantje", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(visina = visina/10 ) %>%
  ggplot(
    mapping = aes(x = leto, y = visina, color = razred, group = razred)
  ) +
  geom_point()+
  geom_line() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  
  labs(
    title = "Spreminjanje povprečne višine fantov skozi leta 2009-2021 za razrede 1-9",
    x = "Leto",
    y = "Višina v cm",
    color = "Razred"
  )+
  theme(
    panel.border = element_blank(),
    axis.line.x  = element_line(color = 'black'),
    axis.line.y =  element_line(color = 'black')
  )


# graf 1 prikazuje preminjanje povprečne višine za fante in punce posebej

graf_1 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(visina = visina/10 ) %>%
  ggplot(
    mapping = aes(x = leto, y = visina, fill = spol, group = spol)
  ) +
  geom_bar(stat = "identity", width=.8, position = "dodge") +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje povprečne višine za fante in punce posebej",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Višina v cm",
    color = "Spol"
  )+
  scale_fill_manual(name = 'Spol', 
                    values = c('fantje' = 'forestgreen','punce' = 'orange'), labels = c('fantje','punce'))+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both") 


# Graf 2 prikazuje spreminjanje povprečne teže fantov in punc posebej.

graf_2 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(teza = teza/10 ) %>%
  ggplot(
    mapping = aes(x = leto, y = teza, color = spol, group = spol)
  ) +
  geom_point()+
  geom_line() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje povprečne teže fantov in punc posebej.",
    subtitle = "Za vsak razred posebej in skozi leta 2009-2021",
    x = "Leto",
    y = "Teža v kg",
    color = "Spol"
  )+
  scale_color_manual(name = 'Spol', 
                     values = c('fantje' = 'aquamarine2','punce' = 'coral1'), labels = c('fantje','punce'))+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both") 


# Graf 2_1 prikazuje spreminjanje povprečne teže fantov in punc posebej (obrnjeno)

graf_2_1 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != 2009, leto !="vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(teza = teza/10 ) %>%
  ggplot(
    mapping = aes(x = razred, y =teza, color = spol, group = spol)
  ) +
  geom_point()+
  geom_line() +
  theme_bw()+
  theme(
    axis.text.x = element_text(vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje povprečne teže fantov in punc posebej.",
    subtitle = "Za vsako leto posebej in od 1. do 9. razreda",
    x = "Razred",
    y = "Teža v kg",
    color = "Spol"
  )+
  scale_color_manual(name = 'Spol', 
                     values = c('fantje' = 'aquamarine2','punce' = 'coral1'), labels = c('fantje','punce'))+
  facet_wrap(~ leto, ncol = 3, labeller = "label_both") 


#Graf 2_2 prikazuje spreminjanje teze in visine za fante in punce skupaj
graf_2_2 = sportni_kart %>% filter(spol == "fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  ggplot(
    mapping = aes(x = razred, y = leto )
  ) +
  geom_count(aes(size= visina ,color = teza)) +
  labs(
    x = "Razred",
    y = "Leto",
    title = "Spreminjanje teže in višine za fante in punce skupaj",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    size = "Višina",
    color = "Teža"
  )+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    axis.line.x  = element_line(color = 'black'),
    axis.line.y =  element_line(color = 'black')
  )+
  scale_color_gradient2(space="Lab")


# Graf 2_3 prikazuje spreminjanje visine in teze/visino skozi leta
moje_barve <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = moje_barve(100), limits=c(0.2, 0.4))

graf_2_3 = sportni_kart %>% filter( vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(teza_na_visino = teza/visina) %>%
  ggplot(
    mapping = aes(x = razred, y = spol )
  ) +
  geom_count(aes(size= visina ,color = teza_na_visino)) +
  labs(
    x = "Razred",
    y = "Spol",
    title = "Spreminjanje višine in teže/višino",
    subtitle = "Za razrede od 1. do 9.",
    size = "Višina",
    color = "Teža/višina"
  )+
  theme_bw()+
  theme(
    axis.text.x = element_text(vjust = 0.5),
    axis.title.x = element_text(vjust = 0),
    axis.text.y = element_text(angle = 90,hjust = 0.5)
  ) + 
  theme(
    panel.border = element_blank(),
    axis.line.x  = element_line(color = 'black'),
    axis.line.y =  element_line(color = 'black')
  )+
  sc


# Graf 3 prikazuje spremnjanje kozne gube
graf_3 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  ggplot(
    mapping = aes(x = leto, y = kozna_guba, fill = spol, group = spol)
  ) +
  geom_bar(stat = "identity", width=.8, position = "dodge") +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje povprečne kožne gube za fante in punce posebej",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Kožna guba",
    fill = "Spol"
  )+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both") 



#Graf 3_2 prikazuje korelacija med težo/višino in kozno gubo

graf_3_2 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(teza_na_visino = teza/visina) %>%
  ggplot(
    mapping = aes(x = teza_na_visino, y = kozna_guba , color = spol, group = spol, fill = spol)
  ) +
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  geom_smooth(orientation = "kozna_guba", method = "loess", span= 0.3, formula= y~x)+
  #  geom_violin()+
  #  geom_polygon()+
  theme_bw()+
  labs(
    title = "Povezava med kožno gubo in težo/višino",
    subtitle = "Vključena so vsa leta in vsi razredi za fante in punce posebej",
    x = "Teža/višino",
    y = "Kožna guba",
    color = "Spol"
  )+
  scale_color_manual(name = 'Spol', 
                     values = c('fantje' = 'aquamarine2','punce' = 'coral1'), labels = c('fantje','punce'))+
  scale_fill_manual(name = 'Spol', 
                    values = c('fantje' = 'aquamarine2','punce' = 'coral1'), labels = c('fantje','punce'))+
  theme(
    panel.border = element_blank(),
    axis.line.x  = element_line(color = 'black'),
    axis.line.y =  element_line(color = 'black')
  )



# Graf 4 prikazuje spreminjanje stevila dotikov plosce z roko

graf_4 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  ggplot(
    mapping = aes(x = leto, y = dotikanje_plosce_z_roko, color = spol, group = spol)
  ) +
  geom_point()+
  geom_line() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje povprečega števila dotikov plošče za fante in punce",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Število dotikov plošče",
    color = "Spol"
  )+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both") 


# Graf 5 prikazuje spreminjanje daljine skoka z mesta za fante in punce posebej

graf_5 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  ggplot(
    mapping = aes(x = leto, y = skos_z_mesta, color = spol, group = spol)
  ) +
  geom_point()+
  geom_line() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje daljine skoka z mesta za fante in punce posebej",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Daljina skoka z mesta",
    color = "Spol"
  )+
  scale_color_manual(name = 'Spol', 
                     values = c('fantje' = 'forestgreen','punce' = 'orange'), labels = c('fantje','punce'))+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both")


# Graf 6 prikazuje spreminjanje časa premagovanja ovir nazaj za fante in punce posebej

graf_6 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(premagovanje_ovir_nazaj=premagovanje_ovir_nazaj/10) %>%
  ggplot(
    mapping = aes(x = leto, y = premagovanje_ovir_nazaj, color = spol, group = spol)
  ) +
  geom_point(alpha=0.4)+
  geom_line() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje časa premagovanja ovir nazaj za fante in punce posebej",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Čas premagovanja ovir nazaj v sekundah",
    color = "Spol"
  )+
  scale_color_manual(name = 'Spol', 
                     values = c('fantje' = 'forestgreen','punce' = 'orange'), labels = c('fantje','punce'))+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both")



#Graf 7 prikazuje spreminjanje povprečnega števila trebušnjakov za fante in punce

graf_7 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  ggplot(
    mapping = aes(x = leto, y = trebusnjaki, color = spol, group = spol)
  ) +
  geom_point()+
  geom_path() +
  theme_bw()+
  theme(
    
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje povprečnega števila trebušnjakov za fante in punce",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Število trebušnjakov",
    color = "Spol"
  )+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both") 


#Graf 8 prikazuje spreminjanje predklona naprej za fante in punce posebej

graf_8 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(premagovanje_ovir_nazaj=premagovanje_ovir_nazaj/10) %>%
  ggplot(
    mapping = aes(x = leto, y = predklon_naprej, group = spol, fill= spol)
  ) +
  geom_bar(stat = "identity", width=.8, position = "dodge") +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje predklona naprej za fante in punce posebej",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Predklon naprej v cm",
    color = "Spol"
  )+
  scale_fill_manual(name = 'Spol', 
                    values = c('fantje' = 'deeppink3','punce' = 'lightpink'), labels = c('fantje','punce'))+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both")



#Graf 9 prikazuje spreminjanje časa vese v zgibi za fante in punce posebej

graf_9 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  ggplot(
    mapping = aes(x = leto, y = vesa_v_zgibi, color = spol, group = spol)
  ) +
  geom_point()+
  geom_path() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje časa vese v zgibi za fante in punce posebej",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Čas vese v zgibi",
    color = "Spol"
  )+
  scale_color_manual(name = 'Spol', 
                     values = c('fantje' = 'royalblue3','punce' = 'palegreen3'), labels = c('fantje','punce'))+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both") 


# Graf 10 prikazuje spreminjanje časa teka na 60m za fante in punce posebej.

graf_10 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta", razred != "vsi_razredi_skupaj") %>%
  mutate(tek_na_60m=tek_na_60m/10)%>%
  ggplot(
    mapping = aes(x = leto, y = tek_na_60m, color = spol, group = spol)
  ) +
  geom_point()+
  geom_path() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje časa teka na 60m za fante in punce posebej",
    subtitle = "Skozi leta 2009-2021 za razrede od 1. do 9.",
    x = "Leto",
    y = "Čas teka na 60m v sekundah",
    color = "Spol"
  )+
  scale_color_manual(name = 'Spol', 
                     values = c('fantje' = 'thistle3','punce' = 'slateblue3'), labels = c('fantje','punce'))+
  facet_wrap(~ razred, ncol = 3, labeller = "label_both") 



#Graf 11 prikazuje spreminjanje časa teka na 60m za fante in punce posebej

graf_11 = sportni_kart %>% filter(spol!="fantje_in_punce", vrsta == "povprecje", leto != "vsa_leta",leto != 2009, razred != "vsi_razredi_skupaj") %>%
  mutate(tek_na_60m=tek_na_60m/10)%>%
  ggplot(
    mapping = aes(x = razred, y = tek_na_60m, color = spol, group = spol)
  ) +
  geom_point()+
  geom_path() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) + 
  labs(
    title = "Spreminjanje časa teka na 60m za fante in punce posebej",
    subtitle = "Skozi razrede od 1. do 9. in za leta 2010-2021",
    x = "Leto",
    y = "Čas teka na 60m v sekundah",
    color = "Spol"
  )+
  scale_color_manual(name = 'Spol', 
                     values = c('fantje' = 'thistle3','punce' = 'slateblue3'), labels = c('fantje','punce'))+
  facet_wrap(~ leto, ncol = 3, labeller = "label_both") 






###############################################################################
# Zemljevid
###############################################################################


source("lib/uvozi.zemljevid.r")

zemljevid <-
  uvozi.zemljevid(
    "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
    "ne_50m_admin_0_countries",
    mapa = "zemljevidi",
    pot.zemljevida = "",
    encoding = "UTF-8"
  ) %>%
  fortify() %>% 
  filter(CONTINENT %in% c("Europe"), long < 50 & long > -30 & lat > 35 & lat < 85)

#spreminimo ime
colnames(zemljevid)[26] <- "drzava"


#uvozimo nase podatke za zemljevid
podatki_zemljevid = read_csv("podatki/podatki_ITT")

#Popravimo podatke
podatki_zemljevid$drzava[podatki_zemljevid$drzava == "Bosnia and Herzegovina"] = "Bosnia and Herz."
podatki_zemljevid$drzava[podatki_zemljevid$drzava == "Czech Republic"] = "Czechia"
podatki_zemljevid$drzava[podatki_zemljevid$drzava == "Macedonia (TFYR)"] = "North Macedonia"
podatki_zemljevid$drzava[podatki_zemljevid$drzava == "Russian Federation"] = "Russia"

podatki_zemljevid_2 = podatki_zemljevid %>% filter(drzava != "Cyprus",drzava != "Georgia",drzava != "Turkey")



#narisemo zemljevid ki prikazuje povprecen ITT v drzavah evrope za fante stare 19 let, v letu 2019
graf_zemljevid_fantje = podatki_zemljevid_2 %>% filter(splo == "Boys", leto==2019, starostna_skupina== 19)%>% 
  right_join(zemljevid, by ="drzava")%>%
  ggplot() +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = povprecen_ITT),
    color = "grey") +
  labs(
    title = "Povprečen indeks telesne teže v državah Evrope v letu 2019",
    subtitle = "Za fante stare 19 let",
  )+ 
  theme(axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(), 
        panel.background = element_blank()) +
  scale_fill_gradient(low = munsell::mnsl("5B 7/8"),
                      high = munsell::mnsl("5Y 7/8")) +
  labs(fill="Povprecen ITT") +
  geom_path(data = right_join(podatki_zemljevid_2, zemljevid, by = "drzava"),
            aes(x = long, y = lat, group = group), 
            color = "white", size = 0.1)



#narisemo zemljevid ki prikazuje povprecen ITT v drzavah evrope za punce stare 19 let, v letu 2019

graf_zemljevid_punce = podatki_zemljevid_2 %>% filter(splo == "Girls", leto==2019, starostna_skupina== 19)%>% 
  right_join(zemljevid, by ="drzava")%>%
  ggplot() +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = povprecen_ITT),
    color = "grey") +
  labs(
    title = "Povprečen indeks telesne teže v državah Evrope v letu 2019",
    subtitle = "Za punce stare 19 let",
  )+ 
  theme(axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(), 
        panel.background = element_blank()) +
  scale_fill_gradient(low = munsell::mnsl("5B 7/8"),
                      high = munsell::mnsl("5Y 7/8")) +
  labs(fill="Povprecen ITT") +
  geom_path(data = right_join(podatki_zemljevid_2, zemljevid, by = "drzava"),
            aes(x = long, y = lat, group = group), 
            color = "white", size = 0.1)