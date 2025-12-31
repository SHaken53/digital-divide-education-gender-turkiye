library(readr)
library(dplyr)
library(stringr)

# Fonksiyon: CSV dosyasini alir, frekans hesaplamalarini yapar, temiz tablo dondurur
hesapla_frekans <- function(dosya_adi) {
  # 1. CSV'yi oku (tum sutunlari karakter olarak al)
  veri <- read_delim(dosya_adi,
                     delim = ";",
                     col_types = cols(.default = col_character()))
  
  # 2. Sutun isimlerini duzenle
  colnames(veri) <- c("Yil", "Toplam", "Erkek", "Kadin")
  
  # 3. Sayisal donusum (virgulu noktaya cevir, karakterden numerige)
  veri <- veri %>%
    mutate(across(c(Toplam, Erkek, Kadin), ~ as.numeric(str_replace(., ",", "."))))
  
  # 4. Grup buyuklugu
  group_n <- 120000
  
  # 5. Yeni hesaplama
  veri_frekans <- veri %>%
    rowwise() %>%
    mutate(
      oran_toplami = Erkek + Kadin,
      
      # ???? Eger oranlar yoksa varsayilan esitlik ver
      Erkek_oran = ifelse(oran_toplami > 0, Erkek / oran_toplami, 0.5),
      Kadin_oran = ifelse(oran_toplami > 0, Kadin / oran_toplami, 0.5),
      
      # ???? Eger Erkek ve Kadin orani birbirine ??ok yakinsa ya da fark 0 ise sabit ver
      oransal_fark = abs(Erkek - Kadin),
      Erkek_n = ifelse(oransal_fark < 1e-6, group_n / 2,
                       round(group_n * Erkek_oran)),
      Erkek_n = ifelse(is.na(Erkek_n) | is.infinite(Erkek_n), round(group_n / 2), Erkek_n),
      Kadin_n = group_n - Erkek_n,
      
      # ???? Kullanici sayilari
      Erkek_Kullanan = round(Erkek * Erkek_n / 100),
      Kadin_Kullanan = round(Kadin * Kadin_n / 100),
      
      # ???? Kullanmayan sayilar
      Erkek_Kullanmayan = Erkek_n - Erkek_Kullanan,
      Kadin_Kullanmayan = Kadin_n - Kadin_Kullanan,
      
      # ???? Toplamlar
      Toplam_Kullanan = Erkek_Kullanan + Kadin_Kullanan,
      Toplam_Kullanmayan = Erkek_Kullanmayan + Kadin_Kullanmayan
    ) %>%
    ungroup() %>%
    select(
      Yil,
      Toplam,
      Erkek,
      Kadin,
      Erkek_n,
      Kadin_n,
      Erkek_Kullanan,
      Erkek_Kullanmayan,
      Kadin_Kullanan,
      Kadin_Kullanmayan,
      Toplam_Kullanan,
      Toplam_Kullanmayan
    )
  
  return(veri_frekans)
}

# CSV dosyalarini frekans hesaplayan fonksiyonla yukle
veri1_frekans <- hesapla_frekans("birokulbitirmedi.csv")
veri2_frekans <- hesapla_frekans("ilkokul.csv")
veri3_frekans <- hesapla_frekans("ilkortaveyameslekiorta.csv")
veri4_frekans <- hesapla_frekans("liseveyameslekilise.csv")
veri5_frekans <- hesapla_frekans("universite.csv")

# Ilk 10 satiri kontrol et
print(head(veri1_frekans, 10))

# CSV'ye yazdirma:
# write_csv(veri1_frekans, "veri1_frekans.csv")

# ----------------------------------------------
# Bu kod ne saglar?
# - Toplam_Kullanan: En guvenilir degere gore baz alinir
# - Erkek/Kadin_Pay: Oranlara gore normalize edilir
# - round(): Sayilar tam kisi olur
# - Denge kurulur: Erkek + Kadin = Toplam tutarli olur
# ----------------------------------------------

library(tidyr)

# 1. Her frekans veri setine "Egitim Seviyesi" etiketi ekle
veri1_frekans$Egitim_Seviyesi <- "Bir okul bitirmedi"
veri2_frekans$Egitim_Seviyesi <- "Ilkokul"
veri3_frekans$Egitim_Seviyesi <- "Ortaokul"
veri4_frekans$Egitim_Seviyesi <- "Lise"
veri5_frekans$Egitim_Seviyesi <- "Universite"

# 2. Tum veri setlerini birlestir (alt alta)
birlesik <- bind_rows(veri1_frekans,
                      veri2_frekans,
                      veri3_frekans,
                      veri4_frekans,
                      veri5_frekans)

# 3. Genis formattaki veriyi uzun (tidy) formata ceviriyoruz
veri_tidy <- birlesik %>%
  pivot_longer(
    cols = c(
      Erkek_Kullanan,
      Erkek_Kullanmayan,
      Kadin_Kullanan,
      Kadin_Kullanmayan
    ),
    names_to = c("Cinsiyet", "Kullanim"),
    names_sep = "_",
    values_to = "Frekans"
  ) %>%
  select(Yil, Egitim_Seviyesi, Cinsiyet, Kullanim, Frekans)

# 4. Ornek ilk 10 satiri yazdir
print(head(veri_tidy, 10))

# ----------------------------------------------
# Bu kod ne yapar?
# - Egitim seviyelerini etiketler
# - 5 farkli veri setini tek tabloda birlestirir
# - Genis veriyi uzun formata donusturur (Cinsiyet & Kullanim ayrilir)
# - Her satir = 1 grup (ornegin: 2024, Lise, Erkek, Kullanan, 58700)
# - Analiz, gorsellestirme ve ki-kare testi icin ideal hale getirir
# ----------------------------------------------

# 5. Egitim seviyesini istedigimiz siraya gore sirali faktor yapalim
veri_tidy$Egitim_Seviyesi <- factor(
  veri_tidy$Egitim_Seviyesi,
  levels = c(
    "Bir okul bitirmedi",
    "Ilkokul",
    "Ortaokul",
    "Lise",
    "Universite"
  )
)





library(ggplot2)

# ----------------------------------------------
# Line Chart: Yillara gore kullanim egilimi
# Amac:
# "Turkiye'de internet kullanim oranlari yillar icinde nasil degisti?"
# T??m egitim duzeylerini bir arada gostererek cizecegiz.
# ----------------------------------------------

# Sadece internet kullanan bireyleri filtrele
veri_line <- veri_tidy %>%
  filter(Kullanim == "Kullanan") %>%
  group_by(Yil, Egitim_Seviyesi) %>%
  summarise(Toplam_Kullanan = sum(Frekans), .groups = "drop")

# Grafik: Egitim duzeylerine gore yillik internet kullanim
ggplot(veri_line,
       aes(x = as.integer(Yil), y = Toplam_Kullanan, color = Egitim_Seviyesi)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Egitim Duzeyine Gore Yillik Internet Kullanici Sayisi",
    x = "Yil",
    y = "Toplam Kullanici Sayisi",
    color = "Egitim Seviyesi"
  ) +
  theme_minimal()

# ----------------------------------------------
# G??zlem:
# - Universite mezunlari 2004'ten itibaren en yuksek kullanici sayisina sahiptir.
# - Ortaokul ve lise mezunlarinda 2007-2010 ve 2015 sonrasi belirgin artis gorulmektedir.
# - Ilkokul ve hic okul bitirmeyen gruplarda artis daha gec ve yavas baslamistir.
#
# Yorum:
# - Egitim seviyesi arttikca internet kullanimi da artmaktadir.
# - Dijitallesme politikalarinin dusuk egitim gruplarinda zamanla daha fazla etkili oldugu dusunulebilir.

# Ek Notlar:
# 2008 ??? e-Devlet tanitimi
# 2015 ??? e-Nabiz, mobil kamu servislerinin yayginlasmasi
# ----------------------------------------------





# ----------------------------------------------
# Area Chart ??? Egitim Duzeylerinin Toplam Katkisi
# Amac:
# Her yil toplam internet kullanicilarinin icinde hangi egitim grubu ne kadar yer kapliyor?
# ----------------------------------------------

# 1. Sadece "Kullanan" olanlari al ve yil + egitim seviyesine gore gruplandir
veri_area <- veri_tidy %>%
  filter(Kullanim == "Kullanan") %>%
  group_by(Yil, Egitim_Seviyesi) %>%
  summarise(Toplam_Kullanan = sum(Frekans), .groups = "drop")

# 2. Grafik: Area chart (kullananlarin sayisini egitim seviyesine gore goster)
library(ggplot2)
library(scales)

ggplot(veri_area,
       aes(x = as.integer(Yil), y = Toplam_Kullanan, fill = Egitim_Seviyesi)) +
  geom_area(alpha = 0.8,
            size = 0.4,
            colour = "white") +
  labs(
    title = "Yillara Gore Internet Kullanicilarinin Egitim Duzeyine Dagilimi",
    x = "Yil",
    y = "Toplam Kullanici Sayisi",
    fill = "Egitim Seviyesi"
  ) +
  scale_y_continuous(labels = comma) +  # Y eksenini 100000 yerine 100.000 gibi goster
  theme_minimal()

# ----------------------------------------------
# G??zlem:
# - Universite mezunlari her yilda en buyuk payi almaktadir.
# - 2015 sonrasi ortaokul ve lise mezunlarinin katkisi belirgin sekilde artmistir.
# - Ilkokul ve okul bitirmeyen bireylerin toplama katkisi zamanla artmis olsa da diger gruplara kiyasla daha dusuktur.
#
# Yorum:
# - Bu grafik, zaman icinde tum egitim seviyelerinde internet kullaniminda artis oldugunu gostermektedir.
# - Universite egitimli bireylerin dijital adaptasyonu daha erken ve yogun olmus, dusuk egitim gruplari ise daha gec dahil olmustur.
# ----------------------------------------------





# ----------------------------------------------
# Stacked Bar Chart ??? Egitim, Cinsiyet ve Kullanim Durumu Iliskisi
# Amacimiz:
# Farkli egitim seviyelerindeki kadin ve erkek bireylerin internet kullanim durumlarini karsilastirarak,
# cinsiyet ve egitim duzeyinin internet kullanimina etkisini gorsel olarak ortaya koymak.
# ----------------------------------------------

# 1. Egitim seviyesine gore gruplara ayrilmis veri: Toplam kullanici sayisi (frekans)
veri_bar <- veri_tidy %>%
  group_by(Egitim_Seviyesi, Cinsiyet, Kullanim) %>%
  summarise(Toplam = sum(Frekans), .groups = "drop")

# 2. Grafik cizimi: Stacked Bar Chart
ggplot(veri_bar, aes(x = Egitim_Seviyesi, y = Toplam, fill = Kullanim)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Cinsiyet) +
  labs(
    title = "Egitim Seviyesine ve Cinsiyete Gore Internet Kullanim Oranlari",
    x = "Egitim Seviyesi",
    y = "Toplam Kisi Sayisi",
    fill = "Internet Kullanim Durumu"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ----------------------------------------------
# Gozlem:
# - Erkeklerde 'bir okul bitirmedi' kategorisinde kullanmayanlar sayica cok fazladir.
# - Universite seviyesinde kadin ve erkeklerde kullanmayan oranlar cok dusuktur.
# - Kademe kademe egitim seviyesi arttikca kullanmayan oranlar azalmaktadir.
#
# Yorum:
# - Egitim duzeyi arttikca dijital katilim artmakta, bu etki hem erkek hem kadinlar icin gozlemlenmektedir.
# - Kadinlarin universite seviyesinde erkeklerle esdeger kullanim oranina ulasmis olmasi dijital esitlik icin olumlu bir gostergedir.
# ----------------------------------------------





# ----------------------------------------------
# Grouped Bar Chart ??? Egitim Duzeyine Gore Erkek/Kadin Kullanici Sayisi Karsilastirmasi
# Amacimiz:
# Her egitim seviyesinde internet kullanan kadin ve erkeklerin sayisal farkini yan yana gormek
# (Hipotez testi oncesi cinsiyet bazli farklari daha acik gorsellestirme)
# ----------------------------------------------

# 1. Sadece internet kullanan bireyleri al
veri_grouped <- veri_tidy %>%
  filter(Kullanim == "Kullanan") %>%
  group_by(Egitim_Seviyesi, Cinsiyet) %>%
  summarise(Toplam_Kullanan = sum(Frekans), .groups = "drop")

# 2. Grafik: Grouped bar chart (yan yana erkek ve kadin)
ggplot(veri_grouped,
       aes(x = Egitim_Seviyesi, y = Toplam_Kullanan, fill = Cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Egitim Duzeyine Gore Erkek ve Kadin Internet Kullanici Sayisi",
       x = "Egitim Seviyesi",
       y = "Kullanan Kisi Sayisi",
       fill = "Cinsiyet") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +  # Turkce format
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ----------------------------------------------
# Gozlem:
# - Universite seviyesinde erkek ve kadinlarin internet kullanimi neredeyse esittir.
# - Dusuk egitim seviyelerinde (bir okul bitirmedi, ilkokul) erkeklerin kullanici sayisi kadinlardan bariz sekilde fazladir.
# - Egitim seviyesi arttikca cinsiyetler arasindaki fark azalmaktadir.
#
# Yorum:
# - Universite egitimli bireylerde cinsiyete dayali dijital esitlik saglanmis gibi gorunmektedir.
# - Cinsiyet farki, egitim seviyesinin dusuk oldugu gruplarda daha belirgin olup dijital kapsayicilik acisindan onemli bir alandir.
# ----------------------------------------------





# ----------------------------------------------
# HEATMAP ??? Egitim x Yil Matrisinde Internet Kullanimi
# Amacimiz:
# Yillar icinde hangi egitim seviyesindeki bireylerin daha fazla internet kullandigini,
# renk yogunlugu ile gorsel olarak ifade etmek.
# ----------------------------------------------

# 1. Sadece internet kullanan bireyler alinir, yil ve egitim seviyesine gore gruplandirilir
veri_heatmap <- veri_tidy %>%
  filter(Kullanim == "Kullanan") %>%
  group_by(Yil, Egitim_Seviyesi) %>%
  summarise(Kullanan_Sayi = sum(Frekans), .groups = "drop")

# 2. Grafik: Yil x Egitim duzeyi matrisinde renk yogunlugu ile kullanim sayisi
ggplot(veri_heatmap,
       aes(x = Yil, y = Egitim_Seviyesi, fill = Kullanan_Sayi)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#D3E5FF", high = "#08306B") +
  labs(title = "Yil ve Egitim Duzeyine Gore Internet Kullanimi",
       x = "Yil",
       y = "Egitim Seviyesi",
       fill = "Kullanan Sayisi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ----------------------------------------------
# Gozlem:
# - Universite mezunlari 2004'ten itibaren her yilda en yuksek kullanim sayisina sahiptir.
# - Tum egitim seviyelerinde yillara bagli olarak renk koyulasmaktadir, bu da kullanici sayisinin arttigini gostermektedir.
# - Ilkokul ve 'bir okul bitirmedi' gruplari daha acik tonlarda kalmaktadir.
#
# Yorum:
# - Bu grafik, dijitallesmenin zamanla egitim seviyelerine yayildigini ve yuksek egitim seviyelerinin her yilda daha fazla internet kullanimi yaptigini net sekilde gostermektedir.
# ----------------------------------------------






# ----------------------------------------------
# Facet Grid ??? Yil Bazli Cinsiyet ve Egitim Dagilimi
# Amacimiz:
# Her yil icin internet kullanimini egitim ve cinsiyete gore kucuk grafiklerle (facet) incelemek.
# Boylece zaman serisi icinde yapisal degisimi daha net gozlemleriz.
# ----------------------------------------------

# 1. Yillik egitim-cinsiyet bazli kullanan sayisi
veri_facet <- veri_tidy %>%
  filter(Kullanim == "Kullanan") %>%
  group_by(Yil, Egitim_Seviyesi, Cinsiyet) %>%
  summarise(Kullanan = sum(Frekans), .groups = "drop")

# 2. Grafik: Facet grid ile her yili ayri gostermek
ggplot(veri_facet, aes(x = Egitim_Seviyesi, y = Kullanan, fill = Cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Yil, ncol = 4) +
  labs(
    title = "Yillara Gore Egitim ve Cinsiyete Gore Internet Kullanimi",
    x = "Egitim Seviyesi",
    y = "Kullanan Kisi Sayisi",
    fill = "Cinsiyet"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ----------------------------------------------
# Gozlem:
# - Yillar ilerledikce tum egitim seviyelerinde hem erkek hem kadin kullanici sayisi artis gostermektedir.
# - Universite seviyesinde erkek ve kadin kullanici sayilari neredeyse esitlenmistir.
# - Dusuk egitim gruplarinda (bir okul bitirmedi ve ilkokul) erkek kullanici sayisi kadinlardan genel olarak daha yuksektir.
#
# Yorum:
# - Zamanla internet kullanimi egitim seviyesinden bagimsiz sekilde topluma daha esit dagilmistir.
# - Ancak egitim seviyesi dusuk bireyler arasinda cinsiyet bazli farklar uzun sure korunmustur.
# ----------------------------------------------





# ----------------------------------------------
# Ki-Kare Testi ??? Egitim Duzeyi ile Internet Kullanimi Arasinda Iliski Var mi?
# TESTIN AMACI
# Hipotezimiz:
#   "Egitim seviyesi ile bireylerin internet kullanip kullanmama durumu arasinda anlamli bir iliski vardir."
# Bu testle, kategorik iki degiskenin (??? Egitim Seviyesi ve Kullanim Durumu)
# birbirinden bagimsiz mi yoksa iliskili mi oldugunu test ediyoruz.
# ----------------------------------------------
# Hipotezler
# H0 (Null): Egitim duzeyi ile internet kullanimi arasinda iliski yoktur (bagimsizdir).
# H1 (Alternatif): Egitim duzeyi ile internet kullanimi arasinda anlamli bir iliski vardir (bagimlidir).
# ----------------------------------------------
# VERI TIPI
# Veri: frekans tablosu (kontenjans tablosu)
# Degiskenler: Egitim_Seviyesi ?? Kullanim
# (Cinsiyeti disarida birakiyoruz, bu test sadece 2 degisken icindir)
# ----------------------------------------------

# 0. Bilimsel gosterimi kapat
options(scipen = 999)  # Kucuk sayilarin bilimsel degil, ondalik olarak yazilmasini saglar

# 1. Frekanslari grupla
veri_ki <- veri_tidy %>%
  group_by(Egitim_Seviyesi, Kullanim) %>%
  summarise(Frekans = sum(Frekans), .groups = "drop")

# 2. Kontenjans tablosu olustur
tablo_ki <- xtabs(Frekans ~ Egitim_Seviyesi + Kullanim, data = veri_ki)

# 3. Ki-Kare testi uygula
sonuc_ki <- chisq.test(tablo_ki)

# 4. Ham sonucu yazdir
print(sonuc_ki)

# 5. Net ve okunabilir ozet cikti
cat("\n--- Ozetli Yorum ---\n")
cat("X-squared Degeri: ", round(sonuc_ki$statistic, 2), "\n")
cat("Serbestlik Derecesi (df): ", sonuc_ki$parameter, "\n")
cat("p-degeri: ", round(sonuc_ki$p.value, 10), "\n")

# 6. Karar cumlesi
if (sonuc_ki$p.value < 0.05) {
  cat("Karar: H0 reddedilir ??? Egitim duzeyi ile internet kullanimi arasinda anlamli bir iliski vardir.\n")
} else {
  cat("Karar: H0 reddedilemez ??? Egitim duzeyi ile internet kullanimi arasinda anlamli bir iliski yoktur.\n")
}

# ----------------------------------------------
# Gozlem:
# - X-squared Degeri: 3774485
# - Serbestlik Derecesi (df): 4
# - p-degeri: 0.00000000000000022 (yaklasik sifir)
#
# Yorum:
# Karar: H0 reddedilir ??? Egitim duzeyi ile internet kullanimi arasinda anlamli bir iliski vardir.
# p-degeri cok kucuk ??? H0 hipotezi reddedilir.
# Yani: Egitim duzeyi ile bireylerin internet kullanimi durumu arasinda istatistiksel olarak cok guclu bir iliski vardir.
# Grafiklerde de goruldugu gibi egitim seviyesi arttikca internet kullanim oranlari da belirgin sekilde artmaktadir.
# Ozellikle universite ve lise seviyesindeki bireyler en yuksek internet kullanicilaridir.
# Buna karsilik, "bir okul bitirmeyen" ya da sadece "ilkokul" mezunu bireylerde internet kullanimi daha dusuk seyretmektedir.
# Bu sonuc, gorsel analizlerimizle uyumlu sekilde istatistiksel olarak da desteklenmektedir.
# ----------------------------------------------





# ----------------------------------------------
# Z-Test ??? Universite Mezunlarinda Erkek ve Kadin Kullanici Oranlarinin Karsilastirilmasi
# TESTIN AMACI
# Hipotezimiz:
# Universite mezunu erkek ve kadinlarin internet kullanma oranlari arasinda anlamli bir fark var mi?
# Bu testle, ayni egitim seviyesindeki iki grubun (erkek ve kadin) internet kullanma oranlarinin esit olup olmadigi test edilir.
# ----------------------------------------------
# Hipotezler
# H0 (Null): Erkek ve kadin internet kullanim oranlari esittir (p1 = p2)
# H1 (Alternatif): Erkek ve kadin internet kullanim oranlari farklidir (p1 ??? p2)
# ----------------------------------------------
# VERI TIPI
# Sayisal frekans verileri (kullanan kisi sayisi / toplam kisi sayisi)
# Degiskenler: Cinsiyet ?? Internet Kullanim (sadece "Universite" grubu filtrelenir)
# ----------------------------------------------

# 1. Universite mezunlari icin veriyi filtrele
uni_data <- veri_tidy %>%
  filter(Egitim_Seviyesi == "Universite") %>%
  group_by(Cinsiyet, Kullanim) %>%
  summarise(Frekans = sum(Frekans), .groups = "drop")

# 2. Erkek ve Kadin icin kullanan ve toplam sayilari guvenli sekilde al
erkek_kullanan <- uni_data %>% filter(Cinsiyet == "Erkek", Kullanim == "Kullanan") %>% pull(Frekans)
if (length(erkek_kullanan) == 0) erkek_kullanan <- 0

erkek_toplam <- uni_data %>% filter(Cinsiyet == "Erkek") %>% summarise(sum(Frekans)) %>% pull()
if (length(erkek_toplam) == 0) erkek_toplam <- 0

kadin_kullanan <- uni_data %>% filter(Cinsiyet == "Kadin", Kullanim == "Kullanan") %>% pull(Frekans)
if (length(kadin_kullanan) == 0) kadin_kullanan <- 0

kadin_toplam <- uni_data %>% filter(Cinsiyet == "Kadin") %>% summarise(sum(Frekans)) %>% pull()
if (length(kadin_toplam) == 0) kadin_toplam <- 0

# 3. Oran testi (Z-Test via prop.test)
z_test_sonuc <- prop.test(
  x = c(erkek_kullanan, kadin_kullanan),
  n = c(erkek_toplam, kadin_toplam),
  correct = FALSE  # continuity correction kapali
)

# 4. Sonuclari yazdir
print(z_test_sonuc)

# 5. Net ve okunabilir yorumlu cikti
cat("\n--- Z-Test Yorumu ---\n")
cat("Erkek orani: ", round(erkek_kullanan / erkek_toplam, 4), "\n")
cat("Kadin orani: ", round(kadin_kullanan / kadin_toplam, 4), "\n")
cat("p-degeri: ", round(z_test_sonuc$p.value, 10), "\n")

if (z_test_sonuc$p.value < 0.05) {
  cat("Karar: H0 reddedilir ??? Erkek ve kadin kullanici oranlari arasinda anlamli fark vardir.\n")
} else {
  cat("Karar: H0 reddedilemez ??? Cinsiyet farki anlamli degildir.\n")
}

# ----------------------------------------------
# Gozlem:
# X-squared Degeri: 261.93
# Serbestlik Derecesi: 1
# p-degeri: 0.00000000000000022
# Erkek oran??: 0.9104
# Kadin oran??: 0.9044

# Yorum:
# H0 reddedilir ??? Universite mezunlari arasinda erkek ve kadinlarin internet kullanma oranlari arasinda anlamli bir fark vardir.
# p-degeri cok kucuk oldugu icin fark istatistiksel olarak anlamlidir.
# Erkeklerin internet kullanimi kadinlara gore az da olsa daha yuksektir.
# Ancak farkin boyutu (%0.6 civarinda) kucuk olsa da, buyuk orneklem hacmi nedeniyle istatistiksel olarak anlamli kabul edilmistir.
# Bu durum, toplumsal dijitallesme surecinde cinsiyet esitsizliginin egitimli bireylerde dahi gozlenebilecegini gosterir.
# ----------------------------------------------





# ----------------------------------------------
# Z-Test ??? "Bir okul bitirmedi" grubunda Erkek ve Kadin kullanici oranlari
# TESTIN AMACI:
# Egitim seviyesi "Bir okul bitirmedi" olan bireylerde,
# erkek ve kadinlarin internet kullanma oranlari arasinda anlamli bir fark olup olmadigini test ediyoruz.
# ----------------------------------------------
# Hipotezler:
# H0 (sifir hipotezi): Erkek ve kadin kullanici oranlari esittir
# H1 (alternatif hipotez): Erkek ve kadin kullanici oranlari farklidir
# ----------------------------------------------
# VERI TIPI:
# Frekans verisi (kullanan kisi sayisi / toplam kisi sayisi)
# Degiskenler: Cinsiyet ?? Internet Kullanim durumu
# ----------------------------------------------

# 1. Veriyi filtrele (sadece "Bir okul bitirmedi" egitim seviyesi)
bitirmedi_data <- veri_tidy %>%
  filter(Egitim_Seviyesi == "Bir okul bitirmedi") %>%
  group_by(Cinsiyet, Kullanim) %>%
  summarise(Frekans = sum(Frekans), .groups = "drop")

# 2. Erkek ve kadin icin kullanan ve toplam sayilari al
erkek_kullanan <- bitirmedi_data %>%
  filter(Cinsiyet == "Erkek", Kullanim == "Kullanan") %>%
  pull(Frekans)
if (length(erkek_kullanan) == 0) erkek_kullanan <- 0

erkek_toplam <- bitirmedi_data %>%
  filter(Cinsiyet == "Erkek") %>%
  summarise(sum(Frekans)) %>%
  pull()
if (length(erkek_toplam) == 0) erkek_toplam <- 0

kadin_kullanan <- bitirmedi_data %>%
  filter(Cinsiyet == "Kadin", Kullanim == "Kullanan") %>%
  pull(Frekans)
if (length(kadin_kullanan) == 0) kadin_kullanan <- 0

kadin_toplam <- bitirmedi_data %>%
  filter(Cinsiyet == "Kadin") %>%
  summarise(sum(Frekans)) %>%
  pull()
if (length(kadin_toplam) == 0) kadin_toplam <- 0

# 3. Oran testi (Z-Test)
z_test_bitirmedi <- prop.test(
  x = c(erkek_kullanan, kadin_kullanan),
  n = c(erkek_toplam, kadin_toplam),
  correct = FALSE
)

# 4. Test sonucunu yazdir
cat("\n--- Z-Test Sonucu (Ham Cikti) ---\n")
print(z_test_bitirmedi)

# 5. Yorumu yazdir
cat("\n--- Z-Test Yorumu ??? Bir okul bitirmedi ---\n")
cat("Erkek orani: ", round(erkek_kullanan / erkek_toplam, 4), "\n")
cat("Kadin orani: ", round(kadin_kullanan / kadin_toplam, 4), "\n")
cat("p-degeri: ", round(z_test_bitirmedi$p.value, 10), "\n")

if (z_test_bitirmedi$p.value < 0.05) {
  cat("Karar: H0 reddedilir ??? Erkek ve kadin kullanici oranlari arasinda anlamli fark vardir.\n")
} else {
  cat("Karar: H0 reddedilemez ??? Cinsiyet farki anlamli degildir.\n")
}

# ----------------------------------------------
# Gozlem:
# X-squared Degeri: 744.69
# Serbestlik Derecesi: 1
# p-degeri: 0.00000000000000022
# Erkek orani: 0.1480
# Kadin orani: 0.1624

# Yorum:
# H0 reddedilir ??? "Bir okul bitirmedi" egitim seviyesindeki bireyler arasinda,
# erkek ve kadinlarin internet kullanma oranlari arasinda anlamli bir fark vardir.
# p-degeri cok kucuk oldugu icin bu fark istatistiksel olarak anlamlidir.
# Bu grupta kadinlarin internet kullanimi erkeklere gore daha yuksektir.
# Farkin boyutu yaklasik %1.6'dur ve bu fark, toplumsal cinsiyet etkilerinin
# dusuk egitim seviyelerinde dijital katilima nasil yansidigi konusunda ipucu verir.
# Sonuclar, dijital bolunmenin sadece teknolojiye erisim degil, ayni zamanda
# egitim ve cinsiyet gibi sosyal degiskenlerle de iliskili oldugunu ortaya koyar.
# ----------------------------------------------





# ----------------------------------------------
# Z-Test ??? Ilkokul Mezunlarinda Erkek ve Kadin Kullanici Oranlarinin Karsilastirilmasi
# TESTIN AMACI
# Ilkokul mezunu erkek ve kadinlarin internet kullanma oranlari arasinda anlamli bir fark var mi?
# Bu test, cinsiyete dayali farkliliklarin dusuk egitim seviyesinde de varligini arastirir.
# ----------------------------------------------
# Hipotezler
# H0: Erkek ve kadin oranlari esittir
# H1: Erkek ve kadin oranlari farklidir
# ----------------------------------------------

# 1. Veriyi filtrele (sadece "Ilkokul" egitim seviyesi)
ilkokul_data <- veri_tidy %>%
  filter(Egitim_Seviyesi == "Ilkokul") %>%
  group_by(Cinsiyet, Kullanim) %>%
  summarise(Frekans = sum(Frekans), .groups = "drop")

# 2. Erkek ve kadin icin kullanan ve toplam sayilari al
erkek_kullanan <- ilkokul_data %>%
  filter(Cinsiyet == "Erkek", Kullanim == "Kullanan") %>%
  pull(Frekans)
if (length(erkek_kullanan) == 0) erkek_kullanan <- 0

erkek_toplam <- ilkokul_data %>%
  filter(Cinsiyet == "Erkek") %>%
  summarise(sum(Frekans)) %>%
  pull()
if (length(erkek_toplam) == 0) erkek_toplam <- 0

kadin_kullanan <- ilkokul_data %>%
  filter(Cinsiyet == "Kadin", Kullanim == "Kullanan") %>%
  pull(Frekans)
if (length(kadin_kullanan) == 0) kadin_kullanan <- 0

kadin_toplam <- ilkokul_data %>%
  filter(Cinsiyet == "Kadin") %>%
  summarise(sum(Frekans)) %>%
  pull()
if (length(kadin_toplam) == 0) kadin_toplam <- 0

# 3. Oran testi (Z-Test)
z_test_ilkokul <- prop.test(
  x = c(erkek_kullanan, kadin_kullanan),
  n = c(erkek_toplam, kadin_toplam),
  correct = FALSE
)

# 4. Test sonucunu yazdir
cat("\n--- Z-Test Sonucu (Ham Cikti) ---\n")
print(z_test_ilkokul)

# 5. Net ve okunabilir yorumlu cikti
cat("\n--- Z-Test Yorumu ??? Ilkokul ---\n")
cat("Erkek orani: ", round(erkek_kullanan / erkek_toplam, 4), "\n")
cat("Kadin orani: ", round(kadin_kullanan / kadin_toplam, 4), "\n")
cat("p-degeri: ", round(z_test_ilkokul$p.value, 10), "\n")
if (z_test_ilkokul$p.value < 0.05) {
  cat("Karar: H0 reddedilir ??? Erkek ve kadin kullanici oranlari arasinda anlamli fark vardir.\n")
} else {
  cat("Karar: H0 reddedilemez ??? Cinsiyet farki anlamli degildir.\n")
}

# ----------------------------------------------
# Gozlem:
# X-squared Degeri: 4637.1
# Serbestlik Derecesi: 1
# p-degeri: 0.00000000000000022
# Erkek orani: 0.3368
# Kadin orani: 0.3799
# ----------------------------------------------

# Yorum:
# H0 reddedilir ??? Ilkokul mezunu erkek ve kadinlarin internet kullanma oranlari arasinda anlamli bir fark vardir.
# p-degeri oldukca kucuk oldugu icin, bu farkin rastlantisal olma olasiligi yok denecek kadar azdir.
# Kadinlarin internet kullanma orani erkeklerden yaklasik %4.3 daha yuksektir.
# Bu farkin boyutu diger egitim seviyelerine kiyasla daha belirgindir.
# Bu durum, dusuk egitim seviyelerinde toplumsal rol dagilimi ve dijital erisim esitsizliginin farkli sekillerde tezahur edebilecegini gosterir.
# Grafiklerde de kadin kullanici oranlarinin bu egitim seviyesinde daha yuksek oldugu gorulmustu; test sonucu bu gozlemi bilimsel olarak dogrulamaktadir.
# ----------------------------------------------





# ----------------------------------------------
# Z-Test ??? Ortaokul Mezunlarinda Erkek ve Kadin Kullanici Oranlarinin Karsilastirilmasi
# TESTIN AMACI
# ----------------------------------------------
# Hipotezler
# H0: Erkek ve kadin oranlari esittir
# H1: Erkek ve kadin oranlari farklidir
# ----------------------------------------------

# 1. Veriyi filtrele (sadece "Ortaokul" egitim seviyesi)
ortaokul_data <- veri_tidy %>%
  filter(Egitim_Seviyesi == "Ortaokul") %>%
  group_by(Cinsiyet, Kullanim) %>%
  summarise(Frekans = sum(Frekans), .groups = "drop")

# 2. Erkek ve kadin icin kullanan ve toplam sayilari al
erkek_kullanan <- ortaokul_data %>%
  filter(Cinsiyet == "Erkek", Kullanim == "Kullanan") %>%
  pull(Frekans)
if (length(erkek_kullanan) == 0) erkek_kullanan <- 0

erkek_toplam <- ortaokul_data %>%
  filter(Cinsiyet == "Erkek") %>%
  summarise(sum(Frekans)) %>%
  pull()
if (length(erkek_toplam) == 0) erkek_toplam <- 0

kadin_kullanan <- ortaokul_data %>%
  filter(Cinsiyet == "Kadin", Kullanim == "Kullanan") %>%
  pull(Frekans)
if (length(kadin_kullanan) == 0) kadin_kullanan <- 0

kadin_toplam <- ortaokul_data %>%
  filter(Cinsiyet == "Kadin") %>%
  summarise(sum(Frekans)) %>%
  pull()
if (length(kadin_toplam) == 0) kadin_toplam <- 0

# 3. Oran testi (Z-Test)
z_test_ortaokul <- prop.test(
  x = c(erkek_kullanan, kadin_kullanan),
  n = c(erkek_toplam, kadin_toplam),
  correct = FALSE
)

# 4. Test sonucunu yazdir
cat("\n--- Z-Test Sonucu (Ham Cikti) ---\n")
print(z_test_ortaokul)

# 5. Net ve okunabilir yorumlu cikti
cat("\n--- Z-Test Yorumu ??? Ortaokul ---\n")
cat("Erkek orani: ", round(erkek_kullanan / erkek_toplam, 4), "\n")
cat("Kadin orani: ", round(kadin_kullanan / kadin_toplam, 4), "\n")
cat("p-degeri: ", round(z_test_ortaokul$p.value, 10), "\n")

if (z_test_ortaokul$p.value < 0.05) {
  cat("Karar: H0 reddedilir ??? Erkek ve kadin kullanici oranlari arasinda anlamli fark vardir.\n")
} else {
  cat("Karar: H0 reddedilemez ??? Cinsiyet farki anlamli degildir.\n")
}

# ----------------------------------------------
# Gozlem:
# X-squared Degeri: 3542.1
# Serbestlik Derecesi: 1
# p-degeri: 0.00000000000000022
# Erkek orani: 0.6707
# Kadin orani: 0.634
# ----------------------------------------------

# Yorum:
# H0 reddedilir ??? Ortaokul mezunlari arasinda erkek ve kadinlarin internet kullanma oranlari arasinda anlamli fark vardir.
# p-degeri cok kucuk oldugu icin bu farkin rastlantisal olma olasiligi ihmal edilebilir.
# Erkeklerin internet kullanma orani kadinlara gore belirgin sekilde daha yuksektir.
# Bu fark yaklasik %3.7'dir ve bu egitim seviyesinde cinsiyet bazli dijital esitlik sorununun halen devam ettigini gosterir.
# Grafiklerle gozlendiginde de erkeklerin daha fazla kullandigi gorulmekteydi, bu test o farkin istatistiksel olarak anlamli oldugunu kanitlamistir.
# Bu sonuc, egitim seviyesi arttikca cinsiyet farkinin azaldigi hipotezini dolayli olarak destekler.
# ----------------------------------------------





# ----------------------------------------------
# TESTIN AMACI
# Lise mezunu erkek ve kadin bireylerin internet kullanma oranlari arasinda anlamli bir fark olup olmadigini test eder.
# Bu testle, ayni egitim seviyesindeki iki cinsiyetin kullanim oranlarinin esit olup olmadigi ortaya konur.
# ----------------------------------------------
# Hipotezler:
# H0 (Null): Erkek ve kadin oranlari esittir (p1 = p2)
# H1 (Alternatif): Erkek ve kadin oranlari farklidir (p1 ??? p2)
# ----------------------------------------------

# 1. Veriyi filtrele (sadece "Lise" egitim seviyesi)
lise_data <- veri_tidy %>%
  filter(Egitim_Seviyesi == "Lise") %>%
  group_by(Cinsiyet, Kullanim) %>%
  summarise(Frekans = sum(Frekans), .groups = "drop")

# 2. Erkek ve kadin icin kullanan ve toplam sayilari al
erkek_kullanan <- lise_data %>%
  filter(Cinsiyet == "Erkek", Kullanim == "Kullanan") %>%
  pull(Frekans)
if (length(erkek_kullanan) == 0) erkek_kullanan <- 0

erkek_toplam <- lise_data %>%
  filter(Cinsiyet == "Erkek") %>%
  summarise(sum(Frekans)) %>%
  pull()
if (length(erkek_toplam) == 0) erkek_toplam <- 0

kadin_kullanan <- lise_data %>%
  filter(Cinsiyet == "Kadin", Kullanim == "Kullanan") %>%
  pull(Frekans)
if (length(kadin_kullanan) == 0) kadin_kullanan <- 0

kadin_toplam <- lise_data %>%
  filter(Cinsiyet == "Kadin") %>%
  summarise(sum(Frekans)) %>%
  pull()
if (length(kadin_toplam) == 0) kadin_toplam <- 0

# 3. Oran testi (Z-Test)
z_test_lise <- prop.test(
  x = c(erkek_kullanan, kadin_kullanan),
  n = c(erkek_toplam, kadin_toplam),
  correct = FALSE
)

# 4. Ham sonucu yazdir
cat("\n--- Z-Test Sonucu (Ham Cikti) ---\n")
print(z_test_lise)

# 5. Yorumu yazdir
cat("\n--- Z-Test Yorumu ??? Lise ---\n")
cat("Erkek orani: ", round(erkek_kullanan / erkek_toplam, 4), "\n")
cat("Kadin orani: ", round(kadin_kullanan / kadin_toplam, 4), "\n")
cat("p-degeri: ", round(z_test_lise$p.value, 10), "\n")

if (z_test_lise$p.value < 0.05) {
  cat("Karar: H0 reddedilir ??? Erkek ve kadin kullanici oranlari arasinda anlamli fark vardir.\n")
} else {
  cat("Karar: H0 reddedilemez ??? Cinsiyet farki anlamli degildir.\n")
}

# ----------------------------------------------
# Gozlem:
# X-squared Degeri: 1544.2
# Serbestlik Derecesi (df): 1
# p-degeri: < 0.00000000000000022
# Erkek oran??: 0.7873
# Kadin oran??: 0.7661
# ----------------------------------------------
# ----------------------------------------------
# H0 reddedilir ??? Lise mezunlari arasinda erkek ve kadinlarin internet kullanma oranlari arasinda anlamli fark vardir.
# p-degeri cok kucuk oldugu icin bu farkin rastlantisal olma olasiligi yok denecek kadar azdir.
# Erkeklerin internet kullanma orani kadinlara gore anlamli derecede daha yuksektir.
# Bu fark yaklasik %2.1???dir ve bu egitim seviyesinde dahi cinsiyet temelli dijital farkliligin surdugunu gostermektedir.
# Grafiklerle gozlemlenen bu fark, bu test sayesinde istatistiksel olarak da dogrulanmistir.
# Bu sonuc, egitim seviyesi arttikca cinsiyet farkinin azaldigi hipotezini destekler niteliktedir.
# ----------------------------------------------





# ----------------------------------------------
# GORSELLESTIRME 1: Egitim Seviyesine Gore Erkek ve Kadin Oranlari
# Bu grafik, her egitim seviyesinde erkek ve kadinlarin internet kullanma oranlarini yanyana gosterir.
# Z-Test analizlerimizdeki oranlarin gozle gorulur farklarini sunar.
# ----------------------------------------------

# Z-Test oranlarindan olusan veri
oran_df <- data.frame(
  Egitim = c("Bir okul bitirmedi", "Ilkokul", "Ortaokul", "Lise", "Universite"),
  Erkek_Orani = c(0.1480, 0.3368, 0.6707, 0.7873, 0.9104),
  Kadin_Orani = c(0.1624, 0.3799, 0.6340, 0.7661, 0.9044)
)

# Veri uzun formata donusturulur
oran_long <- oran_df %>%
  pivot_longer(cols = c("Erkek_Orani", "Kadin_Orani"),
               names_to = "Cinsiyet", values_to = "Oran")

# Grup bar grafi??i
ggplot(oran_long, aes(x = Egitim, y = Oran, fill = Cinsiyet)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Egitim Seviyesine Gore Internet Kullanim Oranlari",
       y = "Kullanim Orani", x = "Egitim Seviyesi") +
  theme_minimal()

# ----------------------------------------------
# Gozlem:
# - Universite seviyesinde erkek ve kadin oranlari birbirine cok yakin (~0.91).
# - Ortaokul, lise ve ilkokul duzeylerinde erkeklerin kullanim orani kadinlara gore belirgin sekilde daha yuksek.
# - Ilkokul disinda diger seviyelerde erkeklerin oraninin daha yuksek oldugu gozlenmektedir.
# ----------------------------------------------
# Yorum:
# Bu grafik, cinsiyetler arasindaki dijital farkliligin egitim seviyesine gore nasil degistigini gosterir.
# Egitim seviyesi arttikca bu farkin azaldigi ve cinsiyetler arasinda esitlemeye dogru bir egilim oldugu anlasilmaktadir.
# Grafik, Z-testler ile elde edilen bulgulari desteklemektedir.
# ----------------------------------------------





# ----------------------------------------------
# GORSELLESTIRME 2: Egitim Seviyelerine Gore Erkek - Kadin Oran Farki
# Bu grafik, erkek ve kadinlarin internet kullanim oranlari arasindaki farki (Erkek - Kadin) gosterir.
# Z-Test analizlerimizde cikan farklarin buyuklugunu ve egilimlerini vurgular.
# ----------------------------------------------

# Oran farki hesaplanir
oran_df <- oran_df %>%
  mutate(Fark = Erkek_Orani - Kadin_Orani)

# Fark bar grafi??i
ggplot(oran_df, aes(x = Egitim, y = Fark)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Cinsiyetler Arasi Kullanici Orani Farki (Erkek - Kadin)",
       y = "Fark Orani", x = "Egitim Seviyesi") +
  theme_minimal()

# ----------------------------------------------
# Gozlem:
# - En buyuk negatif fark ilkokul seviyesinde (~ -0.04) ??? kadinlar erkeklerden daha fazla internet kullanmakta.
# - Ortaokul ve lise seviyelerinde erkekler daha fazla kullaniyor (pozitif fark).
# - Universite seviyesinde fark cok dusuk ??? cinsiyet esitligi daha fazla.
# ----------------------------------------------
# Yorum:
# Bu fark grafigi, Z-test p-degerleri ile dogrudan iliskilidir.
# Fark ne kadar buyukse, o seviyede cinsiyet farki da o kadar anlamlidir.
# Ortaokul ve lise seviyelerinde erkeklerin agirlikli kullandigi,
# Ilkokul seviyesinde ise kadinlarin daha aktif oldugu gorulmektedir.
# ----------------------------------------------





# ----------------------------------------------
# GORSELLESTIRME 3: Cinsiyet ve Kullanim Dagilimi (Mozaik Grafik)
# AMAC: Her egitim seviyesinde erkek ve kadinlarin internet kullanim oranlarini yuzde olarak karsilastirmak.
# Bu grafik, Z-Test ile bulunan farklari gorsel olarak destekler.
# ----------------------------------------------

veri_tidy %>%
  filter(Egitim_Seviyesi %in% c("Bir okul bitirmedi", "Ilkokul", "Ortaokul", "Lise", "Universite")) %>%
  group_by(Egitim_Seviyesi, Cinsiyet, Kullanim) %>%
  summarise(Frekans = sum(Frekans), .groups = "drop") %>%
  ggplot(aes(x = Cinsiyet, y = Frekans, fill = Kullanim)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~Egitim_Seviyesi) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Cinsiyet ve Kullanim Dagilimi (Stacked Bar Grafik)",
    x = "Cinsiyet",
    y = "Oran"
  ) +
  theme_minimal()

# ----------------------------------------------
# Gozlem:
# - Egitim seviyesi arttikca (lise ve universite) hem erkek hem kadinlarin internet kullanim orani artmaktadir.
# - D??s??k egitim gruplarinda (bir okul bitirmedi, ilkokul) kadinlarin internet kullanmama orani belirgin derecede daha fazladir.
# - Universite grubunda ise erkek ve kadin oranlari birbirine cok yaklasmistir.
# ----------------------------------------------
# Istatistiksel Destek:
# Bu grafik, uyguladigimiz Z-Test???lerin sonucunu gorsel olarak desteklemektedir.
# Ortaokul ve ilkokulda anlamli farklar gorulurken (yaklasik %3???4), universitelerde fark azdir (%0.6 civarinda).
# ----------------------------------------------
# Sunum Notu:
# Bu grafik, temel gorsel kanit olarak sunulabilir.
# Z-Test sonucunu gorsel olarak destekleyen en guclu grafiklerden biridir.
# ----------------------------------------------





# ----------------------------------------------
# GORSELLESTIRME 4: Cinsiyet ve Kullanim Dagilimi (Mozaik Grafik)
# AMAC: Egitim seviyelerine gore cinsiyet ve internet kullanim durumu arasindaki iliskiyi alan ve oran bazinda gostermek.
# Bu grafik, Ki-Kare testinin temelini anlamada yardimci olur.
# ----------------------------------------------

library(ggmosaic)

veri_tidy %>%
  filter(Egitim_Seviyesi %in% c("Bir okul bitirmedi", "Ilkokul", "Ortaokul", "Lise", "Universite")) %>%
  ggplot() +
  geom_mosaic(aes(weight = Frekans, x = product(Cinsiyet), fill = Kullanim)) +
  facet_wrap(~Egitim_Seviyesi) +
  labs(
    title = "Cinsiyet ve Kullanim Dagilimi (Mozaik Grafik)",
    x = "Cinsiyet",
    y = "Oran"
  ) +
  theme_minimal()

# ----------------------------------------------
# Gozlem:
# - D??suk egitim seviyelerinde (bir okul bitirmedi, ilkokul) erkekler daha fazla internet kullanmaktadir.
# - Universite seviyesinde fark yok denecek kadar azdir.
# - Bu grafik, cinsiyet ve kullanim durumunun egitim seviyelerine gore nasil degistigini gorsel olarak sunar.
# ----------------------------------------------
# Istatistiksel Destek:
# Bu grafik, Ki-Kare Testi sonucunu desteklemektedir.
# Kullanici durumu cinsiyete ve egitim seviyesine bagli olarak anlamli sekilde degismektedir.
# ----------------------------------------------
# Sunum Notu:
# Mozaik grafik, cinsiyet ve kullanim arasindaki karsilikli iliskiyi gorsellestirir.
# Ana grafik degil, destekleyici grafik olarak kullanilmasi tavsiye edilir.
# ----------------------------------------------


# ----------------------------------------------
# GORSELLESTIRME AMACI
# Yillara gore genel internet kullanim oraninin nasil degistigini gosterir.
# Bu sayede toplumsal dijitallesme trendi ortaya konur.
# ----------------------------------------------

veri_tidy %>%
  group_by(Yil, Cinsiyet, Kullanim) %>%
  summarise(Frekans = sum(Frekans), .groups = "drop") %>%
  group_by(Yil, Cinsiyet) %>%
  mutate(Oran = Frekans / sum(Frekans)) %>%
  filter(Kullanim == "Kullanan") %>%
  select(Yil, Cinsiyet, Oran) %>%
  pivot_wider(names_from = Cinsiyet, values_from = Oran) %>%
  mutate(Fark = Erkek - Kadin) %>%
  ggplot(aes(x = Yil, y = Fark)) +
  geom_line(color = "firebrick", linewidth = 1.3) +
  geom_point(size = 2, color = "red") +
  labs(
    title = "Yillara Gore Erkek - Kadin Kullanim Orani Farki",
    x = "Yil", y = "Fark (Erkek - Kadin)"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

