# =============================================================================
# TURKIYE INOVASYON EKOSISTEMI VERI GORSELLESTIRME
# =============================================================================

# 1. GEREKLI KUTUPHANELERIN YUKLENMESI VE CAGIRILMASI
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(scales)) install.packages("scales")
if(!require(readxl)) install.packages("readxl")
if(!require(ggrepel)) install.packages("ggrepel")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readxl)
library(ggrepel)

# 2. CALISMA DIZINININ AYARLANMASI
# Scriptin bulundugu klasor otomatik algilanir.
tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nchar(p) > 0) setwd(dirname(p))
  }
}, error = function(e) {
  # Rscript ile calistiriliyorsa
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("--file=", "", file_arg)
    setwd(dirname(normalizePath(script_path)))
  }
})
cat("Calisma dizini:", getwd(), "\n\n")

# 3. YARDIMCI FONKSIYONLAR VE ORTAK TEMA
# Veri donusumleri icin kisa fonksiyonlar
s  <- function(x) suppressWarnings(as.numeric(as.character(x)))
si <- function(x) suppressWarnings(as.integer(as.character(x)))

# Bazi grafiklerde kullanilan ortak tema
tema <- theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle    = element_text(color = "grey35", size = 12, hjust = 0, margin = margin(b = 12)),
    plot.caption     = element_text(color = "grey55", size = 10, hjust = 1),
    plot.margin      = margin(16, 20, 12, 16),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position  = "top",
    legend.text      = element_text(size = 12)
  )

# Ortak Renk Paleti
MAVI    <- "#1B6CA8"
TURUNCU <- "#D95F02"
YESIL   <- "#1B8858"
KIRMIZI <- "#C0392B"

# ============================================================
# GRAFIK 1: Yan Yana Sutun Grafigi - Genel Toplam Rakamlari
# ============================================================
patent_ham <- read_excel("Patent_Yillik_Basvuru.xls", skip = 3)
patent_temiz <- patent_ham %>%
  select(Yil = 1, Patent = 12) %>%
  mutate(
    Yil = s(Yil),
    Patent = s(Patent)
  ) %>%
  filter(!is.na(Yil) & Yil >= 2014 & Yil <= 2024)

faydali_ham <- read_excel("Faydali_Model_Basvurularinin_Yillara_Gore_Dagilimi.xls", skip = 3)
faydali_temiz <- faydali_ham %>%
  select(Yil = 1, Faydali_Model = 10) %>%
  mutate(
    Yil = s(Yil),
    Faydali_Model = s(Faydali_Model)
  ) %>%
  filter(!is.na(Yil) & Yil >= 2014 & Yil <= 2024)

veri_birlestirilmis <- merge(patent_temiz, faydali_temiz, by = "Yil")
veri_uzun <- veri_birlestirilmis %>%
  pivot_longer(cols = c("Patent", "Faydali_Model"), names_to = "Kategori", values_to = "Sayi")

veri_uzun$Kategori <- factor(veri_uzun$Kategori, levels = c("Patent", "Faydali_Model"))

p_genel <- ggplot(veri_uzun, aes(x = factor(Yil), y = Sayi, fill = Kategori)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", size = 0.3) +
  geom_text(aes(label = format(Sayi, big.mark = ".")), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, color = "black", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Patent" = "#E67E22", "Faydali_Model" = "#2C3E50"), 
                    labels = c("Patent (Genel Toplam)", "Faydali Model (Genel Toplam)"), 
                    name = "Bulus Turu") +
  scale_y_continuous(labels = label_number(big.mark = "."), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Turkiye Inovasyon Ekosistemi: Toplam Uretim Hacmi (2014-2024)",
    subtitle = "Yerli ve Yabanci Basvurularin Tumu (Genel Toplam) Dahil Edilmistir",
    x = "Yillar",
    y = "Genel Toplam Basvuru Sayisi"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2C3E50"),
    plot.subtitle = element_text(size = 12, color = "darkgrey"),
    axis.text.x = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )
print(p_genel)

# ============================================================
# GRAFIK 2: Turkiye'ye En Cok Patent Basvurusu Yapan 3 Ulke
# ============================================================
ham_veri_ulke <- read_excel("patent_basvurularinin_ulkelere_gore_dagilimi.xls", col_names = FALSE, skip = 2)
satirlar <- 2:nrow(ham_veri_ulke)

ulke_df <- do.call(rbind, lapply(satirlar, function(r) {
  ulke <- trimws(as.character(ham_veri_ulke[[2]][r]))
  vals <- sapply(22:32, function(col) {
    v <- ham_veri_ulke[[col]][r]
    ifelse(is.na(v), 0, as.numeric(v))
  })
  data.frame(Ulke = ulke, Yil = 2014:2024, Basvuru = vals, stringsAsFactors = FALSE)
}))

ulke_df <- ulke_df %>%
  filter(
    !is.na(Ulke),
    nchar(trimws(Ulke)) > 1,
    !grepl("T.RK.YE|TURKEY", Ulke, ignore.case = TRUE),
    !grepl("TOPLAM",          Ulke, ignore.case = TRUE),
    !grepl("D.\\u011fER|DIGER|D.GER", Ulke, ignore.case = TRUE),
    trimws(Ulke) != ""
  )

top3_isimler <- ulke_df %>%
  group_by(Ulke) %>%
  summarise(Toplam = sum(Basvuru), .groups = "drop") %>%
  arrange(desc(Toplam)) %>%
  slice(1:3) %>%
  pull(Ulke)

grafik_df       <- ulke_df[ulke_df$Ulke %in% top3_isimler, ]
grafik_df$Ulke  <- factor(grafik_df$Ulke, levels = top3_isimler)
grafik_df$Yil   <- factor(grafik_df$Yil)

p_ulke <- ggplot(grafik_df, aes(x = Yil, y = Basvuru, fill = Ulke)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, color = NA) +
  scale_fill_manual(values = c("#E67E22", "#2C3E50", "#3498DB"), name = "Ulke") +
  scale_y_continuous(labels = label_number(big.mark = "."), limits = c(0, 3000), breaks = seq(0, 3000, by = 500), expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Turkiye'ye En Cok Patent Basvurusu Yapan 3 Ulke (2014-2024)",
    subtitle = "Yillara Gore Yabanci Patent Basvuru Sayilari - Almanya, A.B.D. ve Italya",
    x        = "Yil",
    y        = "Patent Basvuru Sayisi"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16, color = "#2C3E50"),
    plot.subtitle      = element_text(size = 11, color = "darkgrey"),
    axis.text.x        = element_text(face = "bold", size = 10, color = "#2C3E50"),
    axis.text.y        = element_text(size = 10),
    axis.title         = element_text(face = "bold", size = 12, color = "#2C3E50"),
    legend.title       = element_text(face = "bold", size = 11),
    legend.position    = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )
print(p_ulke)

# ============================================================
# GRAFIK 3: Burokrasi ve Onay Performansi
# ============================================================
tescil_ham <- read_excel("Patent_Yillik_Tescil.xls", skip = 3)
tescil_temiz <- tescil_ham %>%
  select(Yil = 1, Tescil = 5) %>%
  mutate(
    Yil    = s(Yil),
    Tescil = s(Tescil)
  ) %>%
  filter(!is.na(Yil) & Yil >= 2014 & Yil <= 2024)

basvuru_ham_yerli <- read_excel("Patent_Yillik_Basvuru.xls", skip = 3)
basvuru_temiz_yerli <- basvuru_ham_yerli %>%
  select(Yil = 1, Basvuru = 5) %>%
  mutate(
    Yil     = s(Yil),
    Basvuru = s(Basvuru)
  ) %>%
  filter(!is.na(Yil) & Yil >= 2014 & Yil <= 2024)

burokrasi_data <- merge(basvuru_temiz_yerli, tescil_temiz, by = "Yil")

p_burokrasi <- ggplot(burokrasi_data, aes(x = Basvuru, y = Tescil)) +
  geom_point(size = 6, color = "#E67E22", alpha = 0.9) +
  geom_text_repel(aes(label = Yil), size = 5, fontface = "bold", color = "#2C3E50", box.padding = 0.8, min.segment.length = 0) +
  scale_x_continuous(labels = label_number(big.mark = "."), limits = c(0, 12000), breaks = seq(0, 12000, by = 2000)) +
  scale_y_continuous(labels = label_number(big.mark = "."), limits = c(0, 12000), breaks = seq(0, 12000, by = 2000)) +
  labs(
    title    = "Turkiye Inovasyon Ekosistemi: Burokrasi ve Onay Performansi",
    subtitle = "Yillik Patent Basvurulari (Talep) ile Tescil Edilenler (Onay) Arasindaki Korelasyon",
    x        = "Toplam Patent Basvuru Sayisi (Talep - Sistem Girisi)",
    y        = "Toplam Patent Tescil Sayisi (Onay - Sistem Cikisi)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 16, color = "#2C3E50"),
    plot.subtitle    = element_text(size = 12, color = "darkgrey"),
    axis.title       = element_text(face = "bold", size = 12, color = "#2C3E50"),
    axis.text        = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "gray90", fill = NA, linewidth = 1)
  )
print(p_burokrasi)

# ============================================================
# GRAFIK 4: Yerli Patent Basvurusu, Tescili ve Basari Orani
# ============================================================
raw_pb  <- read_excel("Patent_Yillik_Basvuru.xls", col_names = FALSE)
pat_bas <- data.frame(
  Yil     = si(raw_pb[[1]][4:nrow(raw_pb)]),
  Yerli   = s(raw_pb[[5]][4:nrow(raw_pb)]),
  Yabanci = s(raw_pb[[10]][4:nrow(raw_pb)])
) %>% filter(!is.na(Yil), !is.na(Yerli), Yil >= 2014, Yil <= 2024) %>%
  mutate(Toplam = Yerli + Yabanci)

raw_pt  <- read_excel("Patent_Yillik_Tescil.xls", col_names = FALSE)
pat_tes <- data.frame(
  Yil     = si(raw_pt[[1]][4:nrow(raw_pt)]),
  Yerli   = s(raw_pt[[5]][4:nrow(raw_pt)]),
  Yabanci = s(raw_pt[[10]][4:nrow(raw_pt)])
) %>% filter(!is.na(Yil), !is.na(Yerli), Yil >= 2014, Yil <= 2024) %>%
  mutate(Toplam = Yerli + Yabanci)

df1_lines <- bind_rows(
  pat_bas %>% select(Yil, Sayi = Yerli) %>% mutate(Tur = "Yerli Basvuru"),
  pat_tes %>% select(Yil, Sayi = Yerli) %>% mutate(Tur = "Yerli Tescil")
)
etiket1 <- df1_lines %>% filter(Yil %in% c(2014, 2024))

df1_oran <- data.frame(
  Yil  = pat_bas$Yil,
  Oran = round(pat_tes$Yerli / pat_bas$Yerli * 100, 1)
)

max_adet <- max(c(pat_bas$Yerli, pat_tes$Yerli), na.rm = TRUE)
max_oran <- max(df1_oran$Oran, na.rm = TRUE)
k1 <- max_adet / max_oran

g1 <- ggplot() +
  geom_col(data = df1_oran, aes(x = Yil, y = Oran * k1), fill = "grey70", alpha = 0.30, width = 0.7) +
  geom_text(data = df1_oran, aes(x = Yil, y = Oran * k1, label = paste0("%", Oran)), vjust = -0.4, size = 3.2, fontface = "bold", color = "grey50") +
  geom_line(data = df1_lines, aes(x = Yil, y = Sayi, color = Tur, group = Tur), linewidth = 2) +
  geom_point(data = df1_lines, aes(x = Yil, y = Sayi, color = Tur), size = 4, fill = "white", shape = 21, stroke = 2.5) +
  geom_label(data = etiket1, aes(x = Yil, y = Sayi, label = comma(Sayi), color = Tur), size = 3.5, fontface = "bold", show.legend = FALSE, label.padding = unit(0.25, "lines")) +
  scale_color_manual(values = c("Yerli Basvuru" = MAVI, "Yerli Tescil" = TURUNCU)) +
  scale_y_continuous(labels = comma, limits = c(0, NA), expand = expansion(mult = c(0.02, 0.15)), sec.axis = sec_axis(transform = ~ . / k1, name = "Basari Orani (%)", labels = function(x) paste0("%", round(x)))) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Yerli Patent Basvurusu, Tescili ve Basari Orani (2014-2024)",
    subtitle = "Bulgu: Basvurular artarken basari orani dusme egiliminde | Gri sutunlar: Basari orani (%)",
    x = NULL, y = "Adet", color = NULL) + tema
print(g1)

# ============================================================
# GRAFIK 5: IPC Isi Haritasi
# ============================================================
raw_ipc <- read_excel("Patent_IPC_Basvuru.xls", col_names = FALSE)
ipc_listesi <- c("A", "B", "C", "D", "E", "F", "G", "H")
bloklar <- list(
  list(yil_satir = 3,  veri_bas = 5),
  list(yil_satir = 16, veri_bas = 18),
  list(yil_satir = 29, veri_bas = 31),
  list(yil_satir = 42, veri_bas = 44)
)

ipc_rows <- list()
for (blok in bloklar) {
  yil_satiri <- blok$yil_satir
  veri_bas   <- blok$veri_bas
  for (col_offset in seq(2, 16, by = 2)) {
    yil_val <- suppressWarnings(as.integer(as.character(unlist(raw_ipc[yil_satiri, col_offset]))))
    if (is.na(yil_val) || yil_val < 2014 || yil_val > 2024) next
    for (i in seq_along(ipc_listesi)) {
      satir_no <- veri_bas + i - 1
      yerli_val <- suppressWarnings(as.numeric(as.character(unlist(raw_ipc[satir_no, col_offset]))))
      if (is.na(yerli_val)) yerli_val <- 0
      ipc_rows[[length(ipc_rows) + 1]] <- data.frame(IPC = ipc_listesi[i], Yil = yil_val, Sayi = yerli_val)
    }
  }
}
ipc_data <- bind_rows(ipc_rows) %>% arrange(IPC, Yil)

ipc_aciklama <- c(
  "A" = "A: Insan Ihtiyaclari", "B" = "B: Islem & Tasima", "C" = "C: Kimya & Metalurji", "D" = "D: Tekstil & Kagit",
  "E" = "E: Insaat & Madencilik", "F" = "F: Mekanik Muh.", "G" = "G: Fizik", "H" = "H: Elektrik"
)
ipc_data <- ipc_data %>% mutate(IPC_Label = ipc_aciklama[IPC])
medyan_val <- median(ipc_data$Sayi, na.rm = TRUE)

g4 <- ggplot(ipc_data, aes(x = factor(Yil), y = IPC_Label, fill = Sayi)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = comma(Sayi, big.mark = "."), color = ifelse(Sayi > medyan_val, "beyaz", "siyah")), size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("beyaz" = "white", "siyah" = "grey20")) +
  scale_fill_gradient(low = "#D0E4F3", high = "#08306B", name = "Basvuru\nSayisi", labels = comma_format(big.mark = ".")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title = "IPC Siniflarina Gore Yerli Patent Basvurularinin Yogunluk Haritasi (2014-2024)",
    subtitle = "Renk koyulastikca basvuru sayisi artar | Soluk mavi: az, Koyu mavi: cok",
    x = "Yil", y = "IPC Teknoloji Sinifi"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray30", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "right"
  )
print(g4)

# ============================================================
# GRAFIK 6: Patent Tescilinde En Fazla Artis Gosteren Ilk 10 Il
# ============================================================
raw_il <- read_excel("Patent_tescillerinin_illere_gore_dagilimi.xls", col_names = FALSE)
yv_il <- suppressWarnings(as.numeric(as.character(unlist(raw_il[3, ]))))

col_1995 <- which(yv_il == 1995)
col_2014 <- which(yv_il == 2014)
col_2024 <- which(yv_il == 2024)

il_col <- 2
tum_satir <- 5:nrow(raw_il)
il_adlari <- as.character(unlist(raw_il[tum_satir, il_col]))
veri_satirlari <- tum_satir[!is.na(il_adlari) & il_adlari != "" & trimws(toupper(il_adlari)) != "TOPLAM"]

cols_to_2014 <- col_1995:col_2014
cols_to_2024 <- col_1995:col_2024

kum_2014 <- apply(raw_il[veri_satirlari, cols_to_2014], 1, function(x) sum(s(x), na.rm = TRUE))
kum_2024 <- apply(raw_il[veri_satirlari, cols_to_2024], 1, function(x) sum(s(x), na.rm = TRUE))

il_isimleri <- trimws(as.character(unlist(raw_il[veri_satirlari, il_col])))

df7 <- data.frame(Il = il_isimleri, Kum_2014 = kum_2014, Kum_2024 = kum_2024) %>%
  filter(!is.na(Il), Il != "", Kum_2014 > 0) %>%
  mutate(Artis_Pct = round((Kum_2024 - Kum_2014) / Kum_2014 * 100, 1)) %>%
  arrange(desc(Artis_Pct)) %>%
  slice_head(n = 10) %>%
  mutate(Il = factor(Il, levels = rev(Il)))

g7 <- ggplot(df7, aes(x = Il, y = Artis_Pct)) +
  geom_col(width = 0.72, alpha = 0.85, fill = "#2471A3") +
  geom_text(aes(label = paste0("%", comma(Artis_Pct, accuracy = 0.1))), hjust = -0.1, size = 3.8, fontface = "bold", color = "grey15") +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0("%", comma(x)), expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Patent Tescilinde En Fazla Artis Gosteren Ilk 10 Il (2014-2024)",
    subtitle = "Kumulatif toplam: 2014'e kadar vs 2024'e kadar | Buyukten kucuge siralanmis",
    x = NULL, y = "Artis Orani (%)"
  ) +
  tema +
  theme(
    axis.text.y = element_text(size = 12, face = "bold", color = "grey15"),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )
print(g7)

cat("\nTum grafikler basariyla olusturuldu.\n")