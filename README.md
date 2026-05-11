# Türkiye İnovasyon Ekosistemi Veri Görselleştirme (2014-2024)

Bu proje, R programlama dili kullanılarak Türkiye'nin 2014-2024 yılları arasındaki inovasyon ekosistemini (patent başvuruları, faydalı model tescilleri, onay performansları vb.) analiz eden kapsamlı bir veri görselleştirme çalışmasıdır.

## Proje Hakkında
TÜRKPATENT verileri baz alınarak, ülkenin fikri mülkiyet, Ar-Ge ve inovasyon performansı farklı görselleştirme teknikleriyle incelenmiştir. Kodlar tamamen dinamik olarak çalışmakta olup, veri setlerini okuyup otomatik olarak temizler ve grafikleri çizer.

### İçerilen Grafikler:
1. **Toplam Üretim Hacmi:** Yıllara göre patent ve faydalı model başvurularının genel toplam kıyaslaması (Yan yana sütun grafiği).
2. **Uluslararası İlgi:** Türkiye'ye en çok patent başvurusu yapan ilk 3 yabancı ülke (Almanya, ABD, İtalya) dağılımı.
3. **Bürokrasi ve Onay Performansı:** Yıllık patent başvuruları (sistem girişi) ile tescil edilen patentler (sistem çıkışı) arasındaki korelasyon (Nokta dağılım grafiği).
4. **Yerli Başarı Oranı:** Yerli patent başvuru ve tescil adetlerinin yıllara göre oransal başarı eğilimi (Çift eksenli çizgi ve arka plan çubuk grafiği).
5. **IPC Yoğunluk Haritası:** Uluslararası Patent Sınıflandırmasına (A'dan H'ye teknoloji sınıfları) göre yerli patent başvurularının yoğunluğu (Isı haritası / Heatmap).
6. **İl Bazlı Yükseliş:** 2014-2024 arasında patent tescilinde kümülatif olarak en fazla yüzdesel artış gösteren ilk 10 il (Yatay sütun grafiği).

## Kurulum ve Kullanım
Projeyi kendi bilgisayarınızda (RStudio üzerinde) çalıştırmak için:
1. Bu depoyu (repository) bilgisayarınıza klonlayın veya indirin.
2. `TR_inovasyon_ekosistemi.R` dosyasını RStudio ile açın.
3. Kodların tamamını seçip çalıştırın (`Run`). 
4. Eksik olan paketler (`ggplot2`, `dplyr`, `tidyr`, `scales`, `readxl`, `ggrepel`) kodun başındaki otomatik kontrol mekanizması ile sisteminize kendi kendine kurulacaktır. Dışarıdan manuel bir dosya yolu girmenize gerek yoktur.

## Kaynak
TURKPATENT
https://www.turkpatent.gov.tr/patent-istatistik

## Hazırlayanlar

### Osman Çizmeci
- E-Posta: osmancizme7@gmail.com
- LinkedIn: https://www.linkedin.com/in/osman-cizmeci/
- GitHub: https://github.com/BATosi7

### Talayhan Tuğra Tokur
- E-Posta: talayhantokur@gmail.com
- LinkedIn: https://www.linkedin.com/in/talayhantugratokur/
- GitHub: https://github.com/thetokur

## Danışman:
- Doç.Dr. Volkan Soner ÖZSOY
