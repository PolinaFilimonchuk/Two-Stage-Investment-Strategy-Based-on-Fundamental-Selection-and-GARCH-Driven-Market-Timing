# Two-Stage-Investment-Strategy-Based-on-Fundamental-Selection-and-GARCH-Driven-Market-Timing
### Analiza statystyk opisowych

library(readxl)
library(tidyverse)
library(psych)  
library(moments) 

plik_excel <- "dane_PS_kwartalne_25_spolek.xlsx"
dane <- read_excel(plik_excel)
glimpse(dane)

dane$Data <- as.Date(dane$Data)
dane_numeryczne <- dane %>% select(-Data)

statystyki_opisowe <- describe(dane_numeryczne) %>% 
  as.data.frame() %>% 
  select(n, mean, sd, median, min, max, skew, kurtosis)

names(statystyki_opisowe) <- c("Liczba_obserwacji", "Srednia", "Odchylenie_std", "Mediana", "Min", "Max", "Skosnosc", "Kurtoza")

print(statystyki_opisowe)

dane_long <- dane %>% 
  pivot_longer(cols = -Data, names_to = "Spolka", values_to = "PS")

ggplot(dane_long, aes(x = reorder(Spolka, PS, FUN = median), y = PS, fill = Spolka)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  coord_flip() + 
  labs(
    title = "Rozkład wskaźnika Price-to-Sales (P/S) w latach 2016–2025",
    subtitle = "Analiza dla 25 spółek handlowych i konsumenckich w USA",
    x = "Spółka",
    y = "Wskaźnik P/S"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

### Sprawdzanie stacjonarności za pomocą rozszerzonego testu ADF


sprawdz_stacjonarnosc <- function(seria) {
  seria_czysta <- na.omit(seria)
  
  test_adf <- adf.test(seria_czysta, alternative = "stationary")
  tibble(
    Statystyka_ADF = test_adf$statistic,
    Lags = test_adf$parameter,
    P_Value = test_adf$p.value
  )
}
wyniki_adf <- dane %>% 
  select(-Data) %>% 
  map_df(~ sprawdz_stacjonarnosc(.x), .id = "Spolka")
print(wyniki_adf, n = 25)

stacjonarne <- wyniki_adf %>% filter(P_Value < 0.05)
cat("\nSpółki ze stacjonarnym wskaźnikiem P/S w poziomie (p < 0.05):\n")
print(stacjonarne$Spolka)

### Logarytmiczne stopy zwrotu 

dane_log_diff <- dane %>%
  mutate(across(-Data, ~ log(.x) - log(lag(.x)))) %>%
  drop_na() 

sprawdz_stacjonarnosc <- function(seria) {
  test_adf <- tseries::adf.test(seria, alternative = "stationary")
  tibble(
    Statystyka_ADF = test_adf$statistic,
    P_Value = test_adf$p.value
  )
}

wyniki_adf_diff <- dane_log_diff %>% 
  select(-Data) %>% 
  map_df(~ sprawdz_stacjonarnosc(.x), .id = "Spolka")

print(wyniki_adf_diff, n = 25)
stacjonarne_diff <- wyniki_adf_diff %>% filter(P_Value < 0.05)
cat("\nSpółki ze stacjonarnymi logarytmicznymi zmianami P/S (p < 0.05):\n")
print(stacjonarne_diff$Spolka)

####Druga różnica 

dane_log_diff2 <- dane %>%
  mutate(across(-Data, ~ c(NA, NA, diff(log(.x), differences = 2)))) %>%
  drop_na() 

sprawdz_stacjonarnosc <- function(seria) {
  test_adf <- tseries::adf.test(seria, alternative = "stationary")
  tibble(
    Statystyka_ADF = test_adf$statistic,
    P_Value = test_adf$p.value
  )
}


wyniki_adf_diff2 <- dane_log_diff2 %>% 
  select(-Data) %>% 
  map_df(~ sprawdz_stacjonarnosc(.x), .id = "Spolka")
print(wyniki_adf_diff2, n = 25)


nierozwiazane <- wyniki_adf %>% filter(P_Value < 0.05)
cat("\nSpółki ze stacjonarnym wskaźnikiem P/S w poziomie (p < 0.05):\n")
print(nierozwiazane$Spolka)

###Test ARCH-LM


library(FinTS) 


testuj_arch_stabilny <- function(seria) {
  tryCatch({
    test_wynik <- ArchTest(seria, lags = 1)
    
    tibble(
      Status = "OK",
      Statystyka_LM = test_wynik$parameter,
      P_Value_ARCH = test_wynik$p.value
    )
  }, error = function(e) {
    tibble(
      Status = "Błąd",
      Statystyka_LM = NA_real_,
      P_Value_ARCH = NA_real_
    )
  })
}


wyniki_arch_stabilne <- dane_log_diff %>% 
  select(-Data) %>% 
  map_df(~ testuj_arch_stabilny(.x), .id = "Spolka")

print(wyniki_arch_stabilne, n = 25)


istotne_arch <- wyniki_arch_stabilne %>% filter(P_Value_ARCH < 0.05)
cat("\nSpółki z istotnym efektem ARCH (p < 0.05):\n")
print(istotne_arch$Spolka)

### Opóźnienia 2 rzędu

testuj_arch_lags2 <- function(seria) {
  tryCatch({
    test_wynik <- ArchTest(seria, lags = 2)
    tibble(
      Status = "OK",
      Statystyka_LM = test_wynik$parameter,
      P_Value_ARCH = test_wynik$p.value
    )
  }, error = function(e) {
    tibble(
      Status = "Błąd",
      Statystyka_LM = NA_real_,
      P_Value_ARCH = NA_real_
    )
  })
}

wyniki_arch_lags2 <- dane_log_diff %>% 
  select(-Data) %>% 
  map_df(~ testuj_arch_lags2(.x), .id = "Spolka")

print(wyniki_arch_lags2, n = 25)

istotne_arch2 <- wyniki_arch_lags2 %>% filter(P_Value_ARCH < 0.05)
cat("\nSpółki z istotnym efektem ARCH przy lags = 2 (p < 0.05):\n")
print(istotne_arch2$Spolka)

### Estymacja modelu GARCH(1,1)

library(fGarch)

model_garch_bkng <- garchFit(~garch(1, 1), data = dane_log_diff$BKNG, cond.dist = "norm", trace = TRUE)
summary(model_garch_bkng)


plot(model_garch_bkng, which = 3)
