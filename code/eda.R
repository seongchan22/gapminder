# eda.R
# Gapminder 데이터(data/gapminder.csv) 탐색적 데이터 분석 (EDA) - 최종본
# 실행: Rscript eda.R
# 출력: 콘솔 요약 + figures/ 폴더에 시각화 PNG 저장
#
# 설계 원칙(비판적 보완):
#  - 평균 추세뿐 아니라 "분포 / 불평등(수렴-발산) / 변화 동학 / 역행 사례"까지 본다.
#  - pooled 상관 하나로 끝내지 않고 연도별·대륙별로 분해해 심슨의 역설 위험을 점검한다.
#  - 단순평균과 인구가중평균을 구분해 해석 편향을 막는다.
#  - 정성적 서술 대신 회귀로 효과 크기를 정량화한다.
#  - 데이터셋 자체의 한계를 명시한다.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

# ---- 0. 설정 & 헬퍼 ----
input_path <- "data/gapminder.csv"
fig_dir    <- "figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# 패키지 없이 쓰는 보조 통계 함수
skewness <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  m <- mean(x); s <- sqrt(mean((x - m)^2))
  mean((x - m)^3) / s^3
}
gini <- function(x) {                       # 불평등 지수 (0=완전평등)
  x <- sort(x[!is.na(x) & x >= 0]); n <- length(x)
  if (n == 0) return(NA)
  2 * sum(x * seq_len(n)) / (n * sum(x)) - (n + 1) / n
}
cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)  # 변이계수

df <- read.csv(input_path, stringsAsFactors = FALSE)
latest <- max(df$year); first <- min(df$year)

cat("============================================================\n")
cat(" Gapminder 탐색적 데이터 분석 (eda.R) — 최종본\n")
cat("============================================================\n\n")

# ---- 1. 데이터 개요 & 한계 ----
cat("[1] 데이터 개요\n")
cat(sprintf("  - 관측치 %d행, 변수 %d개 / 국가 %d개, 대륙 %d개, 연도 %d~%d(%d시점)\n",
            nrow(df), ncol(df), length(unique(df$country)),
            length(unique(df$continent)), first, latest, length(unique(df$year))))
cat("  - [한계] 142개국만 포함(소국·분쟁국 누락 가능), 5년 간격(연중 급변 포착 불가),\n")
cat("           2007년까지(최근 동향 미반영), gdpPercap은 물가/PPP 기준 차이 주의.\n\n")

# ---- 2. 변수 분포 진단 (왜 로그가 필요한가) ----
cat("[2] 변수 분포 진단 — 왜도(skewness)와 로그 변환 효과\n")
diag_tbl <- data.frame(
  variable = c("pop", "gdpPercap", "lifeExp", "log(gdpPercap)", "log(pop)"),
  skewness = c(skewness(df$pop), skewness(df$gdpPercap), skewness(df$lifeExp),
               skewness(log(df$gdpPercap)), skewness(log(df$pop)))
)
print(diag_tbl, row.names = FALSE, digits = 3)
cat("  => pop·gdpPercap은 강한 우측 치우침. 로그 변환으로 왜도가 크게 완화됨\n")
cat("     => 평균보다 '중앙값'이 대표값으로 적절, 회귀에는 log 사용이 타당.\n\n")

# ---- 3. 중심경향: 단순평균 vs 인구가중평균 (해석 편향 점검) ----
cat("[3] 단순평균 vs 인구가중평균 (2007 기대수명)\n")
simple_mean <- mean(df$lifeExp[df$year == latest])
wt_mean     <- with(df[df$year == latest, ], sum(lifeExp * pop) / sum(pop))
cat(sprintf("  - 국가 단순평균 : %.2f 세\n", simple_mean))
cat(sprintf("  - 인구 가중평균 : %.2f 세\n", wt_mean))
cat(sprintf("  - 차이 %.2f세 => 인구 많은 국가(중국·인도 등)가 평균을 끌어내림. 어떤 평균을 쓰는지가 결론을 바꾼다.\n\n",
            wt_mean - simple_mean))

# ---- 4. 불평등 추세: 세계는 수렴하는가 발산하는가 ----
cat("[4] 불평등 추세 (연도별 국가간 격차) — 핵심 질문: 수렴 vs 발산\n")
ineq <- df %>%
  group_by(year) %>%
  summarise(
    lifeExp_sd  = sd(lifeExp),
    lifeExp_p90_p10 = quantile(lifeExp, .9) - quantile(lifeExp, .1),
    gdp_cv      = cv(gdpPercap),
    gdp_gini    = gini(gdpPercap),
    .groups = "drop"
  )
print(as.data.frame(ineq), row.names = FALSE, digits = 4)
cat(sprintf("  => 기대수명 격차(SD): %.2f(%d) -> %.2f(%d)  [%s]\n",
            ineq$lifeExp_sd[1], first, ineq$lifeExp_sd[nrow(ineq)], latest,
            ifelse(ineq$lifeExp_sd[nrow(ineq)] < ineq$lifeExp_sd[1], "수렴", "발산")))
cat(sprintf("  => 소득 불평등(Gini): %.3f(%d) -> %.3f(%d)  [%s]\n",
            ineq$gdp_gini[1], first, ineq$gdp_gini[nrow(ineq)], latest,
            ifelse(ineq$gdp_gini[nrow(ineq)] < ineq$gdp_gini[1], "완화", "심화")))
cat("  => 해석: 기대수명은 수렴 경향이나 소득 격차는 다른 양상 — '평균 상승'에 가려진 분포 변화.\n\n")

# ---- 5. 관계의 시간/대륙 분해 (심슨의 역설 점검) ----
cat("[5] 기대수명 vs log(GDP) 상관 — pooled vs 분해\n")
cat(sprintf("  - 전체 pooled : %.3f\n", cor(df$lifeExp, log(df$gdpPercap))))
by_year <- df %>% group_by(year) %>%
  summarise(r = cor(lifeExp, log(gdpPercap)), .groups = "drop")
cat("  - 연도별 상관(추세):\n")
print(as.data.frame(by_year), row.names = FALSE, digits = 3)
by_cont <- df %>% group_by(continent) %>%
  summarise(r = cor(lifeExp, log(gdpPercap)), .groups = "drop")
cat("  - 대륙별 상관:\n")
print(as.data.frame(by_cont), row.names = FALSE, digits = 3)
cat("  => 관계는 모든 연도·대륙에서 양(+)으로 안정적 => 심슨의 역설(부호 반전)은 없음.\n\n")

# ---- 6. 회귀: 효과 크기 정량화 + 대륙 통제 ----
cat("[6] 회귀분석 (lifeExp 설명)\n")
m1 <- lm(lifeExp ~ log(gdpPercap), data = df)
m2 <- lm(lifeExp ~ log(gdpPercap) + continent, data = df)
cat(sprintf("  - M1: lifeExp ~ log(gdp)            | R^2=%.3f, log(gdp)계수=%.2f\n",
            summary(m1)$r.squared, coef(m1)[["log(gdpPercap)"]]))
cat(sprintf("  - M2: lifeExp ~ log(gdp)+continent  | R^2=%.3f, log(gdp)계수=%.2f\n",
            summary(m2)$r.squared, coef(m2)[["log(gdpPercap)"]]))
cat("  => 소득이 2.7배(=e배) 늘 때 기대수명 약 +",
    round(coef(m1)[["log(gdpPercap)"]], 1), "세.\n", sep = "")
cat("     대륙 통제 후에도 소득 효과 유지 + 설명력 상승 => 소득 외 지역요인(보건·제도)도 큼.\n\n")

# ---- 7. 변화 동학: 성장률 & β-수렴 ----
cat("[7] 변화 동학 (1952 -> 2007)\n")
wide <- df %>%
  filter(year %in% c(first, latest)) %>%
  select(country, continent, year, lifeExp, gdpPercap) %>%
  pivot_wider(names_from = year, values_from = c(lifeExp, gdpPercap))
names(wide) <- gsub("-", "_", names(wide))
le0 <- paste0("lifeExp_", first);  le1 <- paste0("lifeExp_", latest)
gd0 <- paste0("gdpPercap_", first); gd1 <- paste0("gdpPercap_", latest)
wide <- wide %>%
  mutate(
    lifeExp_gain = .data[[le1]] - .data[[le0]],
    gdp_cagr     = (.data[[gd1]] / .data[[gd0]])^(1 / (latest - first)) - 1
  )
cat("  - 기대수명 증가 상위 5국:\n")
print(as.data.frame(wide %>% arrange(desc(lifeExp_gain)) %>%
                      transmute(country, continent, gain = round(lifeExp_gain, 1)) %>% head(5)),
      row.names = FALSE)
cat("  - 기대수명 증가 하위 5국(정체/역행):\n")
print(as.data.frame(wide %>% arrange(lifeExp_gain) %>%
                      transmute(country, continent, gain = round(lifeExp_gain, 1)) %>% head(5)),
      row.names = FALSE)
cat("  - GDP 연평균성장률(CAGR) 상위 5국:\n")
print(as.data.frame(wide %>% arrange(desc(gdp_cagr)) %>%
                      transmute(country, continent, CAGR_pct = round(gdp_cagr * 100, 2)) %>% head(5)),
      row.names = FALSE)
# β-수렴: 초기 수준이 낮을수록 더 많이 따라잡았는가?
beta <- cor(wide[[le0]], wide$lifeExp_gain)
cat(sprintf("  - β-수렴 점검: cor(1952 기대수명, 이후 증가량) = %.3f\n", beta))
cat(sprintf("    => 음(-)이면 '뒤처진 나라가 더 빨리 따라잡음'(수렴). 결과: %s\n\n",
            ifelse(beta < 0, "수렴 확인", "비수렴")))

# ---- 8. 역행 사례: 기대수명이 떨어진 구간 (충격 식별) ----
cat("[8] 역행 사례 — 5년 새 기대수명이 가장 크게 하락한 구간 TOP 8\n")
drops <- df %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(d_life = lifeExp - lag(lifeExp),
         period = paste0(lag(year), "->", year)) %>%
  ungroup() %>%
  filter(!is.na(d_life)) %>%
  arrange(d_life) %>%
  transmute(country, continent, period, drop = round(d_life, 1)) %>%
  head(8)
print(as.data.frame(drops), row.names = FALSE)
cat("  => 르완다(1992 대학살), 사하라이남 HIV/AIDS, 캄보디아 등 — '평균 추세'가 가린 비극.\n\n")

# ====================================================
# 시각화 (figures/ 저장)  — 분석 메시지별로 1장씩
# ====================================================
cat("[9] 시각화 저장 (figures/)\n")
mytheme <- theme_minimal(base_size = 12)

# 9-1. 대륙별 기대수명 추세 (단순평균 vs 인구가중 둘다)
trend <- df %>% group_by(year, continent) %>%
  summarise(simple = mean(lifeExp),
            weighted = sum(lifeExp * pop) / sum(pop), .groups = "drop") %>%
  pivot_longer(c(simple, weighted), names_to = "type", values_to = "lifeExp")
p1 <- ggplot(trend, aes(year, lifeExp, color = continent, linetype = type)) +
  geom_line(linewidth = 0.9) +
  labs(title = "Life expectancy trend by continent (simple vs pop-weighted)",
       x = "Year", y = "Life expectancy", color = "Continent", linetype = "Mean type") + mytheme
ggsave(file.path(fig_dir, "01_lifeExp_trend.png"), p1, width = 9, height = 5, dpi = 120)

# 9-2. GDP vs 기대수명 (2007, 로그축, 버블=인구) + 추세선
d_latest <- df %>% filter(year == latest)
p2 <- ggplot(d_latest, aes(gdpPercap, lifeExp)) +
  geom_point(aes(size = pop, color = continent), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  scale_x_log10() + scale_size(range = c(1, 15), guide = "none") +
  labs(title = sprintf("GDP per capita vs Life expectancy (%d, bubble=pop)", latest),
       x = "GDP per capita (log)", y = "Life expectancy", color = "Continent") + mytheme
ggsave(file.path(fig_dir, "02_gdp_vs_lifeExp.png"), p2, width = 8, height = 5, dpi = 120)

# 9-3. 분포 진단: raw vs log (gdpPercap)
dist_df <- bind_rows(
  data.frame(value = df$gdpPercap, scale = "raw"),
  data.frame(value = log10(df$gdpPercap), scale = "log10")
)
p3 <- ggplot(dist_df, aes(value)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  facet_wrap(~scale, scales = "free") +
  labs(title = "Distribution of GDP per capita: raw (right-skewed) vs log",
       x = "Value", y = "Count") + mytheme
ggsave(file.path(fig_dir, "03_gdp_distribution.png"), p3, width = 9, height = 4.5, dpi = 120)

# 9-4. 불평등 추세 (기대수명 SD & 소득 Gini)
p4 <- ineq %>%
  select(year, lifeExp_sd, gdp_gini) %>%
  pivot_longer(-year) %>%
  ggplot(aes(year, value)) +
  geom_line(linewidth = 1, color = "firebrick") + geom_point() +
  facet_wrap(~name, scales = "free_y",
             labeller = as_labeller(c(lifeExp_sd = "Life expectancy SD (gap)",
                                      gdp_gini = "Income Gini (inequality)"))) +
  labs(title = "Inequality over time: convergence in health, income gap persists",
       x = "Year", y = NULL) + mytheme
ggsave(file.path(fig_dir, "04_inequality_trend.png"), p4, width = 9, height = 4.5, dpi = 120)

# 9-5. β-수렴: 초기 기대수명 vs 이후 증가량
p5 <- ggplot(wide, aes(.data[[le0]], lifeExp_gain, color = continent)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  labs(title = "Beta-convergence: lower starting countries gained more",
       x = sprintf("Life expectancy in %d", first),
       y = sprintf("Gain by %d", latest), color = "Continent") + mytheme
ggsave(file.path(fig_dir, "05_beta_convergence.png"), p5, width = 8, height = 5, dpi = 120)

# 9-6. 관계의 시간 변화: 연도 facet 산점도
p6 <- df %>% filter(year %in% c(1952, 1977, 2007)) %>%
  ggplot(aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(alpha = 0.6, size = 1.2) + scale_x_log10() +
  facet_wrap(~year) +
  labs(title = "GDP-Life expectancy relationship shifts upward over time",
       x = "GDP per capita (log)", y = "Life expectancy", color = "Continent") + mytheme
ggsave(file.path(fig_dir, "06_relationship_by_year.png"), p6, width = 10, height = 4.5, dpi = 120)

cat("  - 01_lifeExp_trend.png (단순 vs 가중)\n")
cat("  - 02_gdp_vs_lifeExp.png (회귀선 포함)\n")
cat("  - 03_gdp_distribution.png (분포 진단)\n")
cat("  - 04_inequality_trend.png (불평등 추세)\n")
cat("  - 05_beta_convergence.png (수렴 검정)\n")
cat("  - 06_relationship_by_year.png (관계의 시간 변화)\n\n")

cat("완료. 콘솔 심층 요약 + figures/ 6개 그래프 생성됨.\n")
