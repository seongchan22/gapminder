# clean.R
# Gapminder 데이터(data/gapminder.csv) 품질 확인 스크립트
# 실행: Rscript clean.R

# ---- 0. 설정 ----
input_path <- "data/gapminder.csv"

cat("======================================================\n")
cat(" Gapminder 데이터 품질 점검 (clean.R)\n")
cat("======================================================\n\n")

# ---- 1. 로드 ----
df <- read.csv(input_path, stringsAsFactors = FALSE)

cat("[1] 기본 정보\n")
cat(sprintf("  - 파일 경로 : %s\n", input_path))
cat(sprintf("  - 행 수     : %d\n", nrow(df)))
cat(sprintf("  - 열 수     : %d\n", ncol(df)))
cat(sprintf("  - 컬럼      : %s\n\n", paste(names(df), collapse = ", ")))

# ---- 2. 컬럼별 자료형 ----
cat("[2] 컬럼별 자료형\n")
for (col in names(df)) {
  cat(sprintf("  - %-10s : %s\n", col, class(df[[col]])))
}
cat("\n")

# ---- 3. 결측치(NA) ----
cat("[3] 결측치(NA) 개수\n")
na_counts <- sapply(df, function(x) sum(is.na(x)))
for (col in names(na_counts)) {
  cat(sprintf("  - %-10s : %d\n", col, na_counts[col]))
}
cat(sprintf("  => 총 결측치: %d\n\n", sum(na_counts)))

# ---- 4. 중복 행 ----
cat("[4] 중복 점검\n")
dup_rows <- sum(duplicated(df))
cat(sprintf("  - 완전 중복 행            : %d\n", dup_rows))
# (country, year) 조합 중복 -> 키 무결성
key_dup <- sum(duplicated(df[, c("country", "year")]))
cat(sprintf("  - (country, year) 중복 키 : %d\n\n", key_dup))

# ---- 5. 수치형 컬럼 범위/요약 ----
cat("[5] 수치형 컬럼 요약\n")
num_cols <- names(df)[sapply(df, is.numeric)]
for (col in num_cols) {
  v <- df[[col]]
  cat(sprintf("  - %s\n", col))
  cat(sprintf("      min=%.4g  median=%.4g  mean=%.4g  max=%.4g\n",
              min(v, na.rm = TRUE), median(v, na.rm = TRUE),
              mean(v, na.rm = TRUE), max(v, na.rm = TRUE)))
}
cat("\n")

# ---- 6. 값 유효성(도메인 규칙) ----
cat("[6] 값 유효성 점검\n")
checks <- list(
  "pop <= 0"            = if ("pop" %in% names(df)) sum(df$pop <= 0, na.rm = TRUE) else NA,
  "lifeExp < 0 또는 >120" = if ("lifeExp" %in% names(df)) sum(df$lifeExp < 0 | df$lifeExp > 120, na.rm = TRUE) else NA,
  "gdpPercap <= 0"      = if ("gdpPercap" %in% names(df)) sum(df$gdpPercap <= 0, na.rm = TRUE) else NA,
  "year 범위 밖(1700~2100)" = if ("year" %in% names(df)) sum(df$year < 1700 | df$year > 2100, na.rm = TRUE) else NA
)
for (nm in names(checks)) {
  cat(sprintf("  - %-22s : %s\n", nm, ifelse(is.na(checks[[nm]]), "컬럼없음", checks[[nm]])))
}
cat("\n")

# ---- 7. 범주형 컬럼 점검 ----
cat("[7] 범주형 컬럼\n")
if ("continent" %in% names(df)) {
  cat("  - continent 분포:\n")
  print(table(df$continent))
}
if ("country" %in% names(df)) {
  cat(sprintf("  - 고유 country 수 : %d\n", length(unique(df$country))))
}
if ("year" %in% names(df)) {
  cat(sprintf("  - 연도 범위       : %d ~ %d (고유 %d개)\n",
              min(df$year), max(df$year), length(unique(df$year))))
}
cat("\n")

# ---- 8. 공백/이상 문자열 ----
cat("[8] 문자열 컬럼 공백 점검\n")
chr_cols <- names(df)[sapply(df, is.character)]
for (col in chr_cols) {
  trimmed_diff <- sum(df[[col]] != trimws(df[[col]]), na.rm = TRUE)
  empty <- sum(trimws(df[[col]]) == "", na.rm = TRUE)
  cat(sprintf("  - %-10s : 앞뒤공백 %d개, 빈문자열 %d개\n", col, trimmed_diff, empty))
}
cat("\n")

# ---- 9. 패널 균형성(국가별 관측 연도 수) ----
if (all(c("country", "year") %in% names(df))) {
  cat("[9] 패널 균형성 (국가별 관측 수)\n")
  per_country <- table(df$country)
  expected <- length(unique(df$year))
  unbalanced <- names(per_country)[per_country != expected]
  cat(sprintf("  - 연도 수(기대값) : %d\n", expected))
  cat(sprintf("  - 불균형 국가 수  : %d\n", length(unbalanced)))
  if (length(unbalanced) > 0) {
    cat(sprintf("    %s\n", paste(unbalanced, collapse = ", ")))
  } else {
    cat("    => 모든 국가가 동일한 연도 수를 가짐 (균형 패널)\n")
  }
  cat("\n")
}

# ---- 10. 종합 판정 ----
cat("[10] 종합 판정\n")
issues <- 0
if (sum(na_counts) > 0) { cat("  ! 결측치 존재\n"); issues <- issues + 1 }
if (dup_rows > 0)       { cat("  ! 완전 중복 행 존재\n"); issues <- issues + 1 }
if (key_dup > 0)        { cat("  ! (country, year) 중복 키 존재\n"); issues <- issues + 1 }
if (!is.na(checks[["pop <= 0"]]) && checks[["pop <= 0"]] > 0) { cat("  ! 비정상 pop 값\n"); issues <- issues + 1 }
if (!is.na(checks[["gdpPercap <= 0"]]) && checks[["gdpPercap <= 0"]] > 0) { cat("  ! 비정상 gdpPercap 값\n"); issues <- issues + 1 }
if (issues == 0) {
  cat("  => 발견된 품질 이슈 없음. 데이터 양호.\n")
} else {
  cat(sprintf("  => 총 %d개 유형의 품질 이슈 발견.\n", issues))
}
cat("\n완료.\n")
