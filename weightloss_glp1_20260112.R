library(pacman)
pacman::p_load(gridExtra, cobalt, table1, FSA, purrr, nortest, showtext, stringr, scales, forcats, plotly, patchwork, tidyverse, googleVis, magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot)


# ========================
# input data                    
# ========================
setwd("/Users/clover_teoh/Documents/cofit/GLP_1_成效分析/glp1_compare_update_20260114")
getwd()

glp1_data <- read.csv("glp_1_users_20260108.csv")
raw_data <- read_excel("final_analysis_ready_2019_20260108.xlsx")
paper_data <- read_excel("paper_GLP_1_dataset_20251203.xlsx", sheet = "Sheet1")


# ============================================================================
# data cleaning
# ============================================================================

# 抓到有5個人的client_id, started_at, adjusted_end一樣的人
raw_data %>%
  filter(client_id %in% c("379029","381744","736733","746478","754442")) %>%
  group_by(client_id, started_at, adjusted_end) %>%
  summarise(
    n = n(),
    across(everything(), n_distinct, .names = "n_distinct_{.col}"),
    all_unique = all(across(everything(), n_distinct) == 1),
    .groups = "drop"
  )

nrow(raw_data)

raw_data_clean <- raw_data %>%
  # 為所有記錄按 client_id, started_at, adjusted_end 分組
  group_by(client_id, started_at, adjusted_end) %>%
  # 對於這 5 個特定的 client_id，如果有重複就只保留第一筆
  filter(
    !(client_id %in% c("379029", "381744", "736733", "746478", "754442") & 
        n() > 1 & 
        row_number() > 1)
  ) %>%
  ungroup()

nrow(raw_data_clean) #應該要是raw_data -5


# ============================================================================
# 目的：為每位客戶的治療記錄按時間順序編號（for 初療和續療分組）
# ============================================================================

#genesis_data <- raw_data_clean %>%
  # 按 client_id 分組
  group_by(client_id) %>%
  # 在每個組內按 started_at 排序（升序）
  arrange(started_at, .by_group = TRUE) %>%
  # 創建 treatment 欄位，按順序編號（1, 2, 3, ...）
  mutate(treatment = row_number()) %>%
  # 取消分組
  ungroup()


genesis_data <- raw_data_clean %>%
group_by(client_id) %>%
  arrange(started_at, .by_group = TRUE) %>%
  mutate(
    days_gap = as.numeric(difftime(started_at, lag(adjusted_end), units = "days"))
  ) %>%
  group_modify(~ {
    # 初始化 treatment 欄位
    .x$treatment <- NA_integer_
    
    # 第一筆設為 1
    if (nrow(.x) > 0) {
      .x$treatment[1] <- 1L
      
      # 從第二筆開始判斷
      if (nrow(.x) > 1) {
        for (i in 2:nrow(.x)) {
          # 關鍵：只有當「前一筆有 treatment」且「間隔 ≤ 60 天」時，才繼續編號
          if (!is.na(.x$treatment[i-1]) && 
              !is.na(.x$days_gap[i]) && 
              .x$days_gap[i] <= 60) {
            .x$treatment[i] <- .x$treatment[i-1] + 1L
          }
          # 否則 treatment 保持 NA
        }
      }
    }
    .x
  }) %>%
  select(-days_gap) %>%
  ungroup()

# 檢視結果
genesis_data %>% count(treatment)


# 檢查是否有缺失的治療序號(treatment)
genesis_data %>%
  group_by(client_id) %>%
  summarise(
    has_gap = !all(diff(treatment) == 1)
  ) %>%
  filter(has_gap == TRUE)


# 計算年齡
## 改錯的生日
genesis_data <- genesis_data %>%
  mutate(
    birthday = case_when(
      client_id == "681404" ~ ymd("1958-03-26"),
      client_id == "659338" ~ ymd("1971-07-07"),
      TRUE ~ as_date(birthday)
    )
  )


genesis_data <- genesis_data %>%
  mutate(birthday = if_else(birthday == as.Date("1970-01-01"), 
                            as.Date(NA), 
                            birthday))


genesis_data <- genesis_data %>%
  mutate(
    age = floor(time_length(interval(ymd(birthday), ymd(started_at)), "years"))
  )


# ============================================================================
# 先cleaning GLP-1資料
# ============================================================================

# 先建立藥物分類（整理 english_name→ drug_group）
glp1_data <- glp1_data %>%
  rename(
    dispense_date = `出庫時間`,
    client_id     = `客戶ID`
  )

glp1_data <- glp1_data %>%
  filter(退藥註記 == "false") %>% 
  filter(發藥量 > 0)

glp1_data <- glp1_data %>%
  mutate(
    drug_group = case_when(
      str_detect(english_name, "Wegovy")   ~ "Wegovy",
      str_detect(english_name, "Mounjaro") ~ "Mounjaro",
      str_detect(english_name, "Ozempic")  ~ "Ozempic",
      str_detect(english_name, "Rybelsus") ~ "Rybelsus",
      str_detect(english_name, "Saxenda")  ~ "Saxenda",
      TRUE ~ NA_character_
    )
  )




# 若日期欄位不是 Date 格式，記得先轉換
glp1_data$dispense_date <- as.Date(glp1_data$dispense_date)

genesis_data$started_at   <- as.Date(genesis_data$started_at)
genesis_data$adjusted_end <- as.Date(genesis_data$adjusted_end)


library(data.table)

# 轉成 data.table
glp1_data <- glp1_data %>% 
  mutate(date_start = dispense_date,
         date_end   = dispense_date
  )

genesis_data <- genesis_data %>%
  mutate(
    started_at_expanded = started_at - days(14),
    adjusted_end_shortened = adjusted_end - days(14)
  )

genesis_data$started_at_expanded   <- as.Date(genesis_data$started_at_expanded)
genesis_data$adjusted_end_shortened   <- as.Date(genesis_data$adjusted_end_shortened)


g1  <- as.data.table(glp1_data)
gen <- as.data.table(genesis_data)

# 確保兩邊的 client_id 類型一致
g1[, client_id := as.character(client_id)]
gen[, client_id := as.character(client_id)]

# interval join：glp1 出庫日期在 genesis 的期間內
setkey(gen, client_id, started_at_expanded, adjusted_end_shortened)


merged <- foverlaps(
  x = g1[, .(client_id, date_start, date_end, drug_group)],
  y = gen,
  by.x = c("client_id", "date_start", "date_end"),
  by.y = c("client_id", "started_at_expanded", "adjusted_end_shortened"),
  type = "within",
  nomatch = 0
)


drug_wide <- merged %>%
  distinct(client_id, started_at_expanded, adjusted_end_shortened, drug_group) %>%
  mutate(flag = 1) %>%
  pivot_wider(
    id_cols = c(client_id, started_at_expanded, adjusted_end_shortened),
    names_from = drug_group,
    values_from = flag,
    values_fill = 0
  )


genesis_data <- genesis_data %>%
  left_join(drug_wide,
            by = c("client_id", "started_at_expanded", "adjusted_end_shortened")) %>%
  mutate(across(c(Wegovy, Mounjaro, Ozempic, Rybelsus, Saxenda),
                ~ replace_na(., 0)))


genesis_data %>% count(client_id, started_at, adjusted_end) %>% filter(n>1)


genesis_data <- genesis_data %>%
  mutate(
    across(c(Wegovy, Mounjaro, Ozempic, Rybelsus, Saxenda),
           ~ as.numeric(. == 1))  # TRUE→1, FALSE/NA→0
  )


genesis_data <- genesis_data %>%
  mutate(
    glp1_sum = rowSums(across(c(Wegovy, Mounjaro, Ozempic, Rybelsus, Saxenda)), na.rm = TRUE)
  )


genesis_data %>% count(glp1_sum)




# --------- for excel 上的比較更新 -----------
# 分出有用glp-1和沒用的人
genesis_data <- genesis_data %>%
  mutate(
    glp1_gp = case_when(
      glp1_sum >= 1 ~ "GLP-1_user",
      glp1_sum == 0 ~ "non_GLP-1_user",
      TRUE ~ NA)
  )





# 抓出有完整3次療程的人 (2019/09/01-2026/01/08)
# 全部人不管有沒有用過GLP-1
# -------------------------------------------------
genesis_data_3t <- genesis_data %>%
  filter(!is.na(treatment)) %>% 
  group_by(client_id) %>%
  filter(all(c(1, 2, 3) %in% treatment)) %>%
  ungroup() 

n_distinct(genesis_data_3t$client_id)

# 抓出w0，w8，w16，w24，計算weight loss
genesis_data_3t_wide <- genesis_data_3t %>%
  group_by(client_id) %>%
  summarise(
    weight_w0 = weight_baseline[treatment == 1],
    weight_w8 = weight_endpoint[treatment == 1],
    weight_w16 = weight_endpoint[treatment == 2],
    weight_w24 = weight_endpoint[treatment == 3],
    body_fat_w0 = body_fat_mass_baseline[treatment == 1],
    body_fat_w8 = body_fat_mass_endpoint[treatment == 1],
    body_fat_w16 = body_fat_mass_endpoint[treatment == 2],
    body_fat_w24 = body_fat_mass_endpoint[treatment == 3],
    body_fat_mass_percentage_w0 = body_fat_mass_percentage_baseline[treatment == 1],
    body_fat_mass_percentage_w8 = body_fat_mass_percentage_endpoint[treatment == 1],
    body_fat_mass_percentage_w16 = body_fat_mass_percentage_endpoint[treatment == 2],
    body_fat_mass_percentage_w24 = body_fat_mass_percentage_endpoint[treatment == 3],
    muscle_mass_w0 = muscle_mass_baseline[treatment == 1],
    muscle_mass_w8 = muscle_mass_endpoint[treatment == 1],
    muscle_mass_w16 = muscle_mass_endpoint[treatment == 2],
    muscle_mass_w24 = muscle_mass_endpoint[treatment == 3],
    bmi_w0 = bmi_baseline[treatment == 1]
  ) %>%
  ungroup() %>%
  filter(complete.cases(weight_w0, weight_w8, weight_w16, weight_w24))

n_distinct(genesis_data_3t_wide$client_id) #體重沒有缺的人 n=322

# 只保留BMI>=24的人
genesis_data_3t_wide <- genesis_data_3t_wide %>% 
  filter(bmi_w0 >= 24)

n_distinct(genesis_data_3t_wide$client_id) #排除BMI normal的人 n=261 (排除61人)


genesis_data_3t_wide <- genesis_data_3t_wide %>%
  mutate(
    # Weight change (後減前)
    wl_w8 = weight_w8 - weight_w0,
    wl_w16 = weight_w16 - weight_w0,
    wl_w24 = weight_w24 - weight_w0,
    
    # Weight change percentage
    wl_pct_w8 = (weight_w8 - weight_w0) / weight_w0 * 100,
    wl_pct_w16 = (weight_w16 - weight_w0) / weight_w0 * 100,
    wl_pct_w24 = (weight_w24 - weight_w0) / weight_w0 * 100,
    
    # fat change
    fl_w8 = body_fat_w8 - body_fat_w0,
    fl_w16 = body_fat_w16 - body_fat_w0,
    fl_w24 = body_fat_w24 - body_fat_w0,
    
    # fat change percentage
    fl_pct_w8 = (body_fat_w8 - body_fat_w0) / body_fat_w0 * 100,
    fl_pct_w16 = (body_fat_w16 - body_fat_w0) / body_fat_w0 * 100,
    fl_pct_w24 = (body_fat_w24 - body_fat_w0) / body_fat_w0 * 100,
    
    # fat pct change (體脂率變化，不是百分比變化)
    fpc_w8 = body_fat_mass_percentage_w8 - body_fat_mass_percentage_w0,
    fpc_w16 = body_fat_mass_percentage_w16 - body_fat_mass_percentage_w0,
    fpc_w24 = body_fat_mass_percentage_w24 - body_fat_mass_percentage_w0,
    
    # muscle change
    ml_w8 = muscle_mass_w8 - muscle_mass_w0,
    ml_w16 = muscle_mass_w16 - muscle_mass_w0,
    ml_w24 = muscle_mass_w24 - muscle_mass_w0)


summary(genesis_data_3t_wide[, c("wl_w8", "wl_w16", "wl_w24", 
"wl_pct_w8", "wl_pct_w16", "wl_pct_w24",
"fl_w8", "fl_w16", "fl_w24", 
"fl_pct_w8", "fl_pct_w16", "fl_pct_w24",
"fpc_w8", "fpc_w16", "fpc_w24",
"ml_w8", "ml_w16", "ml_w24",
"fmlf_w8", "fmlf_w16", "fmlf_w24",
"lmlf_w8", "lmlf_w16", "lmlf_w24")])
 



# 方法 3: 使用 summarise 一次計算多個
mean_results <- genesis_data_3t_wide %>%
  summarise(
    n_w8 = sum(wl_w8 <= -0.5, na.rm = TRUE),
    mean_wl_w8 = mean(wl_w8[wl_w8 <= -0.5], na.rm = TRUE),
    mean_wl_pct_w8 = mean(wl_pct_w8[wl_w8 <= -0.5], na.rm = TRUE),
    mean_fl_w8 = mean(fl_w8[wl_w8 <= -0.5], na.rm = TRUE),
    mean_fmlf_w8 = mean_fl_w8/mean_wl_w8 * 100,
    mean_lmlf_w8 = 100-mean_fmlf_w8,
    
    n_w16 = sum(wl_w16 <= -0.5, na.rm = TRUE),
    mean_wl_w16 = mean(wl_w16[wl_w16 <= -0.5], na.rm = TRUE),
    mean_wl_pct_w16 = mean(wl_pct_w16[wl_w16 <= -0.5], na.rm = TRUE),
    mean_fl_w16 = mean(fl_w16[wl_w16 <= -0.5], na.rm = TRUE),
    mean_fmlf_w16 = mean_fl_w16/mean_wl_w16 * 100,
    mean_lmlf_w16 = 100-mean_fmlf_w16,
    
    n_w24 = sum(wl_w24 <= -0.5, na.rm = TRUE),
    mean_wl_w24 = mean(wl_w24[wl_w24 <= -0.5], na.rm = TRUE),
    mean_wl_pct_w24 = mean(wl_pct_w24[wl_w24 <= -0.5], na.rm = TRUE),
    mean_fl_w24 = mean(fl_w24[wl_w24 <= -0.5], na.rm = TRUE),
    mean_fmlf_w24 = mean_fl_w24/mean_wl_w24 * 100,
    mean_lmlf_w24 = 100-mean_fmlf_w24
  ) 

view(mean_results)

genesis_data_3t_wide %>% filter(wl_w8 >= -0.5) %>% select(client_id)
genesis_data_3t_wide %>% filter(wl_w16 >= -0.5) %>% select(client_id) #有一個人不一樣924583 / 983950
genesis_data_3t_wide %>% filter(wl_w24 >= -0.5) %>% select(client_id) #w24和w8排除的兩個人是一樣的client_id == 615543 / 911597  

write.csv(mean_results, "weight_fat_lean_loss_summary_20260126.csv", row.names = FALSE)


# 抓出有完整3次療程的人 (2025/01/01-2026/01/08)
# 全部人不管有沒有用過GLP-1
# -------------------------------------------------
# 抓出2025年的所有人
genesis_data_2025 <- genesis_data %>%
  filter(between(started_at, as.Date("2025-01-01"), as.Date("2026-01-08")))

# 抓出有完整3次療程的人
genesis_data_2025_3t <- genesis_data_2025 %>%
  filter(!is.na(treatment)) %>% 
  group_by(client_id) %>%
  filter(all(c(1, 2, 3) %in% treatment)) %>%
  ungroup() 

n_distinct(genesis_data_2025_3t$client_id) #未排除體重缺值 n=169


# 抓出w0，w8，w16，w24，計算weight loss
genesis_data_2025_3t_wide <- genesis_data_2025_3t %>%
  group_by(client_id) %>%
  summarise(
    weight_w0 = weight_baseline[treatment == 1],
    weight_w8 = weight_endpoint[treatment == 1],
    weight_w16 = weight_endpoint[treatment == 2],
    weight_w24 = weight_endpoint[treatment == 3],
    body_fat_w0 = body_fat_mass_baseline[treatment == 1],
    body_fat_w8 = body_fat_mass_endpoint[treatment == 1],
    body_fat_w16 = body_fat_mass_endpoint[treatment == 2],
    body_fat_w24 = body_fat_mass_endpoint[treatment == 3],
    body_fat_mass_percentage_w0 = body_fat_mass_percentage_baseline[treatment == 1],
    body_fat_mass_percentage_w8 = body_fat_mass_percentage_endpoint[treatment == 1],
    body_fat_mass_percentage_w16 = body_fat_mass_percentage_endpoint[treatment == 2],
    body_fat_mass_percentage_w24 = body_fat_mass_percentage_endpoint[treatment == 3],
    muscle_mass_w0 = muscle_mass_baseline[treatment == 1],
    muscle_mass_w8 = muscle_mass_endpoint[treatment == 1],
    muscle_mass_w16 = muscle_mass_endpoint[treatment == 2],
    muscle_mass_w24 = muscle_mass_endpoint[treatment == 3],
    bmi_w0 = bmi_baseline[treatment == 1]
  ) %>%
  ungroup() %>%
  filter(complete.cases(weight_w0, weight_w8, weight_w16, weight_w24))

n_distinct(genesis_data_2025_3t_wide$client_id) #體重沒有缺的人 n=88

# 只保留BMI>=24的人
genesis_data_2025_3t_wide <- genesis_data_2025_3t_wide %>% 
  filter(bmi_w0 >= 24)

n_distinct(genesis_data_2025_3t_wide$client_id) #排除BMI normal的人 n=82 (排除6人)


genesis_data_2025_3t_wide <- genesis_data_2025_3t_wide %>%
  mutate(
    # Weight change (後減前)
    wl_w8 = weight_w8 - weight_w0,
    wl_w16 = weight_w16 - weight_w0,
    wl_w24 = weight_w24 - weight_w0,
    
    # Weight change percentage
    wl_pct_w8 = (weight_w8 - weight_w0) / weight_w0 * 100,
    wl_pct_w16 = (weight_w16 - weight_w0) / weight_w0 * 100,
    wl_pct_w24 = (weight_w24 - weight_w0) / weight_w0 * 100,
    
    # fat change
    fl_w8 = body_fat_w8 - body_fat_w0,
    fl_w16 = body_fat_w16 - body_fat_w0,
    fl_w24 = body_fat_w24 - body_fat_w0,
    
    # fat change percentage
    fl_pct_w8 = (body_fat_w8 - body_fat_w0) / body_fat_w0 * 100,
    fl_pct_w16 = (body_fat_w16 - body_fat_w0) / body_fat_w0 * 100,
    fl_pct_w24 = (body_fat_w24 - body_fat_w0) / body_fat_w0 * 100,
    
    # fat pct change (體脂率變化，不是百分比變化)
    fpc_w8 = body_fat_mass_percentage_w8 - body_fat_mass_percentage_w0,
    fpc_w16 = body_fat_mass_percentage_w16 - body_fat_mass_percentage_w0,
    fpc_w24 = body_fat_mass_percentage_w24 - body_fat_mass_percentage_w0,
    
    # muscle change
    ml_w8 = muscle_mass_w8 - muscle_mass_w0,
    ml_w16 = muscle_mass_w16 - muscle_mass_w0,
    ml_w24 = muscle_mass_w24 - muscle_mass_w0)


# 方法 3: 使用 summarise 一次計算多個
mean_results_2025 <- genesis_data_2025_3t_wide %>%
  summarise(
    n_w8 = sum(wl_w8 <= -0.5, na.rm = TRUE),
    mean_wl_w8 = mean(wl_w8[wl_w8 <= -0.5], na.rm = TRUE),
    mean_wl_pct_w8 = mean(wl_pct_w8[wl_w8 <= -0.5], na.rm = TRUE),
    mean_fl_w8 = mean(fl_w8[wl_w8 <= -0.5], na.rm = TRUE),
    mean_fmlf_w8 = mean_fl_w8/mean_wl_w8 * 100,
    mean_lmlf_w8 = 100-mean_fmlf_w8,
    
    n_w16 = sum(wl_w16 <= -0.5, na.rm = TRUE),
    mean_wl_w16 = mean(wl_w16[wl_w16 <= -0.5], na.rm = TRUE),
    mean_wl_pct_w16 = mean(wl_pct_w16[wl_w16 <= -0.5], na.rm = TRUE),
    mean_fl_w16 = mean(fl_w16[wl_w16 <= -0.5], na.rm = TRUE),
    mean_fmlf_w16 = mean_fl_w16/mean_wl_w16 * 100,
    mean_lmlf_w16 = 100-mean_fmlf_w16,
    
    n_w24 = sum(wl_w24 <= -0.5, na.rm = TRUE),
    mean_wl_w24 = mean(wl_w24[wl_w24 <= -0.5], na.rm = TRUE),
    mean_wl_pct_w24 = mean(wl_pct_w24[wl_w24 <= -0.5], na.rm = TRUE),
    mean_fl_w24 = mean(fl_w24[wl_w24 <= -0.5], na.rm = TRUE),
    mean_fmlf_w24 = mean_fl_w24/mean_wl_w24 * 100,
    mean_lmlf_w24 = 100-mean_fmlf_w24
  ) 

view(mean_results_2025)


write.csv(mean_results_2025, "2025_weight_fat_lean_loss_summary_20260126.csv", row.names = FALSE)

#output
wb <- createWorkbook()
addWorksheet(wb, "2019-202601")
writeData(wb, "2019-202601", mean_results)
addWorksheet(wb, "2025-202601")
writeData(wb, "2025-202601", mean_results_2025)
saveWorkbook(wb, "weight_fat_lean_loss_summary_20260126.xlsx", overwrite = TRUE)



summary(genesis_data_2025_3t_wide[, c("wl_w8", "wl_w16", "wl_w24", 
                                 "wl_pct_w8", "wl_pct_w16", "wl_pct_w24",
                                 "fl_w8", "fl_w16", "fl_w24", 
                                 "fl_pct_w8", "fl_pct_w16", "fl_pct_w24",
                                 "fpc_w8", "fpc_w16", "fpc_w24",
                                 "ml_w8", "ml_w16", "ml_w24")])


#summary <- genesis_data_3t_wide %>%
  summarise(across(
    c(wl_w8, wl_w16, wl_w24, 
      wl_pct_w8, wl_pct_w16, wl_pct_w24,
      fl_w8, fl_w16, fl_w24, 
      fl_pct_w8, fl_pct_w16, fl_pct_w24,
      fpc_w8, fpc_w16, fpc_w24,
      ml_w8, ml_w16, ml_w24,
      fmlf_w8, fmlf_w16, fmlf_w24,
      lmlf_w8, lmlf_w16, lmlf_w24),
    list(mean = ~mean(., na.rm = TRUE)),
    .names = "mean_{.col}"
  ))


#write.csv(summary, "weight_fat_lean_loss_summary_20260126.csv", row.names = FALSE)



#############################
# Visualization
#############################
# 方法 1: 基本線條圖
# 定義顏色
colors <- c(
    "Genesis (20190901-20260108)" = "#FF7F00",
    "Genesis (20250101-20260108)" = "#FFAE42",
    "Mounjaro 5mg"                = "#92c5de",
    "Mounjaro 10mg"               = "#4393c3",
    "Mounjaro 15mg"               = "#2166ac",
    "Ozempic 0.5mg"               = "#a6dba0",
    "Ozempic 1.0mg"               = "#1b7837",
    "Rybelsus"                    = "#DC0000",
    "Saxenda 1.8mg"               = "#e7d4e8",
    "Saxenda 3.0mg"               = "#c2a5cf",
    "Wegovy"                      = "#f4a582"
  )  
  

# weight loss pct
#_______________________
paper_data_long <- paper_data %>%
  # 選擇百分比欄位
  select(Source, starts_with("weight_loss_pct_w")) %>%
  # 轉成 long format
  pivot_longer(
    cols = starts_with("weight_loss_pct_w"),
    names_to = "week",
    values_to = "weight_loss_pct"
  ) %>%
  # 提取週數
  mutate(
    week_num = as.numeric(str_extract(week, "\\d+"))
  ) %>%
  # 移除 NA
  filter(!is.na(weight_loss_pct))


# 創建互動式圖表
p <- plot_ly(data = paper_data_long,
             x = ~week_num,
             y = ~weight_loss_pct,
             color = ~Source,
             colors = colors,
             type = 'scatter',
             mode = 'lines+markers',
             line = list(width = 3),
             marker = list(size = 8),
             hovertemplate = paste(
               '<b>%{fullData.name}</b><br>',
               'Week: %{x}<br>',
               'Weight Loss: %{y:.2f}%<br>',
               '<extra></extra>'
             )) %>%
  layout(
    title = list(
      text = "<b>Weight Loss (%)</b><br><sup>Different Drug + Lifestyle vs. Genesis</sup>",
      x = 0.5,
      xanchor = 'center'
    ),
    xaxis = list(
      title = "<b>Weeks</b>",
      tickmode = "linear",
      tick0 = 0,
      dtick = 8,
      gridcolor = '#E5E5E5'
    ),
    yaxis = list(
      title = "<b>Weight Loss (%)</b>",
      gridcolor = '#E5E5E5'
    ),
    legend = list(
      orientation = "v",
      x = 1.05,
      y = 0.5,
      xanchor = "left",
      yanchor = "middle"
    ),
    hovermode = 'closest',
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    annotations = list(
      list(
        text = "Note: Negative values indicate weight loss.",
        x = 0,
        y = -0.15,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        showarrow = FALSE,
        font = list(size = 10, color = "gray")
      )
    ),
    margin = list(r = 150)  # 右邊留空間給圖例
  )

# 顯示圖表
p


htmlwidgets::saveWidget(p, "glp1_weight_loss_pct.html")


# weight loss kg
#_______________________
paper_data_long_kg <- paper_data %>%
  # 選擇百分比欄位
  select(Source, starts_with("weight_loss_w")) %>%
  # 轉成 long format
  pivot_longer(
    cols = starts_with("weight_loss_w"),
    names_to = "week",
    values_to = "weight_loss_kg"
  ) %>%
  # 提取週數
  mutate(
    week_num = as.numeric(str_extract(week, "\\d+"))
  ) %>%
  # 移除 NA
  filter(!is.na(weight_loss_kg))



# 創建互動式圖表
p_2 <- plot_ly(data = paper_data_long_kg,
             x = ~week_num,
             y = ~weight_loss_kg,
             color = ~Source,
             colors = colors,
             type = 'scatter',
             mode = 'lines+markers',
             line = list(width = 3),
             marker = list(size = 8),
             hovertemplate = paste(
               '<b>%{fullData.name}</b><br>',
               'Week: %{x}<br>',
               'Weight Loss: %{y:.2f}kg<br>',
               '<extra></extra>'
             )) %>%
  layout(
    title = list(
      text = "<b>Weight Loss (kg)</b><br><sup>Different Drug + Lifestyle vs. Genesis</sup>",
      x = 0.5,
      xanchor = 'center'
    ),
    xaxis = list(
      title = "<b>Weeks</b>",
      tickmode = "linear",
      tick0 = 0,
      dtick = 8,
      gridcolor = '#E5E5E5'
    ),
    yaxis = list(
      title = "<b>Weight Loss (kg)</b>",
      gridcolor = '#E5E5E5'
    ),
    legend = list(
      orientation = "v",
      x = 1.05,
      y = 0.5,
      xanchor = "left",
      yanchor = "middle"
    ),
    hovermode = 'closest',
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    annotations = list(
      list(
        text = "Note: Negative values indicate weight loss.",
        x = 0,
        y = -0.15,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        showarrow = FALSE,
        font = list(size = 10, color = "gray")
      )
    ),
    margin = list(r = 150)  # 右邊留空間給圖例
  )

# 顯示圖表
p_2


htmlwidgets::saveWidget(p_2, "glp1_weight_loss_kg.html")


# 方法 4: 只比較特定藥物（例如 Genesis vs GLP-1 藥物）
paper_data_long %>%
  filter(Source %in% c("Genesis", "Wegovy", "Ozempic 1.0mg", "Saxenda 3.0mg", "Mounjaro 10mg")) %>%
  ggplot(aes(x = week_num, y = weight_loss_pct, 
             color = Source, group = Source)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Genesis vs GLP-1 藥物體重變化比較",
    x = "週數 (Weeks)",
    y = "體重變化百分比 (%)",
    color = "療程/藥物"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0, 80, by = 8)) +
  scale_color_manual(values = c(
    "Genesis" = "#FF7F00",
    "Wegovy" = "#377EB8",
    "Ozempic 1.0mg" = "#4DAF4A",
    "Saxenda 3.0mg" = "#E41A1C",
    "Mounjaro 10mg" = "#EF4F04"
  ))

######################################
# 畫 fat & lean mass loss fraction 圖
######################################

# 1. 建立資料集
data <- data.frame(
  Group = c("Genesis (20250101-20260108)", 
            "Genesis (20190901-20260108)", 
            "Mounjaro (SURMOUNT 1)", 
            "Wegovy (STEP 1)"),
  Lean_Mass = c(23.3, 22.2, 25.7, 40.0),
  Fat_Mass = c(76.7, 77.8, 74.3, 60.0)
)

# 2. 轉換為長格式 (Long format) 以利 ggplot 繪製
data_long <- data %>%
  pivot_longer(cols = c(Lean_Mass, Fat_Mass), 
               names_to = "Component", 
               values_to = "Percentage")

# 為了確保圖表順序，將 Group 設為 factor
data_long$Group <- factor(data_long$Group, levels = rev(c(
  "Genesis (20250101-20260108)", 
  "Genesis (20190901-20260108)", 
  "Mounjaro (SURMOUNT 1)", 
  "Wegovy (STEP 1)"
)))

# 3. 繪圖
p3 <- ggplot(data_long, aes(x = Group, y = Percentage, fill = Component)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # 橫向長條圖，較易閱讀藥物名稱
  # 設定 Nature 風格配色：瘦體重用紅色系，脂肪用藍色系
  scale_fill_manual(
    values = c("Lean_Mass" = "#4393c3", "Fat_Mass" = "#E64B35"),
    labels = c("Lean_Mass" = "Lean Mass Loss Fraction", "Fat_Mass" = "Fat Mass Loss Fraction")
  ) +
  labs(
    title = "Composition of Weight Loss",
    subtitle = "Comparison of Lean vs. Fat Mass Loss Fractions",
    x = "",
    y = "Percentage of Total Weight Loss (%)",
    fill = "Component"
  ) +
  theme_minimal(base_size = 45) +
  theme(
    text = element_text(family = "sans"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  ) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", fontface = "bold", size = 15) 

p3

# 儲存圖片
ggsave("fat_lean_mass_loss_fraction.jpeg", p3, 
       width = 6, height = 4, units = "in", dpi = 300,
       scale = 1.5)











#############################
# Fat mass loss fraction
#############################

genesis_data_3t_wide <- genesis_data_3t_wide %>%
  mutate( 
    # Fat Mass Loss Fraction (%)
    # 公式：(脂肪減少量 / 體重減少量) * 100
    fmlf_w8 = case_when(
      wl_w8 >= -0.5 ~ NA_real_,  # 體重未明顯減少
      is.na(fl_w8) ~ NA_real_,   # 脂肪資料缺失
      TRUE ~ (fl_w8 / wl_w8) * 100
    ),
    fmlf_w16 = case_when(
      wl_w16 >= -0.5 ~ NA_real_,
      is.na(fl_w16) ~ NA_real_,
      TRUE ~ (fl_w16 / wl_w16) * 100
    ),
    fmlf_w24 = case_when(
      wl_w24 >= -0.5 ~ NA_real_,
      is.na(fl_w24) ~ NA_real_,
      TRUE ~ (fl_w24 / wl_w24) * 100
    ),
    lmlf_w8 = 100 - fmlf_w8,
    lmlf_w16 = 100 - fmlf_w16,
    lmlf_w24 = 100 - fmlf_w24
  )




#############################
# Lean mass loss fraction
#############################
genesis_data_3t_wide <- genesis_data_3t_wide %>%
  mutate(
    # 先計算瘦體重變化
    lean_loss_w8 = wl_w8 - fl_w8,
    lean_loss_w16 = wl_w16 - fl_w16,
    lean_loss_w24 = wl_w24 - fl_w24,
    
    # Lean mass loss fraction
    # 只在體重減少且減少量 > 0.5 kg 時計算
    lml_w8 = case_when(
      wl_w8 >= 0 ~ NA_real_,  # 體重增加或不變，不計算
      abs(wl_w8) < 0.5 ~ NA_real_,  # 體重變化太小，不計算
      is.na(fl_w8) ~ NA_real_,  # 脂肪資料缺失，不計算
      TRUE ~ lean_loss_w8 / wl_w8 * 100  # 正常計算
    ),
    lml_w16 = case_when(
      wl_w16 >= 0 ~ NA_real_,
      abs(wl_w16) < 0.5 ~ NA_real_,
      is.na(fl_w16) ~ NA_real_,
      TRUE ~ lean_loss_w16 / wl_w16 * 100
    ),
    lml_w24 = case_when(
      wl_w24 >= 0 ~ NA_real_,
      abs(wl_w24) < 0.5 ~ NA_real_,
      is.na(fl_w24) ~ NA_real_,
      TRUE ~ lean_loss_w24 / wl_w24 * 100
    ),
    
    lml_w24_loss_only = case_when(
      wl_w24 >= 0 ~ NA_real_,  # 體重增加
      abs(wl_w24) < 0.5 ~ NA_real_,  # 體重變化太小
      is.na(fl_w24) ~ NA_real_,  # 資料缺失
      lean_loss_w24 <= 0 ~ 0,  # 瘦體重維持或增加，設為 0
      TRUE ~ lean_loss_w24 / wl_w24 * 100  # 瘦體重流失的比例
    ),
    
    # 額外分類：減重品質評估
    weight_loss_quality_w24 = case_when(
      is.na(lml_w24) ~ "無法評估",
      lml_w24 < 0 ~ "優異（增肌減脂）",
      lml_w24 < 20 ~ "優良（主要減脂）",
      lml_w24 < 35 ~ "良好（適度保肌）",
      lml_w24 < 50 ~ "尚可（需注意肌肉）",
      TRUE ~ "不佳（肌肉流失多）"
    )
  )
# 檢查結果分佈
summary(genesis_data_3t_wide[, c("lml_w8", "lml_w16", "lml_w24","lean_loss_w24", "lml_w24_loss_only")])






# 下面都是更之前的 
# ----------------------------------------------分割線-----------------------------------------------------------------


# 抓出有完整3次療程的人 (2019/09/01-2026/01/08)
# 有用過GLP-1
# -------------------------------------------------
#排除掉3次療程都沒用過glp-1的人
genesis_data_3t_glp1 <- genesis_data_3t %>%
  group_by(client_id) %>%
  filter(any(glp1_gp == "GLP-1_user", na.rm = TRUE)) %>%  # 排除都沒用 GLP-1 的人
  ungroup()

# 檢查結果
n_distinct(genesis_data_3t_glp1$client_id)

# 抓出w0，w8，w16，w24，計算weight loss
genesis_data_3t_glp1_wide <- genesis_data_3t_glp1 %>%
  group_by(client_id) %>%
  summarise(
    bmi_w0 = bmi_baseline[treatment == 1],
    weight_w0 = weight_baseline[treatment == 1],
    weight_w8 = weight_endpoint[treatment == 1],
    weight_w16 = weight_endpoint[treatment == 2],
    weight_w24 = weight_endpoint[treatment == 3]
  ) %>%
  ungroup() %>%
  filter(complete.cases(weight_w0, weight_w8, weight_w16, weight_w24))

n_distinct(genesis_data_3t_glp1_wide$client_id) #體重沒有缺的人 n=291

genesis_data_3t_glp1_wide <- genesis_data_3t_glp1_wide %>%
  mutate(
    # Weight change (後減前)
    wl_w8 = weight_w8 - weight_w0,
    wl_w16 = weight_w16 - weight_w0,
    wl_w24 = weight_w24 - weight_w0,
    
    # Weight change percentage
    wl_pct_w8 = (weight_w8 - weight_w0) / weight_w0 * 100,
    wl_pct_w16 = (weight_w16 - weight_w0) / weight_w0 * 100,
    wl_pct_w24 = (weight_w24 - weight_w0) / weight_w0 * 100
  )


summary(genesis_data_3t_glp1_wide[, c("wl_w8", "wl_w16", "wl_w24", 
                                      "wl_pct_w8", "wl_pct_w16", "wl_pct_w24")])


genesis_data_3t_glp1_wide %>%
  summarise(
    mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
    mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
    mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
    mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
    mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
    mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
  )


# 抓出有完整3次療程的人 (2019/09/01-2026/01/08)
# ****沒有用過GLP-1
# -------------------------------------------------
# 抓出3次療程都沒用過glp-1的人
genesis_data_3t_nglp1 <- genesis_data_3t %>%
  group_by(client_id) %>%
  filter(all(glp1_gp == "non_GLP-1_user", na.rm = TRUE)) %>%   # 排除都沒用 GLP-1 的人
  ungroup()

# 檢查結果
n_distinct(genesis_data_3t_nglp1$client_id) #n=230

genesis_data_3t_nglp1_wide <- genesis_data_3t_nglp1 %>%
  group_by(client_id) %>%
  summarise(
    bmi_w0 = bmi_baseline[treatment == 1],
    weight_w0 = weight_baseline[treatment == 1],
    weight_w8 = weight_endpoint[treatment == 1],
    weight_w16 = weight_endpoint[treatment == 2],
    weight_w24 = weight_endpoint[treatment == 3]
  ) %>%
  ungroup() %>%
  filter(complete.cases(weight_w0, weight_w8, weight_w16, weight_w24))

n_distinct(genesis_data_3t_nglp1_wide$client_id) #75







## ----------------------------------------------------------------------------



# 抓出有完整3次療程的人 (2025/01/01-2026/01/08)
# 全部人不管有沒有用過GLP-1
# -------------------------------------------------
# 抓出2025年的所有人
genesis_data_2025 <- genesis_data %>%
  filter(between(started_at, as.Date("2025-01-01"), as.Date("2026-01-08")))

# 抓出有完整3次療程的人
genesis_data_2025_3t <- genesis_data_2025 %>%
  group_by(client_id) %>%
  filter(all(c(1, 2, 3) %in% treatment)) %>%
  ungroup() 

n_distinct(genesis_data_2025_3t$client_id) #未排除體重缺值 n=169


# 抓出w0，w8，w16，w24，計算weight loss
genesis_data_2025_3t_wide <- genesis_data_2025_3t %>%
  group_by(client_id) %>%
  summarise(
    weight_w0 = weight_baseline[treatment == 1],
    weight_w8 = weight_endpoint[treatment == 1],
    weight_w16 = weight_endpoint[treatment == 2],
    weight_w24 = weight_endpoint[treatment == 3]
  ) %>%
  ungroup() %>%
  filter(complete.cases(weight_w0, weight_w8, weight_w16, weight_w24))

n_distinct(genesis_data_2025_3t_wide$client_id) #體重沒有缺的人 n=89

genesis_data_2025_3t_wide <- genesis_data_2025_3t_wide %>%
  mutate(
    # Weight change (後減前)
    wl_w8 = weight_w8 - weight_w0,
    wl_w16 = weight_w16 - weight_w0,
    wl_w24 = weight_w24 - weight_w0,
    
    # Weight change percentage
    wl_pct_w8 = (weight_w8 - weight_w0) / weight_w0 * 100,
    wl_pct_w16 = (weight_w16 - weight_w0) / weight_w0 * 100,
    wl_pct_w24 = (weight_w24 - weight_w0) / weight_w0 * 100
  )

# 有沒有用glp-1都混在一起
summary(genesis_data_2025_3t_wide[, c("wl_w8", "wl_w16", "wl_w24", 
                                 "wl_pct_w8", "wl_pct_w16", "wl_pct_w24")])


genesis_data_2025_3t_wide %>%
  summarise(
    mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
    mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
    mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
    mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
    mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
    mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
  )





# 抓出有完整3次療程的人 (2025/01/01-2026/01/08)
# 有用過GLP-1
# -------------------------------------------------
#排除掉3次療程都沒用過glp-1的人
genesis_data_2025_3t_glp1 <- genesis_data_2025_3t %>%
  group_by(client_id) %>%
  filter(any(glp1_gp == "GLP-1_user", na.rm = TRUE)) %>%  # 排除都沒用 GLP-1 的人
  ungroup()

# 檢查結果
n_distinct(genesis_data_2025_3t_glp1$client_id)

# 抓出w0，w8，w16，w24，計算weight loss
genesis_data_2025_3t_glp1_wide <- genesis_data_2025_3t_glp1 %>%
  group_by(client_id) %>%
  summarise(
    bmi_w0 = bmi_baseline[treatment == 1],
    weight_w0 = weight_baseline[treatment == 1],
    weight_w8 = weight_endpoint[treatment == 1],
    weight_w16 = weight_endpoint[treatment == 2],
    weight_w24 = weight_endpoint[treatment == 3]
  ) %>%
  ungroup() %>%
  filter(complete.cases(weight_w0, weight_w8, weight_w16, weight_w24))

n_distinct(genesis_data_2025_3t_glp1_wide$client_id) #體重沒有缺的人 n=87

genesis_data_2025_3t_glp1_wide <- genesis_data_2025_3t_glp1_wide %>%
  mutate(
    # Weight change (後減前)
    wl_w8 = weight_w8 - weight_w0,
    wl_w16 = weight_w16 - weight_w0,
    wl_w24 = weight_w24 - weight_w0,
    
    # Weight change percentage
    wl_pct_w8 = (weight_w8 - weight_w0) / weight_w0 * 100,
    wl_pct_w16 = (weight_w16 - weight_w0) / weight_w0 * 100,
    wl_pct_w24 = (weight_w24 - weight_w0) / weight_w0 * 100
  )


summary(genesis_data_2025_3t_glp1_wide[, c("wl_w8", "wl_w16", "wl_w24", 
                                      "wl_pct_w8", "wl_pct_w16", "wl_pct_w24")])


genesis_data_2025_3t_glp1_wide %>%
  summarise(
    mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
    mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
    mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
    mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
    mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
    mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
  )



# 抓出有完整3次療程的人 (2025/01/01-2026/01/08)
# ****沒有用過GLP-1
# -------------------------------------------------
# 抓出3次療程都沒用過glp-1的人
genesis_data_2025_3t_nglp1 <- genesis_data_2025_3t %>%
  group_by(client_id) %>%
  filter(all(glp1_gp == "non_GLP-1_user", na.rm = TRUE)) %>%   # 排除都沒用 GLP-1 的人
  ungroup()

# 檢查結果
n_distinct(genesis_data_2025_3t_nglp1$client_id) #n=3






# 抓出有完整3次療程的人 (2019/09/01-2024/12/31)
# 全部人不管有沒有用過GLP-1
# -------------------------------------------------
# 抓出2024年的所有人
genesis_data_2024 <- genesis_data %>%
  filter(between(started_at, as.Date("2019-09-01"), as.Date("2024-12-31")))

genesis_data_2024 <- genesis_data_2024 %>%
  group_by(client_id) %>%
  filter(all(c(1, 2, 3) %in% treatment)) %>%
  ungroup() 

# 抓出w0，w8，w16，w24，計算weight loss
genesis_data_2024_wide <- genesis_data_2024 %>%
  group_by(client_id) %>%
  summarise(
    weight_w0 = weight_baseline[treatment == 1],
    weight_w8 = weight_endpoint[treatment == 1],
    weight_w16 = weight_endpoint[treatment == 2],
    weight_w24 = weight_endpoint[treatment == 3]
  ) %>%
  ungroup() %>%
  filter(complete.cases(weight_w0, weight_w8, weight_w16, weight_w24))

n_distinct(genesis_data_2024_wide$client_id) #體重沒有缺的人 n=366

genesis_data_2024_wide <- genesis_data_2024_wide %>%
  mutate(
    # Weight change (後減前)
    wl_w8 = weight_w8 - weight_w0,
    wl_w16 = weight_w16 - weight_w0,
    wl_w24 = weight_w24 - weight_w0,
    
    # Weight change percentage
    wl_pct_w8 = (weight_w8 - weight_w0) / weight_w0 * 100,
    wl_pct_w16 = (weight_w16 - weight_w0) / weight_w0 * 100,
    wl_pct_w24 = (weight_w24 - weight_w0) / weight_w0 * 100
  )

# 有沒有用glp-1都混在一起
summary(genesis_data_2024_wide[, c("wl_w8", "wl_w16", "wl_w24", 
                                   "wl_pct_w8", "wl_pct_w16", "wl_pct_w24")])


genesis_data_2024_wide %>%
  summarise(
    mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
    mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
    mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
    mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
    mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
    mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
  )






## ------------------------------------------------------------------------------
# SUMMARY & OUTPUT 
## ---------------------------------------------------------
# 建立結果統整表
results_summary <- data.frame(
  Period = character(),
  Group = character(),
  N = numeric(),
  mean_wl_w8 = numeric(),
  mean_wl_w16 = numeric(),
  mean_wl_w24 = numeric(),
  mean_wl_pct_w8 = numeric(),
  mean_wl_pct_w16 = numeric(),
  mean_wl_pct_w24 = numeric(),
  stringsAsFactors = FALSE
)

# 2019-2026 全部人
results_summary <- rbind(results_summary, data.frame(
  Period = "2019-2026",
  Group = "All",
  N = n_distinct(genesis_data_3t_wide$client_id),
  genesis_data_3t_wide %>%
    summarise(
      mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
      mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
      mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
      mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
      mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
      mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
    )
))

# 2019-2026 GLP-1 使用者
results_summary <- rbind(results_summary, data.frame(
  Period = "2019-2026",
  Group = "GLP-1 user",
  N = n_distinct(genesis_data_3t_glp1_wide$client_id),
  genesis_data_3t_glp1_wide %>%
    summarise(
      mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
      mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
      mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
      mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
      mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
      mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
    )
))



# 2025-2026 全部人
results_summary <- rbind(results_summary, data.frame(
  Period = "2025-2026",
  Group = "All",
  N = n_distinct(genesis_data_2025_3t_wide$client_id),
  genesis_data_2025_3t_wide %>%
    summarise(
      mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
      mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
      mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
      mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
      mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
      mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
    )
))

# 2025-2026 GLP-1 使用者
results_summary <- rbind(results_summary, data.frame(
  Period = "2025-2026",
  Group = "GLP-1 user",
  N = n_distinct(genesis_data_2025_3t_glp1_wide$client_id),
  genesis_data_2025_3t_glp1_wide %>%
    summarise(
      mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
      mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
      mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
      mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
      mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
      mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
    )
))


# 2019-2024 全部使用者
results_summary <- rbind(results_summary, data.frame(
  Period = "2019-2024",
  Group = "All",
  N = n_distinct(genesis_data_2024_wide$client_id),
  genesis_data_2024_wide %>%
    summarise(
      mean_wl_w8 = mean(wl_w8, na.rm = TRUE),
      mean_wl_w16 = mean(wl_w16, na.rm = TRUE),
      mean_wl_w24 = mean(wl_w24, na.rm = TRUE),
      mean_wl_pct_w8 = mean(wl_pct_w8, na.rm = TRUE),
      mean_wl_pct_w16 = mean(wl_pct_w16, na.rm = TRUE),
      mean_wl_pct_w24 = mean(wl_pct_w24, na.rm = TRUE)
    )
))


# 查看結果
print(results_summary) 

# 輸出成 CSV
write.csv(results_summary, "results_weight_loss_summary_20260114.csv", row.names = FALSE)




library(dplyr)
library(stringr)

# 先在原始資料中建立藥物分類
genesis_data <- genesis_data %>%
  mutate(
    drug_type = case_when(
      str_detect(english_name, "Wegovy")   ~ "Wegovy",
      str_detect(english_name, "Mounjaro") ~ "Mounjaro",
      str_detect(english_name, "Ozempic")  ~ "Ozempic",
      str_detect(english_name, "Rybelsus") ~ "Rybelsus",
      str_detect(english_name, "Saxenda")  ~ "Saxenda",
      TRUE ~ "Other"
    )
  )

# ============================================
# 1. Doctor 分佈比較
# ============================================

# 2019-2026 全部人
doctor_2019_all <- genesis_data_3t %>%
  group_by(doctor) %>%
  summarise(n = n_distinct(client_id)) %>%
  mutate(
    Period = "2019-2026",
    Group = "All",
    percentage = n / sum(n) * 100
  )

# 2019-2026 GLP-1 使用者
doctor_2019_glp1 <- genesis_data_3t_glp1 %>%
  group_by(doctor) %>%
  summarise(n = n_distinct(client_id)) %>%
  mutate(
    Period = "2019-2026",
    Group = "GLP-1 user",
    percentage = n / sum(n) * 100
  )

# 2019-2026 非 GLP-1 使用者
doctor_2019_nglp1 <- genesis_data_3t_nglp1 %>%
  group_by(doctor) %>%
  summarise(n = n_distinct(client_id)) %>%
  mutate(
    Period = "2019-2026",
    Group = "Non-GLP-1 user",
    percentage = n / sum(n) * 100
  )

# 2025-2026 全部人
doctor_2025_all <- genesis_data_2025_3t %>%
  group_by(doctor) %>%
  summarise(n = n_distinct(client_id)) %>%
  mutate(
    Period = "2025-2026",
    Group = "All",
    percentage = n / sum(n) * 100
  )

# 2025-2026 GLP-1 使用者
doctor_2025_glp1 <- genesis_data_2025_3t_glp1 %>%
  group_by(doctor) %>%
  summarise(n = n_distinct(client_id)) %>%
  mutate(
    Period = "2025-2026",
    Group = "GLP-1 user",
    percentage = n / sum(n) * 100
  )

# 2025-2026 非 GLP-1 使用者
doctor_2025_nglp1 <- genesis_data_2025_3t_nglp1 %>%
  group_by(doctor) %>%
  summarise(n = n_distinct(client_id)) %>%
  mutate(
    Period = "2025-2026",
    Group = "Non-GLP-1 user",
    percentage = n / sum(n) * 100
  )

# 合併所有 doctor 分佈
doctor_distribution <- bind_rows(
  doctor_2019_all,
  doctor_2019_glp1,
  doctor_2019_nglp1,
  doctor_2025_all,
  doctor_2025_glp1,
  doctor_2025_nglp1
) %>%
  arrange(Period, Group, desc(n))

# ============================================
# 2. 藥物分佈比較
# ============================================


# ============================================
# 3. 輸出結果
# ============================================

# 查看結果
print("Doctor Distribution:")
print(doctor_distribution)

print("Drug Distribution:")
print(drug_distribution)

# 輸出成 CSV
write.csv(doctor_distribution, "doctor_distribution_comparison.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(drug_distribution, "drug_distribution_comparison.csv", row.names = FALSE)

# ============================================
# 4. 視覺化（可選）
# ============================================

library(ggplot2)

# Doctor 分佈圖
ggplot(doctor_distribution, aes(x = doctor, y = percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Period) +
  labs(title = "Doctor Distribution Comparison",
       x = "Doctor", y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("doctor_distribution.png", width = 12, height = 6)

# 藥物分佈圖
ggplot(drug_distribution, aes(x = drug_type, y = percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Period) +
  labs(title = "Drug Distribution Comparison",
       x = "Drug Type", y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("drug_distribution.png", width = 12, height = 6)
