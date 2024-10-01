
# 依存関係 (ctrl + shift + R)--------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggsci)
library(fixest)


# データ読み込み -----------------------------------------------------------------

df <- read_excel("data/ローデータGT表2402_公共交通課題の実態把握調査.xlsx",sheet = "1;group～fs1")
head(df)


# 抽出 --------------------------------------------------------------------

# Q17は複数回答可→縦持ちに(user_idが重複する)
df <- df %>%  mutate(
  IsUphold = case_when(
    q22 < 3 ~ "uphold",
    q22 == 3 | q22 == 4 ~ "opposit",
    TRUE ~ "indifferent"
  )
)

upholders <- df %>% filter(
  IsUphold == "uphold"
)

opposits <- df %>% filter(
  IsUphold == "opposit"
)

g <- ggplot(data = df, aes(x = 1, fill = as.factor(IsUphold))) +
  geom_bar(stat = "count", color = "black") +
  coord_polar(theta = "y")
g <- g + labs(y = "人数", fill = "ライドシェアへの立場")
plot(g)
ggsave("plot/IssueRate.png", plot = g, width = 8, height = 6, dpi = 300)

df_t <- df %>% pivot_longer(
  cols = c("q17-1", "q17-2", "q17-3", "q17-4", "q17-5", "q17-6", "q17-7", "q17-8", "q17-9", "q17-10"), # 対象列
  names_to = "q17", 
  names_prefix = "q17-",
  values_to = "q17_Ans"
) %>% filter(
  q17_Ans > 0
)
df_t$q17 <- as.numeric(df_t$q17)
df_t <- df_t %>% mutate(
  q17_category = case_when(
    q17 == 1 ~ "1.仕事",
    q17 == 2 ~ "2.急な病気やケガによる通院",
    q17 == 3 ~ "3.日常の通院",
    q17 == 4 ~ "4.行事・イベント",
    q17 == 5 ~ "5.飲酒",
    q17 == 6 ~ "6.接待",
    q17 == 7 ~ "7.時間短縮",
    q17 == 8 ~ "8.知らない場所の移動",
    q17 == 9 ~ "9.送迎",
    q17 == 10 ~ "上記以外の外出"
  )
)


upholders_t <- upholders %>% pivot_longer(
  cols = c("q17-1", "q17-2", "q17-3", "q17-4", "q17-5", "q17-6", "q17-7", "q17-8", "q17-9", "q17-10"), # 対象列
  names_to = "q17", 
  names_prefix = "q17-",
  values_to = "q17_Ans"
) %>% filter(
  q17_Ans > 0
)
upholders_t$q17 <- as.numeric(upholders_t$q17)
# upholders_t$q17 <- as.factor(upholders_t$q17)

opposits_t <- opposits %>% pivot_longer(
  cols = c("q17-1", "q17-2", "q17-3", "q17-4", "q17-5", "q17-6", "q17-7", "q17-8", "q17-9", "q17-10"), # 対象列
  names_to = "q17",
  names_prefix = "q17-",
  values_to = "q17_Ans"
) %>% filter(
  q17_Ans > 0
)
opposits_t$q17 <- as.numeric(opposits_t$q17)
# opposits_t$q17 <- as.factor(opposits_t$q17)

# g <- ggplot(df, aes(x = q14,fill = as.factor(IsUphold)))
# g <- g + geom_histogram(binwidth = 1,position = "dodge")
# g <- g + scale_fill_npg()
# plot(g)

g <- ggplot(df_t, aes(x = as.factor(q17_category), fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")  # geom_histogram -> geom_bar
g <- g + scale_fill_npg()
g <- g + labs(x = "移動に困った場面", y = "人数", fill = "ライドシェアへの立場")
plot(g)

ggsave("plot/useCase.png", plot = g, width = 8, height = 6, dpi = 300)

# 実はライドシェアに消極的な人も同様の悩みを抱えている→問題意識の潜在化。ライドシェアへの不理解。
# ライドシェアをしたら悪い影響があると思っている？

# そもそもこの人たちはタクシーをよく使わないんじゃない？あまり自分事として思っていない。
# q11 : タクシーの利用頻度/月
# q13 : (コロナ以降？)タクシーの利用が減った一番の理由はなんですか？
# q14 : 交通手段において不便を感じた回数/月　（タクシーが15分以上来ないも含む）
# q21 : タクシーに使う平均金額

df <- df %>% mutate(
  q11 = case_when(
    q11 == 1 ~ 0,
    q11 == 2 ~ 1,
    q11 == 3 ~ 2,
    q11 == 4 ~ 3,
    q11 == 5 ~ 5,
    q11 == 6 ~ 7
  ),
  q13 = case_when(
    q13 == 1 ~ "1.流しのタクシーがつかまらないから",
    q13 == 2 ~ "2.呼んでも来るのが遅い・来ないから",
    q13 == 3 ~ "3.価格が高いから",
    q13 == 4 ~ "4.他の交通手段が増えたから",
    q13 == 5 ~ "5.外出する頻度が減ったから",
    q13 == 6 ~ "6.その他",
    TRUE ~ "無回答"
  ),
  q14 = case_when(
    q14 == 1 ~ 0,
    q14 == 2 ~ 1,
    q14 == 3 ~ 2,
    q14 == 4 ~ 3,
    q14 == 5 ~ 5,
    q14 == 6 ~ 7
  ),
  q21 = case_when(
    q21 == 1 ~ 500,
    q21 == 2 ~ 750,
    q21 == 3 ~ 1250,
    q21 == 4 ~ 1750,
    q21 == 5 ~ 2250,
    q21 == 6 ~ 2750,
    q21 == 7 ~ 3000,
    # 選択肢8は`わからない`
    q21 == 8 ~ 0,
  )
)

g <- ggplot(df, aes(x = as.factor(q11),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + scale_fill_npg()
g <- g + labs(y = "人数",
              fill = "ライドシェアへの立場",
              x = "タクシーの利用回数"
)
plot(g)
ggsave("plot/UseNum_ALL.png", plot = g, width = 8, height = 6, dpi = 300)

dfTaxiUser <- df %>% filter(
  q11 > 0
)

g <- ggplot(data = dfTaxiUser, aes(x = 1, fill = as.factor(IsUphold))) +
  geom_bar(stat = "count", color = "black") +
  coord_polar(theta = "y")
g <- g + labs(y = "人数",
              fill = "ライドシェアへの立場",
              title = "タクシーを平均月に一回以上利用するユーザーに限定した場合"
              )
plot(g)
ggsave("plot/IssueRate_AU.png", plot = g, width = 8, height = 6, dpi = 300)

g <- ggplot(dfTaxiUser, aes(x = as.factor(q11),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + scale_fill_npg()
g <- g + labs(y = "人数",
              fill = "ライドシェアへの立場",
              x = "タクシーの利用回数",
              title = "タクシーを平均月に一回以上利用するユーザーに限定した場合"
)
plot(g)
ggsave("plot/UseNum_AU.png", plot = g, width = 8, height = 6, dpi = 300)

g <- ggplot(df, aes(x = as.factor(q14),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + scale_fill_npg()
g <- g + labs(x = "交通手段において不便を感じた回数/月",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "全人口対象"
)
plot(g)
ggsave("plot/troubleCase_ALL.png", plot = g, width = 8, height = 6, dpi = 300)

g <- ggplot(dfTaxiUser, aes(x = as.factor(q14),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + scale_fill_npg()
g <- g + labs(x = "交通手段において不便を感じた回数/月",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "タクシーのMonthly Active Userを対象としたとき"
              )
plot(g)
ggsave("plot/troubleCase_AU.png", plot = g, width = 8, height = 6, dpi = 300)

model <- feols(q14 ~ as.factor(IsUphold) + q11,data = dfTaxiUser)
summary(model)


dfTaxiUser <- dfTaxiUser %>%  filter(
  q21 > 0
)
df_temp <- df %>%  filter(
  q21 > 0
)

g <- ggplot(df_temp, aes(x = as.factor(q21),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "fill")
g <- g + labs(x = "タクシー一回当たりにかける料金",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "全員を対象としたとき"
)
plot(g)
ggsave("plot/howMuch_ALL.png", plot = g, width = 8, height = 6, dpi = 300)


g <- ggplot(dfTaxiUser, aes(x = as.factor(q21),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "fill")
g <- g + labs(x = "タクシー一回当たりにかける料金",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "タクシーのMonthly Active Userを対象としたとき"
)
plot(g)
ggsave("plot/howMuch_AU.png", plot = g, width = 8, height = 6, dpi = 300)

g <- ggplot(dfTaxiUser, aes(x = as.factor(q21),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + labs(x = "タクシー一回当たりにかける料金",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "タクシーのMonthly Active Userを対象としたとき"
)
plot(g)

df_reduce <- df %>% filter(
  q13 != "無回答"
)

df_reduceAU <- dfTaxiUser %>% filter(
  q13 != "無回答"
)


g <- ggplot(df_reduce, aes(x = as.factor(q13),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + scale_fill_npg()
g <- g + labs(x = "(コロナ以降？)タクシーの利用が減った一番の理由はなんですか？",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "全人口を対象としたとき"
)
plot(g)
ggsave("plot/whyReduce_ALL.png", plot = g, width = 8, height = 6, dpi = 300)

g <- ggplot(df_reduce, aes(x = as.factor(q13),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "fill")
g <- g + scale_fill_npg()
g <- g + labs(x = "(コロナ以降？)タクシーの利用が減った一番の理由はなんですか？",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "全人口を対象としたとき"
)
plot(g)

g <- ggplot(df_reduceAU, aes(x = as.factor(q13),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + scale_fill_npg()
g <- g + labs(x = "(コロナ以降？)タクシーの利用が減った一番の理由はなんですか？",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "AUを対象としたとき"
)
plot(g)
ggsave("plot/whyReduce_AU.png", plot = g, width = 8, height = 6, dpi = 300)

g <- ggplot(df_reduceAU, aes(x = as.factor(q13),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "fill")
g <- g + scale_fill_npg()
g <- g + labs(x = "(コロナ以降？)タクシーの利用が減った一番の理由はなんですか？",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "AUを対象としたとき"
)
plot(g)


# 不便を感じたことがある人に絞ったとき、価格を問題意識に持っている反対派が多いことに気づく
# 意識が潜在化してしまっている。
# 問題意識は持っているのに、ライドシェア反対。
# 反対派はほかの代替手段に頼る傾向が比較的強い。（それで足りている？）
# けど、絶対数としては（ライドシェアで改善できる）価格問題に対して、問題意識を持っている。
# """もっとそういう点を訴えていけば、意識が顕在化するかも。"""
# 安さ　＜　安全性　？
df_reduceIsIncov <- df %>% filter(
  q13 != "無回答",
  q14 > 0
)

g <- ggplot(df_reduceIsIncov, aes(x = as.factor(q13),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + scale_fill_npg()
g <- g + labs(x = "(コロナ以降？)タクシーの利用が減った一番の理由はなんですか？",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "全人口（不便を感じている）を対象としたとき"
)
plot(g)
ggsave("plot/whyReduce_ALL_inconv.png", plot = g, width = 8, height = 6, dpi = 300)


g <- ggplot(df_reduceIsIncov, aes(x = as.factor(q13),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "fill")
g <- g + scale_fill_npg()
g <- g + labs(x = "(コロナ以降？)タクシーの利用が減った一番の理由はなんですか？",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "全人口（不便を感じている）を対象としたとき"
)
plot(g)

df_reduceIsIncov_AU <- df %>% filter(
  q13 != "無回答",
  q14 > 0,
  q11 > 0
)

g <- ggplot(df_reduceIsIncov_AU, aes(x = as.factor(q13),fill = as.factor(IsUphold)))
g <- g + geom_bar(position = "dodge")
g <- g + scale_fill_npg()
g <- g + labs(x = "(コロナ以降？)タクシーの利用が減った一番の理由はなんですか？",
              y = "人数",
              fill = "ライドシェアへの立場",
              title =  "AU（不便を感じている）を対象としたとき"
)
plot(g)
ggsave("plot/whyReduce_AU_inconv.png", plot = g, width = 8, height = 6, dpi = 300)


# どのくらい損失があるのかは試算したい。
# 待ち時間×最低賃金（下限値）

# 平均使用額×乗れなくて困った数



# （追加分析）料金×困った頻度 ----------------------------------------------------------


# df_temp1 <- df_temp %>% filter(
#   # q11 > 0,
#   q14 > 0
# )
df_heatmap <- df_temp %>% group_by(
  q14,
  q21
) %>% mutate(
  Freq = n()
) %>% ungroup() %>% distinct(
  q14,
  q21,
  Freq
)
g <- ggplot(df_heatmap, aes(x = as.factor(q21), y = as.factor(q14), fill = Freq))
g <- g + geom_tile()
g <- g + scale_fill_gradient(low = "white", high = "red")
g <- g + theme_bw()
g <- g + labs(x = "タクシー一回当たりにかける料金",
              y = "交通手段において不便を感じた回数/月",
              title =  "損失度ヒートマップ"
)
plot(g)


df_pivot <- df_heatmap %>%  arrange(
  q21
) %>% pivot_wider(names_from = q21, values_from = Freq, values_fill = 0) %>% arrange(
  q14
)
ggsave("plot/heatMap_ALL.png", plot = g, width = 8, height = 6, dpi = 300)



df_temp_up <- df_temp %>% filter(
  IsUphold == "uphold"
)
df_heatmap_up <- df_temp_up %>% group_by(
  q14,
  q21
) %>% mutate(
  Freq = n()
) %>% ungroup() %>% distinct(
  q14,
  q21,
  Freq
)
g <- ggplot(df_heatmap_up, aes(x = as.factor(q21), y = as.factor(q14), fill = Freq))
g <- g + geom_tile()
g <- g + theme_bw()
g <- g + scale_fill_gradient(low = "white", high = "red")
g <- g + labs(x = "タクシー一回当たりにかける料金",
              y = "交通手段において不便を感じた回数/月",
              title =  "損失度ヒートマップ(uphold)"
)
plot(g)
ggsave("plot/heatMap_Uphold.png", plot = g, width = 8, height = 6, dpi = 300)

df_up_piv <- df_heatmap_up  %>%  arrange(
  q21
) %>% pivot_wider(names_from = q21, values_from = Freq, values_fill = 0) %>% arrange(
  q14
)





df_temp_op <- df_temp %>% filter(
  IsUphold == "opposit"
)
df_heatmap_op <- df_temp_op %>% group_by(
  q14,
  q21
) %>% mutate(
  Freq = n()
) %>% ungroup() %>% distinct(
  q14,
  q21,
  Freq
)
g <- ggplot(df_heatmap_op, aes(x = as.factor(q21), y = as.factor(q14), fill = Freq))
g <- g + geom_tile()
g <- g + scale_fill_gradient(low = "white", high = "red")
g <- g + theme_bw()

g <- g + labs(x = "タクシー一回当たりにかける料金",
              y = "交通手段において不便を感じた回数/月",
              title =  "損失度ヒートマップ(oppose)"
)
plot(g)
ggsave("plot/heatMap_Opposit.png", plot = g, width = 8, height = 6, dpi = 300)

df_op_piv <- df_heatmap_op %>%  arrange(
  q21
) %>% pivot_wider(names_from = q21, values_from = Freq, values_fill = 0) %>% arrange(
  q14
)

# （追加分析）決定木で賛成派、反対派の特徴だしをする -----------------------------------------------

# require(rpart)
# 
# df <- df %>% filter(
#   q21 > 0
# )

# C	3 	q1	Q1	あなたの性別をお知らせください。	SA	4 	1 	男
# 2 	女
# 3 	その他
# 4 	回答しない
# D	4 	q2	Q2	あなたの年齢をお知らせください。	SA	15 	1 	１５歳未満
# 2 	１５－１９歳
# 3 	２０－２４歳
# 4 	２５－２９歳
# 5 	３０－３４歳
# 6 	３５－３９歳
# 7 	４０－４４歳
# 8 	４５－４９歳
# 9 	５０－５４歳
# 10 	５５－５９歳
# 11 	６０－６４歳
# 12 	６５－６９歳
# 13 	７０－７４歳
# 14 	７５－７９歳
# 15 	８０歳以上

# df <- df %>% rename(
#   
# )
# 
# # データの処理なしで、シンプルに決定木を作成
# rpart_model <- rpart(
#   formula =  ~ .,
#   data = train.dt,  # 使用するデータ
#   method = 'class', # 分類問題
#   control = rpart.control(minsplit = 10, cp = 0.001)  # 木の複雑度を制御
# )



data <- c(16,3,11,29,4,64)
data
m <- mean(data) 
m
v <- var(data)
v
s <- sd(data)
s

summary <- summary(data)
summary
