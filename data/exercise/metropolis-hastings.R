# https://www.anarchive-beta.com/entry/2021/02/07/191522
library(tidyverse)
library(gganimate)
library(gifski)
library(png)

# 繰り返し回数を指定
K <- 100000

# 一様乱数の範囲(ステップ幅)を指定
c <- 4

# xの初期値を指定
x <- 0

# (重いので)表示する間隔を指定
n_interval <- 50

# フレーム数を計算:(割り切れるように要設定)
n_frame <- K / n_interval
n_frame

# Main loop
trace_x <- rep(0, n_frame)
anime_df <- tibble()
for(k in 1:K) {
  # xの更新候補を計算
  dash_x <- x + runif(n = 1, min = -c, max = c)
  
  # テスト値を生成
  r <- runif(n = 1, min = 0, max = 1)
  
  # メトロポリステストによりxを更新
  if(r < exp((0.5 * x^2) - (0.5 * dash_x^2))) {
    x <- dash_x
  }
  
  # 更新値を記録
  trace_x[k] <- x
  
  # 結果を記録
  if(k %% n_interval == 0) {
    
    # k番目までの更新値を格納
    tmp_df <- tibble(
      k = k, 
      x = trace_x
    )
    
    # 更新結果を結合
    anime_df <- rbind(anime_df, tmp_df)
    
    # 途中経過を表示
    print(paste0(k, " (", round(k / K * 100, 1), "%)"))
  }
}

true_df <- tibble(
  x = seq(-c, c, 0.01), # 描画範囲
  P_x = exp(-x^2 / 2) / sqrt(2 * pi) # 確率密度
  #P_x = dnorm(x = x, mean = 0, sd = 1) # 確率密度
)

# アニメーション用のヒストグラムを作図
hist_anime <- ggplot() + 
  geom_histogram(data = anime_df, aes(x = x, y = ..density..), 
                 binwidth = 0.05, fill = "purple", color = "purple") + # ヒストグラム
  geom_line(data = true_df, aes(x = x, y = P_x), 
            linetype = "dashed", color = "red") + # 真の確率密度
  gganimate::transition_manual(k) + # フレーム
  ylim(c(0, 1)) + # y軸の範囲
  labs(title = "Metropolis Method", 
       subtitle = "k={current_frame}", 
       x = "x", y = "P(x)")

# gif画像を作成
print(gganimate::animate(hist_anime, nframes = n_frame, fps = 25, renderer = gifski_renderer()))