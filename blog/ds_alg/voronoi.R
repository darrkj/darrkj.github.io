df <- data.frame(x = c(1, 4, 3, 1, 4, 2),
                 y = c(1, 1, 3, 4, 4, 2))


plot(df$x, df$y, ylim = c(0, 5), xlim = c(0, 5))


df <- df[order(df$x), ]


q <- Queue$new()
