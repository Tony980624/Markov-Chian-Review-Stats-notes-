p = matrix(c(0.6,0.3,0.1,0.3,0.4,0.3,0.2,0.3,0.5),nrow=3,byrow = T)
p
current_state = c(1,0,0)

matrix_power <- function(x, n){
  if (n == 0) {
    # 返回与 x 尺寸相同的单位矩阵
    return(diag(nrow(x)))
  } else if (n > 0) {
    # 初始化结果矩阵为单位矩阵
    result <- diag(nrow(x))
    for (i in 1:n) {
      result <- result %*% x
    }
    return(result)
  } else if (n < 0) {
    # 计算矩阵的逆矩阵
    inv_x <- solve(x)
    # 初始化结果矩阵为单位矩阵
    result <- diag(nrow(x))
    for (i in 1:abs(n)) {
      result <- result %*% inv_x
    }
    return(result)
  } else {
    stop("n 必须是整数。")
  }
}

current_state %*% matrix_power(p,25)


