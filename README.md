马尔科夫链的复习

#  天气预测例子

假设有晴天，阴天，雨天三种天气，那么它们之间互相转换为另一种天气都有一定的概率.


| 当前状态\\下一状态 | 晴天 | 阴天 | 雨天 |
|------------------|------|------|------|
| **晴天**         | 0.6  | 0.3  | 0.1  |
| **阴天**         | 0.3  | 0.4  | 0.3  |
| **雨天**         | 0.2  | 0.3  | 0.5  |

那么上面的表格可以用矩阵表示：
  
$$
\begin{pmatrix}
0.6 & 0.3 & 0.1 \\
0.3 & 0.4 & 0.1 \\
0.2 & 0.3 & 0.5
\end{pmatrix}
$$

假设今天是晴天，请问三天后天气为各种情况的概率？

```
p = matrix(c(0.6,0.3,0.1,0.3,0.4,0.3,0.2,0.3,0.5),ncol=3)
current_state = c(1,0,0)
current_state %*% p^3
```
结果是： (0.421, 0.333, 0.246)

## 有趣的发现

假设我们想得知12天后各种天气的概率：

```
# 定义矩阵次方幂函数
matrix_power = function(x, n){
  if (n == 0) {
    # 返回与 x 尺寸相同的单位矩阵
    return(diag(nrow(x)))
  } else if (n > 0) {
    # 初始化结果矩阵为单位矩阵
    result = diag(nrow(x))
    for (i in 1:n) {
      result <- result %*% x
    }
    return(result)
  } else if (n < 0) {
    # 计算矩阵的逆矩阵
    inv_x = solve(x)
    # 初始化结果矩阵为单位矩阵
    result = diag(nrow(x))
    for (i in 1:abs(n)) {
      result = result %*% inv_x
    }
    return(result)
  } else {
    stop("n 必须是整数。")
  }
}
```

```
current_state %*% matrix_power(p,12)
```

结果是 ： (0.3888973, 0.3333333, 0.2777694)

那假如是第20天呢？

结果是 ： (0.3888889, 0.3333333, 0.2777778)

第25天？

结果是 ： (0.3888889, 0.3333333, 0.2777778)

神奇的收敛了(概率不再变化了)

# 稳定状态(收敛概率)

令 $$\pi = \pi*p$$

$\pi_1$ = 0.6 * $$\pi_1$$ + 0.3 * $$\pi_2$$ + 0.2 * $$\pi_3$$ 

$\pi_2$ = 0.3 * $$\pi_1$$ + 0.4 * $$\pi_2$$ + 0.3 * $$\pi_3$$ 

$\pi_3$ = 0.1 * $$\pi_1$$ + 0.3 * $$\pi_2$$ + 0.5 * $$\pi_3$$ 

$\pi_1$ + $\pi_2$ + $\pi_3$ = 1

解出答案： (0.3888889, 0.3333333, 0.2777778)
