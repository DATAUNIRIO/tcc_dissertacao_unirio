# https://kevin-kotze.gitlab.io/tsm/ts-2-tut/
# https://terrytangyuan.github.io/2018/02/12/autoplotly-intro/
library(autoplotly)
library(strucchange)
autoplotly(breakpoints(Nile ~ 1), ts.colour = "blue", ts.linetype = "dashed",
           cpt.colour = "dodgerblue3", cpt.linetype = "solid")