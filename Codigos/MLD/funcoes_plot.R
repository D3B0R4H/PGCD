# Função de nível


plt_TS = function(data, titulo, nome){
  
  save_plot = ggplot( data, aes(x=xValue, y=yValue)) +
    geom_line( color="black", size=0.5, alpha=0.9, linetype=1) +
    labs(x="Mês", y = titulo)+
    scale_x_date(date_breaks = "3 month" , date_labels = "%Y \ %b")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size =14),axis.title.x = element_text(size=20),
          axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 14))

  ggsave(
    nome,
    path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
    width = 30,
    height = 20,
    units = "cm"
  )


}  

plt_hist = function(data, titulo, nome){
  
save_plot = ggplot(data, aes(x = yValue)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 10)+
  geom_density(alpha=.2, fill="blue")+
  labs(x=titulo, y = "Densidade")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 14))

ggsave(
  nome,
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30,
  height = 20,
  units = "cm"
)
  
}

plt_boxplot = function(data, titulo, nome){

save_plot = ggplot(data, aes(y = yValue)) + 
  geom_boxplot() + 
  scale_x_discrete() +
  labs( y = titulo)
theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
      axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 14))

ggsave(
  nome,
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30,
  height = 20,
  units = "cm"
)

}

plt_nivel = function(filtro_d,filtro_d2,nome,mes=12){

filtro_d["Ym"] = filtro_d2$yFilt_m1

save_plot = ggplot( filtro_d, aes(x=Data, y=Dados)) +
  labs(x="Mês", y = "Volume de chuva")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 14)) +
  geom_line(aes(y=Dados),col='darkgrey', size = 1)+
  geom_line(aes(y=yFilt_f),col='green', size = 1, linetype = "dashed")+
  geom_line(aes(y=Ym),col='blue', size = 1, linetype = "dashed")

ggsave(
  nome,
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30,
  height = 20,
  units = "cm"
)

}

plt_tendencia = function(filtro_d2,mes=93,nome){
  
  save_plot = ggplot( tail(filtro_d2,92), aes(x=Data, y=yFilt_m)) +
    labs(x="Mês", y = "Tendência")+
    theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 14)) +
    geom_line(aes(y=yFilt_m),col='blue', size = 1)+
    geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')+
    geom_line(aes(y=0),col='red', size = 0.5)
  
  ggsave(
    nome,
    path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
    width = 30,
    height = 20,
    units = "cm"
  )  
  
  
}

plt_prev2 = function(prev2,meses=18, nome, ylim1, ylim2){
  
  prev3=tail(prev2,meses)
  
  save_plot = ggplot( prev3, aes(x=Data, y=window.y..start...c.2014..1..)) +
    labs(x="Mês", y = "Volume")+
    ylim(ylim1, ylim2)+
    theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 14))+
    #  geom_line(aes(y=window.ySmooth.s...1...start...c.2014..1..),col='grey')+       # Suavizado
    geom_line(aes(y=Y_real),col='darkgrey', size = 0.5, linetype = "dashed")+ # O que ocorreu
    geom_line(aes(y=),col='darkgrey', size = 1)+                              # Dados reais
    geom_line(aes(y=yFore.f...1.),col='blue', size = 1)+                      # Previsão
    geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')                # Intervalo de credibilidade
  
  ggsave(
    nome,
    path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
    width = 30,
    height = 20,
    units = "cm"
  )
  
}

plt_prev = function(prev,meses=18, nome, ylim1, ylim2){
  
prev=tail(prev,meses)

save_plot = ggplot( prev, aes(x=Data, y=window.y..start...c.2014..1..)) +
  labs(x="Mês", y = "Volume")+
  ylim(ylim1, ylim2)+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 14))+
#  geom_line(aes(y=window.ySmooth.s..start...c.2014..1..),col='grey')+       # Suavizado
  geom_line(aes(y=Y_real),col='darkgrey', size = 0.5, linetype = "dashed")+ # O que ocorreu
  geom_line(aes(y=),col='darkgrey', size = 1)+                              # Dados reais
  geom_line(aes(y=yFore.a...1.),col='green', size = 1)+                      # Previsão
  geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'green')                # Intervalo de credibilidade

ggsave(
  nome,
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30,
  height = 20,
  units = "cm"
)

}


plt_s = function(suavizado, nome){
  
  save_plot = ggplot( suavizado, aes(x=Data, y=real)) +
    labs(x="Mês", y = "Volume de chuva")+
    theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 14)) +
    geom_line(aes(y=real),col='darkgrey', size = 1)+
    geom_line(aes(y=s1),col='green', size = 1, linetype = "dashed")+
    geom_line(aes(y=s2),col='blue', size = 1, linetype = "dashed")                

  ggsave(
    nome,
    path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
    width = 30,
    height = 20,
    units = "cm"
  )

}