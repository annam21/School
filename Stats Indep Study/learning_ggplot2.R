set.seed(42)
small <- diamonds[sample(nrow(diamonds),1000),] 

ggplot(small) +
  geom_point(aes(x = carat, y = price, colour = cut)) +
  scale_y_log10() +
  facet_wrap(~cut, nrow = 1) +
  facet_grid(cut~color) +
  ggtitle("First example")

  
p<-ggplot(diamonds,aes(x=carat,y=price,colour=cut))
p<-p+scale_x_log10()+scale_y_log10()
p<-p+geom_point(alpha=0.3)+geom_smooth(method="lm",colour='black')
p<-p+facet_wrap(~cut)
print(p)
