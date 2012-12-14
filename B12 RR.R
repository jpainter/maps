# Stacie  B12 April 5

library(ggplot2)

rr<-data.frame(  
  
    group=c('Adult','Adult','Adult','Adult','Adult','Adult','Adult',
          'Children','Children','Children','Children','Children','Children','Children')  ,
  
    exp=c('Vegetarian' , 'Meat <1x/week' , 'Beef-free diet' , 'Egg-free diet' , 'Eggs <1x/week' , 'Dairy =7x/week' , 'Unilito =7x/week' ,
          'Vegetarian' , 'Meat <1x/week' , 'Beef-free diet' , 'Egg-free diet' , 'Eggs <1x/week' , 'Dairy =7x/week' , 'Unilito =7x/week' ) ,
    
    alphabet=c('g' , 'f' , 'e' , 'd' , 'c' , 'b' , 'a' ,
               'g' , 'f' , 'e' , 'd' , 'c' , 'b' , 'a'  ) ,
  
    rr=c(1.8, 1.6, 1.4, 1.4, 1.2, 1.5, 1.4, 2.9, 1.7, 0.7, 5.8, 2.5, 3.3, 3.1), 
    ll=c(1.4, 1.3, 1.1, 1.1, 0.9, 0.9, 1.0, 0.9, 0.7, 0.3, 2.6, 0.8, 0.5, 0.4), 
    ul=c(2.3, 2.1, 1.9, 1.8, 1.6, 2.5, 1.9, 9.7, 4.5, 1.7, 13.1, 8.5, 23.6, 22.3)
    )

dummy<-rep('',14)

# cap ul to 10
  rr$ul10=ifelse(rr$ul>10, 10, rr$ul)

ggplot(rr, aes(x = rr, xmin = ll, xmax = ul10, y = factor(alphabet))) +  
      geom_point(size=3) + 
      geom_segment( aes(x = ll, xend = ul10, y = factor(alphabet), yend=factor(alphabet)), lwd=1 ) + 
      geom_vline(xintercept=c(1), linetype="dotted") +
      theme_bw() + opts(axis.title.x = theme_text(size = 12, vjust = .25))+  
      xlab("Relative Risk") + ylab("Exposure") +  
      opts(title = expression("B12 Deficiency"), axis.text.y=theme_blank() ) +
      facet_grid(group ~ .)
