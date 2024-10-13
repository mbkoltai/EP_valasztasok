### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### function to calculate x and y position of centre points of hexagons --------------

fcn_corner_calc <- function(x_val,y_val,l_par) {
  l_par$r=l_par$l*sqrt(3)/2
  data.frame(p_x=c(p1=x_val-l_par$r,
                   p2=x_val,
                   p3=x_val+l_par$r,
                   p4=x_val+l_par$r,
                   p5=x_val,
                   p6=x_val-l_par$r),
             p_y=c(p1=y_val+l_par$l/2,
                   p2=y_val+l_par$l,
                   p3=y_val+l_par$l/2,
                   p4=y_val-l_par$l/2,
                   p5=y_val-l_par$l,
                   p6=y_val-l_par$l/2))
}
