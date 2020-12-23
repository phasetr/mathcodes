# https://qiita.com/xr0038/items/80e30db5fa7bcd391b18
### convert (r,g,b) to #RRGGBB
rgb(r,g,b) = sprintf("#%02x%02x%02x",r%256,g%256,b%256);
### convert (h,s,v) to #RRGGBB
hsv_hi(h) = floor((h%361)/60.)%6
hsv_f(h) = (h/60.)-floor(h/60.)
hsv_p(s,v) = floor(v*(1.-(s/255.))+0.5)
hsv_q(h,s,v) = floor(v*(1.-(s/255.)*hsv_f(h))+0.5)
hsv_t(h,s,v) = floor(v*(1.-(s/255.)*(1.-hsv_f(h)))+0.5)
hsv(h,s,v) = hsv_hi(h)==0?rgb(v,hsv_t(h,s,v),hsv_p(s,v)):\
             hsv_hi(h)==1?rgb(hsv_q(h,s,v),v,hsv_p(s,v)):\
             hsv_hi(h)==2?rgb(hsv_p(s,v),v,hsv_t(h,s,v)):\
             hsv_hi(h)==3?rgb(hsv_p(s,v),hsv_q(h,s,v),v):\
             hsv_hi(h)==4?rgb(hsv_t(h,s,v),hsv_p(s,v),v):\
             rgb(v,hsv_p(s,v),hsv_q(h,s,v))
###