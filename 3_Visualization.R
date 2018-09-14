library(tidyverse)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = year, y = cyl, colour = displ < 5))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~class, nrow = 2)
             
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv~cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(.~cyl)

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv), show.legend = F)
  
# Same dataset
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# Different dataset for layers, se is for confidence intervla around smooth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(stroke = 2) + 
  geom_smooth(se = F, aes(group = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(stroke = 2, aes(color = drv)) + 
  geom_smooth(se = F, aes(linetype = drv))

ggplot(data = diamonds) + 
  geom_pointrange(aes(x = cut, y = depth), stat = "summary",
                  fun.ymin = min, fun.ymax = max, fun.y = median)

# In barchart with proportion group must be set to 1
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop.., group = 1))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop.., group = color))

# In barchart position argument can be identity, fill and dodge
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "identity")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# Scetterplots: setting position to "jitter" adds random noise and reduces overplotting
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy))

# Another idea to fight overplotting is geom_count, changing sizes of dots
ggplot(data = mpg) + 
  geom_count(mapping = aes(x = displ, y = hwy))
