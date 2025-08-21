ggplot() +
  geom_tile(
    rbr_test %>% 
      filter(
        site == 'Paint Lake - Shallow'
      ), 
    mapping = aes(yday, depth, fill = temp)
  )+
  # geom_line(
  #   test3,
  #   mapping = aes(x = yday, y = depth),
  #   color ='black',
  #   linewidth = 2
  # )
  scale_y_reverse()+
  geom_line(
    rbr_test %>%
      filter(
        site == 'Paint Lake - Shallow'
      ),
    mapping = aes(yday, z_temp4c),
    linewidth = 2
  )+
  
  #scale_fill_cmocean(name = 'balance', direction = 1)+
  
  # scale_fill_gradient2(
  #   low = cmocean("balance")(100)[15], # Choose a low color from the palette
  #   #mid = cmocean("balance")(100)[50],                 # Set the midpoint color
  #   high = cmocean("balance")(100)[85], # Choose a high color from the palette
  #   midpoint = 4,                  # Specify the value at which the midpoint color appears
  #   #name = "Z Value"
  # ) +
  
  scale_fill_gradient2(
    low = 'blue', # Choose a low color from the palette
    mid = "yellow",                 
    high = 'red', 
    midpoint = 4,                

  ) +
  
  
  coord_cartesian(expand = FALSE)+
  xlab('')+
  ylab('Temperature \u00b0C')+
  xlim(c(10, 90))+
  facet_wrap(
    ~year, 
    ncol = 2
  )+
  theme_classic()+
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    strip.text = element_blank()
  )#+
  # guides(
  #   fill = guide_colorbar(ticks.colour = NA)
  # )

test <- rbr %>% 
  filter(
    site == 'Paint Lake - Shallow'
  )

max(test$temp, na.rm = T)
min(test$temp, na.rm = T)

"#181C43FF" "#1A204AFF" "#1D2352FF" "#1F265AFF" "#212A62FF" "#232D6BFF" "#253074FF"
[8] "#27347DFF" "#283786FF" "#293B8FFF" "#293E99FF" "#2842A3FF" "#2646ABFF" "#224AB4FF"
[15] "#1B50BAFF" "#1456BDFF" "#0C5BBEFF" "#0A61BEFF" "#0B66BDFF" "#106BBCFF" "#1771BBFF"
[22] "#1E76BBFF" "#257ABAFF" "#2B7FBAFF" "#3283BAFF" "#3988BAFF" "#3F8CBAFF" "#4791BAFF"
[29] "#4E95BAFF" "#5599BAFF" "#5C9DBBFF" "#64A1BBFF" "#6CA5BCFF" "#75AABEFF" "#7CADBFFF"
[36] "#85B1C0FF" "#8DB4C2FF" "#95B8C4FF" "#9DBBC6FF" "#A5BFC9FF" "#ADC4CCFF" "#B4C8CEFF"
[43] "#BCCCD2FF" "#C3CFD4FF" "#CBD4D8FF" "#D2D8DBFF" "#D9DCDEFF" "#E1E1E3FF" "#E7E6E6FF"
[50] "#EEEAEAFF" "#F0EAE9FF" "#EDE4E1FF" "#EBDEDBFF" "#E8D7D3FF" "#E5D1CCFF" "#E4CBC5FF"
[57] "#E1C6BDFF" "#E0C0B5FF" "#DEBAAEFF" "#DCB4A7FF" "#DAAD9FFF" "#D8A897FF" "#D7A290FF"
[64] "#D59C89FF" "#D39681FF" "#D1907AFF" "#CF8A72FF" "#CE846CFF" "#CC7E64FF" "#CA795DFF"
[71] "#C87357FF" "#C76D50FF" "#C46749FF" "#C26042FF" "#C05A3DFF" "#BE5437FF" "#BC4D32FF"
[78] "#B9472DFF" "#B74029FF" "#B33926FF" "#B03224FF" "#AC2C24FF" "#A82524FF" "#A31F25FF"
[85] "#9E1A26FF" "#991627FF" "#921228FF" "#8C0F29FF" "#860E29FF" "#7F0E29FF" "#780E28FF"
[92] "#710E27FF" "#6B0E25FF" "#630E23FF" "#5D0E20FF" "#560E1EFF" "#500D1BFF" "#490C18FF"
[99] "#420A15FF" "#3C0912FF"
