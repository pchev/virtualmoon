
# use algorithm=nearest because average do not work with color image
# reduce to L4 size
reduce from=WAC_CSHADE_128P.cub to=l4.cub mode=total ons=40000 onl=20000 algorithm=nearest

# reduce to L3 size
reduce from=l4.cub to=l3.cub mode=total ons=20000 onl=10000 algorithm=nearest


