north_of(X1,X2):-location(X1,Y1,Z1), location(X2,Y2,Z2), Y2 < Y1.

location(new_york, 41,74).
location(chicago, 42,88).
location(tokyo,35,140).
location(oslo,60,11).
location(halto,0,80).
location(cairo,30,30).

