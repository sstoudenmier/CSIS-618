flower(flox).
flower(petunia).
flower(rose).
flower(daisy).
type(flox, perennial).
type(petunia, annual).
type(rose, bush).
type(daisy, perennial).
type(daisy, annual).
perennial_garden(F) :- flower(F), type(F, perennial).
