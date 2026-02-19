# Create a match object

Create a match object

## Usage

``` r
create_match(
  id,
  round,
  position,
  participant1 = NA_character_,
  participant2 = NA_character_,
  next_match = NA_integer_,
  next_slot = NA_integer_,
  bracket_type = "main",
  loser_next_match = NA_integer_,
  loser_next_slot = NA_integer_
)
```

## Arguments

- id:

  Match ID

- round:

  Round number

- position:

  Position within round

- participant1:

  First participant (or NA for TBD)

- participant2:

  Second participant (or NA for TBD)

- next_match:

  ID of match winner advances to (or NA for final)

- next_slot:

  Which slot (1 or 2) in next match

- bracket_type:

  For double elim: "winners", "losers", or "grand_final"

## Value

A bracket_match object
