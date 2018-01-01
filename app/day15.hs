
--- Day 15: Dueling Generators ---

-- Here, you encounter a pair of dueling generators. The generators, called generator A and generator B, are trying to agree on a sequence of numbers. However, one of them is malfunctioning, and so the sequences don't always match.

-- As they do this, a judge waits for each of them to generate its next value, compares the lowest 16 bits of both values, and keeps track of the number of times those parts of the values match.

-- To calculate each generator's first value,
-- it instead uses a specific starting value as its "previous value" (as listed in your puzzle input).
aInitial = 883 :: Int
bInitial = 879 :: Int

-- After 40 million pairs, what is the judge's final count?

main = pure ()
