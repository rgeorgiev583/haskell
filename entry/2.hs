-- Usage:  Load the source file in GHCi and enter the following input in the REPL to run the example tests:
-- *Main> isInLanguage "a^nb^n" "1122"
-- *Main> isInLanguage "a^nb^n" "aabb"
-- *Main> isInLanguage "a^nb^nc^n" "abb"
-- *Main> isInLanguage "a^nb^nc^n" "aabbcc"


isInLanguage language word  =  validate rules word (getFirstLength word (head word))
    where
        rules = map (\rule -> head rule) (split "^n" language)


validate []    []   expectedLength  =  True
validate []    word expectedLength  =  False
validate rules word expectedLength  =  helper rules word (head word) 0 expectedLength where
    helper rules word character length expectedLength
        | length < expectedLength  =
            if head word /= character then
                False
            else
                helper rules (tail word) character (length + 1) expectedLength
        | otherwise  =
            if not (null word) && head word == character then
                False
            else
                validate (tail rules) word expectedLength


getFirstLength []    letter  =  0
getFirstLength word  letter
    | head word /= letter    =  0
    | otherwise              =  1 + getFirstLength (tail word) letter


split [] string         =  [string]
split delimiter []      =  []
split delimiter string
    | position == -1    =  [string]
    | otherwise         =  take position string : split delimiter (drop (position + (length delimiter)) string)
    where
        position = find delimiter string


find needle haystack             =  helper needle haystack 0 where
    helper needle [] position    =  -1
    helper needle haystack position
        | match needle haystack  =  position
        | otherwise              =  helper needle (tail haystack) (position + 1)


match []     haystack               =  True
match needle []                     =  False
match needle haystack
    | head needle /= head haystack  =  False
    | otherwise                     =  match (tail needle) (tail haystack)
