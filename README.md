# Haskell Shell


## Examples

Take the first line of a stream:

```
head -n 1
```

```
hs -d '\n' "L.take 1"
```

Take the last two lines of a stream:

```
tail -n 2
```

```
hs -d '\n' "L.reverse . L.take 2 . L.reverse"
```

Print the 10th element of each line:

```
awk '{print $10}'
```

```
hs -m "at 10 . vwords"
```

Print the elements from the 2nd to the 4th of each line:

```
awk '{for (i=2; i<5; i++){ printf "%s ",$i }; printf "\n"}'
```

```
hs -m "V.take 3 . V.drop 1 . vwords"
```

Get the number of words:

```
wc -w
```

```
hs "L.length . L.concatMap words"
```

Get the number of lines:

```
wc -l
```

```
hs -d '\n' "L.length"
```

Sort integers and remove duplicates:

```
sort -n | uniq
```

```
hs -d '\n' "L.nub . L.sort . L.map asInt"
```

Sum the 2nd elements of every line:

```
awk '{print $2}' | paste -sd+ - | bc
```

```
hs -d '\n' "P.sum . L.map (asFloat . at 1 . vwords)"
```

Split each line on a delimiter ':' and print the second element
([StackOverflow: bash split string on delimiter](http://stackoverflow.com/questions/15777996/bash-split-string-on-delimiter-assign-segments-to-array)):

```
hs -m "L.head . L.tail . split ':'"
```

Remove empty lines:

```
grep -v "^$"
```

```
hs -d '\n' "L.filter (not . null)"
```

Filter lines that match a pattern:

```
grep "t\\w\\wt"
```

```
hs -d '\n' "L.filter (`match` "t\\w\\wt")"
```
