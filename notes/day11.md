### Thoughts ###
This puzzle is about cycles I think

Running this 10 times on just [0]

```
[0]
[1]
[2024]
[20,24]
[2,0,2,4]
[4048,1,4048,8096]
[40,48,2024,40,48,80,96]
[4,0,4,8,20,24,4,0,4,8,8,0,9,6]
[8096,1,8096,16192,2,0,2,4,8096,1,8096,16192,16192,1,18216,12144]
[80,96,2024,80,96,32772608,4048,1,4048,8096,80,96,2024,80,96,32772608,32772608,2024,36869184,24579456]
```

The length is now 10
We can see that every time we produce a 0 the cycle repeats inside
0 -> 1 -> -> 2[0]24 -> 1 -> 2 -> 2[0]24 -> 0
So every 0 is going to add an element every 3 cycles

Same for some other digits
```
[3]
[6072]
[60,72]
[6,0,7,2]
[12144,1,14168,4048]
[24579456,2024,28676032,40,48]
[2457,9456,20,24,2867,6032,4,0,4,8]
[24,57,94,56,2,0,2,4,28,67,60,32,8096,1,8096,16192]
[2,4,5,7,9,4,5,6,4048,1,4048,8096,2,8,6,7,6,0,3,2,80,96,2024,80,96,32772608]
[4048,8096,10120,14168,18216,8096,10120,12144,40,48,2024,40,48,80,96,4048,16192,12144,14168,12144,1,6072,4048,8,0,9,6,20,24,8,0,9,6,3277,2608]
```
3 -> 6072 -> ... -> [24,57,94,56,2,0,2,4,28,67,60,[3]2,8096,1,8096,16192] -> 3
So the 3 does 26 splits split every 8 cycles
But also it produces a bunch of other numbers which will cycle somehow


Much of the time, the same number keep appearing over and over again:
```
[5]
[10120]
[20482880]
[2048,2880]
[20,48,28,80]
[2,0,4,8,2,8,8,0]
[4048,1,8096,16192,4048,16192,16192,1]
[40,48,2024,80,96,32772608,40,48,32772608,32772608,2024]
[4,0,4,8,20,24,8,0,9,6,3277,2608,4,0,4,8,3277,2608,3277,2608,20,24]
[8096,1,8096,16192,2,0,2,4,16192,1,18216,12144,32,77,26,8,8096,1,8096,16192,32,77,26,8,32,77,26,8,2,0,2,4]
```

Maybe there aren't that many "seeds". Maybe you could generate some sort of map: of Integer -> Number of occurences
That's a start - we won't be operating over such a huge list then. After that we might be able to use some sort of memoization so we don't keep
calculating the same numbers over and over again
