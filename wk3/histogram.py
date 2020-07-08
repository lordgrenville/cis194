# warming up by doing it imperatively. pretty ugly!

bars = []
li = [1, 1, 1, 5]

# each pass through the list counts one of each unique number and then removes
# them, until list is empty
while li != []:
    arr = '          '
    done = []
    for idx, x in enumerate(li):
        if arr[x] == ' ':
            arr = arr[:x] + '*' + arr[x + 1:]  # create new string with *
            done.append(idx)
    li = [x for idx, x in enumerate(li) if idx not in done]  # remove used
    bars.append(arr)
for bar in bars[::-1]:  # flip the list
    print(bar)
print('0123456789')
print('==========')
