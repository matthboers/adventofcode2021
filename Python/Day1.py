f = open('C:\Users\matth\Desktop\Advent Of Code 2021\Input\day1-input.txt')

input = f.readlines()

intput = list(map(lambda x: int(x), input))

f.close()

larger = 0

for i in range(len(intput) - 1):
    if (intput[i + 1] > intput[i]):
        larger += 1;

print (larger)
    