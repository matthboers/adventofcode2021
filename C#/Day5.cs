using System;
using System.IO;
using System.Linq;

namespace AoC
{
    class Day5
    {
        ((int, int),(int, int))[] coordinatePairs;
        int[,] accumArr;

        public Day5()
        {
            string[] lines = File.ReadAllLines("C:/Users/matth/Desktop/Advent Of Code 2021/Input/day5.txt");
            //string[] lines = new string[] { "0,9 -> 5,9", "8,0 -> 0,8", "9,4 -> 3,4", "2,2 -> 2,1", "7,0 -> 7,4", "6,4 -> 2,0", "0,9 -> 2,9", "3,4 -> 1,4", "0,0 -> 8,8", "5,5 -> 8,2" };
            coordinatePairs = new ((int, int), (int, int))[lines.Length];
            accumArr = new int[1000, 1000];

            for (int i = 0; i < lines.Length; i++)
            {
                string[] splitString = lines[i].Split(" -> ");
                string[] coords1 = splitString[0].Split(',');
                string[] coords2 = splitString[1].Split(',');
                coordinatePairs[i] =
                    (
                        (int.Parse(coords1[0]), int.Parse(coords1[1])),
                        (int.Parse(coords2[0]), int.Parse(coords2[1]))
                    );
            }

            Part1();
        }

        private void Part1()
        {
            var nonDiagonals = coordinatePairs.Where(tup => tup.Item1.Item1 == tup.Item2.Item1 || tup.Item1.Item2 == tup.Item2.Item2).ToArray();

            foreach (((int x, int y) xy1, (int x, int y) xy2) tup in nonDiagonals)
            {
                bool vertical = tup.xy2.x == tup.xy1.x;         // X coordinate stays the same, thus line is vertical.
                int lower = vertical ? Math.Min(tup.xy1.y, tup.xy2.y) : Math.Min(tup.xy1.x, tup.xy2.x);   // Lower and upper bound decided by changing coordinate
                int upper = vertical ? Math.Max(tup.xy1.y, tup.xy2.y) : Math.Max(tup.xy1.x, tup.xy2.x);

                for (int i = lower; i <= upper; i++) // LEQ?
                {
                    if (vertical)
                        accumArr[tup.xy1.x, i]++;
                    else
                        accumArr[i, tup.xy1.y]++;
                }
            }

            int sum = Flatten2DArray(accumArr).Count(x => x > 1);

            Console.WriteLine($"Part 1 answer: {sum}");
        }

        public static T[] Flatten2DArray<T>(T[,] inputArray)
        {
            T[] outputArray = new T[inputArray.GetLength(0) * inputArray.GetLength(1)];
            for (int x = 0; x < inputArray.GetLength(0); x++)
                for (int y = 0; y < inputArray.GetLength(1); y++)
                {
                    outputArray[y + inputArray.GetLength(1) * x] = inputArray[x, y];
                }
            return outputArray;
        }

        public static void PrintArray<T>(T[,] inputArray)
        {
            int rowLength = inputArray.GetLength(0);
            int colLength = inputArray.GetLength(1);

            for (int i = 0; i < rowLength; i++)
            {
                for (int j = 0; j < colLength; j++)
                {
                    Console.Write(string.Format("{0} ", inputArray[i, j]));
                }
                Console.Write(Environment.NewLine + Environment.NewLine);
            }
        }
    }
}
