using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace AoC
{
    class Day9
    {
        char[][] lines = File.ReadAllLines("C:/Users/matth/Desktop/Advent Of Code 2021/Input/day9.txt").Select(x => x.ToCharArray()).ToArray();
        int[,] input;

        public Day9() {
            input = new int[lines.Length, lines[0].Length];
            for (int line = 0; line < lines.Length; line++)
                for (int chr = 0; chr < lines[0].Length; chr++)
                {
                    input[line, chr] = int.Parse(lines[line][chr].ToString());
                }

            Part1();
            Part2();
            Console.ReadLine();
        }

        (int, int)[] operators = new (int, int)[] { (1, 0), (-1, 0), (0, 1), (0, -1) };
        private void Part1()
        {
            int riskLevel = 0;

            // For each number, check if it is larger than it's neighbours.
            // Neighbours are defined in the 'operators' array above. 4-neighbourhood.
            // Using a flag to keep track of all four neighbours in a foreach iterator.
            for (int line = 0; line < input.GetLength(0); line++)
                for (int chr = 0; chr < input.GetLength(1); chr++)
                {
                    bool flag = true;
                    foreach ((int x, int y) in operators)
                        // Item is either OutOfBounds or compared to it's neighbour.
                        // OutOfBounds values are flagged as true, otherwise edge values could never be lowest.
                        flag &= outOfBounds(line - x, chr - y) || input[line - x, chr - y] > input[line, chr];

                    // If flag remains true, this value is the lowest in it's neighbourhood
                    if (flag)
                        riskLevel += 1 + input[line, chr];
                }

            Console.WriteLine($"Part 1 answer: {riskLevel}");
        }

        private void Part2()
        {
            Stack<(int x, int y, int i)> stack = new Stack<(int x, int y, int i)>();
            HashSet<(int x, int y)> visited = new HashSet<(int x, int y)>();
            Dictionary<int, int> basinSize = new Dictionary<int, int>();

            // Push all coordinates not equal to nine to the stack
            int fillAcc = 0;
            foreach ((int x, int y) in GetAllCoords(input))
            {
                if (input[x, y] != 9)
                    stack.Push((x, y, fillAcc++));
            }

            // Enter DFS
            while (stack.Count > 0)
            {
                // Coordinate plus fill index
                (int x, int y, int i) = stack.Pop();

                // Visited nodes and nines are not evaluated in this DFS
                if (visited.Contains((x, y)) || input[x, y] == 9)
                    continue;

                // Evaluate step
                // Increment the basin with the specified index by one
                // If the index is not found, set this index to one
                if (basinSize.ContainsKey(i)) basinSize[i]++;
                else                          basinSize[i] = 1;

                visited.Add((x, y));

                // Push neighbours to the stack
                // Only if they are in bounds and not visited
                foreach ((int u, int v) in operators)
                    if (!outOfBounds(x + u, y + v) && !visited.Contains((x + u, y + v))) 
                        stack.Push((x + u, y + v, i));
            }

            // Sort final sizes, and take only the largest three.
            // Use Linq Aggregate to multiply all numbers together.
            var SortedSizes = basinSize.Values.ToList(); SortedSizes.Sort();
            var LargestSizes = SortedSizes.TakeLast(3);
            Console.WriteLine($"Part 2 answer: {LargestSizes.Aggregate((x, y) => x * y)}");
        }

        private bool outOfBounds (int x, int y)
        {
            return x < 0                   || y < 0 
                || x >= input.GetLength(0) || y >= input.GetLength(1);
        }

        private static (int, int)[] GetAllCoords(int[,] inputArray)
        {
            (int, int)[] outputArray = new (int, int)[inputArray.GetLength(0) * inputArray.GetLength(1)];
            int acc = 0;
            for (int x = 0; x < inputArray.GetLength(0); x++)
                for (int y = 0; y < inputArray.GetLength(1); y++)
                {
                    outputArray[acc++] = (x, y);
                }
            return outputArray;
        }
    }
}
