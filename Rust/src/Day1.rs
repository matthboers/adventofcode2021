// From path ./Rust/src (location of the *.rs file)
// To path   ./Input
const INPUT: &str = include_str!("..\\..\\Input\\day1.txt");

fn main() {
    let list_of_ints: Vec<i32> = INPUT
        .lines() // Every newline \n is a new element in an array
        .map(|f| f.parse::<i32>().unwrap()) // Potentially something similar to 'Maybe' in Haskell? Parse returning a maybe monad?
        .collect(); // Transforms 'iterator' into collection

    part2(list_of_ints);
}

fn part1(list_of_ints: Vec<i32>) {
    let mut increase_count = 0; // Ugly and naïve solution, uses mutable
    for x in 1..list_of_ints.len() {
        if list_of_ints[x] > list_of_ints[x - 1] {
            increase_count += 1;
        }
    }

    println!("Part 1 Answer: {}", increase_count);
}

fn part2(list_of_ints: Vec<i32>) {
    let groups_of_three: Vec<i32> = list_of_ints
        .windows(3) // Window method, returns all adjacent groups of three
        .map(|w| w.iter().sum()) // Returns the sum of all groups of three
        .collect(); // Transform to Vector
    let part2_answer = groups_of_three
        .windows(2) // All adjacent combinations of the previous groups of three
        .filter(|f| f[0] < f[1]) // Return only the tuples where the newer element is larger than the previous
        .count(); // Count the amount of elements left. Answer

    println!("Part 2 Answer: {}", part2_answer);
}
