const INPUT: &str = include_str!("..\\..\\Input\\day2.txt");

fn main() {
    let transformations: Vec<(i32, i32)> = INPUT
        .lines()
        .map(|x| x.split_at(x.find(' ').unwrap()))
        .map(|(d, s)| parse_input(d, s.trim()))
        .collect();
    part2(transformations);
}

fn part1(transformations: Vec<(i32, i32)>) {
    // Sum up all coordinate tuples
    let final_pos = transformations
        .iter()
        .fold((0, 0), |(u, v), (x, y)| (u + x, v + y));

    println!("Part 1 solution : {}", final_pos.0 * final_pos.1)
}

fn part2(transformations: Vec<(i32, i32)>) {
    let transformations3pair: Vec<(i32, i32, i32)> =
        transformations.iter().map(|(u, v)| (*u, *v, 0)).collect();

    // Tuple (aim, horizontal, depth)
    // Sum up all coordinate tuples
    let final_pos = transformations3pair
        .iter()
        .fold((0, 0, 0), |(u, v, w), (x, y, _)| {
            ((u + x), (v + y), (w + u * y))
        });

    println!("Part 2 solution : {}", final_pos.1 * final_pos.2)
}

// Tuple (depth, horizontal)
fn parse_input(direction: &str, scalar_str: &str) -> (i32, i32) {
    let scalar: i32 = scalar_str.parse().unwrap();
    match direction {
        "down" => (scalar, 0),
        "up" => (-scalar, 0),
        "forward" => (0, scalar),
        _other => (0, 0),
    }
}
