const INPUT: &str = include_str!("..\\..\\Input\\day3.txt");

fn main() {
    let input_list: Vec<&str> = INPUT.lines().collect();

    part1(input_list.clone());
    part2(input_list.clone());
}

fn part1(input_list: Vec<&str>) {
    let mut true_bit_counter = [0; 12];
    let mut false_bit_counter = [0; 12];

    for elem in input_list.iter() {
        for (i, bit) in elem.chars().enumerate() {
            if bit == '1' {
                true_bit_counter[i] += 1;
            } else {
                false_bit_counter[i] += 1;
            }
        }
    }

    let comparison_array = true_bit_counter.iter().zip(false_bit_counter.iter());

    let gamma: Vec<bool> = comparison_array
        .into_iter()
        .map(|(lhs, rhs)| lhs > rhs)
        .collect();

    let epsilon: Vec<bool> = gamma.clone().into_iter().map(|b| !b).collect();

    let solution = boollist_to_int(gamma) * boollist_to_int(epsilon);

    println!("Part 1 solution: {}", solution);
}

fn part2(input_list: Vec<&str>) {
    let mut true_bit_counter = [0; 12];
    let mut false_bit_counter = [0; 12];

    for elem in input_list.iter() {
        for (i, bit) in elem.chars().enumerate() {
            if bit == '1' {
                true_bit_counter[i] += 1;
            } else {
                false_bit_counter[i] += 1;
            }
        }
    }

    let comparison_array = true_bit_counter.iter().zip(false_bit_counter.iter());

    let gamma: Vec<bool> = comparison_array
        .clone()
        .into_iter()
        .map(|(lhs, rhs)| lhs >= rhs)
        .collect();

    let epsilon: Vec<bool> = comparison_array
        .clone()
        .into_iter()
        .map(|(lhs, rhs)| lhs <= rhs)
        .collect();

    let mut oxygen_list = input_list.clone();
    let mut co2_list = input_list.clone();

    let mut index: usize = 0;

    while oxygen_list.len() > 2 {
        let oxygen_char = if gamma[index] { '1' } else { '0' };
        oxygen_list = oxygen_list
            .into_iter()
            .filter(|s| s.chars().nth(index).unwrap() == oxygen_char)
            .collect();
        index += 1;
    }

    index = 0;
    while co2_list.len() > 2 {
        let co2_char = if epsilon[index] { '1' } else { '0' };
        co2_list = co2_list
            .into_iter()
            .filter(|s| s.chars().nth(index).unwrap() == co2_char)
            .collect();
        index += 1;
    }

    let oxy_rating = boollist_to_int(str_to_boollist(co2_list[0]));
    let co2_rating = boollist_to_int(str_to_boollist(oxygen_list[0]));
    let solution = oxy_rating * co2_rating;

    println!("Part 2 solution: {}", solution);
}

fn boollist_to_int(boollist: Vec<bool>) -> i32 {
    let mut result: i32 = 0;
    for (i, elem) in boollist.iter().enumerate() {
        let ex = *elem as i32;
        result += 2i32.pow(11 - i as u32) * ex;
    }

    result
}

fn str_to_boollist(stringput: &str) -> Vec<bool> {
    stringput.chars().map(|c| c == '1').collect()
}
