const INPUT: &str = include_str!("..\\..\\Input\\day3.txt");

fn max_bit(nums: &[u32], bit: usize) -> u32 {
    let mut c = [0, 0];
    for &x in nums {
        c[(x as usize >> bit) & 1] += 1
    }
    (c[1] >= c[0]) as u32
}

fn part1(nums: &[u32]) -> u32 {
    let x = (0..12).map(|i| max_bit(nums, i) << i).sum::<u32>();
    x * (!x & 0xfff)
}

fn part2(nums: &[u32], oxygen: u32) -> u32 {
    let mut nums = nums.to_vec();
    for i in (0..12).rev() {
        let keep = max_bit(&nums, i) ^ oxygen;
        nums.retain(|x| (x >> i) & 1 == keep);
        if nums.len() == 1 {
            break;
        }
    }
    nums[0]
}

fn main() {
    let input = INPUT
        .lines()
        .map(|l| u32::from_str_radix(l, 2).unwrap())
        .collect::<Vec<_>>();
    let p1 = part1(&input);
    let p2 = part2(&input, 1) * part2(&input, 0);
    println!("Part 1 solution: {}", p1);
    println!("Part 2 solution: {}", p2);
}
