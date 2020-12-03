


fn n_hit(trees: &Vec<char>, x_inc: u32, y_inc: u32) -> u32 {
    let mut trees_hit = 0;
    let mut x = 0;
    let mut y = 0;
    while y <= 322 {
        if trees[(31 * y + x) as usize] == '#' {
            trees_hit += 1;
        }
        x = (x + x_inc) % 31;
        y += y_inc;
    }
    return trees_hit;
}

fn part_one(trees: &Vec<char>) -> u32 {
    return n_hit(trees, 3, 1);
}

fn part_two(trees: &Vec<char>) -> u32 {
    return n_hit(&trees, 1, 1)
    * n_hit(&trees, 3, 1)
    * n_hit(&trees, 5, 1)
    * n_hit(&trees, 7, 1)
    * n_hit(&trees, 1, 2);
}

/*
0123456789A
..##....... 0
#...#...#.. 1
.#....#..#. 2
..#.#...#.# 3
.#...##..#. 4
..#.##..... 5
.#.#.#....# 6
.#........# 7
#.##...#... 8
#...##....# 9
.#..#...#.# A <- == 10 + 10*11 = 131

*/

pub fn main() {
    //let trees = "..##.......#...#...#...#....#..#...#.#...#.#.#...##..#...#.##......#.#.#....#.#........##.##...#...#...##....#.#..#...#.#".chars().collect();
    let trees = getTrees();

    println!("{}", part_two(&trees));
}

fn getTrees() -> Vec<char> {
    // 31 x 323
    return ".........###......#...#.......#.#.#...........#..#..#.........#.......#.................#.............#.#.........#......###.....#......##...##...............##...#......#.....#.....##..#.#..#....#...#....#......#............##.....#.....#.#.............#.....#......#...##.#.......####...#.......##.....#.#............#....#......##..........##...........###.#................#............#............#.#..#..##........#.....#..#........................#....##..#...........##.....###...#.#.#....#......#...........#..........##.##...#...#......##.#.............#..#......#.#.#..#.#.............#.#..#..........#...#.#...........##...........#....#............#...##.#...................#...........#..................#......#..#...#....#..#...#.....#.#....#.#......#...#.#.#..........###..#....#................##..#..#...#..#....#..##..........#...#..#.#........#...#.#........##.##....##.#.....#.##....#...#.......#.#..#....#......##...#.#.#.#.....#....#............#..............#.........................###.#......#.....#...#...#.#.......#....#........#..#...................#........##.##........#......#.....#...##.#...#...........#......#.........#...#....##......#.......#..............#..#..#.#......##...#...#...##....#......#.#....#...#......##.....#...##.#...#.#..................#....#.##.....#......#..........#.....#..#.......#...................#.....#.........#........#..........#...##.##.#..#..##.#..#.#.............#.........#.#......#..##..........#..#..##.#.#...#......#.......#...#.....##.#......#......#...#...........###..........#.........#.....#.........#..........#.......##...#.......#..#..#..............#....#.####..#...##...#.#............#....#.......#........#.....#....#.##.....#..#.....#.#.#..#........#..#..##.......#.........#.....#...........#.....#.............#..........##..##.#.....#.....#........#.....#..#.......#..........#...#......##..#...#............#.........#....#.........#..###.......#.....###.##.#...#.#..#..#..#.#.##...#....#....#...#..#......##......................##.....##..............##.#..###.#....#...#...#.##........#...#......#...#.##...#....#......#.....###.##.#..........#..#.#.##....#..##.....##.....#...#...#..........##................#......#...#.....##.......##....##.#.............#.##.........#.##.............#..#.....#.........##.#...#.#.#..............#..#......#..#.....#.....#....#....#....#.......###.....#...#..##....#..##..#...##..###......#...........###......#..................####.#....#.#....#.#.....#.#....#..#..........#......#....##......#..#..#.#.#...#.##.....#.#.......##..#..........##.................#..#..##.#....#.#.............#.......................####.#.#..#.......#..#...#..#..#.....#...#.....#.#.#........#....#...##........#..#....#......#..##.................#......##.#....#..#...#..........#..#...........#...........#.............###........##.#.#......#.#..#....##..#..#..........#........#......#...#...........####......##..#.#...##......##...#..#.##.............#...#.....##.....###..##...#.##.....#....##....#.........#....##..#.....#.#......#.#......#.......#...#....#...#.#................#.........#...........#..#....##..#....#....#.....#.......#..#....##....#.........#.........#...........##....##.#........#.#...............##..#...##.#...............#.......#....#..#......#..#.###...##..#..#.........#.#......#.....#..#......#...........##........##..#.........#................#..#...#...............#...#....#..#.#......##.........#.#.................#.....................#.....#..#...#.#.#.......#...#..#..........#..................#.#.....#.#......#...#.....##......#.#..##...##..#..###...#........#......#.#......#.##.....#.#......#...#.......#....#..............#....#.#..#.....##...#...........#.#....#.##....#.#.#.#....#..#.........###....#.................#..##.......#...........................#.##.#..##...............##....#..#.#...#.#..#.##...#.............#......#...........#............#....#......#........##....#.#.##..#.#..#........#....#....#....#.#.....#.##....#.....#..#...#........#...#..........#..###......#.#.......#........#........#.......##.####..........#.......#.#..#......##..#.........#..#..#...##.#.......#...#.##...#.##.#..........#..#.#.#...............#......#............#.............###...#.......#.............#.##....#....#...............#.#....##.....#.....#.........#.............#......#........#....#...#....##..#......#......#.....#.#............#.............#........##.........#...#.......#........#..#.#.#...##.##....#...#..#.......#....##....#...##.#.#.....#.......#............#........#.#.....#...##.....#....#..##......#.#.....#...#....#.....#......#.....#.......#.#....#......#...##.............#.......#...#...............#........#........#.............#.#.#......#...#..#..........#..##...#.........#........#..#..#.#.#...#.#.......#.....#...#.....#..............#..........#.#.#...#.###.............#......................#.....###.##.#........#..........#....#..#..........##....#..#..##..............#...#.....######.......#......##.....##.#...#.........#.#.#.......#...#..#.#.#...........#........###.............#...#.##.....#.........#.............#..#...#.....#.......................#....#.....#......#.#....#...#....#........##...#.......#...##.#...#.....#................#.##....##..#.........#...........#..#.#....#...#......#.#.............##..##...#..#..###..#...........#.........####....#.#...........#...........###...........#................#..........#....#............#....#.#...#...#.......#...............#.....##.#.......#.#...........#.......#.#.#.#..#...#.............####.#...#.#......#.....##....##...#.....#.#......#..#..............#..#....#......###......................#....##...#.....#......#........#...##..#..##...#.........#.#......#......#........#...#....#......#.....#..#.......#...............##........##......#...........##.#.......#..#....#....#.##..........#....#..#.#.###....#..........#......#..#..............#.....#..........#...#..#.#...#.........#......#..#......#....#.......##.....#............####..#.......#.#...#....#..#...#..#.#.......##.......................#.##........#...........####..#.....#......#.......#.#....#...#.......#....#.....#....#...##......#..##.#...........#..#...#...........#.##.#.#...#.#..#.....#.......#.#....#..#.............#.......##..#.............#.....#..#....#....#...#....#....#......#.#...##..........#..#....#.#.......#.........#......#.#...............#.............#....##..#.......................##....#............#......#..........#....#..##......##......#..##.....#..#..........#.........#.........#.......#..##.........#.##.....#.#...#...#.....#.##...........#.#..#...#.#..........#.............##...#.#..#.....#....#......................#..#...##.#.......#.##..#.###.....##.#.#...##........##...#.........##..#..#.....#..#.#..#...#.......##...............##.#...##......#..#....#...##..#..#.###.........##..#...#.....##.......#..........#...#..##.#........##.#........#...#..........##.......................##.#........#...#...#..###.#.......#.##....#....#.#..........#.##.......#..........##...#....#............#.....#.....#..#.........#..##..##..#..#....#..#.......##.............#............##.......#.#.#.......###.........#.....##.#..........#.#...#.#......#........#..#.#.#..#.............##...#.....##.......#..#..#....#......#..#.......##.#.#.............#.........##........#..........##..................##....##.....#................#......#..................##...#.#.........#..............#.........#......#..#..#....#..#...##..#.##......##...#.#......#.#......#.#...###....#...#.#..#....#....#..#.......#.....#..##.#.#.#.#.#..#.......#####.#..##..#..#..........#.....#..#.#..#......#......#...#..#.#..#..#..........#....##...#..........#.##.#.#.##..#...#..................#.......#.###..#..#..#.......#......#....#...#..#............####.........#........#.......#......#..#.................#....##...#.#.............##......#...##....#.##.............##......#............#..#..#.....#.........#........#...#.....#.#...##..#.##..#.....................##.#........#...#..#...#.#.........#..#...........##.....#...........#.#....#..##...#.....#.........#......#.###..##.............#.......#...##.##.....#....#.....##....#........#..##......#..#..#..#..#.#...#...#..............##...#......#.........#..#..##....#.....#...........##........##....#.#........#........#.#.#....#....................#...#.......#...#.#......#......##.##..##...........###......#...#......#.......#...........#....##.#.......#.........#.................##.#.#..#...#...#...##.##...........#.........##.#.#...#..........#.#....#....##.....#.....#..##..#............#.........#.........#.#...##..#...#.#.....#.........###..#......#.#.##.#...................#.......####....#.......#.......#.........#..#..#....#..##..........#..........#...##........#...........#..#....#.....#....#..#.#.................#....#...........#......#.....#...##.........#..............#...........###...........##.#...........#....####.......#...#....#.#...#...##.#................#...........#..#....#.....#.....##..#...##.#....#....##..........#.#..#...#....#.....#..................#...#....#..#..#.##.##..#........#....#.##.....#...#......#.......#................#..#..#....##..#...#....#.#.....#..#...#...#..............#.#.....#.#.....#.........#.##...#.#....#....................#..##.#.......#.....#.....#.............#..#..........####....###..##...#........#........#...#......##..##......#.#..........#....#.#...###................#.#....#....#..#.##.#.............#..#..........#............#.....##.........#.....#....#.........#.....#..#...........###.#....#........#............##...#........##..#....#..#....#.....#.......#...#..#.#.#.##.#..#...#.....#.............#..#....................#.#......#..##........#....................#...............#.......#.......#....#.........#...........#....#.................#..#..........#....#..##............#.#.#.........#......#.....#...##.....#.#.......#...#.........#....#...#.......#....".chars().collect();
}