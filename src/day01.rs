use std::time::{Duration, Instant};

// O(n^2) ~ 115.2µs (100 runs)
fn part_one_v1(nums: &Vec<usize>) -> usize {
    for x in nums.iter() {
        for y in nums.iter() {
            if x + y == 2020 {
                return x * y;
            }
        }
    }

    return 0;
}

// O(n) ~ 7.115µs (100 runs)
fn part_one_v2(nums: &Vec<usize>) -> usize {
    return get_product_v2(&nums, 2020);
}

// O(n)
fn get_product_v2(nums: &Vec<usize>, sum: usize) -> usize {
    let mut visited = [false; 2020];

    for n in nums.iter() {
        if n > &sum {
            continue;
        }
        let m = sum - n;
        if visited[m] {
            return n * (m);
        }

        visited[m] = true;
    }

    return 0;
}

// O(n^3) ~ 61.351709ms, (100 runs)
fn part_two_v1(nums: &Vec<usize>) -> usize {
    for x in nums.iter() {
        for y in nums.iter() {
            for z in nums.iter() {
                if x + y + z == 2020 {
                    return x * y * z;
                }
            }
        }
    }

    return 0;
}

// O(n^2) ~ 
fn part_two_v2(nums: &Vec<usize>) -> usize {
    for x in nums.iter() {
        let product = get_product_v2(&nums, 2020 - x);
        if product != 0 {
            return x * product;
        }
    }

    return 0;
}

fn benchmark(runs: u32, fun_to_test: &dyn Fn()) {
    let start = Instant::now();
    for _ in 0..runs {
        fun_to_test()
    }
    let end = Instant::now();

    println!("{:?}, ({} runs)", (end - start) / runs, runs)
}

pub fn main() {
    let input = "1753\n1976\n1574\n308\n1384\n1191\n1731\n1829\n1658\n1908\n1663\n2001\n1298\n1888\n1134\n1213\n965\n2009\n1071\n1591\n1402\n1184\n1836\n1536\n1038\n1871\n1354\n1149\n1863\n1728\n1896\n1599\n1556\n1222\n1909\n1858\n1754\n1947\n1907\n1656\n1135\n1845\n1504\n1473\n1401\n1700\n1067\n1790\n1783\n1539\n1087\n1614\n1856\n1895\n1564\n1106\n1204\n1492\n1361\n1897\n1977\n1210\n1867\n1797\n1232\n1148\n1520\n1989\n210\n1259\n570\n1512\n1894\n1309\n1154\n1327\n1817\n1875\n1702\n1885\n1664\n1220\n1208\n2000\n1178\n1423\n1454\n1780\n1710\n1362\n1816\n1491\n1363\n1478\n1648\n1163\n1554\n1195\n1500\n1320\n1698\n1636\n1097\n1573\n1846\n1747\n1138\n1083\n1505\n1387\n1900\n1143\n1905\n1826\n1735\n1496\n1687\n1704\n1916\n1991\n1750\n1637\n1742\n691\n1967\n1272\n1657\n1140\n1070\n1985\n1405\n1959\n1218\n1878\n1340\n1722\n2003\n1258\n1726\n1766\n1868\n1714\n1463\n2006\n1537\n1570\n1526\n1578\n1744\n1734\n1325\n196\n1935\n1849\n1424\n1972\n1602\n1859\n1341\n1177\n1901\n1902\n1247\n2004\n1350\n1965\n1407\n836\n1899\n1804\n975\n1510\n1898\n1560\n1777\n1523\n1822\n1830\n1855\n1839\n1482\n1661\n1835\n1343\n1278\n1449\n1136\n1732\n2008\n1686\n1775\n1952\n1444\n1499\n1680\n1752\n1597\n1963\n1117\n776".to_string();
    
    let nums: Vec<usize> = input
        .split("\n")
        .map(|n| n.parse().unwrap())
        .collect();

    // -- BENCHMARKS --

    // print!("part_one_v1 O(n^2): ");
    // benchmark(1000, &|| {
    //     part_one_v1(&nums);
    //     return;
    // });

    print!("part_one_v2 O(n): ");
    benchmark(1000, &|| {
        part_one_v2(&nums);
        return;
    });

    // print!("part_two_v1 O(n^3): ");
    // benchmark(100, &|| {
    //     part_two_v1(&nums);
    //     return;
    // });

    print!("part_two_v2 O(n^2): ");
    benchmark(1000, &|| {
        part_two_v2(&nums);
        return;
    });
}