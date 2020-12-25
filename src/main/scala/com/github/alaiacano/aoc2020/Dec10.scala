package com.github.alaiacano.aoc2020

object Dec10 extends App {
  type Adapter = Int
  type Jolt = Int
  def loadAdapters(input: String): Set[Adapter] =
    input.split("\n").map(i => i.toInt).toSet

  def findNextAdapterDifference(
      currentJolts: Jolt,
      remainingAdapters: collection.mutable.Set[Adapter]
  ): Int = {
    currentJolts match {
      case j if remainingAdapters.contains(j + 1) =>
        1
      case j if remainingAdapters.contains(j + 2) =>
        2
      case j if remainingAdapters.contains(j + 3) =>
        3
      case j =>
        throw new Exception(
          s"no adapter available for current joltage $j in $remainingAdapters"
        )
    }
  }

  def getUsedAdapters(initialAdapters: Set[Adapter]): Seq[Adapter] = {
    val usedAdapters = collection.mutable.Buffer.empty[Adapter]
    val maxJoltage = initialAdapters.max + 3

    var remainingAdapters: collection.mutable.Set[Adapter] =
      collection.mutable.Set()
    initialAdapters.foreach(remainingAdapters.add(_))

    var currentJoltage: Jolt = 0
    do {
      val nextAdapterDifference =
        findNextAdapterDifference(currentJoltage, remainingAdapters)
      usedAdapters.addOne(nextAdapterDifference)
      currentJoltage += nextAdapterDifference
      remainingAdapters.remove(currentJoltage)
    } while (currentJoltage < initialAdapters.max)
    
    // It can always go 3 higher than the maximum adapter in the bag.
    usedAdapters.addOne(3)
    usedAdapters.toSeq
  }

  def partOne(adapters: Set[Adapter]): ((Int, Int), Int) = {
    val usedAdapters = getUsedAdapters(adapters)
    val adapterCount = usedAdapters.foldLeft((0, 0)) {
      case ((ones, threes), adapter) =>
        adapter match {
          case 1 => (ones + 1, threes)
          case 3 => (ones, threes + 1)
          case _ => (ones, threes)
        }
    }
    (adapterCount, adapterCount._1 * adapterCount._2)
  }
  println(s"part one demo1: ${partOne(loadAdapters(Dec10Input.demoInput1))}")
  println(s"part one demo2: ${partOne(loadAdapters(Dec10Input.demoInput2))}")
  println(s"part one actual: ${partOne(loadAdapters(Dec10Input.input))}")
}

object Dec10Input {
  val demoInput1 = """16
10
15
5
1
11
7
19
6
12
4"""

  val demoInput2 = """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""

val input = """8
131
91
35
47
116
105
121
56
62
94
72
13
82
156
102
12
59
31
138
46
120
7
127
126
111
2
123
22
69
18
157
75
149
88
81
23
98
132
1
63
142
37
133
61
112
122
128
155
145
139
66
42
134
24
60
9
28
17
29
101
148
96
68
25
19
6
67
113
55
40
135
97
79
48
159
14
43
86
36
41
85
87
119
30
108
80
152
158
151
32
78
150
95
3
52
49"""
}
