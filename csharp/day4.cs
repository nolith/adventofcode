using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace csharp {

  class Day4 {

        internal class D4Comparer : IComparer<KeyValuePair<char, int>>
        {
            int IComparer<KeyValuePair<char, int>>.Compare(KeyValuePair<char, int> x, KeyValuePair<char, int> y)
            {
                //note inverted comparison only by value
                var byValue = y.Value.CompareTo(x.Value);

                return byValue != 0 ? byValue : x.Key.CompareTo(y.Key); 
            }
        }
        internal class Room {
      static Regex parser = new Regex(@"^([a-z-]+)-(\d+)\[([a-z]{5})\]");

      internal string name { get; set; }
      internal int id { get; set; }
      internal string checksum { get; set; }

      internal bool isDecoy() {
        return checksum != calculateChecksum();
      }

      internal string calculateChecksum() {
        return new String(name
                  .Where(c => c != '-')
                  .Aggregate(
                    new Dictionary<char, int>(),
                    (acc, c) => {
                      int sum = 1;
                      
                      if(acc.TryGetValue(c, out sum))
                        sum++;
                      
                      acc[c] = sum;
                      return acc;
                    })
                  .OrderBy(pair => pair, new D4Comparer())
                  .Select(pair => pair.Key)
                  .Take(5)
                  .ToArray());
      }

      public string ToString() {
        return string.Format(
          "[name: {0}, id: {1}, checksum: {2}, calculated_checksum: {3}, decoy: {4}]",
          name, id, checksum, calculateChecksum(), isDecoy()
        );
      }
      internal static Room FromLine(string line) {
        var m = parser.Match(line);
        if (!m.Success) {
          Console.WriteLine("Can't parse line: {0}", line);
          return null;
        }

        return new Room {
          name = m.Groups[1].Value,
          id = int.Parse(m.Groups[2].Value),
          checksum = m.Groups[3].Value
        };
      }
    }

    private IList<Room> parseInput() {
      var rooms = new List<Room>();
      string line;
      while( (line = Console.ReadLine()) != null) {
        var r = Room.FromLine(line);
        if (r != null) {
          rooms.Add(r);
          Program.Log(r.ToString());
        }
      }

      return rooms;
    }

    public int solution() {
      return parseInput()
              .Where(r => !r.isDecoy())
              .Select(r => r.id).
              Sum();
    }
  }
}
