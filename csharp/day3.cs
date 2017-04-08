using System;
using System.Text.RegularExpressions;
using System.Linq;
using System.Collections.Generic;

namespace csharp {
  class Day3 {

    private Regex parser = new Regex(@"^\s*(\d+)\s*(\d+)\s*(\d+)");

    private IList<int[]> parseTriangles() {
      var parsed = new List<int[]>();
      string line;
      do {
        line = Console.ReadLine();
        if (line != null) {
          var match = parser.Match(line);
          if (!match.Success || match.Groups.Count != 4) {
            Console.WriteLine("Can't parse this line: {0}", line);
          } else {
            int[] t = new int[3];
            for( int i = 0; i < 3; i++ )
              t[i] = int.Parse(match.Groups[i+1].Value);
            parsed.Add(t);
          }
        }
      }while (line != null);

      return parsed;
    }
    public int solution() {
      return parseTriangles().Count(t => isTriangle(t));
    }

    public bool isTriangle(int[] t) {
      
      return t[0] + t[1] > t[2] &&
             t[0] + t[2] > t[1] &&
             t[1] + t[2] > t[0];
    }    

    public int solutionPlus() {
      var hTriangles = parseTriangles();
      var linesCnt = hTriangles.Count;
      var vTriangles = new List<int[]>(linesCnt);

      logTriangles(hTriangles);

      if (linesCnt % 3 != 0) {
        Console.WriteLine("Lines must be divisible by 3");
        return -1;
      }

      int parsed = 0;
      while(parsed < linesCnt) {
        var tmp = new int[3, 3];
        var batch = hTriangles.Skip(parsed).Take(3);
        for( int i = 0; i < 3; i++ ) {
          var t = batch.ElementAt(i);
          tmp[0, i] = t[0];
          tmp[1, i] = t[1];
          tmp[2, i] = t[2];
        }
        
        for( int i = 0; i < 3; i++)
          vTriangles.Add(new int[] { tmp[i, 0], tmp[i, 1], tmp[i, 2]});
        parsed += 3;
      }

      logTriangles(vTriangles);
      return vTriangles.Count(isTriangle);
    }

    private void logTriangles(IList<int[]> triangles) {
      Program.Log("***** Maybe Triangles ****");
      foreach (var t in triangles)
        Program.Log(String.Format("[ {0} {1} {2} ]", t[0], t[1], t[2]));
    }
  }
} 
