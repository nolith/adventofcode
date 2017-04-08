using System;

namespace csharp
{
    class Program
    {
        public static bool debug;

        static void Main(string[] args)
        {
          if (args.Length < 1) {
            Console.WriteLine("Missing proble index");
            Environment.Exit(1);
          }

          debug = Environment.GetEnvironmentVariable("DEBUG") != null;

          var problem = args[0];

          switch (problem)
          {
            case "3":
              var d3_result = new Day3().solution();
              Console.WriteLine(d3_result);
              break;
            case "3+":
              var d3_plus_result = new Day3().solutionPlus();
              Console.WriteLine(d3_plus_result);
              break;
            default:
              Console.WriteLine("Unlknown problem {0}", problem);
              Environment.Exit(2);
              break;
          }
        }

        public static void Log(string line) {
          if (debug)
            Console.WriteLine(line);
        } 
    }
}
