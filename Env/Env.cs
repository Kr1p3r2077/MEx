using AntlrTest.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest
{
    public static class PEnv
    {
        public static Dictionary<string, MexType?> Variables { get; } = new();

        public static Dictionary<string, Func<object?[], object?>> Functions { get; } = new();

        public static void Init()
        {
            Functions.Add("Write", Write);
            Functions.Add("WriteLn", WriteLn);
            Functions.Add("ReadLn", ReadLn);
            Functions.Add("ToInt", ToInt);
        }

        static object? Write(object?[] args)
        {
            foreach (var v in args)
            {
                Console.Write(args[0] + " ");
            }
            return null;
        }

        static object? WriteLn(object?[] args)
        {
            foreach (var v in args)
            {
                Console.WriteLine(args[0]);
            }
            return null;
        }

        static object? ReadLn(object?[] args)
        {
            return Console.ReadLine();
        }

        static object? ToInt(object?[] args)
        {
            int result;
            if (int.TryParse(args[0]?.ToString(), out result))
            {
                return result;
            }
            return null;
        }

        public static void CreateVariable(string type, string name, object value)
        {
            Variables.Add(name, new MexType(type, name, value));
        }
    }
}
