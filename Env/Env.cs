﻿using AntlrTest.Env;
using AntlrTest.Mex.Env;
using AntlrTest.Visit;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest
{
    public static class PEnv
    {
        public static Dictionary<string, MexType?> Variables { get; set; } = new();

        public static int currentBlockLevel = 0;

        public static Dictionary<string, object> Functions { get; } = new();
        
        public static void CreateVariable(MexType vari)
        {
            Variables.Add(vari.GetName(), vari);
        }
        public static void CreateVariable(string type, string name, object? value)
        {
            if (type == "int") PEnv.CreateVariable(new IntegerType("int", name, value));
            if (type == "string") PEnv.CreateVariable(new StringType("string", name, value));
            if (type == "float") PEnv.CreateVariable(new FloatType("float", name, value));
            if (type == "bool") PEnv.CreateVariable(new BoolType("bool", name, value));
        }
        public static void DeleteVariable(string name)
        {
            Variables.Remove(name);
        }

        public static void DeleteLayerVariables(int layer)
        {
            var keys = Variables.Where(v => v.Value?.level == layer).Select(v => v.Key);
            foreach(var k in keys)
            {
                Variables.Remove(k);
            }
        }

        public static void Init()
        {
            Functions.Add("Write", new Func<object?[], object?>(Write));
            Functions.Add("WriteLn", new Func<object?[], object?>(WriteLn));
            Functions.Add("ReadLn", new Func<object?[], object?>(ReadLn));
            Functions.Add("ToInt", new Func<object?[], object?>(ToInt));
            Functions.Add("ToString", new Func<object?[], object?>(ToString));
            Functions.Add("ToFloat", new Func<object?[], object?>(ToFloat));
            Functions.Add("WHITESNAKE", new Func<object?[], object?>(WhiteSnake));
        }

        //public static Dictionary<string, MexType> FunctionVariables = new Dictionary<string, MexType>();

        public static string inFunction = "";
        //public static string inFunctionReturnType = "";
        public static bool returnedAlready = false;
        public static List<string> funcVariableNames = new List<string>();
        public static object? UserFunction(object?[] args)
        {
            PEnv.currentBlockLevel++;
            dynamic f = Functions[args.Last().ToString()];
            string inFunctionReturnType = f[4].ToString();
            PEnv.CreateVariable(inFunctionReturnType, PEnv.inFunction + ".return", null);
            for (int i = 0; i < f[1].Count; i++) 
            {
                string nm = f[1][i][1];
                if (PEnv.inFunction != "")
                {
                    nm = PEnv.inFunction + "." + f[1][i][1];
                    funcVariableNames.Add(f[1][i][1]);

                }
                CreateVariable(f[1][i][0],nm, args[i]);
            }
            PEnv.currentBlockLevel--;
            var value = f[2](f[3]);
            PEnv.returnedAlready = false;
            DeleteLayerVariables(currentBlockLevel + 1);
            inFunctionReturnType = "";
            inFunction = inFunction.Substring(0,inFunction.Length - args.Last().ToString().Length);
            if(inFunction == "") funcVariableNames.Clear();
            return value;
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

        static object? WhiteSnake(object?[] args)
        {
            Console.WriteLine("M|A|D|E|I|N|H|E|A|V|E|N\n\n");
            Thread.Sleep(2000);
            Console.WriteLine("Винтовая лестница");
            Thread.Sleep(1800);
            Console.WriteLine("Жук-носорог");
            Thread.Sleep(1600);
            Console.WriteLine("Город-призрак");
            Thread.Sleep(1400);
            Console.WriteLine("Инжирный пирог");
            Thread.Sleep(1200);
            Console.WriteLine("Жук-носорог");
            Thread.Sleep(1000);
            Console.WriteLine("Виа Долороза");
            Thread.Sleep(800);
            Console.WriteLine("Жук-носорог");
            Thread.Sleep(550);
            Console.WriteLine("Точка сингулярности");
            Thread.Sleep(400);
            Console.WriteLine("Джотто");
            Thread.Sleep(380);
            Console.WriteLine("Ангел");
            Thread.Sleep(320);
            Console.WriteLine("Гортензия");
            Thread.Sleep(250);
            Console.WriteLine("Жук-носорог");
            Thread.Sleep(200);
            Console.WriteLine("Точка сингулярности");
            Thread.Sleep(100);
            Console.WriteLine("Тайный император");
            Thread.Sleep(50);
            for (int i = 0; i < 1000000; i++)
            {
                Console.Write("GΔCT ");
            }
            return false;
        }

        static object? ToString(object?[] args)
        {
            return args[0]?.ToString();
        }

        static object? ToFloat(object?[] args)
        {
            float result;
            if (float.TryParse(args[0]?.ToString(), out result))
            {
                return result;
            }
            return null;
        }
    }
}
