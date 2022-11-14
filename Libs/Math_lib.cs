using AntlrTest.Mex.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex.Libs
{
    internal class Math_lib : LibBase
    {
        public void OnAdd()
        {
            PEnv.Functions.Add("Fact", new Func<object?[], object?>(Factorial));
            PEnv.Functions.Add("Sqrt", new Func<object?[], object?>(Sqrt));

            PEnv.Variables.Add("PI", new FloatType("float", "PI", Math.PI));
            PEnv.Variables.Add("E", new FloatType("float", "E", Math.E));
            PEnv.Variables.Add("TAU", new FloatType("float", "E", Math.Tau));
        }

        object? Factorial(object?[] args)
        {
            int a = (int)args[0];

            for(int i = a - 1; i >= 2; i--)
            {
                a *= i;
            }

            return a;
        }

        object? Sqrt(object?[] args)
        {
            return (float)Math.Sqrt(Convert.ToDouble(args[0]));
        }
    }
}
