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

        public static void CreateVariable(string type, string name, object value)
        {
            Variables.Add(name, new MexType(type, name, value));
        }
    }
}
