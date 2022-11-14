using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex.Libs
{
    public static class Libs
    {
        static public Dictionary<string, LibBase> libraries;
        public static void Init()
        {
            libraries = new Dictionary<string, LibBase>();
            libraries.Add("Math", new Math_lib());
        }
    }
}
