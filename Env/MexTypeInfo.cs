using AntlrTest.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex.Env
{
    public class MexTypeInfo
    {
        public List<string[]> variables = new List<string[]>();
        public string name;
        public MexTypeInfo(string _name, List<string[]> _variables)
        {
            variables = _variables;
            name = _name;
        }

        public Dictionary<string, MexVariable> GetMexVariables()
        {
            Dictionary<string, MexVariable> v = new Dictionary<string, MexVariable>();

            foreach(var vr in variables)
            {
                v.Add(vr[1], new MexVariable(vr[0], vr[1], null));
            }
            return v;
        }

        public Dictionary<string, Func<object[]?, object>> GetMexFunctions()
        {
            Dictionary<string, Func<object[]?, object>> v = new Dictionary<string, Func<object[]?, object>>();

            foreach (var vr in variables)
            {

            }
            return v;
        }
    }
}
