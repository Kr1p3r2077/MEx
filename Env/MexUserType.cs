using AntlrTest.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex.Env
{
    public class MexUserType : MexType
    {
        Dictionary<string, MexType?> fields;
        Dictionary<string, object> funcs;
        public MexUserType(string type, string name, Dictionary<string, string> _fields, Dictionary<string, object> _funcs) : base(type, name, null)
        {
            foreach(var v in _fields)
            {
                //fields.Add(v.Key,)
            }
            funcs = _funcs;
        }

        public object GetFieldValue(string nm)
        {
            return fields[nm].GetValue();
        }

        public object RunFunction(string nm, object[]? parametrs)
        {
            return ((dynamic)funcs[nm])(parametrs);
        }
    }
}
