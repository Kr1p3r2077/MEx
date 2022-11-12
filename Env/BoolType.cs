using AntlrTest.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex.Env
{
    public class BoolType : MexType
    {
        public BoolType(string type, string name, object? value) : base(type, name, value)
        {
            if (value is not bool)
            {
                this.value = false;
                this.type = "bool";
            }
        }

        public override void Set(object? o)
        {
            if(o is bool) value = (bool)o;
        }
    }
}
