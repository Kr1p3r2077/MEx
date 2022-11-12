using AntlrTest.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex.Env
{
    public class StringType : MexType
    {
        public StringType(string type, string name, object? value) : base(type, name, value)
        {
            if (value is not string)
            {
                this.value = "";
                this.type = "string";
            }
        }

        public override void Set(object? o)
        {
            value = o?.ToString();
        }
        public override void Add(object? o)
        {
            if (o is string) value += o?.ToString();
        }
    }
}
