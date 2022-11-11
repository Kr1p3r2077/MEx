using AntlrTest.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex.Env
{
    public class IntegerType : MexType
    {
        public IntegerType(string type, string name, object? value) : base(type, name, value)
        {
            if(value is not int)
            {
                this.value = 0;
                this.type = "int";
            }
        }

        public override void Set(object? o)
        {
            value = (int)o;
        }

        public override void Add(object? o)
        {
            if(o is int) value += (int)o;
        }
        public override void Subtract(object? o)
        {
            if (o is int) value -= (int)o;
        }
        public override void Multiply(object? o)
        {
            if (o is int) value *= (int)o;
        }
        public override void Divide(object? o)
        {
            if (o is int) value /= (int)o;
        }
    }
}
