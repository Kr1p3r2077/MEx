using AntlrTest.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex.Env
{
    public class FloatType : MexType
    {
        public FloatType(string type, string name, object? value) : base(type, name, value)
        {
            if (value is not float)
            {
                this.value = 0f;
                this.type = "float";
            }
        }

        public override void Set(object? o)
        {
            if (o is float) value = (float)o;
        }
        public override void Add(object? o)
        {
            if (o is float) value += (float)o;
        }
        public override void Subtract(object? o)
        {
            if (o is float) value -= (float)o;
        }
        public override void Multiply(object? o)
        {
            if (o is float) value *= (float)o;
        }
        public override void Divide(object? o)
        {
            if (o is float) value /= (float)o;
        }
        public override void Inc()
        {
            value++;
        }
        public override void Dec()
        {
            value--;
        }
        public override void Square()
        {
            value *= value;
        }
    }
}
