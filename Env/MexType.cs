using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Env
{
    enum TypeEnum
    {

    }

    public static class TypeTrans
    {

    }

    public class MexType
    {
        protected dynamic? value;
        protected string name;
        protected string type;

        public MexType(string type, string name, object? value)
        {
            this.value = value;
            this.name = name;
            this.type = type;
        }

        public object? GetValue()
        {
            return value;
        }

        public virtual void Set(object? o) { }
        public virtual void Add(object? o) { }
        public virtual void Subtract(object? o) { }
        public virtual void Multiply(object? o) { }
        public virtual void Divide(object? o) { }
    }
}
