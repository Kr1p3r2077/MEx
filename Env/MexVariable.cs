using AntlrTest.Mex.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Env
{
    public class MexVariable
    {
        protected object? value;
        protected string name;
        protected MexTypeInfo type;
        public int level = 0;

        Dictionary<string, MexVariable> variables;
        Dictionary<string, Func<object?[], object?>> functions;
        public MexVariable(string _type, string name, object? value)
        {
            if (_type == "int")
            {

            }

            this.value = value;
            this.name = name;
            this.type = PEnv.Types[_type];
            variables = this.type.GetMexVariables();
            //functions = this.type.GetMexFunctions();
            this.level = PEnv.currentBlockLevel;
        }

        public string GetVarType()
        {
            return type.name;
        }
        public object? GetValue()
        {
            return value;
        }

        public bool HasField(string n)
        {
            return variables.ContainsKey(n);
        }

        public MexVariable? GetField(string fieldName)
        {
            if (variables.ContainsKey(fieldName))
            {
                return variables[fieldName];
            }
            return null;
        }

        public new Func<object?[], object?>? GetFunction(string funcName)
        {
            if (functions.ContainsKey(funcName))
            {
                return functions[funcName];
            }
            return null;
        }
        public virtual void Set(object? o) { value = o; }
        public virtual void Add(object? o) { }
        public virtual void Subtract(object? o) { }
        public virtual void Multiply(object? o) { }
        public virtual void Divide(object? o) { }
        public virtual void Inc() { }
        public virtual void Dec() { }
        public virtual void Square() { }
    }
}
