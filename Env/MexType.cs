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

    public class MexType
    {
        private dynamic value;
        private string name;
        private string type;

        public MexType(string type, string name, object value)
        {
            this.value = value;
            this.name = name;
            this.type = type;
        }

        public void Add(object _value)
        {
            if(_value is int && type == "int")
            {
                value += (int)_value;
                return;
            }
            if (_value is int && type == "string")
            {
                value += _value;
                return;
            }
            if (_value is string && type == "string")
            {
                value += _value.ToString();
                return;
            }
            if (_value is float && type == "float")
            {
                value += (float)_value;
                return;
            }
        }

        public void Set(object _value)
        {
            if (_value is int && type == "int")
            {
                value = (int)_value;
                return;
            }
            if (_value is bool && type == "bool")
            {
                value = (int)_value;
                return;
            }
            if (_value is float && type == "float")
            {
                value = (int)_value;
                return;
            }
            if (_value is string && type == "string")
            {
                value = (int)_value;
                return;
            }
        }

        public void Multiply(object _value)
        {
            if (_value is int && type == "int")
            {
                value *= (int)_value;
                return;
            }
            if (_value is float && type == "float")
            {
                value *= (float)_value;
                return;
            }
        }
    }
}
