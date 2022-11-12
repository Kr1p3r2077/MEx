using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Mex
{
    public static class OutputCompErrors
    {
        public static bool GetErrors(string text)
        {
            bool result = false;

            string[] strs = text.Split(';');

            for(int i = 0; i < strs.Length; i++)
            {
                if (strs[i].Contains("<missing '"))
                {
                    while(strs[i].Contains("<missing '"))
                    {
                        int coord = strs[i].IndexOf("<missing '") + 10;
                        Console.WriteLine("Missing " + strs[i][coord] + " in line " + (i + 1));
                        strs[i] = strs[i].Remove(coord - 10, 12);
                    }
                    result = true;
                }
            }

            return result;
        }
    }
}
