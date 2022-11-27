﻿using Antlr4.Runtime.Misc;
using AntlrTest.Env;
using AntlrTest.Mex.Env;
using AntlrTest.Mex.Libs;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Linq.Expressions;
using System.Security.AccessControl;
using System.Text;
using System.Threading.Tasks;
using System.Transactions;

namespace AntlrTest.Visit
{

    public class MexVisitor : MexBaseVisitor<object?>
    {
        public override object? VisitVariableInitExpr([NotNull] MexParser.VariableInitExprContext context)
        {
            string variableType = context.vInit().IDENTIFIER(0).GetText();
            string variableName = context.vInit().IDENTIFIER(1).GetText();

            if (PEnv.inFunction != "")
            {
                variableName = PEnv.inFunction + "." + variableName;
            }

            PEnv.CreateVariable(variableType, variableName, null);

            return variableName;
        }

        public override object? VisitVariableInitExprAssignment([NotNull] MexParser.VariableInitExprAssignmentContext context)
        {
            string variableType = context.IDENTIFIER(0).GetText();
            string variableName = context.IDENTIFIER(1).GetText();

            if (PEnv.inFunction != "")
            {
                variableName = PEnv.inFunction + "." + variableName;
            }

            var variableValue = Visit(context.expression());

            PEnv.CreateVariable(variableType, variableName, variableValue);

            return variableName;
        }

        public override object? VisitStandartAssignment ([NotNull] MexParser.StandartAssignmentContext context)
        {
            string variableName = context.IDENTIFIER().GetText();
            var value = Visit(context.expression());

            string op = context.assignmentOp().GetText();

            if (op == "=") ((dynamic)PEnv.Variables[variableName])?.Set(value);
            if (op == "+=") ((dynamic)PEnv.Variables[variableName])?.Add(value);
            if(op == "-=") ((dynamic)PEnv.Variables[variableName])?.Subtract(value);
            if (op == "*=") ((dynamic)PEnv.Variables[variableName])?.Multiply(value);
            if (op == "/=") ((dynamic)PEnv.Variables[variableName])?.Divide(value);
            //if (op == "%=") PEnv.Variables[variableName] %= (dynamic)value;

            return null;
        }

        public override object? VisitConstantExpression([NotNull] MexParser.ConstantExpressionContext context)
        {
            return Visit(context.constant());
        }
        public override object? VisitConstant([NotNull] MexParser.ConstantContext context)
        {
            if (context.INTEGER() != null)
            {
                return int.Parse(context.INTEGER().GetText());
            }
            if (context.FLOAT() != null)
            {
                string s = context.FLOAT().GetText();
                return float.Parse(s, CultureInfo.InvariantCulture);
            }
            if (context.STRING() != null)
            {
                var str = context.STRING().GetText();
                return str.Substring(1, str.Length - 2);
            }
            if (context.BOOL() != null)
            {
                return context.BOOL().GetText() == "true";
            }
            if (context.NULL() != null)
            {
                return null;
            }
            return null;
        }

        public override object? VisitIdentifierExpression([NotNull] MexParser.IdentifierExpressionContext context)
        {
            string identifier = context.IDENTIFIER().GetText();

            if (PEnv.inFunction != "" && PEnv.funcVariableNames.Contains(identifier))
            {
                identifier = PEnv.inFunction + "." + identifier;
            }
            else if (PEnv.inClassVarName != "" && !PEnv.funcVariableNames.Contains(identifier))
            {
                if (((MexVariable)PEnv.Variables[PEnv.inClassVarName]).HasField(identifier))
                {
                    return ((MexVariable)PEnv.Variables[PEnv.inClassVarName]).GetField(identifier).GetValue();
                }
                return ((MexVariable)PEnv.Variables[PEnv.inFunction + "." + identifier]).GetValue();
            }

            if (PEnv.Variables.ContainsKey(identifier))
            {
                return ((dynamic)PEnv.Variables[identifier])?.GetValue();
            }
            return null;
        }

        public override object? VisitFunctionCallExpression([NotNull] MexParser.FunctionCallExpressionContext context)
        {
            return Visit(context.functionCall());
        }
        public override object? VisitFunctionCall([NotNull] MexParser.FunctionCallContext context)
        {
            var functionName = context.IDENTIFIER().GetText();

            var expressions = context.expression().Select(e => Visit(e)).ToArray();

            if(!PEnv.Functions.ContainsKey(functionName)) return null;
            if(PEnv.Functions[functionName] is Func<object?[], object?> func)
            {
                return func(expressions);
            }
            if (PEnv.Functions[functionName] is object[])
            {
                PEnv.inFunction += functionName;
                expressions = expressions.Append(functionName).ToArray();
                object ret = ((Func<object?[], object?>)((object[])PEnv.Functions[functionName])[0])(expressions);
                //PEnv.inFunction = "";
                return ret;
            }
            return null;
        }

        public override object? VisitParenthesizedExpression([NotNull] MexParser.ParenthesizedExpressionContext context)
        {
            return Visit(context.expression());
        }

        public override object? VisitNotExpression([NotNull] MexParser.NotExpressionContext context)
        {
            var expr = Visit(context.expression());
            if (expr is bool)
            {
                return !((bool)expr);
            }
            return null;
        }

        public override object? VisitMultiplyExpression([NotNull] MexParser.MultiplyExpressionContext context)
        {
            string op = context.multOp().GetText();
            var expr0 = Visit(context.expression(0));
            var expr1 = Visit(context.expression(1));

            if (op == "*") return (dynamic?)expr0 * (dynamic?)expr1;
            if(op == "/") return (dynamic?)expr0 / (dynamic?)expr1;
            if(op == "%") return (dynamic?)expr0 % (dynamic?)expr1;
            if (op == "**")
            {
                var v = Math.Pow((dynamic?)expr0, (dynamic?)expr1);
                return v;
            }

            return null;
        }

        public override object? VisitAddExpression([NotNull] MexParser.AddExpressionContext context)
        {
            char op = context.addOp().GetText()[0];
            var expr0 = Visit(context.expression(0));
            var expr1 = Visit(context.expression(1));

            if (op == '+') return (dynamic?)expr0 + (dynamic?)expr1;
            if (op == '-') return (dynamic?)expr0 - (dynamic?)expr1;

            return null;
        }

        public override object? VisitCompareExpression([NotNull] MexParser.CompareExpressionContext context)
        {
            string op = context.compareOp().GetText();
            var expr0 = Visit(context.expression(0));
            var expr1 = Visit(context.expression(1));

            //if (expr0 is not bool || expr1 is not bool) return null;

            if (op == "==") return (dynamic)expr0 == (dynamic)expr1;
            if (op == "!=") return (dynamic)expr0 != (dynamic)expr1;

            if (op == ">") return (dynamic)expr0 > (dynamic)expr1;
            if (op == ">=") return (dynamic)expr0 >= (dynamic)expr1;

            if (op == "<") return (dynamic)expr0 < (dynamic)expr1;
            if (op == "<=") return (dynamic)expr0 <= (dynamic)expr1;

            return null;
        }

        public override object? VisitBooleanExpression([NotNull] MexParser.BooleanExpressionContext context)
        {
            string op = context.boolOp().BOOL_OPERATOR().GetText();
            var expr0 = Visit(context.expression(0));
            var expr1 = Visit(context.expression(1));

            if (expr0 is not bool || expr1 is not bool) return null;

            if (op == "&&") return (bool)expr0 && (bool)expr1;
            if (op == "||") return (bool)expr0 || (bool)expr1;

            return null;
        }


        public override object VisitUnoOpAssignment([NotNull] MexParser.UnoOpAssignmentContext context)
        {
            string variableName = context.IDENTIFIER().GetText();

            string op = context.unoOp().GetText();

            if (op == "++") ((dynamic)PEnv.Variables[variableName]).Inc();
            if (op == "--") ((dynamic)PEnv.Variables[variableName]).Dec();
            if (op == "**") ((dynamic)PEnv.Variables[variableName]).Square();

            return ((dynamic)PEnv.Variables[variableName]).GetValue();
        }

        public override object? VisitIfBlock([NotNull] MexParser.IfBlockContext context)
        {
            if ((bool)Visit(context.expression()))
            {
                Visit(context.block());
            }
            else if(context.elseBlock() != null)
            {
                Visit(context.elseBlock());
            }
            return null;
        }

        public override object? VisitWhileLoop([NotNull] MexParser.WhileLoopContext context)
        {
            while ((bool)Visit(context.expression()))
            {
                Visit(context.block());
            }
            return null;
        }

        public override object? VisitRepeatLoop([NotNull] MexParser.RepeatLoopContext context)
        {
            int count = (int)Visit(context.expression());
            for(int i = 0; i < count; i++)
            {
                Visit(context.block());
            }

            return null;
        }

        public override object? VisitBlock([NotNull] MexParser.BlockContext context)
        {
            if (PEnv.inFunction == "")
            {
                PEnv.currentBlockLevel++;
                foreach (var v in context.line())
                {
                    Visit(v);
                }
                PEnv.DeleteLayerVariables(PEnv.currentBlockLevel);
                PEnv.currentBlockLevel--;
                return null;
            }
            else
            {
                PEnv.currentBlockLevel++;
                foreach (var v in context.line())
                {
                    string s = v.GetText();
                    Visit(v);
                    if (PEnv.returnedAlready) break;
                }
                object resultVariableValue = ((dynamic)PEnv.Variables[PEnv.inFunction + "." + "return"]).GetValue();
                PEnv.DeleteLayerVariables(PEnv.currentBlockLevel);
                PEnv.currentBlockLevel--;
                return resultVariableValue;
            }
        }

        public override object VisitForLoop([NotNull] MexParser.ForLoopContext context)
        {
            PEnv.currentBlockLevel++;

            
            string counterVariableName = (string)Visit(context.variableInit());

            if(Visit(context.expression()) is bool)
            {
                while ((bool)Visit(context.expression()))
                {
                    Visit(context.block());
                    Visit(context.assignment());
                }
            }

            PEnv.DeleteLayerVariables(PEnv.currentBlockLevel);
            PEnv.currentBlockLevel--;
            
            return null;
        }

        public override object VisitLoopLoop([NotNull] MexParser.LoopLoopContext context)
        {
            PEnv.currentBlockLevel++;
            string counterVariableName = context.IDENTIFIER().GetText();
            int a = (int)Visit(context.expression(0));
            int b = (int)Visit(context.expression(1));

            PEnv.CreateVariable("int", counterVariableName, a);

            if (a < b)
            {
                while (((dynamic)PEnv.Variables[counterVariableName]).GetValue() < b)
                {
                    Visit(context.block());
                    ((dynamic)PEnv.Variables[counterVariableName])?.Inc();
                }
            }
            else
            {
                while (((dynamic)PEnv.Variables[counterVariableName]).GetValue() > b)
                {
                    Visit(context.block());
                    ((dynamic)PEnv.Variables[counterVariableName])?.Dec();
                }
            }
            

            PEnv.DeleteLayerVariables(PEnv.currentBlockLevel);
            PEnv.currentBlockLevel--;

            return null;
        }
        /*
        public override object VisitVariableInit([NotNull] MexParser.VariableInitContext context)
        {
            ((MexParser.VariableInitExprContext)context).IDENTIFIER();
            return context;
        }
        */
        public override object VisitFunctionDeclaraction([NotNull] MexParser.FunctionDeclaractionContext context)
        {
            string cl = PEnv.inClass;
            List<string[]> parametrs = new List<string[]>();
            int c = 0;
            foreach(var v in context.variableInit())
            {
                string t = ((MexParser.VariableInitExprContext)v).vInit().IDENTIFIER(0).ToString();
                string n = ((MexParser.VariableInitExprContext)v).vInit().IDENTIFIER(1).ToString();

                parametrs.Add(new string[] { t, n, c.ToString()});
                c++;
            }
            string returnType = context.IDENTIFIER(0).ToString();

            PEnv.Functions.Add(cl + context.IDENTIFIER(1).ToString(), new object[] { PEnv.UserFunction, parametrs, Visit, context.block(), returnType });
            return null;
        }


        public override object VisitReturn([NotNull] MexParser.ReturnContext context)
        {
            ((dynamic)PEnv.Variables[PEnv.inFunction + ".return"]).Set(Visit(context.expression()));
            PEnv.returnedAlready = true;
            return null;
        }


        public override object VisitDobavit([NotNull] MexParser.DobavitContext context)
        {
            var str = context.STRING().GetText();
            str = str.Substring(1, str.Length - 2);

            Libs.libraries[str].OnAdd();
            return null;
        }


        public override object VisitVariableArrayInitExpr([NotNull] MexParser.VariableArrayInitExprContext context)
        {
            string variableType = context.IDENTIFIER(0).GetText();
            string variableName = context.IDENTIFIER(1).GetText();

            if (PEnv.inFunction != "")
            {
                variableName = PEnv.inFunction + "." + variableName;
            }

            //PEnv.CreateVariableArray(variableType, variableName, null);

            return variableName;
        }

        public override object VisitTypeDeclaration([NotNull] MexParser.TypeDeclarationContext context)
        {
            var identifiers = context.typeDecField();

            string className = context.IDENTIFIER().GetText();

            PEnv.inClass = className + ".";

            List<string[]> vars = new List<string[]>();

            for(int i = 0; i < identifiers.Length; i++)
            {
                if (identifiers[i].vInit() != null)
                {
                    string vType = identifiers[i].vInit().IDENTIFIER(0).GetText();
                    string vName = identifiers[i].vInit().IDENTIFIER(1).GetText();
                    vars.Add(new string[] { vType, vName });
                }
                if (identifiers[i].functionDeclaraction() != null)
                {
                    Visit(identifiers[i].functionDeclaraction());
                }
            }

            PEnv.Types.Add(className, new MexTypeInfo(className, vars));
            PEnv.inClass = "";
            return null;
        }

        public override object VisitVariableGetField([NotNull] MexParser.VariableGetFieldContext context)
        {
            string varName = context.IDENTIFIER(0).GetText();
            string fieldName = context.IDENTIFIER(1).GetText();

            if(PEnv.inFunction != "")
            {
                varName = PEnv.inFunction + "." + varName;
            }
            return ((MexVariable)PEnv.Variables[varName]).GetField(fieldName)?.GetValue();
        }

        public override object VisitVariableGetFieldMany([NotNull] MexParser.VariableGetFieldManyContext context)
        {
            string varName = context.IDENTIFIER().GetText();

            var vars = context.getFieldExpression();

            MexVariable result = (MexVariable)PEnv.Variables[varName];
            for (int i = 0; i < vars.Length; i++)
            {
                if (vars[i].IDENTIFIER() != null)
                {
                    result = result.GetField(vars[i].IDENTIFIER().GetText());
                }
                else if(vars[i].functionCall() != null)
                {
                    //result = result.GetField(vars[i].IDENTIFIER().GetText());
                    //Console.WriteLine("function from " + varName);
                    string t = ((MexVariable)PEnv.Variables[varName]).GetVarType();

                    string functionName = t + "." + vars[i].functionCall().IDENTIFIER().GetText();

                    var expressions = vars[i].functionCall().expression().Select(e => Visit(e)).ToArray();

                    if (PEnv.Functions[functionName] is object[])
                    {
                        PEnv.inClassVarName = varName;
                        PEnv.inFunction += functionName;
                        expressions = expressions.Append(functionName).ToArray();
                        object ret = ((Func<object?[], object?>)((object[])PEnv.Functions[functionName])[0])(expressions);
                        //PEnv.inFunction = "";
                        PEnv.inClassVarName = "";
                        return ret;
                    }
                }
                else if (vars[i].expression() != null)
                {
                    Console.WriteLine("indexing from " + varName);
                }
            }

            return result?.GetValue();
        }

        public override object VisitStandartAssignmentField([NotNull] MexParser.StandartAssignmentFieldContext context)
        {
            string varName = context.IDENTIFIER(0).GetText();
            string fieldName = context.IDENTIFIER(1).GetText();

            var value = Visit(context.expression());

            if (PEnv.inFunction != "")
            {
                varName = PEnv.inFunction + "." + varName;
            }
            MexVariable field = ((MexVariable)PEnv.Variables[varName]).GetField(fieldName);

            string op = context.assignmentOp().GetText();

            if (op == "=") field?.Set(value);
            if (op == "+=") field?.Add(value);
            if (op == "-=") field?.Subtract(value);
            if (op == "*=") field?.Multiply(value);
            if (op == "/=") field?.Divide(value);

            return null;
        }

        public override object VisitStandartAssignmentFieldMany([NotNull] MexParser.StandartAssignmentFieldManyContext context)
        {

            var vars = context.IDENTIFIER();

            var value = Visit(context.expression());

            string varName = vars[0].GetText();

            MexVariable field = ((MexVariable)PEnv.Variables[varName]).GetField(vars[1].GetText());
            for (int i = 2; i < vars.Length; i++)
            {
                field = field.GetField(vars[i].GetText());
            }


            string op = context.assignmentOp().GetText();

            if (op == "=") field?.Set(value);
            if (op == "+=") field?.Add(value);
            if (op == "-=") field?.Subtract(value);
            if (op == "*=") field?.Multiply(value);
            if (op == "/=") field?.Divide(value);

            return null;
        }
    }
}
