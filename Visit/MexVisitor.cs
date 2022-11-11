using Antlr4.Runtime.Misc;
using AntlrTest.Env;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AntlrTest.Visit
{

    public class MexVisitor : MexBaseVisitor<object?>
    {
        public override object VisitVariableInitExpr([NotNull] MexParser.VariableInitExprContext context)
        {
            string variableType = context.IDENTIFIER(0).GetText();
            string name = context.IDENTIFIER(1).GetText();

            PEnv.Variables[name] = null;

            return null;
        }

        public override object VisitVariableInitExprAssignment([NotNull] MexParser.VariableInitExprAssignmentContext context)
        {
            string variableType = context.IDENTIFIER().GetText();
            string variableName = context.IDENTIFIER().GetText();

            Visit(context.assignment());

            PEnv.Variables.Add(variableName, new MexType(variableType, variableName, null));

            return null;
        }

        public override object VisitAssignment([NotNull] MexParser.AssignmentContext context)
        {
            string variableName = context.IDENTIFIER().GetText();

            var value = Visit(context.expression());

            string op = context.assignmentOp().GetText();

            if (op == "=") PEnv.Variables[variableName].Set(value);
            if(op == "+=") PEnv.Variables[variableName] += (dynamic)value;
            if(op == "-=") PEnv.Variables[variableName] -= (dynamic)value;
            if(op == "*=") PEnv.Variables[variableName] *= (dynamic)value;
            if(op == "/=") PEnv.Variables[variableName] /= (dynamic)value;
            if(op == "%=") PEnv.Variables[variableName] %= (dynamic)value;

            return null;
        }

        public override object VisitConstantExpression([NotNull] MexParser.ConstantExpressionContext context)
        {
            return Visit(context.constant());
        }
        public override object VisitConstant([NotNull] MexParser.ConstantContext context)
        {
            if (context.INTEGER() != null)
            {
                return int.Parse(context.INTEGER().GetText());
            }
            if (context.FLOAT() != null)
            {
                return float.Parse(context.INTEGER().GetText());
            }
            if (context.STRING() != null)
            {
                return context.INTEGER().GetText();
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

        public override object VisitIdentifierExpression([NotNull] MexParser.IdentifierExpressionContext context)
        {
            string identifier = context.IDENTIFIER().GetText();
            if (PEnv.Variables.ContainsKey(identifier))
            {
                return PEnv.Variables[identifier];
            }
            return null;
        }

        public override object VisitFunctionCallExpression([NotNull] MexParser.FunctionCallExpressionContext context)
        {
            return Visit(context.functionCall());
        }
        public override object VisitFunctionCall([NotNull] MexParser.FunctionCallContext context)
        {
            var functionName = context.IDENTIFIER();

            var expressions = context.expression();
            return null;
        }

        public override object VisitParenthesizedExpression([NotNull] MexParser.ParenthesizedExpressionContext context)
        {
            return Visit(context.expression());
        }

        public override object VisitNotExpression([NotNull] MexParser.NotExpressionContext context)
        {
            var expr = Visit(context.expression());
            if (expr is bool)
            {
                return !((bool)expr);
            }
            return null;
        }

        public override object VisitMultiplyExpression([NotNull] MexParser.MultiplyExpressionContext context)
        {
            char op = context.multOp().GetText()[0];
            var expr0 = Visit(context.expression(0));
            var expr1 = Visit(context.expression(1));

            if (op == '*') return (dynamic)expr0 * (dynamic)expr1;
            if(op == '/') return (dynamic)expr0 / (dynamic)expr1;
            if(op == '%') return (dynamic)expr0 % (dynamic)expr1;

            return null;
        }

        public override object VisitAddExpression([NotNull] MexParser.AddExpressionContext context)
        {
            char op = context.addOp().GetText()[0];
            var expr0 = Visit(context.expression(0));
            var expr1 = Visit(context.expression(1));

            if (op == '+') return (dynamic)expr0 + (dynamic)expr1;
            if (op == '-') return (dynamic)expr0 - (dynamic)expr1;

            return null;
        }

        public override object VisitCompareExpression([NotNull] MexParser.CompareExpressionContext context)
        {
            string op = context.compareOp().GetText();
            var expr0 = Visit(context.expression(0));
            var expr1 = Visit(context.expression(1));

            if (expr0 is not bool || expr1 is not bool) return null;

            if (op == ">") return (dynamic)expr0 > (dynamic)expr1;
            if (op == ">=") return (dynamic)expr0 >= (dynamic)expr1;

            if (op == "<") return (dynamic)expr0 < (dynamic)expr1;
            if (op == "<=") return (dynamic)expr0 <= (dynamic)expr1;

            return null;
        }

        public override object VisitBooleanExpression([NotNull] MexParser.BooleanExpressionContext context)
        {
            string op = context.boolOp().BOOL_OPERATOR().GetText();
            var expr0 = Visit(context.expression(0));
            var expr1 = Visit(context.expression(1));

            if (expr0 is not bool || expr1 is not bool) return null;

            if (op == "&&") return (bool)expr0 && (bool)expr1;
            if (op == "||") return (bool)expr0 || (bool)expr1;

            return null;
        }

        public override object VisitWriteBuiltIn([NotNull] MexParser.WriteBuiltInContext context)
        {
            Console.Write(Visit(context.expression()));
            return null;
        }
        public override object VisitWriteLnBuiltIn([NotNull] MexParser.WriteLnBuiltInContext context)
        {
            Console.WriteLine(Visit(context.expression()));
            return null;
        }
        public override object VisitReadLnBuiltIn([NotNull] MexParser.ReadLnBuiltInContext context)
        {
            return Console.ReadLine();
        }
    }
}
