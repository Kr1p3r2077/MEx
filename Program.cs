using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using AntlrTest;
using AntlrTest.Mex;
using AntlrTest.Visit;

/*
for(float i = 0; i < 5; i += 0.1f)
{
    Console.WriteLine(i);
}
*/

bool dev = true;
if (dev)
{
    var filename = "HelloWrot.mex";

    var contents = File.ReadAllText(filename);

    PEnv.Init();

    AntlrInputStream inputStream = new AntlrInputStream(contents.ToString());
    MexLexer mexLexer = new MexLexer(inputStream);

    CommonTokenStream commonTokenStream = new CommonTokenStream(mexLexer);
    MexParser mexParser = new MexParser(commonTokenStream) { BuildParseTree = true };

    var program = mexParser.program();

    MexVisitor visitor = new MexVisitor();
    visitor.Visit(program);
    Environment.Exit(0);
}
else
{
    Console.WriteLine("Copyright MAKAR MOLOCAEV 228");
    Console.WriteLine("Input file .mex\n");
    var filename = Console.ReadLine();

    var contents = File.ReadAllText(filename);

    PEnv.Init();

    AntlrInputStream inputStream = new AntlrInputStream(contents.ToString());
    MexLexer mexLexer = new MexLexer(inputStream);

    CommonTokenStream commonTokenStream = new CommonTokenStream(mexLexer);
    MexParser mexParser = new MexParser(commonTokenStream) { BuildParseTree = true };

    var program = mexParser.program();

    MexVisitor visitor = new MexVisitor();

    Console.Clear();
    visitor.Visit(program);

    Console.WriteLine("\n--------------");
    Console.WriteLine("Program finished");

    Console.Read();
}
