using Antlr4.Runtime;
using AntlrTest;
using AntlrTest.Visit;

var filename = "HelloWrot.mex";

var contents = File.ReadAllText(filename);
PEnv.Init();

AntlrInputStream inputStream = new AntlrInputStream(contents.ToString());
MexLexer mexLexer = new MexLexer(inputStream);
CommonTokenStream commonTokenStream = new CommonTokenStream(mexLexer);
MexParser mexParser = new MexParser(commonTokenStream);
var program = mexParser.program();
MexVisitor visitor = new MexVisitor();

//Console.WriteLine(DateTime.Now.Millisecond);
visitor.Visit(program);
//Console.WriteLine(DateTime.Now.Millisecond);