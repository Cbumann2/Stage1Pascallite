// Connor Bumann and Ian Park
// CS 4301
// Stage 1

#include <stage1.h>
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <ctime>
#include <iomanip>
#include <vector>
// functions for <getTime>
// functions for <ostrstream>
// functions for <istrstream>

using namespace std;

Compiler::Compiler(char **argv) // constructor
{
    // open sourceFile using argv[1]
    // open listingFile using argv[2]
    // open objectFile using argv[3]
    sourceFile.open(argv[1]);
    listingFile.open(argv[2]);
    objectFile.open(argv[3]);
}

Compiler::~Compiler() // destructor
{
   // close all open files
   sourceFile.close();
   listingFile.close();
   objectFile.close();
}

void Compiler::createListingHeader()
{
    time_t now = time (NULL);

    listingFile << "STAGE1: Connor Bumann, Ian Park        "; // add DATE, TIME OF DAY
    listingFile << ctime(&now) << endl;
    
    listingFile << "LINE NO.              SOURCE STATEMENT" << endl;
    lineNo++;
    listingFile << endl << setw(5) << lineNo << "|";
}

// Methods implementing the grammar productions
void Compiler::parser()
{
    //ch must be initialized to the first character of the source file
    //if (nextToken() != "program")
    //    processError("keyword \"program\" expected");
    //a call to nextToken() has two effects
    // (1) the variable, token, is assigned the value of the next token
    // (2) the next token is read from the source file in order to make
    // the assignment. The value returned by nextToken() is also
    // the next token.
    //prog()
    //parser implements the grammar rules, calling first rule
    nextChar();
    nextToken();
    if (token != "program"){
        processError("keyword \"program\" expected");
    }
    prog();
}

void Compiler::createListingTrailer()
{
    // print "COMPILATION TERMINATED", "# ERRORS ENCOUNTERED"
    // listingFile << endl;
    // listingFile << left;
    // listingFile << setw(28) << "COMPILATION TERMINATED" << errorCount << " ERRORS ENCOUNTERED";
    string error;
    
    if (errorCount == 1) {
        error = " ERROR";
    }
    else {
        error = " ERRORS";
    }
    listingFile << endl << left << setw(28) << "COMPILATION TERMINATED" << errorCount << error << " ENCOUNTERED" << endl;
}

void Compiler::processError(string err)
{
    // Output err to listingFile
    // Call exit(EXIT_FAILURE) to terminate program
    errorCount++;
    listingFile << endl << "Error: Line " << lineNo << ": " << err << endl;
    createListingTrailer();
    exit(EXIT_FAILURE);
}

// Methods implementing the grammar productions
void Compiler::prog() //token should be "program"
{
    if (token != "program")
    {
        processError("keyword \"program\" expected");
    }
    progStmt();
    if (token == "const")
    {
        consts();
    }
    if (token == "var")
    {
        vars();
    }
    if (token != "begin")
    {
      processError("keyword \"begin\" expected");
    }
    beginEndStmt();
    if (token != "$")
    {
      processError("no text may follow \"end\"");
    }
}

void Compiler::progStmt() //token should be "program"
{
    string x;

    if (token != "program")
    {
        processError("keyword \"program\" expected");
    }

    x = nextToken();

    if (!isNonKeyId(token))
    {
        processError("program name expected");
    }

    if (nextToken() != ";")
    {
        processError("semicolon expected");
    }

    nextToken();
    code ("program", x);
    
    insert(x, PROG_NAME,CONSTANT, x, NO, 0);
}
void Compiler::consts() //token should be "const"
{
  if (token != "const")
  {
    processError("keyword \"const\" expected");
  }
  if (!isNonKeyId(nextToken()))
  {
    processError("non-keyword identifier must follow \"const\"");
  }

  constStmts();
}
void Compiler::vars() //token should be "var"
{
    if (token != "var")
    {
        processError("keyword \"var\" expected");
    }
    if (!(isNonKeyId(nextToken())))
    {
        processError("non-keyword identifier must follow \"var\"");
    }
    varStmts();
}
void Compiler::beginEndStmt() //token should be "begin"
{
    if (token != "begin")
    {
        processError("keyword \"begin\" expected");
    }
    
    execStmts();

    if (token != "end")
    {
        processError("keyword \"end\" expected");
    }

    if (nextToken() != ".")
    {
        processError("period expected");
    }
    nextToken();
    code("end", ".");
}
void Compiler::constStmts() //token should be NON_KEY_ID
{
    string x, y;

    if (!isNonKeyId(token))
    {
        processError("non-keyword identifier");
    }

    x = token;

    if (nextToken() != "=")
    {
        processError("\"=\" expected");
    }

    y = nextToken();

    if (y != "+" && y != "-" && y != "not" && !isNonKeyId(y) && y != "true" && y != "false" && !isInteger(y))
    {
        processError("token to right of \"=\" illegal");
    }
    if (y == "+" || y == "-")
    {
        if (!isInteger(nextToken()))
        {
            processError("integer expected after sign");
        }
        y = y + token;
    }
    if (y == "not")
    {  
        if (!isBoolean(nextToken()) && !isNonKeyId(token))
        {
            processError("boolean or non-keyword identifier expected after \"not\"");
        }
        if (isNonKeyId(token)) {
            
            if(whichType(token) == BOOLEAN){
                if(whichValue(token) == "true"){
                    y = "false";
                }
                else{
                    y = "true";
                }
            }
            else {
                processError("boolean expected after not");
            }
        }
        else
        {
            if (token == "true")
            {
              y = "false";
            }
            else if (token == "false")
            {
               y = "true";
            }
        }
       
    }
    if (nextToken() != ";")
    {
        processError("semicolon expected");
    }
    if (whichType(y) != INTEGER && whichType(y) != BOOLEAN)
    {
        processError("data type of token on the right-hand side must be INTEGER or BOOLEAN");
    }
    
    insert(x,whichType(y),CONSTANT,whichValue(y),YES,1);

    x = nextToken();

    if (x != "begin" && x != "var" && !(isNonKeyId(x)))
    {
        processError("non-keyword identifier or \"begin\" or \"var\" expected");
    }
    if (isNonKeyId(x))
    {
        constStmts();
    }
}
void Compiler::varStmts() //token should be NON_KEY_ID
{
    string x, y;

    if(!(isNonKeyId(token)))
    {
        processError("non-keyword identifier expected");
    }

    x = ids();

    if (token != ":")
    {
        processError("\":\" expected");
    }
    
    if ( nextToken() != "integer" && token != "boolean")
    {
        processError("illegal type follows \":\"");
    }

    y = token;

    if (nextToken() != ";")
    {
        processError("semicolon expected");
    }
    
    if (y == "integer") {
        insert(x,INTEGER,VARIABLE,"",YES,1);
    } else {
        insert(x,BOOLEAN,VARIABLE,"",YES,1);
    }
    
    if (!isNonKeyId(nextToken()) && token != "begin")
    {
        processError("non-keyword identifier or \"begin\" expected");
    }

    if (isNonKeyId(token))
    {
        varStmts();
    }
}
string Compiler::ids() //token should be NON_KEY_ID
{
    string temp, tempString;

    if (!(isNonKeyId(token)))
    {
        processError("non-keyword identifier expected");
    }

    tempString = token;
    temp = token;

    if (nextToken() == ",")
    {
        if (!isNonKeyId(nextToken()))
        {
            processError("non-keyword identifier expected");
        }
        tempString = temp + "," + ids();
    }
    return tempString;
}
// TODO STAGE1 START

void Compiler::execStmts() {
    // get the next token and see if its end, and leave function if it is
    if (nextToken() == "end") {
        return;
    }
    if (token != "read" && token != "write" && !isNonKeyId(token)) {
        processError("expected \"read\", \"write\", or non-keyword identifier");
    }
    execStmt();
    execStmts();
}
void Compiler::execStmt() {
    if (isNonKeyId(token)) {
        assignStmt();
    }
    else if (token == "read") {
        readStmt();
    }
    else if (token == "write") {
        writeStmt();
    }
    // I dont think this needs an else but im putting one in anyway!
    // rewrite the process error if needed
    else {
        processError("Error in execStmt");
    }
} 
void Compiler::assignStmt() {
    string x = token;
    if (!isNonKeyId(x)) {
        processError("expected non-keyword identifier");
    }
    pushOperand(x);
    if (nextToken() != ":=") {
        processError("expected \":=\"");
    }
    pushOperator(token);
    if(nextToken() != "not" && token != "true" && token != "false" && token != "("&& token != "+" && token != "-" && !isInteger(token) && !isNonKeyId(token)) {
        processError("expected expression");
    }
    express();
    if (token != ";") {
        processError("expected \";\"");
    }
    code(popOperator(),popOperand(),popOperand());
}
void Compiler::readStmt() {
    string x;
    if (token != "read") {
        processError("expected \"read\"");
    }
    //Read List Start
    if (nextToken() != "(") {
        processError("expected \"(\"");
    }
    nextToken();
    x = ids();
    if (token != ")") {
        processError("expected \")\"");
    }
    code("read",x);
    //Read List End
    if (nextToken() != ";") {
        processError("expected \";\"");
    }
} 
void Compiler::writeStmt() {
    string x;
    if (token != "write") {
        processError("expected \"write\"");
    }
    // Write List Start
    if (nextToken() != "(") {
        processError("expected \"(\"");
    }
    nextToken();
    x = ids();
    if (token != ")") {
        processError("expected \")\"");
    }
    code("write",x);
    //Write List End
    if (nextToken() != ";") {
        processError("expected \";\"");
    }
}
void Compiler::express() {
    if(token != "not" && token != "+" && token != "-" && !isInteger(token) && !isBoolean(token) && !isNonKeyId(token) && token != "(") {
        processError("expected expression");
    }
    term();
    expresses();
} 
void Compiler::expresses() {
    string x = token;
    if (x == "=" || x == "<>" || x == "<=" || x == ">=" || x == "<" || x == ">") {
        pushOperator(x);
        nextToken();
        term();
        string op = popOperator();
        string operand1 = popOperand();
        string operand2 = popOperand();
        code(op, operand1, operand2);
        expresses();
    }
} 
void Compiler::term() {
    if(token != "not" && token != "+" && token != "-" && !isInteger(token) && !isBoolean(token) && !isNonKeyId(token) && token != "(") {
        processError("expected expression");
    }
    factor();
    terms();
}
void Compiler::terms() {
    string x = token;
    if (x == "+" || x == "-" || x == "or") {
        pushOperator(x);
        nextToken();
        factor();
        string op = popOperator();
        string operand1 = popOperand();
        string operand2 = popOperand();
        code(op, operand1, operand2);
        terms();
    }
}    
void Compiler::factor() {
    if (token != "not" && token != "+" && token != "-" && !isInteger(token) && !isBoolean(token) && !isNonKeyId(token) && token != "(") {
        processError("expected expression");
    }
    part();
    nextToken();
    factors();
}    
void Compiler::factors() {
    string x = token;
    //mult_level_op
    if (x == "*" || x == "div" || x == "mod" || x == "and") { 
        pushOperator(x);
        nextToken();
        part();
        string op = popOperator();
        string operand1 = popOperand();
        string operand2 = popOperand();
        code(op, operand1, operand2);
        nextToken();
        factors();
    }
}   
void Compiler::part() {
    string x = token;
    if (token == "not") {
        nextToken();
        x = token;
        if ( token == "(") {
            nextToken();
            express();
            if (token != ")") {
                processError("expected \")\"");
            }
            string popped = popOperand();
            code("not",popped);
        }
        else if (isBoolean(x)) {
            if (x == "true") {
                x = "false";
            }
            else {
                x = "true";
            }
            pushOperand(x);;
        }
        else if (isNonKeyId(x)) {
            code("not", x);
        }
        else {
            processError("expected \"(\", BOOLEAN, or non-keyword identifier");
        }
    }
    else if (token == "+") {
        nextToken();
        x = token;
        if ( token == "(") {
            nextToken();
            express();
            if (token != ")") {
                processError("expected \")\"");
            }
        }
        else if (isInteger(x) || isNonKeyId(x)) {
            pushOperand(x);
        }
        else {
            processError("expected \"(\", INTEGER, or non-keyword identifier");
        }
    }
    else if (token == "-") {
        nextToken();
        x = token;
        if ( token == "(") {
            nextToken();
            express();
            if (token != ")") {
                processError("expected \")\"");
            }
            string popped = popOperand();
            code("neg",popped);
        }
        else if (isInteger(x)) {
            pushOperand("-"+x);
        }
        else if (isNonKeyId(x)) {
            code("neg", x);
        }
        else {
            processError("expected \"(\", INTEGER, or non-keyword identifier");
        }
    }
    else if(isInteger(token) || isBoolean(token) || isNonKeyId(token)) {
        pushOperand(token);
    }
    else if(token == "(") {
        nextToken();
        express();
        if (token != ")") {
            processError("expected \")\"");
        }
    } 
    else {
        processError("part error");
    }
}   
// TODO STAGE1 END

// Helper functions for the Pascallite lexicon
bool Compiler::isKeyword(string s) const { // determines if s is a keyword
    vector<string> keywords = {"program", "begin", "end", "var", "const", "integer", "boolean", "true", "false", "not", "mod", "div", "or", "read", "write"};
    for(uint i = 0; i < keywords.size(); ++i) {
        if(s == keywords[i])
            return true;
    }
    return false;
}

bool Compiler::isSpecialSymbol(char c) const { // determines if c is a special symbol
    vector<char> symbols = {'=',':',',',';','.','+','-',':','*','(',')','<','>'};
    for (uint i = 0; i < symbols.size(); ++i) {
        if(c == symbols[i]){
            return true;
        }
    }
    return false;
}

bool Compiler::isNonKeyId(string s) const {// determines if s is a non_key_id
    if(isKeyword(s)) {
        return false;
    }
    
    if (!isalpha((char)s[0])) {
        return false;
    }
    for(uint i = 1; i < s.length(); ++i) {
        if((char)s[i] == '_' && (char)s[i+1] != '_' && ( isalpha((char)s[i+1]) || isdigit((char)s[i+1]) )) {
            
        }
        else if (isalpha((char)s[i]) || isdigit((char)s[i])) {
            
        }
        else {
            return false;
        }
    }
    return true;
}

bool Compiler::isInteger(string s) const { // determines if s is an integer
    // num nums!
    if(!isdigit((char)s[0])) {
        return false;
    }
    for(uint i = 1; i < s.length(); ++i) {
        if(!isdigit((char)s[i])) {
            return false;
        }
    }
    return true;
}

bool Compiler::isBoolean(string s) const {  // determines if s is a boolean4
    if (s == "true") {
        return true;
    }
    else if (s == "false") {
        return true;
    }
    else {
        return false;
    }
}

// Issues may happen with the + and - part and maybe even not because of spaces
bool Compiler::isLiteral(string s) const {  // determines if s is a literal
    if(isInteger(s)) {
        return true;
    } 
    else if (s == "true" || s == "false") {
        return true;
    }
    else if (s.find("not") != string::npos) {
        s.erase(s.find("not"), 4);
        if(isBoolean(s)){
            return true;
        }
        return false;
    }
    else if (s.find("+") != string::npos) {
        s.erase(s.find("+"), 1);
        if(isInteger(s)){
            return true;
        }
        return false;
    }
    else if (s.find("-") != string::npos) {
        s.erase(s.find("-"), 1);
        if(isInteger(s)){
            return true;
        }
        return false;
    }
    else {
        return false;
    }
}

// Action routines
void Compiler::insert(string externalName,storeTypes inType, modes inMode, string inValue,allocation inAlloc, int inUnits) 
{
    static int numTableEntries;
    string name;
    name = externalName.substr(0, externalName.find(','));
    if (externalName.find(',') != string::npos){
        externalName.erase(0, externalName.find(',')+1);
    } else {
        externalName = "";
    }
    if (name.length() > 15) {
            name.erase(15,name.length());
    }
    while ( name != "") {
        if (symbolTable.find(name) != symbolTable.end()) {
           processError("symbol "+ name +" is multiply defined");
        }
        else if (isKeyword(name)) {
            processError("illegal use of keyword");
        }
        else { //create table entry
            numTableEntries++;
            if (numTableEntries <= 256) {
                if (isupper(name[0])) {
                    //symbolTable[name]=(name,inType,inMode,inValue,inAlloc,inUnits);
                    SymbolTableEntry ste(name,inType,inMode,inValue,inAlloc,inUnits);
                    symbolTable.insert(pair<string, SymbolTableEntry>(name, ste));
                }
                else {
                    //symbolTable[name]=(genInternalName(inType),inType,inMode,inValue,inAlloc,inUnits)
                    SymbolTableEntry ste(genInternalName(inType),inType,inMode,inValue,inAlloc,inUnits);
                    symbolTable.insert(pair<string, SymbolTableEntry>(name, ste));
                }
            }
            else {
                processError("Symbol Table overflow");
            }
        }
        name = externalName.substr(0, externalName.find(','));
        if (externalName.find(',') != string::npos){
            externalName.erase(0, externalName.find(',')+1);
        } else {
            externalName = "";
        }
        if (name.length() > 15) {
            name.erase(15,name.length());
        }
    }
}

storeTypes Compiler::whichType(string name) //tells which data type a name has
{
  storeTypes dataType;

  if(isLiteral(name))
  {
      if(isBoolean(name)) 
      {
          dataType = BOOLEAN;
      }
      else
      {
          dataType = INTEGER;
      }
  }
  else if (symbolTable.find(name) != symbolTable.end())
  {
      dataType = symbolTable.at(name).getDataType();
  }
  else
  {
      processError("reference to undefined constant");
  }
  return dataType;
}

string Compiler::whichValue(string name) //tells which value a name has
{
    string value = "";
    if (isLiteral(name))
    {
        value = name;
    }
    else
    {
        if (symbolTable.find(name) != symbolTable.end())
        {
            value = symbolTable.at(name).getValue();
        }
        else
        {
            processError("reference to undefined constant");
        }
    }
    return value;
}
// TODO
void Compiler::code(string op, string operand1, string operand2)
{
    if (op == "program")
    {
        emitPrologue(operand1);
    }
    else if (op == "end")
    {
        emitEpilogue();
    }
    else if (op == "read") {
        emitReadCode(operand1);
    }
    else if (op == "write") {
        emitWriteCode(operand1);
    }
    else if (op == "+") {
        emitAdditionCode(operand1, operand2);
    }
    else if (op == "-") {
        emitSubtractionCode(operand1, operand2);
    }
    else if (op == "neg") {
       emitNegationCode(operand1);
    }
    else if (op == "not") {
       emitNotCode(operand1);
    }
    else if (op == "*") {
       emitMultiplicationCode(operand1, operand2);
    }
    else if (op == "div") {
       emitDivisionCode(operand1, operand2);
    }
    else if (op == "mod") {
       emitModuloCode(operand1, operand2);
    }
    else if (op == "and") {
       emitAndCode(operand1, operand2);
    }
    else if (op == "or") {
       emitOrCode(operand1, operand2);
    }
    else if (op == ":=") {
       emitAssignCode(operand2, operand1);
    }
    else if (op == "=") {
        emitEqualityCode(operand1, operand2);
    }
    else if (op == "<>") {
      emitInequalityCode(operand1, operand2);
    }
    else if (op == "<") {
      emitLessThanCode(operand1, operand2);
    }
    else if (op == "<=") {
      emitLessThanOrEqualToCode(operand1, operand2);
    }
    else if (op == ">") {
      emitGreaterThanCode(operand1, operand2);
    }
    else if (op == ">=") {
      emitGreaterThanOrEqualToCode(operand1, operand2);
    }
    else
    {
        processError("compiler error since function code should not be called with illegal arguments");
    }
}
// TODO STAGE1 START
void Compiler::pushOperator(string op) { //push name onto operatorStk
    operatorStk.push(op);
}

void Compiler::pushOperand(string operand) {
    if (isLiteral(operand) && symbolTable.find(operand) == symbolTable.end()) {
        if (isBoolean(operand)) 
        {
            if (operand == "true")
                symbolTable.insert(pair<string, SymbolTableEntry>(operand, SymbolTableEntry("TRUE", BOOLEAN, CONSTANT, "true", YES, 1)));
            else if (operand == "false")
                symbolTable.insert(pair<string, SymbolTableEntry>(operand, SymbolTableEntry("FALSE", BOOLEAN, CONSTANT, "false", YES, 1)));
        }
        else
        {
            insert(operand, whichType(operand), CONSTANT, operand, YES, 1);
        }
    }
   operandStk.push(operand);
}
string Compiler::popOperator() { 
   if (!operatorStk.empty()) {
        string popped = operatorStk.top();
        operatorStk.pop();
        return popped;
   }
    else {
        processError("compiler error; operator stack underflow");
        return "";
    }
}
string Compiler::popOperand() {
    if (!operandStk.empty()) {
        string popped = operandStk.top();
        operandStk.pop();
        return popped;
    }
    else {
        processError("compiler error; operand stack underflow");
        return "";
    }
}
// TODO STAGE1 END

// Emit Functions
void Compiler::emit(string label, string instruction, string operands, string comment)
{
  objectFile << left;
  objectFile << setw(8) << label;
  objectFile << setw(8) << instruction;
  objectFile << setw(24) << operands;
  objectFile << comment;
  objectFile << endl;
}
void Compiler::emitPrologue(string progName, string operand2)
{
    time_t now = time (NULL);
// Identifying Comments
    objectFile << left;
    objectFile << setw(30) << "; Connor Bumann, Ian Park" << ctime(&now);
    
// %INCLUDE Directives
    objectFile << "%INCLUDE \"Along32.inc\"" << endl;
    objectFile << "%INCLUDE \"Macros_Along.inc\"" << endl;

// Begining Statements
    if (progName.length() > 15) {
            progName.erase(15,progName.length());
    }
    objectFile << endl;
    emit("SECTION", ".text");
    emit("global", "_start", "", "; program " + progName);
    objectFile << endl;
    emit("_start:");
}
void Compiler::emitEpilogue(string operand1, string operand2)
{
// Write exit statement
    emit("","Exit", "{0}");
    emitStorage();
}

void Compiler::emitStorage()
{
    // writes .data
    objectFile << endl;
    string internalName;
    string instruction;
    string value;
    string comment_name;
    emit("SECTION", ".data");
    //emit(string label, string instruction, string operands, string comment)
    for (map<string, SymbolTableEntry>::iterator steItr = symbolTable.begin(); steItr != symbolTable.end(); ++steItr) {
        if(steItr->second.getAlloc() == YES && steItr->second.getMode() == CONSTANT) {
            comment_name = "; " + steItr->first;
            internalName = steItr->second.getInternalName();
            instruction = "dd";
            if (steItr->second.getDataType() == BOOLEAN) {
                if (steItr->second.getValue() == "true") 
                {
                    value = "-1";
                }
                else
                {
                    value = "0";
                }
            }
            else 
            {
            value = steItr->second.getValue();
            }
            emit(internalName, instruction, value, comment_name);
        }
    }
    objectFile << endl;
    // write .bss
    emit("SECTION", ".bss");
    for (map<string, SymbolTableEntry>::iterator steItr = symbolTable.begin(); steItr != symbolTable.end(); ++steItr) {
        if(steItr->second.getAlloc() == YES && steItr->second.getMode() == VARIABLE) {
            comment_name = "; " + steItr->first;
            internalName = steItr->second.getInternalName();
            instruction = "resd";
            if(steItr->second.getValue() == "") {
                value = "1";
            }
            emit(internalName, instruction, value, comment_name);
        }
    }
}

// TODO STAGE1 START
void Compiler::emitReadCode(string operand, string operand2) {
    string name;
    name = operand.substr(0, operand.find(','));
    if (operand.find(',') != string::npos){
        operand.erase(0, operand.find(',')+1);
    } else {
        operand = "";
    }
    if (name.length() > 15) {
            name.erase(15,name.length());
    }
    while (name != "")
    {
        if (symbolTable.find(name) == symbolTable.end()) {
            processError("reference to undefined symbol");
        }
        if (whichType(name) != INTEGER ) {
            processError("can't read variables of this type");
        }
        if (symbolTable.at(name).getMode() != VARIABLE ) {
            processError("attempting to read to a read-only location");
        }
        // emit code to call the Irvine ReadInt function
        //emit(string label, string instruction, string operands, string comment)
        emit("", "call", "ReadInt", "; read int; value placed in eax");
        emit("", "mov", "["+symbolTable.at(name).getInternalName()+"],eax", "; store eax at "+name);
        contentsOfAReg = name;
        
        name = operand.substr(0, operand.find(','));
        if (operand.find(',') != string::npos){
            operand.erase(0, operand.find(',')+1);
        } else {
            operand = "";
        }
        if (name.length() > 15) {
                name.erase(15,name.length());
        }
    }
}
void Compiler::emitWriteCode(string operand, string operand2) {
    string name;
    //static bool definedStorage = false;
    name = operand.substr(0, operand.find(','));
    if (operand.find(',') != string::npos){
        operand.erase(0, operand.find(',')+1);
    } else {
        operand = "";
    }
    if (name.length() > 15) {
            name.erase(15,name.length());
    }
    while (name != "")
    {
        if (symbolTable.find(name) == symbolTable.end()) {
            processError("reference to undefined symbol");
        }
        if (name != contentsOfAReg) {
            emit("", "mov", "eax,["+symbolTable.at(name).getInternalName()+"]", "; load "+name+" in eax");
            contentsOfAReg = name;
        }
        if (whichType(name) == INTEGER  || whichType(name) == BOOLEAN) {
            emit("", "call", "WriteInt", "; write int in eax to standard out");
        } 
        //emit(string label, string instruction, string operands, string comment)
        emit("", "call", "Crlf", "; write \\r\\n to standard out");
        name = operand.substr(0, operand.find(','));
        if (operand.find(',') != string::npos){
            operand.erase(0, operand.find(',')+1);
        } else {
            operand = "";
        }
        if (name.length() > 15) {
                name.erase(15,name.length());
        }
    }
}
void Compiler::emitAssignCode(string operand1, string operand2) {         // op2 = op1
    // if types of operands are not the same
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types");
    }
    // if storage mode of operand2 is not VARIABLE
    if (symbolTable.at(operand2).getMode() != VARIABLE) {
        processError("symbol on left-hand side of assignment must have a storage mode of VARIABLE");
    }
    if (operand1 == operand2) {
        return;
    }
    // if operand1 is not in the A register then
    if (operand1 != contentsOfAReg) {
        // emit code to load operand1 into the A register
        emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
    }
    // emit code to store the contents of that register into the memory location pointed to by operand2
    emit("","mov","["+symbolTable.at(operand2).getInternalName()+"],eax", "; " + operand2 + " = AReg");
    contentsOfAReg = operand2;
    // if operand1 is a temp then free its name for reuse
    if (isTemporary(operand1)) {
        freeTemp();
    }
    //operand2 can never be a temporary since it is to the left of ':='
}
void Compiler::emitAdditionCode(string operand1, string operand2) {       // op2 +  op1
    // if type of either operand is not integer
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
            processError("binary '+' requires integer operands");
    }
    // if the A Register holds a temp not operand1 nor operand2 then
    if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
      // emit code to store that temp into memory
      // change the allocate entry for the temp in the symbol table to yes
      // deassign it
      emit("", "mov", "["+contentsOfAReg+"],eax", "; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      contentsOfAReg = "";
    }
    // if the A register holds a non-temp not operand1 nor operand2 then deassign it
    else if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        contentsOfAReg = "";
    }
    // if neither operand is in the A register then
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        // emit code to load operand2 into the A register
        // emit code to perform register-memory addition
        emit("", "mov", "eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand2);
        contentsOfAReg = operand2;
    }
    if(contentsOfAReg == operand1) {
        emit("","add","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1 + " + " + operand2);
    }
    else {
        emit("","add","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand2 + " + " + operand1);
    }
    // deassign all temporaries involved in the addition and free those names for reuse
    if (isTemporary(operand1)) {
        freeTemp();
    }
    if (isTemporary(operand2)) {
        freeTemp();
    }
    // A Register = next available temporary name and change type of its symbol table entry to integer
    // push the name of the result onto operandStk
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}
void Compiler::emitSubtractionCode(string operand1, string operand2) {    // op2 -  op1
    // if type of either operand is not integer
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
            processError("illegal type expected INTEGER");
    }
    // if the A Register holds a temp not operand1 nor operand2 then
    if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
      // emit code to store that temp into memory
      // change the allocate entry for the temp in the symbol table to yes
      // deassign it
      emit("", "mov", "["+contentsOfAReg+"],eax", "; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      contentsOfAReg = "";
    }
    // if the A register holds a non-temp not operand1 nor operand2 then deassign it
    else if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        contentsOfAReg = "";
    }
    // if neither operand is in the A register then
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        // emit code to load operand2 into the A register
        // emit code to perform register-memory addition
        emit("", "mov", "eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand2);
        contentsOfAReg = operand2;
    }
    if(contentsOfAReg == operand1) {
        emit("","sub","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1 + " - " + operand2);
    }
    else {
        emit("","sub","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand2 + " - " + operand1);
    }
    // deassign all temporaries involved in the addition and free those names for reuse
    if (isTemporary(operand1)) {
        freeTemp();
    }
    if (isTemporary(operand2)) {
        freeTemp();
    }
    // A Register = next available temporary name and change type of its symbol table entry to integer
    // push the name of the result onto operandStk
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}
void Compiler::emitMultiplicationCode(string operand1, string operand2) { // op2 *  op1
    //emit(string label, string instruction, string operands, string comment)
    // if type of either operand is not integer
    if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("illegal type expected INTEGER");
    }
    // if the A register holds a temp not operand1 nor operand2 then deassign it
    if(isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
        // emit code to store that temp into memory
        // change the allocate entry for the temp in the symbol table to yes
        // deassign it 
        emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    // if the A register holds a non-temp not operand1 nor operand2 then deassign it
    if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
        contentsOfAReg = "";
    }
    // if neither operand is in the A register then
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2){
        emit("","mov","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand2);
        contentsOfAReg = operand2;
    }
    
    if (contentsOfAReg == operand2) {
        emit("","imul","dword ["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand2 + " * " + operand1);
    }
    else {
        emit("","imul","dword ["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1 + " * " + operand2);
    }
    
    //  deassign all temporaries involved and free those names for reuse
    if (isTemporary(operand1)) {
        freeTemp();
    }
    if (isTemporary(operand2)) {
        freeTemp();
    }
    // A Register = next available temporary name and change type of its symbol table entry to integer
    // push the name of the result onto operandStk
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}
void Compiler::emitDivisionCode(string operand1, string operand2) {       // op2 /  op1
    //emit(string label, string instruction, string operands, string comment)
    // if type of either operand is not integer
    if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("illegal type expected INTEGER");
    }
    // if the A register holds a temp not operand2 then
    if(isTemporary(contentsOfAReg) && contentsOfAReg != operand2){
        // emit code to store that temp into memory
        // change the allocate entry for the temp in the symbol table to yes
        // deassign it 
        emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    // if the A register holds a non-temp not operand2 then deassign it
    if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
        contentsOfAReg = "";
    }
    // if operand2 is not in the A register
    if(contentsOfAReg != operand2){
        emit("","mov","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1);
        contentsOfAReg = operand2;
    }
    emit("","cdq","", "; sign extend dividend from eax to edx:eax");
    emit("","idiv","dword ["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand2 + " div " + operand1);
    
    //  deassign all temporaries involved and free those names for reuse
    if (isTemporary(operand1)) {
        freeTemp();
    }
    if (isTemporary(operand2)) {
        freeTemp();
    }
    // A Register = next available temporary name and change type of its symbol table entry to integer
    // push the name of the result onto operandStk
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}
void Compiler::emitModuloCode(string operand1, string operand2) {         // op2 %  op1
    //emit(string label, string instruction, string operands, string comment)
    // if type of either operand is not integer
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("illegal type expected INTEGER");
    }
    // if the A register holds a temp not operand2 then
    if (isTemporary(contentsOfAReg) && contentsOfAReg != operand2){
        // emit code to store that temp into memory
        // change the allocate entry for the temp in the symbol table to yes
        // deassign it 
        emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    // if the A register holds a non-temp not operand2 then deassign it
    if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
        contentsOfAReg = "";
    }
    // if operand2 is not in the A register
    if (contentsOfAReg != operand2){
        emit("","mov","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand2);
        contentsOfAReg = operand2;
    }
    
    emit("","cdq","", "; sign extend dividend from eax to edx:eax");
    emit("","idiv","dword ["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand2 + " div " + operand1);
    emit("","xchg", "eax,edx", "; exchange quotient and remainder");
    
    //  deassign all temporaries involved and free those names for reuse
    if (isTemporary(operand1)) {
        freeTemp();
    }
    if (isTemporary(operand2)) {
        freeTemp();
    }
    // A Register = next available temporary name and change type of its symbol table entry to integer
    // push the name of the result onto operandStk
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}
void Compiler::emitNegationCode(string operand1, string ) {           // -op1
    //emit(string label, string instruction, string operands, string comment)
}
void Compiler::emitNotCode(string operand1, string ) {                // !op1
    //emit(string label, string instruction, string operands, string comment)
}
void Compiler::emitAndCode(string operand1, string operand2) {            // op2 && op1
    //emit(string label, string instruction, string operands, string comment)
}
void Compiler::emitOrCode(string operand1, string operand2) {             // op2 || op1
    //emit(string label, string instruction, string operands, string comment)
}
void Compiler::emitEqualityCode(string operand1, string operand2) {       // op2 == op1
    //emit(string label, string instruction, string operands, string comment)
}
void Compiler::emitInequalityCode(string operand1, string operand2) {     // op2 != op1
    //emit(string label, string instruction, string operands, string comment)
}
void Compiler::emitLessThanCode(string operand1, string operand2) {       // op2 <  op1
    //emit(string label, string instruction, string operands, string comment)
}
void Compiler::emitLessThanOrEqualToCode(string operand1, string operand2) { // op2 <= op1
    //emit(string label, string instruction, string operands, string comment)
}
void Compiler::emitGreaterThanCode(string operand1, string operand2) {    // op2 >  op1
    //emit(string label, string instruction, string operands, string comment)
   // check if correct type
   if (whichType(operand1) != whichType(operand2))
   {
      processError("illegal type expected INTEGER");
   }      
   // if AReg holds temp and not operand2
   if (isTemporary(contentsOfAReg) && contentsOfAReg != operand2)
   {
      emit("", "mov", "[" + contentsOfAReg + "], eax", "; deassign Areg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      contentsOfAReg = "";
   }
   // if A holds non-temp and isn't operand2
   if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand2)
   {
      contentsOfAReg = "";
   }
   // if operand2 is not in AReg
   if (contentsOfAReg != operand2)
   {
      emit("", "mov", "eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = "  + operand2);
      contentsOfAReg = operand2;
   }
   string label1 = getLabel();
   // emit
   emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
   emit("", "jg", label1, "; if " + operand2 + " > " + operand1 + " then jump to set eax to TRUE");
   emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
   
   // creates FALSES  if it doesn't exsit
   pushOperand("false");
   popOperand();
   
   string label2 = getLabel();
   emit("", "jmp", label2, "; unconditionally jump");
   emit(label1+":","","","");
   emit("","mov", "eax,[TRUE]", "; set eax to TRUE");
   
   // creates TRUE  if it doesn't exsit
   pushOperand("true");
   popOperand();   
   
   emit(label2+":","","","");
   
   // free if temp
   if (isTemporary(operand1))
   {
      freeTemp();
   }
   if (isTemporary(operand2))
   {
      freeTemp();
   }
   
   // assign new temp for result
   contentsOfAReg = getTemp();
   symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
   
   // push result
   pushOperand(contentsOfAReg);
}
void Compiler::emitGreaterThanOrEqualToCode(string operand1, string operand2) { // op2 >= op1
    //emit(string label, string instruction, string operands, string comment)
}
// TODO STAGE1 END

// Lexical routines
char Compiler::nextChar()
{
    static bool insertNewLine = false;
    sourceFile.get(ch);
    if (sourceFile.eof())
    {
        ch = END_OF_FILE;
        return ch;
    }
    if (insertNewLine) {
       lineNo++;
       listingFile << endl << setw(5) << lineNo << "|";
       insertNewLine = false;
    }
 
    if (ch == '\n')
    {
        if (sourceFile.peek() != EOF)
        {
            insertNewLine = true;
        }
        else 
        {
            listingFile << endl;
        }
    }
    else
    {
        listingFile.put(ch);
    }
    
    return ch;
}

string Compiler::nextToken() //returns the next token or end of file marker
{
    token = "";
    while (token == "")
    {
        if (ch == '{') {
            while (nextChar() != END_OF_FILE && ch != '}') 
            {
                
            }
            if (ch == END_OF_FILE)
                processError("unexpected end of file");
            else
                nextChar();
        }
        else if (ch == '}') { 
            processError("'}' cannot begin token");
        }
        else if (isspace(ch)) {
            nextChar(); 
        }
        else if (isSpecialSymbol(ch)) {
            token = ch;
            nextChar();
            // this my cause errors
            if (token == ":" && ch == '=') {
                token += ch;
                nextChar();
            }
            if (token == "<" && (ch == '>' || ch == '=')) {
                token += ch;
                nextChar();
            }
            else if (token == ">" && ch == '=') {
                token += ch;
                nextChar();
            }
                
        }
        else if (islower(ch)) {
            token = ch;
            nextChar();
            while ((isalpha(ch) || isdigit(ch) || ch == '_') && ch != END_OF_FILE)
            {
                token += ch;
                nextChar();
            }
            if (ch == END_OF_FILE)
                processError("unexpected end of file");
        }
        else if (isdigit(ch)) {
            token = ch;
            nextChar();
            while (isdigit(ch) && ch != END_OF_FILE)
            {
                token += ch;
                nextChar();
            }
            if (ch == END_OF_FILE)
                processError("unexpected end of file");
        }
        else if (ch == END_OF_FILE) {
            token = ch;
        }
        else { // Illegal symbol error
            processError("illegal symbol");
        }
    }
    return token;
}


// Other routines
string Compiler::genInternalName(storeTypes stype) const 
{
    static int boolCount = 0;
    static int progCount = 0;
    static int intCount = 0;
    
    string name;
    if (stype == PROG_NAME) {
        name = "P" + to_string(progCount);
        progCount++;
    } 
    else if  (stype == INTEGER) {
        name = "I" + to_string(intCount);
        intCount++;
    }
    else if (stype == BOOLEAN) {
        name = "B" + to_string(boolCount);
        boolCount++;
    }
    
    return name;
}
// TODO STAGE1 START
void Compiler::freeTemp() {
    currentTempNo--;
    if (currentTempNo < -1) {
        processError("compiler error, currentTempNo should be ≥ –1");
    }
}
string Compiler::getTemp()
{
    string temp;
    currentTempNo++;
    temp = "T" + to_string(currentTempNo);
    if (currentTempNo > maxTempNo) 
    {
        insert(temp, UNKNOWN, VARIABLE, "", NO, 1);
        maxTempNo++;
    }
    return temp;
}
string Compiler::getLabel() {
    static int i = -1;
    i++;
    string label = ".L" + to_string(i);
    return label;
}

bool Compiler::isTemporary(string s) const { // determines if s represents a temporary
    if (s.length() > 1 && s[0] == 'T')
    {
        for (size_t i = 1; i < s.length(); ++i)
        {
            if (!isdigit(s[i]))
            {
                return false;
            }
        }
        return true;
    }
    return false;
}

/*
void Compiler::emitSubtractionCode(string operand1, string operand2) { 
   if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) 
   {
       processError("binary '-' requires integer operands");
   }
   if (isTemporary(contentsOfAReg) && contentsOfAReg != operand2) 
   {
       emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
       symbolTable.at(contentsOfAReg).setAlloc(YES);
       contentsOfAReg = "";
   }
  
   if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand2) 
   {
       contentsOfAReg = "";
   }

   if (contentsOfAReg != operand2) 
   {
       emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
       contentsOfAReg = operand2;
   }
   
   emit("", "sub", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " - " + operand1);
   
   if(isTemporary(operand1))
   {
      freeTemp();
   }
   if(isTemporary(operand2))
   {
      freeTemp();
   }
    
   contentsOfAReg = getTemp();
   symbolTable.at(contentsOfAReg).setDataType(INTEGER);
   
   pushOperand(contentsOfAReg);
}
*/
