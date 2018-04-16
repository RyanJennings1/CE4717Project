/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       comp2.c                                                            */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID numbers                                 */
/*                                                                          */
/*           Patrick Lu           15154149                                  */
/*           Rezvee Sikder        15140997                                  */
/*           Ryan Jennings        15152324                                  */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       smallparser                                                        */
/*                                                                          */
/*       An illustration of the use of the character handler and scanner    */
/*       in a parser for the language                                       */
/*                                                                          */
/*       <Program>     :== "BEGIN" { <Statement> ";" } "END" "."            */
/*       <Statement>   :== <Identifier> ":=" <Expression>                   */
/*       <Expression>  :== <Identifier> | <IntConst>                        */
/*                                                                          */
/*                                                                          */
/*       Note - <Identifier> and <IntConst> are provided by the scanner     */
/*       as tokens IDENTIFIER and INTCONST respectively.                    */
/*                                                                          */
/*       Although the listing file generator has to be initialised in       */
/*       this program, full listing files cannot be generated in the        */
/*       presence of errors because of the "crash and burn" error-          */
/*       handling policy adopted. Only the first error is reported, the     */
/*       remainder of the input is simply copied to the output (using       */
/*       the routine "ReadToEndOfFile") without further comment.            */
/*                                                                          */
/*       Innsctructions for compiling: make parser1 parser1.c               */
/*                                     gcc -o parser1 parser1.o libcomp.a   */
/*                                                                          */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"
#include "symbol.h"
#include "code.h"
#include "strtab.h"


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */
PRIVATE FILE *CodeFile;			       /* machine code output file */
PRIVATE int varaddress;			       /* incremented each time a new symbol table entry made */
PRIVATE int writing;               /* set to one while parsing arguments*/
PRIVATE int reading;			         /* set to one while parsing arguments*/
PRIVATE int ERROR_FLAG;            /* if any syntax errors are detected set to 1*/
PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */
PRIVATE int scope;

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] );
PRIVATE void Accept( int code );
PRIVATE void Synchronise(SET *F, SET *FB);
PRIVATE void SetupSets(void);

PRIVATE SYMBOL *MakeSymbolTableEntry(int symtype);
PRIVATE SYMBOL *LookupSymbol();

PRIVATE SET ProgramStatementFS_aug1;
PRIVATE SET ProgramStatementFS_aug2;
PRIVATE SET ProcedureStatementFS_aug1;
PRIVATE SET ProcedureStatementFS_aug2;
PRIVATE SET BlockStatementFS_aug;
PRIVATE SET ProgramStatementFBS;
PRIVATE SET ProcedureStatementFBS;
PRIVATE SET BlockStatementFBS;

PRIVATE void ParseProgram(void);
PRIVATE int ParseDeclarations(void);
PRIVATE void ParseProcDeclaration(void);
PRIVATE void ParseParameterList(void);
PRIVATE void ParseFormalParameter(void);
PRIVATE void ParseBlock(void);
PRIVATE void ParseStatement(void);
PRIVATE void ParseSimpleStatement(void);
PRIVATE void ParseRestOfStatement(SYMBOL *var);
PRIVATE void ParseProcCallList(void);
PRIVATE void ParseAssignment(void);
PRIVATE void ParseActualParameter(void);
PRIVATE void ParseWhileStatement(void);
PRIVATE void ParseIfStatement(void);
PRIVATE void ParseReadStatement(void);
PRIVATE void ParseWriteStatement(void);
PRIVATE void ParseExpression(void);
PRIVATE void ParseCompoundTerm(void);
PRIVATE void ParseTerm(void);
PRIVATE void ParseSubTerm(void);
PRIVATE int ParseBooleanExpression(void);
PRIVATE void ParseAddOp(void);
PRIVATE void ParseMultOp(void);
PRIVATE int ParseRelOp(void);
PRIVATE void ParseVariable(void);
PRIVATE void ParseIntConst(void);
PRIVATE void ParseIdentifier(void);

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Smallparser entry point.  Sets up parser globals (opens input and */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] ) {
    ERROR_FLAG=0;
    varaddress = 0;
    writing=0;
    reading = 0;
    scope = 1;

    if ( OpenFiles( argc, argv ) ) {
        InitCharProcessor( InputFile, ListFile );
        InitCodeGenerator(CodeFile); /*Initialize code generation*/
        CurrentToken = GetToken();
        SetupSets();
        ParseProgram();
        WriteCodeFile();  /*Write out assembly to file*/
        fclose( InputFile );
        fclose( ListFile );
        if(ERROR_FLAG) {
            printf("Syntax Error\n");/*code file has syntax error*/
            return EXIT_FAILURE;
        }
        printf("Valid\n");
        return  EXIT_SUCCESS;
    } else {
        printf("Syntax Error\n");/*command line arguments are wrong*/
        return EXIT_FAILURE;
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseProgram implements:                                                */
/*                                                                          */
/*       <Program> ::== "PROGRAM" <Identifier> ";"  [ <Declarations> ]      */
/*       { <ProcDeclaration> } <Block> "."                                  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseProgram(void) {
    Accept(PROGRAM);
    /*Lookahead token it should be the Program's name, so we call the MakeSymbolTableEntry*/
    MakeSymbolTableEntry(STYPE_PROGRAM);

    Accept(IDENTIFIER);
    Accept(SEMICOLON);
    Synchronise(&ProgramStatementFS_aug1,&ProgramStatementFBS);
    if(CurrentToken.code == VAR)
        ParseDeclarations();
    Synchronise(&ProgramStatementFS_aug2,&ProgramStatementFBS);
    while (CurrentToken.code == PROCEDURE)
        ParseProcDeclaration();
    Synchronise(&ProgramStatementFS_aug2,&ProgramStatementFBS);
    ParseBlock();
    Accept(ENDOFPROGRAM);
    _Emit(I_HALT);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseDeclarations implements:                                           */
/*                                                                          */
/*       <Declarations> ::== "VAR" <Variable> { "," <Variable> }    ";"     */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int ParseDeclarations(void) {
    int vcount = 0;
    Accept(VAR);

    MakeSymbolTableEntry(STYPE_VARIABLE);
    ParseVariable(); /* ParseVariable() --> ParseIdentifier() --> Accept(IDENTIFIER); */
    vcount++;
    while (CurrentToken.code == COMMA) {
        Accept(COMMA);
        MakeSymbolTableEntry(STYPE_VARIABLE);

        ParseVariable(); /* ParseVariable() --> ParseIdentifier() --> Accept(IDENTIFIER); */
        vcount++;
    }
    Accept(SEMICOLON);
    Emit(I_INC,vcount);
    return vcount;

}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseProcDeclarations implements:                                       */
/*                                                                          */
/*       <ProcDeclarations> ::== "PROCEDURE" <Identifier> [ <ParameterList>]*/
/*              ";" [ <Declarations> ] { <ProcDeclaration> } <Block> ";"    */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseProcDeclaration(void) {
    int vcount = 0;
    int backpatch_addr;
    SYMBOL *procedure;
    Accept(PROCEDURE);

    procedure = MakeSymbolTableEntry(STYPE_PROCEDURE);
    Accept(IDENTIFIER);

    backpatch_addr = CurrentCodeAddress();
    Emit( I_BR, 0 );
    procedure->address = CurrentCodeAddress();

    scope++; /* To avoid multiple declarations */

    Accept(SEMICOLON);

    if(CurrentToken.code == VAR)
        vcount = ParseDeclarations();

    ParseBlock();
    Accept(SEMICOLON);

    Emit(I_DEC, vcount);
    _Emit( I_RET );
    BackPatch( backpatch_addr, CurrentCodeAddress() );

    RemoveSymbols(scope);
    scope--;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseParameterList implements:                                          */
/*                                                                          */
/*       <ParameterList> ::== "(" <FormalParameter> {"," <FormalParameter>} */
/*                          ")"                                             */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseParameterList(void) {
    Accept(LEFTPARENTHESIS);
    ParseFormalParameter();

    while (CurrentToken.code == COMMA) {
        Accept(COMMA);
        ParseFormalParameter();
    }
    Accept(RIGHTPARENTHESIS);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseFormalParameter implements:                                        */
/*                                                                          */
/*       <FormalParameter> ::== [ "REF" ] {<Variable>}                      */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseFormalParameter(void) {
    if(CurrentToken.code == REF) {
        MakeSymbolTableEntry(STYPE_REFPAR);
        Accept(REF);
    }

    MakeSymbolTableEntry(STYPE_VARIABLE);

    ParseVariable(); /* ParseVariable() --> ParseIdentifier() --> Accept(IDENTIFIER); */
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseBlock implements:                                                  */
/*                                                                          */
/*       <Block> :== "BEGIN" { <Statement> ";"} "END"                       */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseBlock(void) {
  Accept(BEGIN);

   Synchronise(&BlockStatementFS_aug,&BlockStatementFBS);
   while (CurrentToken.code == IDENTIFIER || CurrentToken.code == WHILE ||
      CurrentToken.code == IF || CurrentToken.code == READ ||
      CurrentToken.code == WRITE) {
      ParseStatement();
  Accept(SEMICOLON);
  }

  Synchronise(&BlockStatementFS_aug,&BlockStatementFBS);

  Accept(END);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseStatement implements:                                              */
/*                                                                          */
/*    <Statement> ::== <SimpleStatement> | <WhileStatement> | <IfStatement> */
/*                     | <ReadStatement> | <WriteStatement>                 */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseStatement(void) {
    switch(CurrentToken.code) {
        case IDENTIFIER:
          ParseSimpleStatement();
          break;
        case WHILE:
          ParseWhileStatement();
          break;
        case IF:
          ParseIfStatement();
          break;
        case READ:
          ParseReadStatement();
          break;
        case WRITE:
          ParseWriteStatement();
          break;
        default:
          SyntaxError( IDENTIFIER, CurrentToken );
          break;
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseSimpleStatment implements:                                         */
/*                                                                          */
/*       <SimpleStatement> ::==  <VarOrProcName> <RestOfStatment>           */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseSimpleStatement(void) {
    SYMBOL *var;
    var = LookupSymbol();
    ParseVariable(); /* ParseVariable() --> ParseIdentifier() --> Accept(IDENTIFIER); */
    ParseRestOfStatement(var);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseRestOfStatement implements:                                        */
/*                                                                          */
/*       <RestOfStatement> ::== <ProcCallList> | <Assignment> | ϵ           */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseRestOfStatement(SYMBOL *var) {
    switch(CurrentToken.code) {
        case LEFTPARENTHESIS:
            ParseProcCallList();
        case SEMICOLON:
            if ( var != NULL ) {
                if ( var->type == STYPE_PROCEDURE ) {
                    _Emit(I_PUSHFP);
                    _Emit(I_BSF);
                    Emit( I_CALL, var->address );
                    _Emit(I_RSF);
                } else {
                    printf("error in parse rest of statement semicolon case");
                    KillCodeGeneration();
                }
            }
            break;
        case ASSIGNMENT:
        default:
            ParseAssignment();
            if ( var != NULL ) {
                if ( var->type == STYPE_VARIABLE ) {
                    Emit( I_STOREA, var->address );
                } else {
                    printf("error in parse rest of statement default case");
                    KillCodeGeneration();
                }
            break;
        }
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseProcCallList implements:                                           */
/*                                                                          */
/*       <ProcCallList> ::== "(" <ActualParameter> {"," <ActualParameter> } */
/*                        ")"                                               */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseProcCallList(void) {
    Accept(LEFTPARENTHESIS);
    if(reading) {                 /*emit READ before each parameter*/
        _Emit(I_READ);
    }

    ParseActualParameter();
    if(writing) {               /*emit WRITE after each parameter*/
       _Emit(I_WRITE);
    }

    while (CurrentToken.code == COMMA) {
        Accept(COMMA);
        if(reading) {
            _Emit(I_READ);
        }

        ParseActualParameter();
        if(writing) {
           _Emit(I_WRITE);
        }
    }
    Accept(RIGHTPARENTHESIS);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseAssignment implements:                                             */
/*                                                                          */
/*       <Assignment> ::== ":=" <Expression>                                */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseAssignment(void) {
    Accept(ASSIGNMENT);
    ParseExpression();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseActualParameter implements:                                         */
/*                                                                          */
/*       <ActualParameter> ::== <Variable> | <Expression>                    */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseActualParameter(void) {
    ParseExpression();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseWhileStatement implements:                                         */
/*                                                                          */
/*       <WhileStatement> ::== "WHILE" <BooleanExpression> "DO" <Block>     */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseWhileStatement(void) {
    int Label1, Label2, L2BackPatchLoc;
    Accept(WHILE);
    Label1 = CurrentCodeAddress();
    L2BackPatchLoc = ParseBooleanExpression();
    Accept(DO);
    ParseBlock();
    Emit(I_BR, Label1);
    Label2 = CurrentCodeAddress();
    BackPatch(L2BackPatchLoc,Label2);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseIfStatement implements:                                            */
/*                                                                          */
/*       <IfParameter> ::== "IF"<BooleanExpression> "THEN" <Block>          */
/*                           ["ELSE" <Block> ]                              */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseIfStatement(void) {
    Accept(IF);
    ParseBooleanExpression();
    Accept(THEN);
    ParseBlock();

    if (CurrentToken.code == ELSE) {
        Accept(ELSE);
        ParseBlock();
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseReadStatement implements:                                          */
/*                                                                          */
/*       <ReadStatement> ::== "READ" "(" <Variable> {"," <Variable> } ")"   */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseReadStatement(void) {
    reading = 1;
    Accept(READ);
    ParseProcCallList();
    reading = 0;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseWriteStatement implements:                                         */
/*                                                                          */
/*       <WriteStatement> ::== "WRITE" "(" <Expression> { ","<Expression> } */
/*                           ")"                                            */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseWriteStatement(void) {
    writing = 1;
    Accept(WRITE);
    ParseProcCallList();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseExpression implements:                                             */
/*                                                                          */
/*       <Expression> ::== <CompoundTerm> {<AddOp> <CompoundTerm>}          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseExpression(void) {
    int op;

    ParseCompoundTerm();
    while((op = CurrentToken.code) == ADD || op == SUBTRACT) {
        ParseAddOp();
        ParseCompoundTerm();

        if(op == ADD) _Emit(I_ADD); else _Emit(I_SUB);
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseCompoundTerm implements:                                           */
/*                                                                          */
/*       <CompoundTerm> ::== <Term> {<MultOp> <Term> }                      */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseCompoundTerm(void) {
    int op;

    ParseTerm();
    while((op = CurrentToken.code) == MULTIPLY || op == DIVIDE) {
        ParseMultOp();
        ParseTerm();

        if(op == MULTIPLY) _Emit(I_MULT); else _Emit(I_DIV);
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseTerm implements:                                                   */
/*                                                                          */
/*       <Term> ::== ["-"] <SubTerm>                                        */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseTerm(void) {
    int negative_flag = 0;

    if (CurrentToken.code == SUBTRACT) {
        negative_flag = 1;
        Accept(SUBTRACT);
    }

    ParseSubTerm();
    if(negative_flag) {
       _Emit(I_NEG);
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseSubTerm implements:                                                */
/*                                                                          */
/*       <SubTerm> ::== <Variable> | <IntConst> | "(" <Expression> ")"      */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseSubTerm(void) {
    int i, dS;
    SYMBOL *var;
    switch(CurrentToken.code) {
        case INTCONST:
            Emit(I_LOADI,CurrentToken.value);
            ParseIntConst(); /* ParseIntConst() --> Accept(INTCONST) */
            break;
        case LEFTPARENTHESIS:
            Accept(LEFTPARENTHESIS);
            ParseExpression();
            Accept(RIGHTPARENTHESIS);
            break;
        case IDENTIFIER:
        default:
            var = LookupSymbol();
            if (var != NULL && var->type == STYPE_VARIABLE) {
                if (writing) {
                    Emit(I_LOADA,var->address); /*Load the parameter value*/
                } else if (reading) {
                    Emit(I_STOREA,var->address); /*Store user input into each parameter*/
                } else {
                    Emit(I_LOADA,var->address);
                }
            } else if ( var->type == STYPE_LOCALVAR ) {
                dS = scope - var->scope;
                if ( dS == 0 )
                  Emit( I_LOADFP, var->address );
                else {
                  _Emit( I_LOADFP );
    
                  for ( i = 0; i < dS - 1; i++ )
                      _Emit( I_LOADSP );
                  Emit( I_LOADSP, var->address );
                }
            }
            else printf("Name undeclared or not a variable..!!");
            ParseVariable(); /* ParseVariable() --> ParseIdentifier() --> Accept(IDENTIFIER); */
            break;
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseBooleanExpression implements:                                      */
/*                                                                          */
/*       <BooleanExpression> ::== <Expression> <RelOp> <Expression>         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int ParseBooleanExpression(void) {
    int BackPatchAddr, RelOpInstruction;
    ParseExpression();
    RelOpInstruction = ParseRelOp();
    ParseExpression();
    ParseRelOp();
    BackPatchAddr = CurrentCodeAddress( );
    Emit( RelOpInstruction, 0 );
    return BackPatchAddr;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseAddOp implements:                                                  */
/*                                                                          */
/*       <AddOp> ::== "+" | "-"                                             */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseAddOp(void) {
    switch(CurrentToken.code) {
        case ADD:
        Accept(ADD); break; 
        case SUBTRACT:
        Accept(SUBTRACT); break;
        default:
        SyntaxError( IDENTIFIER, CurrentToken );
        break;
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseMultOp implements:                                                 */
/*                                                                          */
/*       <MultOp> ::== "*" | "/"                                            */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseMultOp(void) {
    switch(CurrentToken.code) {
        case MULTIPLY:
          Accept(MULTIPLY);
          break;
        case DIVIDE:
          Accept(DIVIDE);
          break;
        default:
          SyntaxError( IDENTIFIER, CurrentToken );
          break;
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseRelOp implements:                                                  */
/*                                                                          */
/*       <RelOp> ::== "=" | "<=" | ">=" | "<" | ">"                         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int ParseRelOp(void) {
    int RelOpInstruction;
    switch(CurrentToken.code) {
        case EQUALITY:
            RelOpInstruction = I_BZ;
            Accept(EQUALITY);
            break;
        case LESSEQUAL:
            RelOpInstruction = I_BG;
            Accept(LESSEQUAL);
            break;
        case GREATEREQUAL:
            RelOpInstruction = I_BL;
            Accept(GREATEREQUAL);
            break;
        case LESS:
            RelOpInstruction = I_BGZ;
            Accept(LESS);
            break;
        case GREATER:
            RelOpInstruction = I_BLZ;
            Accept(GREATER);
            break;

    }
    return RelOpInstruction;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseVariable implements:                                               */
/*                                                                          */
/*       <Variable> ::== <Identifier>                                       */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseVariable(void) {
    ParseIdentifier();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseIntConst implements:                                               */
/*                                                                          */
/*       <IntConst> ::== <Digit> { <Digit> }                                */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseIntConst(void) {
    Accept(INTCONST);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseIdentifier implements:                                             */
/*                                                                          */
/*       <Identifier> ::== <Alpha> { <AlphaNum> }                           */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseIdentifier(void) {
    Accept(IDENTIFIER);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  End of parser.  Support routines follow.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Accept:  Takes an expected token name as argument, and if the current   */
/*           lookahead matches this, advances the lookahead and returns.    */
/*                                                                          */
/*           If the expected token fails to match the current lookahead,    */
/*           this routine reports a syntax error and exits ("crash & burn"  */
/*           parsing).  Note the use of routine "SyntaxError"               */
/*           (from "scanner.h") which puts the error message on the         */
/*           standard output and on the listing file, and the helper        */
/*           "ReadToEndOfFile" which just ensures that the listing file is  */
/*           completely generated.                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Integer code of expected token                          */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: If successful, advances the current lookahead token     */
/*                  "CurrentToken".                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Accept( int ExpectedToken ) {
    static int recovering = 0;

    if (recovering) {
        while(CurrentToken.code != ExpectedToken && CurrentToken.code != ENDOFINPUT)
            CurrentToken = GetToken();
        recovering = 0;
    }

    if ( CurrentToken.code != ExpectedToken )  {
        SyntaxError( ExpectedToken, CurrentToken );
         KillCodeGeneration();
        recovering = 1;
        ERROR_FLAG=1; /* for use in main to avoid printing valid*/
        
    }
    else  CurrentToken = GetToken();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Synchronise:  Synchronise() function                                    */
/*                                                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Two arguments                                           */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: If successful, advances the current lookahead token     */
/*                  "CurrentToken".                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Synchronise(SET *F, SET *FB) {
    SET S;

    S = Union(2,F,FB);
    if(!InSet(F,CurrentToken.code)){
        SyntaxError2(*F,CurrentToken);
        while(!InSet(&S,CurrentToken.code)){
            CurrentToken = GetToken();
        }
    }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  SetupSets:  SetupSets() function                                        */
/*                                                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: If successful, advances the current lookahead token     */
/*                  "CurrentToken".                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void SetupSets(void) {
  InitSet(&ProgramStatementFS_aug1, 3, VAR, PROCEDURE, BEGIN);

  InitSet(&ProgramStatementFS_aug2, 2, PROCEDURE, BEGIN);

  InitSet(&ProgramStatementFBS, 3, END, ENDOFPROGRAM, ENDOFINPUT);

  InitSet(&ProcedureStatementFS_aug1, 3, VAR, PROCEDURE, BEGIN);

  InitSet(&ProcedureStatementFS_aug2, 3, PROCEDURE, BEGIN);

  InitSet(&ProcedureStatementFBS, 3, END, ENDOFPROGRAM, ENDOFINPUT);

  InitSet(&BlockStatementFS_aug, 6, END, IDENTIFIER, WHILE, IF, READ, WRITE);

  InitSet(&BlockStatementFBS, 4, SEMICOLON, ELSE, ENDOFPROGRAM, ENDOFINPUT);

}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  OpenFiles:  Reads strings from the command-line and opens the           */
/*              associated input and listing files.                         */
/*                                                                          */
/*    Note that this routine mmodifies the globals "InputFile" and          */
/*    "ListingFile".  It returns 1 ("true" in C-speak) if the input and     */
/*    listing files are successfully opened, 0 if not, allowing the caller  */
/*    to make a graceful exit if the opening process failed.                */
/*                                                                          */
/*                                                                          */
/*    Inputs:       1) Integer argument count (standard C "argc").          */
/*                  2) Array of pointers to C-strings containing arguments  */
/*                  (standard C "argv").                                    */
/*                                                                          */
/*    Outputs:      No direct outputs, but note side effects.               */
/*                                                                          */
/*    Returns:      Boolean success flag (i.e., an "int":  1 or 0)          */
/*                                                                          */
/*    Side Effects: If successful, modifies globals "InputFile" and         */
/*                  "ListingFile".                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] ) {

    if ( argc != 4 )  {
        fprintf( stderr, "%s <inputfile> <listfile> <assembly input file>\n", argv[0] );
        return 0;
    }

    if ( NULL == ( InputFile = fopen( argv[1], "r" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for input\n", argv[1] );
        return 0;
    }

    if ( NULL == ( ListFile = fopen( argv[2], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[2] );
        fclose( InputFile );
        return 0;
    }

    if ( NULL == ( CodeFile = fopen( argv[3], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[3] );
        fclose( InputFile );
        fclose( ListFile );
        return 0;
    }

    return 1;
}


/*--------------------------------------------------------------------------*/
/* PRIVATE void MakeSymbolTableEntry( int symtype ) creates entries in the  */
/* Symbol Table, which is organized as a Chaining Hash Table. Takes an argu */
/* ment symbol type(symtype) defined in the "symbol.h" header file.         */
/* Uses pointers to symbol location to navigate through the symbol table    */
/* entries. In general maps the identifier with the information record.     */
/*--------------------------------------------------------------------------*/

PRIVATE SYMBOL *MakeSymbolTableEntry( int symtype ) {
   /*〈Variable Declarations here〉*/
    SYMBOL *newsptr; /*new symbol pointer*/
    SYMBOL *oldsptr; /*old symbol pointer*/
    char *cptr;      /*current pointer*/
    int hashindex;
    static int varaddress = 0;
/* check to see if there is an entry in the symbol table with the same name as IDENTIFIER */
    if ( CurrentToken.code == IDENTIFIER ) {      
       if ( NULL == ( oldsptr = Probe( CurrentToken.s, &hashindex )) || oldsptr->scope < scope ) {
           if ( oldsptr == NULL ) cptr = CurrentToken.s;
            else cptr = oldsptr->s;

        /*〈Fatal internal error in EnterSymbol, compiler must exit: code for this goes here〉*/
           if ( NULL == ( newsptr = EnterSymbol( cptr, hashindex ))) {
               printf("Fatal internal error in EnterSymbol..!!\n");
               exit(EXIT_FAILURE);
           } else {
               if ( oldsptr == NULL ) {
                    PreserveString();
                }
               newsptr->scope = scope;
               newsptr->type = symtype;
               if ( symtype == STYPE_VARIABLE || symtype == STYPE_LOCALVAR ) {
                   newsptr->address = varaddress;
                   varaddress++;                 
               }
               else newsptr->address = -1;
           }
       } else { /*〈Error, variable already declared: code for this goes here〉*/
           printf("Error, variable already declared...!!\n");
            KillCodeGeneration();
       }
   } else {

   }
   return newsptr;
}

/*--------------------------------------------------------------------------*/
/*  LookupSymbol:    used to check if symbol has been declared              */
/*                                                                          */
/*                                                                          */
/*    Inputs:       none                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Symbol if it exists in symbol table                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE SYMBOL *LookupSymbol(void) {

    SYMBOL *sptr;

    if (CurrentToken.code == IDENTIFIER) {
        sptr = Probe(CurrentToken.s,NULL);

        if(sptr == NULL) {
            Error("Identifier not declared..", CurrentToken.pos);
            KillCodeGeneration();
        }
    }
    else sptr = NULL;
    return sptr;
}

