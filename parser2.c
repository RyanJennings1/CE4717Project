/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser2                                                            */
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


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */

PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */

PRIVATE int ERROR_FLAG;            /*  Flag is set to 1 if syntax errors    */
                                   /*  occur                                */


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] );
PRIVATE void Accept( int code );
PRIVATE void Synchronise(SET *F, SET *FB);
PRIVATE void SetupSets(void);

PRIVATE void ParseProgram( void);
PRIVATE void ParseDeclarations( void);
PRIVATE void ParseProcDeclaration( void);
PRIVATE void ParseParameterList( void);
PRIVATE void ParseFormalParameter( void);
PRIVATE void ParseBlock( void);
PRIVATE void ParseStatement( void);
PRIVATE void ParseSimpleStatement( void);
PRIVATE void ParseRestOfStatement( void);
PRIVATE void ParseProcCallList( void);
PRIVATE void ParseAssignment( void);
PRIVATE void ParseActualParameter( void);
PRIVATE void ParseWhileStatement( void);
PRIVATE void ParseIfStatement( void);
PRIVATE void ParseReadStatement( void);
PRIVATE void ParseWriteStatement( void);
PRIVATE void ParseExpression( void);
PRIVATE void ParseCompoundTerm( void);
PRIVATE void ParseTerm( void);
PRIVATE void ParseSubTerm( void);
PRIVATE void ParseBooleanExpression( void);
PRIVATE void ParseAddOp( void);
PRIVATE void ParseMultOp( void);
PRIVATE void ParseRelOp( void);
PRIVATE void ParseVariable( void);
PRIVATE void ParseIntConst( void);
PRIVATE void ParseIdentifier( void);

PRIVATE SET ProgramStatementFS_aug1;
PRIVATE SET ProgramStatementFS_aug2;
PRIVATE SET ProcedureStatementFS_aug1;
PRIVATE SET ProcedureStatementFS_aug2;
PRIVATE SET BlockStatementFS_aug;
PRIVATE SET ProgramStatementFSB;
PRIVATE SET ProcedureStatementFSB;
PRIVATE SET BlockStatementFSB;


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Smallparser entry point.  Sets up parser globals (opens input and */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
    ERROR_FLAG = 0;
    if ( OpenFiles( argc, argv ) )  {
        InitCharProcessor( InputFile, ListFile );
        CurrentToken = GetToken();
        SetupSets();
        ParseProgram();
        fclose( InputFile );
        fclose( ListFile );

        if (ERROR_FLAG) {
          printf("Syntax Error\n");
        } else {
          printf("Valid\n");
        }
        
        return  EXIT_SUCCESS;
    }
    else
    {
        printf("Syntax error\n");
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

PRIVATE void ParseProgram( void )
{
    Accept( PROGRAM );
    Accept( IDENTIFIER );
    Accept( SEMICOLON );

    Synchronise(&ProgramStatementFS_aug1, &ProgramStatementFSB);
    if ( CurrentToken.code == VAR ) 
        ParseDeclarations();
    Synchronise(&ProgramStatementFS_aug2, &ProgramStatementFSB);
    while (CurrentToken.code == PROCEDURE ) 
        ParseProcDeclaration();
    Synchronise(&ProgramStatementFS_aug2, &ProgramStatementFSB);
    ParseBlock();
    Accept( ENDOFPROGRAM );
    Accept( ENDOFINPUT );
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

PRIVATE void ParseDeclarations( void)
{
    Accept( VAR);
    ParseVariable();
    while(CurrentToken.code == COMMA )
    {
        Accept ( COMMA);
        ParseVariable();
    }
    Accept( SEMICOLON);  

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

PRIVATE void ParseProcDeclaration( void)
{
    Accept( PROCEDURE);
    Accept( IDENTIFIER);

    if( CurrentToken.code == LEFTPARENTHESIS)
        ParseParameterList();

    Accept( SEMICOLON);
    Synchronise(&ProcedureStatementFS_aug1, &ProcedureStatementFSB);

    if( CurrentToken.code == VAR)
        ParseDeclarations();

    Synchronise(&ProcedureStatementFS_aug2, &ProcedureStatementFSB);

    while( CurrentToken.code == PROCEDURE)
        ParseProcDeclaration();

    Synchronise(&ProcedureStatementFS_aug2, &ProcedureStatementFSB);

    ParseBlock();
    Accept(SEMICOLON);

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

PRIVATE void ParseParameterList( void)
{
    Accept( LEFTPARENTHESIS);
    ParseFormalParameter();

    while( CurrentToken.code == COMMA)
    {
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

PRIVATE void ParseFormalParameter( void)
{
    if( CurrentToken.code == REF)
        Accept( REF); 

   ParseVariable();

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

PRIVATE void ParseBlock( void)
{
    Accept( BEGIN);
    Synchronise(&BlockStatementFS_aug, &BlockStatementFSB);
    while( CurrentToken.code == IDENTIFIER || CurrentToken.code == WHILE || 
        CurrentToken.code == IF || CurrentToken.code == READ ||
        CurrentToken.code == WRITE)
    {
        ParseStatement();
        Accept( SEMICOLON);
    }
    Synchronise(&BlockStatementFS_aug, &BlockStatementFSB);
    Accept( END);

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

PRIVATE void ParseStatement( void)
{
    switch(CurrentToken.code)
    {
        case IDENTIFIER:ParseSimpleStatement(); break;
        case WHILE:ParseWhileStatement(); break;
        case IF:ParseIfStatement(); break;
        case READ:ParseReadStatement(); break;
        case WRITE:ParseWriteStatement(); break;
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

PRIVATE void ParseSimpleStatement( void)
{
    ParseVariable();
    ParseRestOfStatement();
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

PRIVATE void ParseRestOfStatement( void)
{
    if(CurrentToken.code == LEFTPARENTHESIS)
        ParseProcCallList();
    if(CurrentToken.code == ASSIGNMENT)
        ParseAssignment();

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

PRIVATE void ParseProcCallList( void)
{
    Accept( LEFTPARENTHESIS);
    ParseActualParameter();
    while(CurrentToken.code == COMMA)
    {
        Accept( COMMA);
        ParseActualParameter();
    }
    Accept( RIGHTPARENTHESIS);
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

PRIVATE void ParseAssignment( void)
{
    Accept( ASSIGNMENT);
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

PRIVATE void ParseActualParameter( void)
{
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

PRIVATE void ParseWhileStatement( void)
{
    Accept( WHILE);
    ParseBooleanExpression();
    Accept( DO);
    ParseBlock();
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

PRIVATE void ParseIfStatement( void)
{
    Accept( IF);
    ParseBooleanExpression();
    Accept( THEN);
    ParseBlock();
    if(CurrentToken.code == ELSE)
    {
        Accept( ELSE);
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

PRIVATE void ParseReadStatement( void)
{
    Accept( READ);
    ParseProcCallList();

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

PRIVATE void ParseWriteStatement( void)
{
    Accept( WRITE);
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

PRIVATE void ParseExpression( void)
{

    ParseCompoundTerm();
    while(CurrentToken.code == ADD || CurrentToken.code == SUBTRACT)
    {
        ParseAddOp();
        ParseCompoundTerm();
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

PRIVATE void ParseCompoundTerm( void)
{
    ParseTerm();
    while(CurrentToken.code == MULTIPLY || CurrentToken.code == DIVIDE)
    {
        ParseMultOp();
        ParseTerm();
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

PRIVATE void ParseTerm( void)
{
    if(CurrentToken.code == SUBTRACT)
        Accept( SUBTRACT);
    ParseSubTerm();

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

PRIVATE void ParseSubTerm( void)
{
    switch(CurrentToken.code)
    {
        case IDENTIFIER:ParseVariable(); break;
        case INTCONST:ParseIntConst(); break;
        case LEFTPARENTHESIS:Accept(LEFTPARENTHESIS); ParseExpression(); Accept(RIGHTPARENTHESIS); break;
        default:
        SyntaxError(IDENTIFIER,CurrentToken);
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

PRIVATE void ParseBooleanExpression( void)
{
    ParseExpression();
    ParseRelOp();
    ParseExpression();
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

PRIVATE void ParseAddOp( void)
{
    switch( CurrentToken.code ) {
      case ADD:
        Accept(ADD);
        break;
      case SUBTRACT:
        Accept(SUBTRACT);
        break;
      default:
        SyntaxError(IDENTIFIER, CurrentToken);
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

PRIVATE void ParseMultOp( void)
{
    switch( CurrentToken.code ) {
      case MULTIPLY:
        Accept(MULTIPLY);
        break;
      case DIVIDE:
        Accept(DIVIDE);
        break;
      default:
        SyntaxError(IDENTIFIER, CurrentToken);
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

PRIVATE void ParseRelOp( void)
{
    switch( CurrentToken.code ) {
      case EQUALITY:
        Accept(EQUALITY);
        break;
      case LESSEQUAL:
        Accept(LESSEQUAL);
        break;
      case GREATEREQUAL:
        Accept(GREATEREQUAL);
        break;
      case LESS:
        Accept(LESS);
        break;
      case GREATER:
        Accept(GREATER);
        break;
      default:
        SyntaxError(IDENTIFIER, CurrentToken);
    }
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

PRIVATE void ParseVariable( void)
{
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

PRIVATE void ParseIntConst( void)
{
    Accept( INTCONST);
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

PRIVATE void ParseIdentifier( void)
{
    Accept( IDENTIFIER);
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

PRIVATE void Accept( int ExpectedToken )
{
    static int recovering = 0;

    if (recovering) {
      while (CurrentToken.code != ExpectedToken &&
             CurrentToken.code != ENDOFINPUT ) {
        CurrentToken = GetToken();
      }
      recovering = 0;
    }

    if ( CurrentToken.code != ExpectedToken )  {
        SyntaxError( ExpectedToken, CurrentToken );
        recovering = 1;
        ERROR_FLAG = 1;
    }
    else  CurrentToken = GetToken();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Synchronise:    Synchornise() function                                  */
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

PRIVATE void Synchronise(SET *F, SET *FB)
{
  SET S;

  S = Union(2, F, FB);
  if (!InSet(F, CurrentToken.code)) {
    SyntaxError2(*F, CurrentToken);
    while (!InSet(&S, CurrentToken.code)) {
      CurrentToken = GetToken();
    }
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  SetupSets:  SetupSets() funciton                                        */
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

PRIVATE void SetupSets(void)
{

  InitSet(&ProgramStatementFS_aug1, 3, VAR, PROCEDURE, BEGIN);

  InitSet(&ProgramStatementFS_aug2, 2, PROCEDURE, BEGIN);

  InitSet(&ProgramStatementFSB, 3, END, ENDOFPROGRAM, ENDOFINPUT);

  InitSet(&ProcedureStatementFS_aug1, 3, VAR, PROCEDURE, BEGIN);

  InitSet(&ProcedureStatementFS_aug2, 3, PROCEDURE, BEGIN);

  InitSet(&ProcedureStatementFSB, 3, END, ENDOFPROGRAM, ENDOFINPUT);

  InitSet(&BlockStatementFS_aug, 6, END, IDENTIFIER, WHILE, IF, READ, WRITE);

  InitSet(&BlockStatementFSB, 4, SEMICOLON, ELSE, ENDOFPROGRAM, ENDOFINPUT);

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

PRIVATE int  OpenFiles( int argc, char *argv[] )
{

    if ( argc != 3 )  {
        fprintf( stderr, "%s <inputfile> <listfile>\n", argv[0] );
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

    return 1;
}

