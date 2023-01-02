/*									tab:8
 *
 * lc3.f - lexer for the LC-3 assembler
 *
 * "Copyright (c) 2003 by Steven S. Lumetta."
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written 
 * agreement is hereby granted, provided that the above copyright notice
 * and the following two paragraphs appear in all copies of this software,
 * that the files COPYING and NO_WARRANTY are included verbatim with
 * any distribution, and that the contents of the file README are included
 * verbatim as part of a file named README with any distribution.
 * 
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE TO ANY PARTY FOR DIRECT, 
 * INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT 
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE AUTHOR 
 * HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE AUTHOR SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
 * A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" 
 * BASIS, AND THE AUTHOR NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, 
 * UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Author:	    Steve Lumetta and Alexandra Roffe
 * Version:	    1
 * Creation Date:   18 October 2003
 * Updated:         2  January 2023
 * Filename:	    lc3.f
 * History:
 *	SSL	1	18 October 2003
 *		Copyright notices and Gnu Public License marker added.
 */

%option noyywrap nounput

%{

/* questions...

should the assembler allow colons after label names?  are the colons
part of the label?  Currently I allow only alpha followed by alphanum and _.

*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <inttypes.h>
#include <sys/time.h>

#include "symbol.h"


/* return the time in microseconds. */
long getMicrotime(){
	struct timeval currentTime;
	gettimeofday(&currentTime, NULL);
	return currentTime.tv_sec * (int)1e6 + currentTime.tv_usec;
}

typedef enum opcode_t opcode_t;
enum opcode_t {
    /* no opcode seen (yet) */
    OP_NONE,

    /* new instruction opcodes (alphabetical of course)*/
    OP_ICM, OP_MLT, OP_RND, OP_RST, OP_SPR, OP_SUB, OP_TIM,

    
    /* old instruction opcodes*/
    OP_ADD, OP_AND, OP_BR, OP_JMP, OP_JSR, OP_JSRR, OP_LD, OP_LDI, OP_LDR,
    OP_LEA, OP_NOT,  OP_RTI, OP_ST, OP_STI, OP_STR, OP_TRAP,
    
    /* trap pseudo-ops */
    OP_GETC, OP_HALT, OP_IN, OP_OUT, OP_PUTS, OP_PUTSP,

    /* non-trap pseudo-ops */
    OP_FILL, OP_RET, OP_STRINGZ,

    /* directives */
    OP_BLKW, OP_END, OP_ORIG, 

    NUM_OPS
};

static const char* const opnames[NUM_OPS] = {
    /* no opcode seen (yet) */
    "missing opcode",

    /* new instruction opcodes */
    "ICM", "MLT", "RND", "RST", "SPR", "SUB", "TIM",
    
    /* real instruction opcodes */
    "ADD", "AND", "BR", "JMP", "JSR", "JSRR", "LD", "LDI", "LDR", "LEA",
    "NOT",  "RTI", "ST", "STI", "STR", "TRAP",

    /* trap pseudo-ops */
    "GETC", "HALT", "IN", "OUT", "PUTS", "PUTSP",

    /* non-trap pseudo-ops */
    ".FILL", "RET", ".STRINGZ",

    /* directives */
    ".BLKW", ".END", ".ORIG",
};

typedef enum ccode_t ccode_t;
enum ccode_t {
    CC_    = 0,
    CC_P   = 0x0200,
    CC_Z   = 0x0400,
    CC_N   = 0x0800
};

typedef enum operands_t operands_t;
enum operands_t {
    O_RRR, O_RRI,
    O_RR,  O_RI,  O_RL,
    O_R,   O_I,   O_L,   O_S,
    O_,
    NUM_OPERANDS
};

static const int op_format_ok[NUM_OPS] = {
    /* no opcode seen (yet) */
    0x200, /* no opcode, no operands       */

    /* new instruction formats */

    0x0C0, /* ICM: increment a memory address*/
    0x003, /* MLT: RRR or RRI formats only */
    0x020, /* RND: R format only    - take in 1 register and load a random value in it*/
    0x020, /* RST: R format only */
    0x200, /* SPR: no operands */
    0x003, /* SUB: RRR or RRI formats only */
    0x020, /* TIM: R format only    - put the time in microseconds in a register */


    /* real instruction formats */
    0x003, /* ADD: RRR or RRI formats only  0011 */
    0x003, /* AND: RRR or RRI formats only  0011 */
    0x0C0, /* BR: I or L formats only       11000000 */
    0x020, /* JMP: R format only            00100000*/
    0x0C0, /* JSR: I or L formats only     */
    0x020, /* JSRR: R format only          */
    0x018, /* LD: RI or RL formats only     00011000 */
    0x018, /* LDI: RI or RL formats only    00011000 */
    0x002, /* LDR: RRI format only         */
    0x018, /* LEA: RI or RL formats only    00011000*/
    0x004, /* NOT: RR format only           0100  */
    
    0x200, /* RTI: no operands allowed     */
    0x018, /* ST: RI or RL formats only    */
    0x018, /* STI: RI or RL formats only   */
    0x002, /* STR: RRI format only         */
    0x040, /* TRAP: I format only          */

    /* trap pseudo-op formats (no operands) */
    0x200, /* GETC: no operands allowed    001000000000 */
    0x200, /* HALT: no operands allowed    */
    0x200, /* IN: no operands allowed      */
    0x200, /* OUT: no operands allowed     */
    0x200, /* PUTS: no operands allowed    */
    0x200, /* PUTSP: no operands allowed   */

    /* non-trap pseudo-op formats */
    0x0C0, /* .FILL: I or L formats only   */
    0x200, /* RET: no operands allowed     */
    0x100, /* .STRINGZ: S format only      */

    /* directive formats */
    0x040, /* .BLKW: I format only         */
    0x200, /* .END: no operands allowed    */
    0x040  /* .ORIG: I format only         */
};

typedef enum pre_parse_t pre_parse_t;
enum pre_parse_t {
    NO_PP =  0,
    PP_R1 =  1,
    PP_R2 =  2,
    PP_R3 =  4,
    PP_I2 =  8,
    PP_L2 = 16
};

static const pre_parse_t pre_parse[NUM_OPERANDS] = {
    (PP_R1 | PP_R2 | PP_R3), /* O_RRR */
    (PP_R1 | PP_R2),         /* O_RRI */
    (PP_R1 | PP_R2),         /* O_RR  */
    (PP_R1 | PP_I2),         /* O_RI  */
    (PP_R1 | PP_L2),         /* O_RL  */
    PP_R1,                   /* O_R   */
    NO_PP,                   /* O_I   */
    NO_PP,                   /* O_L   */
    NO_PP,                   /* O_S   */
    NO_PP                    /* O_    */
};

typedef struct inst_t inst_t;
struct inst_t {
    opcode_t op;
    ccode_t  ccode;
};

static int pass, line_num, num_errors, saw_orig, code_loc, saw_end;
static inst_t inst;
static FILE* symout;
static FILE* objout;

/* make a file to keep track of hex instructions */
static FILE* out_file;

static void new_inst_line ();
static void bad_operands ();
static void unterminated_string ();
static void bad_line ();
static void line_ignored ();
static void parse_ccode (const char*);
static void generate_instruction (operands_t, const char*);
static void found_label (const char* lname);

%}

/* condition code specification */
CCODE    [Nn]?[Zz]?[Pp]?

/* operand types */
REGISTER [rR][0-7]
HEX      [xX][-]?[0-9a-fA-F]+
DECIMAL  [#]?[-]?[0-9]+
IMMED    {HEX}|{DECIMAL}
LABEL    [A-Za-z][A-Za-z_0-9]*
STRING   \"([^\"]*|(\\\"))*\"
UTSTRING \"[^\n\r]*

/* operand and white space specification */
SPACE     [ \t]
OP_SEP    {SPACE}*,{SPACE}*
COMMENT   [;][^\n\r]*
EMPTYLINE {SPACE}*{COMMENT}?
ENDLINE   {EMPTYLINE}\r?\n\r?

/* operand formats */
O_RRR  {SPACE}+{REGISTER}{OP_SEP}{REGISTER}{OP_SEP}{REGISTER}{ENDLINE}
O_RRI  {SPACE}+{REGISTER}{OP_SEP}{REGISTER}{OP_SEP}{IMMED}{ENDLINE}
O_RR   {SPACE}+{REGISTER}{OP_SEP}{REGISTER}{ENDLINE}
O_RI   {SPACE}+{REGISTER}{OP_SEP}{IMMED}{ENDLINE}
O_RL   {SPACE}+{REGISTER}{OP_SEP}{LABEL}{ENDLINE}
O_R    {SPACE}+{REGISTER}{ENDLINE}
O_I    {SPACE}+{IMMED}{ENDLINE}
O_L    {SPACE}+{LABEL}{ENDLINE}
O_S    {SPACE}+{STRING}{ENDLINE}
O_UTS  {SPACE}+{UTSTRING}{ENDLINE}
O_     {ENDLINE}

/* need to define YY_INPUT... */

/* exclusive lexing states to read operands, eat garbage lines, and
   check for extra text after .END directive */
%x ls_operands ls_garbage ls_finished

%%

    /* rules for real instruction opcodes */
ADD       {inst.op = OP_ADD;   BEGIN (ls_operands);}
AND       {inst.op = OP_AND;   BEGIN (ls_operands);}
BR{CCODE} {inst.op = OP_BR;    parse_ccode (yytext + 2); BEGIN (ls_operands);}
JMP       {inst.op = OP_JMP;   BEGIN (ls_operands);}
JSRR      {inst.op = OP_JSRR;  BEGIN (ls_operands);}
JSR       {inst.op = OP_JSR;   BEGIN (ls_operands);}
LDI       {inst.op = OP_LDI;   BEGIN (ls_operands);}
LDR       {inst.op = OP_LDR;   BEGIN (ls_operands);}
LD        {inst.op = OP_LD;    BEGIN (ls_operands);}
LEA       {inst.op = OP_LEA;   BEGIN (ls_operands);}
NOT       {inst.op = OP_NOT;   BEGIN (ls_operands);}

ICM        {inst.op = OP_ICM;   BEGIN (ls_operands);}
MLT        {inst.op = OP_MLT;   BEGIN (ls_operands);}
RND        {inst.op = OP_RND;   BEGIN (ls_operands);}
RST        {inst.op = OP_RST;   BEGIN (ls_operands);}
SUB        {inst.op = OP_SUB;   BEGIN (ls_operands);}
SPR        {inst.op = OP_SPR;   BEGIN (ls_operands);}
TIM        {inst.op = OP_TIM;   BEGIN (ls_operands);}

RTI       {inst.op = OP_RTI;   BEGIN (ls_operands);}
STI       {inst.op = OP_STI;   BEGIN (ls_operands);}
STR       {inst.op = OP_STR;   BEGIN (ls_operands);}
ST        {inst.op = OP_ST;    BEGIN (ls_operands);}
TRAP      {inst.op = OP_TRAP;  BEGIN (ls_operands);}

    /* rules for trap pseudo-ols */
GETC      {inst.op = OP_GETC;  BEGIN (ls_operands);}
HALT      {inst.op = OP_HALT;  BEGIN (ls_operands);}
IN        {inst.op = OP_IN;    BEGIN (ls_operands);}
OUT       {inst.op = OP_OUT;   BEGIN (ls_operands);}
PUTS      {inst.op = OP_PUTS;  BEGIN (ls_operands);}
PUTSP     {inst.op = OP_PUTSP; BEGIN (ls_operands);}

    /* rules for non-trap pseudo-ops */
\.FILL    {inst.op = OP_FILL;  BEGIN (ls_operands);}
RET       {inst.op = OP_RET;   BEGIN (ls_operands);}
\.STRINGZ {inst.op = OP_STRINGZ; BEGIN (ls_operands);}

    /* rules for directives */
\.BLKW    {inst.op = OP_BLKW; BEGIN (ls_operands);}
\.END     {saw_end = 1;       BEGIN (ls_finished);}
\.ORIG    {inst.op = OP_ORIG; BEGIN (ls_operands);}

    /* rules for operand formats */
<ls_operands>{O_RRR} {generate_instruction (O_RRR, yytext); BEGIN (0);}
<ls_operands>{O_RRI} {generate_instruction (O_RRI, yytext); BEGIN (0);}
<ls_operands>{O_RR}  {generate_instruction (O_RR, yytext);  BEGIN (0);}
<ls_operands>{O_RI}  {generate_instruction (O_RI, yytext);  BEGIN (0);}
<ls_operands>{O_RL}  {generate_instruction (O_RL, yytext);  BEGIN (0);}
<ls_operands>{O_R}   {generate_instruction (O_R, yytext);   BEGIN (0);}
<ls_operands>{O_I}   {generate_instruction (O_I, yytext);   BEGIN (0);}
<ls_operands>{O_L}   {generate_instruction (O_L, yytext);   BEGIN (0);}
<ls_operands>{O_S}   {generate_instruction (O_S, yytext);   BEGIN (0);}
<ls_operands>{O_}    {generate_instruction (O_, yytext);    BEGIN (0);}

    /* eat excess white space */
{SPACE}+ {}  
{ENDLINE} {new_inst_line (); /* a blank line */ }

    /* labels, with or without subsequent colons */\
    /* 
       the colon form is used in some examples in the second edition
       of the book, but may be removed in the third; it also allows 
       labels to use opcode and pseudo-op names, etc., however.
     */
{LABEL}          {found_label (yytext);}
{LABEL}{SPACE}*: {found_label (yytext);}

    /* error handling??? */
<ls_operands>{O_UTS} {unterminated_string (); BEGIN (0);}
<ls_operands>[^\n\r]*{ENDLINE} {bad_operands (); BEGIN (0);}
{O_RRR}|{O_RRI}|{O_RR}|{O_RI}|{O_RL}|{O_R}|{O_I}|{O_S}|{O_UTS} {
    bad_operands ();
}

. {BEGIN (ls_garbage);}
<ls_garbage>[^\n\r]*{ENDLINE} {bad_line (); BEGIN (0);}

    /* parsing after the .END directive */
<ls_finished>{ENDLINE}|{EMPTYLINE}     {new_inst_line (); /* a blank line  */}
<ls_finished>.*({ENDLINE}|{EMPTYLINE}) {line_ignored (); return 0;}

%%

int
main (int argc, char** argv)
{
    int len;
    char* ext;
    char* fname;

    if (argc != 2) {
        fprintf (stderr, "usage: %s <ASM filename>\n", argv[0]);
	return 1;
    }

    /* Make our own copy of the filename. */
    len = strlen (argv[1]);
    if ((fname = malloc (len + 5)) == NULL) {
        perror ("malloc");
	return 3;
    }
    strcpy (fname, argv[1]);

    /* Check for .asm extension; if not found, add it. */
    if ((ext = strrchr (fname, '.')) == NULL || strcmp (ext, ".asm") != 0) {
	ext = fname + len;
        strcpy (ext, ".asm");
    }

    /* Open input file. */
    if ((lc3in = fopen (fname, "r")) == NULL) {
        fprintf (stderr, "Could not open %s for reading.\n", fname);
	return 2;
    }

    /* Open output files. */
    strcpy (ext, ".obj");
    if ((objout = fopen (fname, "w")) == NULL) {
        fprintf (stderr, "Could not open %s for writing.\n", fname);
	return 2;
    }

    /* open the out_file for hex instructions. 
    It will end be names {fname}.txt           */
    strcpy (ext, ".txt");
    if ((out_file = fopen (fname, "w")) == NULL) {
        fprintf (stderr, "Could not open %s for writing.\n", fname);
	return 2;
    }

    strcpy (ext, ".sym");
    if ((symout = fopen (fname, "w")) == NULL) {
        fprintf (stderr, "Could not open %s for writing.\n", fname);
	return 2;
    }

    /* FIXME: Do we really need to exactly match old format for compatibility 
       with Windows simulator? */
    fprintf (symout, "// Symbol table\n");
    fprintf (symout, "// Scope level 0:\n");
    fprintf (symout, "//\tSymbol Name       Page Address\n");
    fprintf (symout, "//\t----------------  ------------\n");

    puts ("STARTING PASS 1");
    pass = 1;
    line_num = 0;
    num_errors = 0;
    saw_orig = 0;
    code_loc = 0x3000;
    saw_end = 0;
    new_inst_line ();
    yylex ();
    if (saw_orig == 0) {
        if (num_errors == 0 && !saw_end)
	    fprintf (stderr, "%3d: file contains only comments\n", line_num);
        else {
	    if (saw_end == 0)
		fprintf (stderr, "%3d: no .ORIG or .END directive found\n", 
			 line_num);
	    else
		fprintf (stderr, "%3d: no .ORIG directive found\n", line_num);
	}
	num_errors++;
    } else if (saw_end == 0 ) {
	fprintf (stderr, "%3d: no .END directive found\n", line_num);
	num_errors++;
    }
    printf ("%d errors found in first pass.\n", num_errors);
    if (num_errors > 0)
    	return 1;
    if (fseek (lc3in, 0, SEEK_SET) != 0) {
        perror ("fseek to start of ASM file");
	return 3;
    }
    yyrestart (lc3in);
    /* Return lexer to initial state.  It is otherwise left in ls_finished
       if an .END directive was seen. */
    BEGIN (0);

    puts ("STARTING PASS 2");
    pass = 2;
    line_num = 0;
    num_errors = 0;
    saw_orig = 0;
    code_loc = 0x3000;
    saw_end = 0;
    new_inst_line ();
    yylex ();
    printf ("%d errors found in second pass.\n", num_errors);
    if (num_errors > 0)
    	return 1;

    fprintf (symout, "\n");

    /* close the three files the code made */
    fclose (symout);
    fclose (objout);
    fclose (out_file);

    return 0;
}

static void
new_inst_line () 
{
    inst.op = OP_NONE;
    inst.ccode = CC_;
    line_num++;
}

static void
bad_operands ()
{
    fprintf (stderr, "%3d: illegal operands for %s\n",
	     line_num, opnames[inst.op]);
    num_errors++;
    new_inst_line ();
}

static void
unterminated_string ()
{
    fprintf (stderr, "%3d: unterminated string\n", line_num);
    num_errors++;
    new_inst_line ();
}

static void 
bad_line ()
{
    fprintf (stderr, "%3d: contains unrecognizable characters\n",
	     line_num);
    num_errors++;
    new_inst_line ();
}

static void 
line_ignored ()
{
    if (pass == 1)
	fprintf (stderr, "%3d: WARNING: all text after .END ignored\n",
		 line_num);
}

static int
read_val (const char* s, int* vptr, int bits)
{
    char* trash;
    long v;

    if (*s == 'x' || *s == 'X')
	v = strtol (s + 1, &trash, 16);
    else {
	if (*s == '#')
	    s++;
	v = strtol (s, &trash, 10);
    }
    if (0x10000 > v && 0x8000 <= v)
        v |= -65536L;   /* handles 64-bit longs properly */
    if (v < -(1L << (bits - 1)) || v >= (1L << bits)) {
	fprintf (stderr, "%3d: constant outside of allowed range\n", line_num);
	num_errors++;
	return -1;
    }
    if ((v & (1UL << (bits - 1))) != 0)
	v |= ~((1UL << bits) - 1);
    *vptr = v;
    return 0;
}

static void
write_value (int val)
{
    unsigned char out[2];

    code_loc = (code_loc + 1) & 0xFFFF;
    if (pass == 1)
        return;
    /* FIXME: just htons... */
    out[0] = (val >> 8);
    out[1] = (val & 0xFF);
    fwrite (out, 2, 1, objout);

    /* print to the out_file the hex instruction (the input to the function called val)*/
    fprintf (out_file, "%x\n", val);
}

static char*
sym_name (const char* name)
{
    unsigned char* local = strdup (name);
    unsigned char* cut;

    /* Not fast, but no limit on label length...who cares? */
    for (cut = local; *cut != 0 && !isspace (*cut) && *cut != ':'; cut++);
    *cut = 0;

    return local;
}

static int
find_label (const char* optarg, int bits)
{
    unsigned char* local;
    symbol_t* label;
    int limit, value;

    if (pass == 1)
        return 0;

    local = sym_name (optarg);
    label = find_symbol (local, NULL);
    if (label != NULL) {
	value = label->addr;
	if (bits != 16) { /* Everything except 16 bits is PC-relative. */
	    limit = (1L << (bits - 1));
	    value -= code_loc + 1;
	    if (value < -limit || value >= limit) {
	        fprintf (stderr, "%3d: label \"%s\" at distance %d (allowed "
			 "range is %d to %d)\n", line_num, local, value,
			 -limit, limit - 1);
	        goto bad_label;
	    }
	    return value;
	}
	free (local);
        return label->addr;
    }
    fprintf (stderr, "%3d: unknown label \"%s\"\n", line_num, local);

bad_label:
    num_errors++;
    free (local);
    return 0;
}

static void 
generate_instruction (operands_t operands, const char* opstr)
{
    // ADD R1, R2, R3.... operands = ADD, opstr = 'R1, R2, R3'
    int val, r1, r2, r3;
    const unsigned char* o1;
    const unsigned char* o2;
    const unsigned char* o3;
    const unsigned char* str;

    if ((op_format_ok[inst.op] & (1UL << operands)) == 0) {
	bad_operands ();
	return;
    }
    o1 = opstr;
    while (isspace (*o1)) o1++;
    if ((o2 = strchr (o1, ',')) != NULL) {
        o2++;
	while (isspace (*o2)) o2++;
	if ((o3 = strchr (o2, ',')) != NULL) {
	    o3++;
	    while (isspace (*o3)) o3++;
	}
    } else
    	o3 = NULL;
    if (inst.op == OP_ORIG) {
	if (saw_orig == 0) {
	    if (read_val (o1, &code_loc, 16) == -1)
		/* Pick a value; the error prevents code generation. */
		code_loc = 0x3000; 
	    else {
	        write_value (code_loc);
		code_loc--; /* Starting point doesn't count as code. */
	    }
	    saw_orig = 1;
	} else if (saw_orig == 1) {
	    fprintf (stderr, "%3d: multiple .ORIG directives found\n",
		     line_num);
	    saw_orig = 2;
	}
	new_inst_line ();
	return;
    }
    if (saw_orig == 0) {
	fprintf (stderr, "%3d: instruction appears before .ORIG\n",
		 line_num);
	num_errors++;
	new_inst_line ();
	saw_orig = 2;
	return;
    }
    if ((pre_parse[operands] & PP_R1) != 0)
        r1 = o1[1] - '0';
    if ((pre_parse[operands] & PP_R2) != 0)
        r2 = o2[1] - '0';
    if ((pre_parse[operands] & PP_R3) != 0)
        r3 = o3[1] - '0';
    if ((pre_parse[operands] & PP_I2) != 0)
        (void)read_val (o2, &val, 9);
    if ((pre_parse[operands] & PP_L2) != 0)
        val = find_label (o2, 9);

    switch (inst.op) {
	/* Generate real instruction opcodes. */

    case OP_MLT:
        if (operands == O_RRI) {
            int tempR;
            (void)read_val (o3, &val, 5);
            // MLT R1, R3, #9 OR MLT R1, R3, #-9 
            if (r1 != r2) {
                // reset r1 and add val to it. Rest of the code is from R1, R1, R2 case
                write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0));
                write_value (0x1020 | (r1 << 9) | (r1 << 6) | (val & 0x1F));
                tempR = r2;
            }
            else {
                int tempR = 0b0;
                if (r1 == 0){
                    tempR = 0b001;
                }
                // store
                write_value (0x3000 | (tempR << 9) | (0x1)); //  
                write_value(0xE01);
                write_value(0xE02);  // This location is tempR

                // reset tempR and add r1 to it
                write_value (0x5020 | (tempR << 9) | (tempR << 6)| (0x0));
                write_value (0x1020 | (tempR << 9) | (tempR << 6) | (val & 0x1F));
            }

            // store r1, r3, r3 in memory PC + 3
            write_value (0x3000 | (r1 << 9) | (0x3)); //  
            write_value (0x3000 | (tempR << 9) | (0x3)); // 
            write_value (0x3000 | (tempR << 9) | (0x3)); // 
            // branch in all case two spots!  and garbage. 
            write_value(0xE03);
            write_value(0xE02);
            write_value(0xE02);
            write_value(0xE02); 

            // loop 
            // check if tempR is negative- add 0
            // add 0 to tempR, br neg
            write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x0));
            write_value(0x807);

            // reset r1. 
            write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0 & 0x1F));   

            // LD tempR = mem[0x1] --> tempR = 3.    
            // pos_mult:    
            // r1 = r1 + tempR
            write_value (0x2000 | (tempR << 9) | (0x1F9));

            write_value (0x1000 | (r1 << 9) | (r1 << 6) | (tempR & 0x1F));
            // LD mem[0x2]: tempR = tempR - 1 
            write_value (0x2000 | (tempR << 9) | (0x1F9));                
            write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x1F));
            // ST tempR in mem[0x2]
            write_value (0x3000 | (tempR << 9) | (0x1F7));

            // br nzp to Here!
            write_value(0xE0E);

            // if tempR negtive:
            // add 0 to r1, br neg_step
            write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x0));
            write_value(0x807);

            // case tempR is neg, tempR is pos:
            // reset r0. 
            write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0));   

            // tempR be the counter and r1 the adder. 
            write_value (0x2000 | (tempR << 9) | (0x1F2));
            write_value (0x1000 | (r1 << 9) | (r1 << 6) | (tempR & 0x1F));
            // LD mem[0x2]: tempR = tempR - 1 
            write_value (0x2000 | (tempR << 9) | (0x1EE)); // F0                
            write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x1F));
            // ST r1 in mem[0x2]
            write_value (0x3000 | (tempR << 9) | (0x1EC));

            // br nzp to Here!
            write_value(0xE05);

            // neg_step
            // both are negative: not and add 1 to tempR and tempR.
            write_value (0x903F | (tempR << 9) | (tempR << 6));
            write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x1));
            write_value (0x903F | (r1 << 9) | (r1 << 6));
            write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x1));
            // br nzp to pos_mult
            write_value(0xFF4);

            // Here!
            // br np to pos_mult
            write_value(0xBEB);

            // restore the tempR or r2 depending on call
            if (r1 != r2){
                write_value (0x2000 | (r2 << 9) | (0x1E5)); 
            }
            else {
                write_value (0x2000 | (tempR << 9) | (0x1DD)); 
            }
            break;
        }
        else {
            if ((r1 == r2 && r1 != r3)  || (r1 != r2 && r1 != r3)) {
                // MLT R1, R1, R2 or MLT R1, R2, R3

                // store r1, r3, r3 in memory PC + 3
                write_value (0x3000 | (r2 << 9) | (0x3)); //  
                write_value (0x3000 | (r3 << 9) | (0x3)); // 
                write_value (0x3000 | (r3 << 9) | (0x3)); // 
                // branch in all case two spots!  and garbage. 
                write_value(0xE03);
                write_value(0xE02);
                write_value(0xE02);
                write_value(0xE02); 

                // loop 
                // check if r3 is negative- add 0
                // add 0 to r3, br neg
                write_value (0x1020 | (r3 << 9) | (r3 << 6) | (0x0));
                write_value(0x807);

                // reset r1. 
                write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0 & 0x1F));   

                // LD r3 = mem[0x1] --> r3 = 3.    
                // pos_mult:    
                // r1 = r1 + r3
                write_value (0x2000 | (r3 << 9) | (0x1F9));
                write_value (0x1000 | (r1 << 9) | (r1 << 6) | (r3));
                // LD mem[0x2]: r2 = r2 - 1 
                write_value (0x2000 | (r3 << 9) | (0x1F9));                
                write_value (0x1020 | (r3 << 9) | (r3 << 6) | (0x1F));
                // ST r2 in mem[0x2]
                write_value (0x3000 | (r3 << 9) | (0x1F7));

                // br nzp to Here!
                write_value(0xE0E);

                // if r3 negtive:
                // add 0 to r1, br neg_step
                write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x0));
                write_value(0x807);

                // case r3 is neg, r2 is pos:
                // reset r0. 
                write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0));   

                // r3 be the counter and r1 the adder. 
                write_value (0x2000 | (r3 << 9) | (0x1F2));
                write_value (0x1000 | (r1 << 9) | (r1 << 6) | (r3));
                // LD mem[0x2]: r3 = r3 - 1 
                write_value (0x2000 | (r3 << 9) | (0x1EE)); // F0                
                write_value (0x1020 | (r3 << 9) | (r3 << 6) | (0x1F));
                // ST r1 in mem[0x2]
                write_value (0x3000 | (r3 << 9) | (0x1EC));

                // br nzp to Here!
                write_value(0xE05);

                // neg_step
                // both are negative: not and add 1 to r2 and r3.
                write_value (0x903F | (r3 << 9) | (r3 << 6));
                write_value (0x1020 | (r3 << 9) | (r3 << 6) | (0x1));
                write_value (0x903F | (r1 << 9) | (r1 << 6));
                write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x1));
                // br nzp to pos_mult
                write_value(0xFF4);

                // Here!
                // br np to pos_mult
                write_value(0xBEB);

                // end of loop, restore r3 at mem[0x3]
                write_value (0x2000 | (r3 << 9) | (0x1E5)); 
                break;
            }
            else if ((r1 == r2 && r2 == r3) || (r1 != r2 && r2 == r3)) {
                int tempR;
                if (r1 != r2 && r2 == r3){
                    tempR = r2; 
                    // clear r1 and put r2 in r1
                    write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0));
                    write_value (0x1000 | (r1 << 9) | (r1 << 6) | r2);
                }
                else if ((r1 == r2 && r2 == r3)) {
                    // printf("%d %d %d", r1, r2, r3); 
                    tempR = 0b0;
                    if (r1 == 0){
                        tempR = 0b001;
                    }
                }

                // store tempR to restore it later. 
                write_value (0x3000 | (tempR << 9) | (0x1)); //  
                write_value(0xE01);
                write_value(0xE02);  // This location is tempR

                // reset tempR and add r1 to it
                write_value (0x5020 | (tempR << 9) | (tempR << 6)| (0x0));
                write_value (0x1000 | (tempR << 9) | (tempR << 6) | r1);
                
                // store r1, r3, r3 in memory PC + 3
                write_value (0x3000 | (r1 << 9) | (0x3)); //  
                write_value (0x3000 | (tempR << 9) | (0x3)); // 
                write_value (0x3000 | (tempR << 9) | (0x3)); // 
                // branch in all case two spots!  and garbage. 
                write_value(0xE03);
                write_value(0xE02);
                write_value(0xE02);
                write_value(0xE02); 

                // loop 
                // check if tempR is negative- add 0
                // add 0 to tempR, br neg
                write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x0));
                write_value(0x807);

                // reset r1. 
                write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0 & 0x1F));   

                // LD tempR = mem[0x1] --> tempR = 3.    
                // pos_mult:    
                // r1 = r1 + tempR
                write_value (0x2000 | (tempR << 9) | (0x1F9));
                write_value (0x1000 | (r1 << 9) | (r1 << 6) | (tempR));
                // LD mem[0x2]: r2 = r2 - 1 
                write_value (0x2000 | (tempR << 9) | (0x1F9));                
                write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x1F));
                // ST r2 in mem[0x2]
                write_value (0x3000 | (tempR << 9) | (0x1F7));

                // br nzp to Here!
                write_value(0xE0E);

                // if tempR negtive:
                // add 0 to r1, br neg_step
                write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x0));
                write_value(0x807);

                // case tempR is neg, r2 is pos:
                // reset r0. 
                write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0));   

                // tempR be the counter and r1 the adder. 
                write_value (0x2000 | (tempR << 9) | (0x1F2));
                write_value (0x1000 | (r1 << 9) | (r1 << 6) | (tempR));
                // LD mem[0x2]: tempR = tempR - 1 
                write_value (0x2000 | (tempR << 9) | (0x1EE)); // F0                
                write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x1F));
                // ST r1 in mem[0x2]
                write_value (0x3000 | (tempR << 9) | (0x1EC));

                // br nzp to Here!
                write_value(0xE05);

                // neg_step
                // both are negative: not and add 1 to r2 and tempR.
                write_value (0x903F | (tempR << 9) | (tempR << 6));
                write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x1));
                write_value (0x903F | (r1 << 9) | (r1 << 6));
                write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x1));
                // br nzp to pos_mult
                write_value(0xFF4);

                // Here!
                // br np to pos_mult
                write_value(0xBEB);

                // end of loop, restore tempR at mem[0x3]
                write_value (0x2000 | (tempR << 9) | (0x1DD)); 
            
                break;
            }
        }
        break;


    case OP_SUB:
    /* check if it is with imm value or 3 registers. */
        if (operands == O_RRI) {
            (void)read_val (o3, &val, 5);
            if (r1 != r2){ // r1 = NOT(r2). 
                write_value (0x903F | (r1 << 9) | (r2 << 6));
            }
            else { // r1 = NOT(r1)
                write_value (0x903F | (r1 << 9) | (r1 << 6));
            }
            // r1 = r1 + val + 1
            write_value (0x1020 | (r1 << 9) | (r1 << 6) | ((val + 0x1) & 0x1F));
            
            // not r1 + 1
            write_value (0x903F | (r1 << 9) | (r1 << 6));
            write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x1));
            break;
            
        } else {
            if (r1 == r2  && r2 == r3) {
                // SUB R1, R1, R1. reset R1 = 0. 
                write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0));
            }
            else if (r1 != r2) {
                // r1 = -r3. NOT(r3) add 1. 
                write_value (0x903F | (r1 << 9) | (r3 << 6));
                write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x1));

                // r1 = r1 + r2
                write_value (0x1000 | (r1 << 9) | (r1 << 6) | r2);
            }
            else {
                // r1 = -r2. NOT(r2) add 1. 
                write_value (0x903F | (r1 << 9) | (r2 << 6));
                write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x1));

                // r1 = r1 + r3
                write_value (0x1000 | (r1 << 9) | (r1 << 6) | r3);

                // not r1 + 1
                write_value (0x903F | (r1 << 9) | (r1 << 6));
                write_value (0x1020 | (r1 << 9) | (r1 << 6) | (0x1));
            }
        }
        break;

    // puts a random value register!
    // This is good for testings - registers will be postive, neg, or zero.
    case OP_RND: ;
        // generate a random number 
        int r = rand();

        // br nzp over 1 spot 
        write_value(0xE01);
        // put random value
        write_value(r);
        // load value of line into register.  
        write_value (0x2000 | (r1 << 9) | (0x1FE));
        break;

    /* put the time in microseconds in a register */
    case OP_TIM: ;

        int a = getMicrotime(); 
        printf("mircoseconds : %x\n", a & 0xffff);

        // branch one spot 
        write_value(0xE01);
        // put value in that random line. 
        write_value(a & 0xffff);
        // load value of line into register.  
        write_value (0x2000 | (r1 << 9) | (0x1FE));

        break; 

    // generate a RANDOM opcode based on time and rand()
    case OP_SPR: ;
        int random_line = rand();
        int b = getMicrotime(); 

        int line_random = b & random_line; 
        // only want 4 digits so AND 0xfff
        write_value(line_random & 0xffff);

        break;

    // Reset a register
    case OP_RST:
		write_value (0x5020 | (r1 << 9) | (r1 << 6)| (0x0));
	    break;

    /* increment a memory location by 1*/
    case OP_ICM:
	    (void)read_val (o1, &val, 11);
        /* plan: copy what is in R0 into memory. 
        load from memory into R0 the location I want to increment (with 'val' as the offset). 
        Increment R0. Store it in memory at the same location. 
        Restore R0. */

        int tempR = 0b0;
        // store
        write_value (0x3000 | (tempR << 9) | (0x1)); //  
        write_value(0xE01);
        write_value(0xE02);  // This what is in R0. 

        // load from memory
        write_value (0x2000 | (tempR << 9) | (val + 2));   
        // add 1 to R0             
        write_value (0x1020 | (tempR << 9) | (tempR << 6) | (0x1));
        // store it in that location
        write_value (0x3000 | (tempR << 9) | (val));

        // restore R0
        write_value (0x2000 | (tempR << 9) | (0x1FB));

        break; 

	case OP_ADD:
	    if (operands == O_RRI) { // with immediate values
	    	/* Check or read immediate range (error in first pass
		   prevents execution of second, so never fails). */
	        (void)read_val (o3, &val, 5);
		write_value (0x1020 | (r1 << 9) | (r2 << 6) | (val & 0x1F));
	    } else
		write_value (0x1000 | (r1 << 9) | (r2 << 6) | r3);
	    break;

	case OP_AND:
	    if (operands == O_RRI) {
	    	/* Check or read immediate range (error in first pass
		   prevents execution of second, so never fails). */
	        (void)read_val (o3, &val, 5);
		write_value (0x5020 | (r1 << 9) | (r2 << 6) | (val & 0x1F));
	    } else
		write_value (0x5000 | (r1 << 9) | (r2 << 6) | r3);
	    break;

	case OP_BR:
	    if (operands == O_I)
	        (void)read_val (o1, &val, 9);
	    else /* O_L */
	        val = find_label (o1, 9);
	    write_value (inst.ccode | (val & 0x1FF));
	    break;
	case OP_JMP:
	    write_value (0xC000 | (r1 << 6));
	    break;


	case OP_JSR:
	    if (operands == O_I)
	        (void)read_val (o1, &val, 11);
	    else /* O_L */
	        val = find_label (o1, 11);
	    write_value (0x4800 | (val & 0x7FF));
	    break;
	case OP_JSRR:
	    write_value (0x4000 | (r1 << 6));
	    break;
	case OP_LD:
	    write_value (0x2000 | (r1 << 9) | (val & 0x1FF));
	    break;
	case OP_LDI:
	    write_value (0xA000 | (r1 << 9) | (val & 0x1FF));
	    break;
	case OP_LDR:
	    (void)read_val (o3, &val, 6);
	    write_value (0x6000 | (r1 << 9) | (r2 << 6) | (val & 0x3F));
	    break;
	case OP_LEA:
	    write_value (0xE000 | (r1 << 9) | (val & 0x1FF));
	    break;
	case OP_NOT:
	    write_value (0x903F | (r1 << 9) | (r2 << 6));
	    break;


	case OP_RTI:
	    write_value (0x8000);
	    break;
	case OP_ST:
	    write_value (0x3000 | (r1 << 9) | (val & 0x1FF));
	    break;
	case OP_STI:
	    write_value (0xB000 | (r1 << 9) | (val & 0x1FF));
	    break;
	case OP_STR:
	    (void)read_val (o3, &val, 6);
	    write_value (0x7000 | (r1 << 9) | (r2 << 6) | (val & 0x3F));
	    break;


	case OP_TRAP:
	    (void)read_val (o1, &val, 8);
	    write_value (0xF000 | (val & 0xFF));
	    break;

	/* Generate trap pseudo-ops. */
	case OP_GETC:  write_value (0xF020); break;
	case OP_HALT:  write_value (0xF025); break;
	case OP_IN:    write_value (0xF023); break;
	case OP_OUT:   write_value (0xF021); break;
	case OP_PUTS:  write_value (0xF022); break;
	case OP_PUTSP: write_value (0xF024); break;

	/* Generate non-trap pseudo-ops. */
    	case OP_FILL:
	    if (operands == O_I) {
		(void)read_val (o1, &val, 16);
		val &= 0xFFFF;
	    } else /* O_L */
		val = find_label (o1, 16);
	    write_value (val);
    	    break;
	case OP_RET:   
	    write_value (0xC1C0); 
	    break;
	case OP_STRINGZ:
	    /* We must count locations written in pass 1;
	       write_value squashes the writes. */
	    for (str = o1 + 1; str[0] != '\"'; str++) {
		if (str[0] == '\\') {
		    switch (str[1]) {
			case 'a': write_value ('\a'); str++; break;
			case 'b': write_value ('\b'); str++; break;
			case 'e': write_value ('\e'); str++; break;
			case 'f': write_value ('\f'); str++; break;
			case 'n': write_value ('\n'); str++; break;
			case 'r': write_value ('\r'); str++; break;
			case 't': write_value ('\t'); str++; break;
			case 'v': write_value ('\v'); str++; break;
			case '\\': write_value ('\\'); str++; break;
			case '\"': write_value ('\"'); str++; break;
			/* FIXME: support others too? */
			default: write_value (str[1]); str++; break;
		    }
		} else {
		    if (str[0] == '\n')
		        line_num++;
		    write_value (*str);
		}
	    }
	    write_value (0);
	    break;
	case OP_BLKW:
	    (void)read_val (o1, &val, 16);
	    val &= 0xFFFF;
	    while (val-- > 0)
	        write_value (0x0000);
	    break;
	
	/* Handled earlier or never used, so never seen here. */
	case OP_NONE:
        case OP_ORIG:
        case OP_END:
	case NUM_OPS:
	    break;
    }
    new_inst_line ();
}

static void 
parse_ccode (const char* ccstr)
{
    if (*ccstr == 'N' || *ccstr == 'n') {
	inst.ccode |= CC_N;
        ccstr++;
    }
    if (*ccstr == 'Z' || *ccstr == 'z') {
	inst.ccode |= CC_Z;
        ccstr++;
    }
    if (*ccstr == 'P' || *ccstr == 'p')
	inst.ccode |= CC_P;

    /* special case: map BR to BRnzp */
    if (inst.ccode == CC_)
        inst.ccode = CC_P | CC_Z | CC_N;
}

static void
found_label (const char* lname) 
{
    unsigned char* local = sym_name (lname);

    if (pass == 1) {
	if (saw_orig == 0) {
	    fprintf (stderr, "%3d: label appears before .ORIG\n", line_num);
	    num_errors++;
	} else if (add_symbol (local, code_loc, 0) == -1) {
	    fprintf (stderr, "%3d: label %s has already appeared\n", 
	    	     line_num, local);
	    num_errors++;
	} else
	    fprintf (symout, "//\t%-16s  %04X\n", local, code_loc);
    }

    free (local);
}

