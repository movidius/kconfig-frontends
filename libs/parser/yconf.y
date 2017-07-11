%{
/*
 * Copyright (C) 2002 Roman Zippel <zippel@linux-m68k.org>
 * Released under the terms of the GNU GPL v2.0.
 */

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "lkc.h"

#define printd(mask, fmt...) if (cdebug & (mask)) printf(fmt)

#define PRINTD		0x0001
#define DEBUG_PARSE	0x0002

int cdebug = PRINTD;

extern int zconflex(void);
static void zconfprint(const char *err, ...);
static void zconf_error(const char *err, ...);
static void zconferror(const char *err);
static bool zconf_endtoken(const struct kconf_id *id, int starttoken, int endtoken);

static void shaveapp_create_shaveapp_list();
static void shaveapp_add_to_list(const char *shaveapp_id);
static void shaveapp_generate_use(const char *shaveapp_id);
static void shaveapp_add_entrypoints(const char *entrypoints);
static void shaveapp_generate_type_choice(const char *shaveapp_id);
static void shaveapp_generate_placement(const char *shaveapp_id);
static void shaveapp_generate_srcs_dir(const char *shaveapp_id);
static void shavegroup_add_to_list(const char *shavegroup_id);
static void shavegroup_add_to_current_shaveapp(const char *shavegroup_id);

struct symbol *symbol_hash[SYMBOL_HASHSIZE];

static struct menu *current_menu, *current_entry;
static char *shave_app_list =NULL;
static const char *current_shaveapp = NULL;
static char *shave_group_list = NULL;

%}
%expect 34

%union
{
	char *string;
	struct file *file;
	struct symbol *symbol;
	struct expr *expr;
	struct menu *menu;
	const struct kconf_id *id;
}

%token <id>T_MAINMENU
%token <id>T_MENU
%token <id>T_ENDMENU
%token <id>T_SOURCE
%token <id>T_CHOICE
%token <id>T_ENDCHOICE
%token <id>T_COMMENT
%token <id>T_CONFIG
%token <id>T_MENUCONFIG
%token <id>T_HELP
%token <string> T_HELPTEXT
%token <id>T_IF
%token <id>T_ENDIF
%token <id>T_DEPENDS
%token <id>T_OPTIONAL
%token <id>T_PROMPT
%token <id>T_TYPE
%token <id>T_DEFAULT
%token <id>T_SELECT
%token <id>T_SHAVECORECOUNT
%token <id>T_SHAVEGROUP
%token <id>T_SHAVEAPP
%token <id>T_ENTRYPOINTS
%token <id>T_RANGE
%token <id>T_VISIBLE
%token <id>T_OPTION
%token <id>T_ON
%token <string> T_WORD
%token <string> T_WORD_QUOTE
%token T_UNEQUAL
%token T_CLOSE_PAREN
%token T_OPEN_PAREN
%token T_EOL

%left T_OR
%left T_AND
%left T_EQUAL T_UNEQUAL
%nonassoc T_NOT

%type <string> prompt
%type <symbol> symbol
%type <expr> expr
%type <expr> if_expr
%type <id> end
%type <id> option_name
%type <menu> if_entry menu_entry choice_entry
%type <string> symbol_option_arg word_opt
%type <string> shaveapp_entry_start

%destructor {
	fprintf(stderr, "%s:%d: missing end statement for this entry\n",
		$$->file->name, $$->lineno);
	if (current_menu == $$)
		menu_end_menu();
} if_entry menu_entry choice_entry

%{
/* Include zconf.hash.c here so it can see the token constants. */
#include "hconf.c"
%}

%%
kcnf: input 
{
  shaveapp_create_shaveapp_list();
  /* zconfdump(stdout);  */
};

input: nl start | start;

start: mainmenu_stmt stmt_list | stmt_list;

stmt_list:
	  /* empty */
	| stmt_list common_stmt
	| stmt_list choice_stmt
	| stmt_list menu_stmt
	| stmt_list end			{ zconf_error("unexpected end statement"); }
	| stmt_list T_WORD error T_EOL	{ zconf_error("unknown statement \"%s\"", $2); }
	| stmt_list option_name error T_EOL
{
	zconf_error("unexpected option \"%s\"", kconf_id_strings + $2->name);
}
	| stmt_list error T_EOL		{ zconf_error("invalid statement"); }
;

option_name:
	T_DEPENDS | T_PROMPT | T_TYPE | T_SELECT | T_OPTIONAL | T_RANGE | T_DEFAULT | T_VISIBLE | T_SHAVECORECOUNT
;

common_stmt:
	  T_EOL
	| if_stmt
	| comment_stmt
	| config_stmt
	| menuconfig_stmt
	| source_stmt
  | shaveapp_stmt
;

option_error:
	  T_WORD error T_EOL		{ zconf_error("unknown option \"%s\"", $1); }
	| error T_EOL			{ zconf_error("invalid option"); }
;


/* config/menuconfig entry */

config_entry_start: T_CONFIG T_WORD T_EOL
{
	struct symbol *sym = sym_lookup($2, 0);
	sym->flags |= SYMBOL_OPTIONAL;
	menu_add_entry(sym);
	printd(DEBUG_PARSE, "%s:%d:config %s\n", zconf_curname(), zconf_lineno(), $2);
};

config_stmt: config_entry_start config_option_list
{
	menu_end_entry();
	printd(DEBUG_PARSE, "%s:%d:endconfig\n", zconf_curname(), zconf_lineno());
};

menuconfig_entry_start: T_MENUCONFIG T_WORD T_EOL
{
	struct symbol *sym = sym_lookup($2, 0);
	sym->flags |= SYMBOL_OPTIONAL;
	menu_add_entry(sym);
	printd(DEBUG_PARSE, "%s:%d:menuconfig %s\n", zconf_curname(), zconf_lineno(), $2);
};

menuconfig_stmt: menuconfig_entry_start config_option_list
{
	if (current_entry->prompt)
		current_entry->prompt->type = P_MENU;
	else
		zconfprint("warning: menuconfig statement without prompt");
	menu_end_entry();
	printd(DEBUG_PARSE, "%s:%d:endconfig\n", zconf_curname(), zconf_lineno());
};

config_option_list:
	  /* empty */
	| config_option_list config_option
	| config_option_list symbol_option
	| config_option_list depends
	| config_option_list help
	| config_option_list option_error
	| config_option_list T_EOL
;

config_option: T_TYPE prompt_stmt_opt T_EOL
{
	menu_set_type($1->stype);
	printd(DEBUG_PARSE, "%s:%d:type(%u)\n",
		zconf_curname(), zconf_lineno(),
		$1->stype);
};

config_option: T_PROMPT prompt if_expr T_EOL
{
	menu_add_prompt(P_PROMPT, $2, $3);
	printd(DEBUG_PARSE, "%s:%d:prompt\n", zconf_curname(), zconf_lineno());
};

config_option: T_DEFAULT expr if_expr T_EOL
{
	menu_add_expr(P_DEFAULT, $2, $3);
	if ($1->stype != S_UNKNOWN)
		menu_set_type($1->stype);
	printd(DEBUG_PARSE, "%s:%d:default(%u)\n",
		zconf_curname(), zconf_lineno(),
		$1->stype);
};

config_option: T_SELECT T_WORD if_expr T_EOL
{
	menu_add_symbol(P_SELECT, sym_lookup($2, 0), $3);
	printd(DEBUG_PARSE, "%s:%d:select\n", zconf_curname(), zconf_lineno());
};

config_option: T_RANGE symbol symbol if_expr T_EOL
{
	menu_add_expr(P_RANGE, expr_alloc_comp(E_RANGE,$2, $3), $4);
	printd(DEBUG_PARSE, "%s:%d:range\n", zconf_curname(), zconf_lineno());
};

config_option: T_SHAVECORECOUNT T_EOL
{};

symbol_option: T_OPTION symbol_option_list T_EOL
;

symbol_option_list:
	  /* empty */
	| symbol_option_list T_WORD symbol_option_arg
{
	const struct kconf_id *id = kconf_id_lookup($2, strlen($2));
	if (id && id->flags & TF_OPTION)
		menu_add_option(id->token, $3);
	else
		zconfprint("warning: ignoring unknown option %s", $2);
	free($2);
};

symbol_option_arg:
	  /* empty */		{ $$ = NULL; }
	| T_EQUAL prompt	{ $$ = $2; }
;

/* choice entry */

choice: T_CHOICE word_opt T_EOL
{
	struct symbol *sym = sym_lookup($2, SYMBOL_CHOICE);
	sym->flags |= SYMBOL_AUTO;
	menu_add_entry(sym);
	menu_add_expr(P_CHOICE, NULL, NULL);
	printd(DEBUG_PARSE, "%s:%d:choice\n", zconf_curname(), zconf_lineno());
};

choice_entry: choice choice_option_list
{
	$$ = menu_add_menu();
};

choice_end: end
{
	if (zconf_endtoken($1, T_CHOICE, T_ENDCHOICE)) {
		menu_end_menu();
		printd(DEBUG_PARSE, "%s:%d:endchoice\n", zconf_curname(), zconf_lineno());
	}
};

choice_stmt: choice_entry choice_block choice_end
;

choice_option_list:
	  /* empty */
	| choice_option_list choice_option
	| choice_option_list depends
	| choice_option_list help
	| choice_option_list T_EOL
	| choice_option_list option_error
;

choice_option: T_PROMPT prompt if_expr T_EOL
{
	menu_add_prompt(P_PROMPT, $2, $3);
	printd(DEBUG_PARSE, "%s:%d:prompt\n", zconf_curname(), zconf_lineno());
};

choice_option: T_TYPE prompt_stmt_opt T_EOL
{
	if ($1->stype == S_BOOLEAN || $1->stype == S_TRISTATE) {
		menu_set_type($1->stype);
		printd(DEBUG_PARSE, "%s:%d:type(%u)\n",
			zconf_curname(), zconf_lineno(),
			$1->stype);
	} else
		YYERROR;
};

choice_option: T_OPTIONAL T_EOL
{
	current_entry->sym->flags |= SYMBOL_OPTIONAL;
	printd(DEBUG_PARSE, "%s:%d:optional\n", zconf_curname(), zconf_lineno());
};

choice_option: T_DEFAULT T_WORD if_expr T_EOL
{
	if ($1->stype == S_UNKNOWN) {
		menu_add_symbol(P_DEFAULT, sym_lookup($2, 0), $3);
		printd(DEBUG_PARSE, "%s:%d:default\n",
			zconf_curname(), zconf_lineno());
	} else
		YYERROR;
};

choice_block:
	  /* empty */
	| choice_block common_stmt
;

/* if entry */

if_entry: T_IF expr nl
{
	printd(DEBUG_PARSE, "%s:%d:if\n", zconf_curname(), zconf_lineno());
	menu_add_entry(NULL);
	menu_add_dep($2);
	$$ = menu_add_menu();
};

if_end: end
{
	if (zconf_endtoken($1, T_IF, T_ENDIF)) {
		menu_end_menu();
		printd(DEBUG_PARSE, "%s:%d:endif\n", zconf_curname(), zconf_lineno());
	}
};

if_stmt: if_entry if_block if_end
;

if_block:
	  /* empty */
	| if_block common_stmt
	| if_block menu_stmt
	| if_block choice_stmt
;

/* mainmenu entry */

mainmenu_stmt: T_MAINMENU prompt nl
{
	menu_add_prompt(P_MENU, $2, NULL);
};

/* menu entry */

menu: T_MENU prompt T_EOL
{
	menu_add_entry(NULL);
	menu_add_prompt(P_MENU, $2, NULL);
	printd(DEBUG_PARSE, "%s:%d:menu\n", zconf_curname(), zconf_lineno());
};

menu_entry: menu visibility_list depends_list
{
	$$ = menu_add_menu();
};

menu_end: end
{
	if (zconf_endtoken($1, T_MENU, T_ENDMENU)) {
		menu_end_menu();
		printd(DEBUG_PARSE, "%s:%d:endmenu\n", zconf_curname(), zconf_lineno());
	}
};

menu_stmt: menu_entry menu_block menu_end
;

menu_block:
	  /* empty */
	| menu_block common_stmt
	| menu_block menu_stmt
	| menu_block choice_stmt
;

source_stmt: T_SOURCE prompt T_EOL
{
	printd(DEBUG_PARSE, "%s:%d:source %s\n", zconf_curname(), zconf_lineno(), $2);
	zconf_nextfile($2);
};

/* comment entry */

comment: T_COMMENT prompt T_EOL
{
	menu_add_entry(NULL);
	menu_add_prompt(P_COMMENT, $2, NULL);
	printd(DEBUG_PARSE, "%s:%d:comment\n", zconf_curname(), zconf_lineno());
};

comment_stmt: comment depends_list
{
	menu_end_entry();
};


shaveapp_stmt: shaveapp_entry_start shaveapp_option_list
{
	printd(DEBUG_PARSE, "%s:%d:shaveapp end %s\n", zconf_curname(), zconf_lineno(), $1);

  shaveapp_generate_use($1);
  shaveapp_generate_type_choice($1);
  shaveapp_generate_placement($1);
  menu_end_menu();
  current_shaveapp = NULL;
};

shaveapp_entry_start: T_SHAVEAPP T_WORD T_EOL
{
  $$ = $2;
  current_shaveapp = $2;
  shaveapp_add_to_list($2);

  menu_add_entry(NULL);

	printd(DEBUG_PARSE, "%s:%d:shaveapp %s\n", zconf_curname(), zconf_lineno(), $2);
};

shaveapp_option_list:
    /* empty */
  | shaveapp_option_list shaveapp_option
  | shaveapp_option_list help
  | shaveapp_option_list T_EOL
;

shaveapp_option: T_PROMPT prompt T_EOL
{
  menu_add_prompt(P_MENU, $2, NULL);
  menu_add_menu();
	printd(DEBUG_PARSE, "%s:%d:shaveapp prompt\n", zconf_curname(), zconf_lineno());
};

shaveapp_option: T_SHAVEGROUP T_WORD T_EOL
{
  shavegroup_add_to_list($2);
  shavegroup_add_to_current_shaveapp($2);
	printd(DEBUG_PARSE, "%s:%d:shaveapp shavegroup\n", zconf_curname(), zconf_lineno());
};

shaveapp_option: T_ENTRYPOINTS T_WORD_QUOTE T_EOL
{
  /* TODO we should check here the menu entry is a shaveapp */
  shaveapp_add_entrypoints($2);
	printd(DEBUG_PARSE, "%s:%d:shaveapp entrypoints\n", zconf_curname(), zconf_lineno());
};

/* help option */

help_start: T_HELP T_EOL
{
	printd(DEBUG_PARSE, "%s:%d:help\n", zconf_curname(), zconf_lineno());
	zconf_starthelp();
};

help: help_start T_HELPTEXT
{
	current_entry->help = $2;
};

/* depends option */

depends_list:
	  /* empty */
	| depends_list depends
	| depends_list T_EOL
	| depends_list option_error
;

depends: T_DEPENDS T_ON expr T_EOL
{
	menu_add_dep($3);
	printd(DEBUG_PARSE, "%s:%d:depends on\n", zconf_curname(), zconf_lineno());
};

/* visibility option */

visibility_list:
	  /* empty */
	| visibility_list visible
	| visibility_list T_EOL
;

visible: T_VISIBLE if_expr
{
	menu_add_visibility($2);
};

/* prompt statement */

prompt_stmt_opt:
	  /* empty */
	| prompt if_expr
{
	menu_add_prompt(P_PROMPT, $1, $2);
};

prompt:	  T_WORD
	| T_WORD_QUOTE
;

end:	  T_ENDMENU T_EOL	{ $$ = $1; }
	| T_ENDCHOICE T_EOL	{ $$ = $1; }
	| T_ENDIF T_EOL		{ $$ = $1; }
;

nl:
	  T_EOL
	| nl T_EOL
;

if_expr:  /* empty */			{ $$ = NULL; }
	| T_IF expr			{ $$ = $2; }
;

expr:	  symbol				{ $$ = expr_alloc_symbol($1); }
	| symbol T_EQUAL symbol			{ $$ = expr_alloc_comp(E_EQUAL, $1, $3); }
	| symbol T_UNEQUAL symbol		{ $$ = expr_alloc_comp(E_UNEQUAL, $1, $3); }
	| T_OPEN_PAREN expr T_CLOSE_PAREN	{ $$ = $2; }
	| T_NOT expr				{ $$ = expr_alloc_one(E_NOT, $2); }
	| expr T_OR expr			{ $$ = expr_alloc_two(E_OR, $1, $3); }
	| expr T_AND expr			{ $$ = expr_alloc_two(E_AND, $1, $3); }
;

symbol:	  T_WORD	{ $$ = sym_lookup($1, 0); free($1); }
	| T_WORD_QUOTE	{ $$ = sym_lookup($1, SYMBOL_CONST); free($1); }
;

word_opt: /* empty */			{ $$ = NULL; }
	| T_WORD

%%

// TODO no longer create the SHAVEAPP_LIST on the fly, but create it upon
// finalization of the configuration, upon parsing end

void shaveapp_create_shaveapp_list()
{
  struct menu *save_current_menu = current_menu;
  current_menu = &rootmenu;

  {
    const char *PROMPT_FORMAT = "Invisible shaveapp internal options";
    menu_add_entry(NULL);
    menu_add_prompt(P_MENU, (char*)PROMPT_FORMAT, NULL);
    menu_add_visibility(expr_alloc_symbol(sym_lookup("n",0)));
  }

  if (shave_app_list != NULL)
  {
    const char *SYMBOL_FORMAT = "APP_USES_SHAVEAPPS";
    struct symbol *sym = sym_lookup(SYMBOL_FORMAT, 0);
    sym->flags |= SYMBOL_OPTIONAL;
    sym->type = S_BOOLEAN;

    menu_add_entry(sym);

    struct expr *left_expr = NULL;
    if (NULL == shave_app_list) {
      left_expr = expr_alloc_symbol(sym_lookup("n",0));
    } else {
      // now build an OR expression with all the shaveapps we found
      const char *sym_list_start = shave_app_list;
      const char *sym_list_end = sym_list_start;
      for (;; sym_list_end++) {

        if (' ' == *sym_list_end || !*sym_list_end) {
          while (' ' == *sym_list_start)
            sym_list_start++; // skip over leading spaces if any;
          size_t sym_length = sym_list_end - sym_list_start;
          char symbol_name[sym_length +1];
          strncpy(symbol_name, sym_list_start, sym_length);
          symbol_name[sym_length] = 0;

          const char *CONFIG_VALUE_FORMAT = "USE_SHAVEAPP_%s";
          size_t config_value_len = strlen(CONFIG_VALUE_FORMAT)+sym_length;
          char *config_value = xcalloc(sizeof(char), config_value_len);
          snprintf(config_value, config_value_len, CONFIG_VALUE_FORMAT, symbol_name);

          struct symbol *curr_symbol = sym_lookup(config_value, 0);
          if (NULL == left_expr) {
            left_expr = expr_alloc_symbol(curr_symbol);
          } else {
            left_expr = expr_alloc_two(E_OR, left_expr,
                expr_alloc_symbol(curr_symbol));
          }

          sym_list_start = sym_list_end; // reset this pointer for next occurrence
        }

        if (!*sym_list_end && sym_list_end == sym_list_start)
          break;
      }
    }
    if (NULL != left_expr) {
      menu_add_expr(P_DEFAULT, left_expr, NULL);
      menu_set_type(S_BOOLEAN);
    }
    menu_end_entry();
    sym_calc_value(sym);
  }

  if (shave_group_list != NULL) {
    /* hack: add a " " to the group list if not already present in order to
     * trick the system when we'll call sym_calc_value below. without this
     * space, the system will replace the name of a single group with it's
     * actual value, which is a library list. this is not the desirable. */
    shavegroup_add_to_list(" ");

    struct symbol *sym_shavegroup_list = sym_lookup("SHAVEGROUP_LIST", 0);
    sym_shavegroup_list->flags |= SYMBOL_OPTIONAL;
    sym_shavegroup_list->type = S_STRING;
    menu_add_entry(sym_shavegroup_list);
    menu_add_prop(P_DEFAULT, NULL,
      expr_alloc_symbol(sym_lookup(shave_group_list,0)), NULL);
    menu_end_entry();
  }

  if (shave_app_list != NULL) {
    struct symbol *sym_shaveapp_list = sym_lookup("SHAVEAPP_LIST", 0);
    sym_shaveapp_list->flags |= SYMBOL_OPTIONAL;
    sym_shaveapp_list->type = S_STRING;
    menu_add_entry(sym_shaveapp_list);
    menu_add_prop(P_DEFAULT, NULL,
      expr_alloc_symbol(sym_lookup(shave_app_list,0)), NULL);
    menu_end_entry();
    sym_calc_value(sym_shaveapp_list);
  }

  {
    menu_end_menu();
    menu_add_menu();
  }

  current_menu = save_current_menu;

	printd(DEBUG_PARSE, "%s:%d:config (shaveapp)%s\n", zconf_curname(),
    zconf_lineno(), "SHAVEAPP_LIST");
}

void append_to_list(char **list, const char *item)
{
  const char *old_list = *list;
  size_t new_list_len = (old_list ? strlen(old_list) : 0)+
    strlen(item)+2;
  char *new_list = xcalloc(sizeof(char), new_list_len);
  new_list[0] = '\0';
  if (old_list) {
    strncat(new_list, old_list, new_list_len);
    new_list_len -= strlen(old_list);
    strncat(new_list, " ", new_list_len);
    new_list_len -= sizeof(char);
  }
  strncat(new_list, item, new_list_len);

  if (old_list) {
      free((void*)old_list);
  }
  *list = new_list;
}

void shaveapp_add_to_list(const char *shaveapp_id)
{
  append_to_list(&shave_app_list, shaveapp_id);
}

void shavegroup_add_to_list(const char *shavegroup_id)
{
  if ((shave_group_list == NULL) ||
      (strstr(shave_group_list, shavegroup_id) == NULL)) {
    append_to_list(&shave_group_list, shavegroup_id);
  }
}

// TODO free the returned pointers over all this program
char *shaveapp_alloc_format_string(const char *format,
    const char *shaveapp_id)
{
  // the size of the format string is longer than needed as it contains the
  // format specifiers that'll get replaced with the actual string value
  size_t length = strlen(format)+strlen(shaveapp_id);
  char *str = xcalloc(sizeof(char), length);
  snprintf(str, length, format, shaveapp_id);
  return str;
}

void shaveapp_create_config_symbol(struct symbol *sym,
    const char *prompt_format, const char *shaveapp_id,
    enum symbol_type type, const char *default_val)
{
  char *prompt = shaveapp_alloc_format_string(prompt_format, shaveapp_id);

  menu_add_entry(sym);
  menu_add_prompt(P_PROMPT, prompt, NULL);
  if (default_val) {
    menu_add_expr(P_DEFAULT, expr_alloc_symbol(sym_lookup(default_val,0)), NULL);
  }
  menu_end_entry();
}

void shaveapp_create_config(const char *sym_format,
    const char *prompt_format, const char *shaveapp_id,
    enum symbol_type type, const char *default_val)
{
  char *sym_name = shaveapp_alloc_format_string(sym_format, shaveapp_id);
  struct symbol *sym = sym_lookup(sym_name, 0);
  sym->flags |= SYMBOL_OPTIONAL;
  sym->type = type;

  shaveapp_create_config_symbol(sym, prompt_format, shaveapp_id, type, default_val);
}

void shaveapp_generate_use(const char *shaveapp_id)
{
  // create the USE_SHAVEAPP_<id> entry with the prompt and the default value
  const char *SYMBOL_FORMAT = "USE_SHAVEAPP_%s";
  const char *PROMPT_FORMAT = "Use SHAVE application %s";
  shaveapp_create_config(SYMBOL_FORMAT, PROMPT_FORMAT, shaveapp_id,
      S_BOOLEAN, "y");

  shaveapp_generate_srcs_dir(shaveapp_id);
}

void shaveapp_generate_srcs_dir(const char *shaveapp_id)
{
  const char *SYMBOL_FORMAT = "SHAVEAPP_%s_SRCS_DIR";
  const char *PROMPT_FORMAT = "SHAVE application's %s sources directory";
  shaveapp_create_config(SYMBOL_FORMAT, PROMPT_FORMAT, shaveapp_id,
      S_STRING, "");
}

void shaveapp_add_entrypoints(const char *entrypoints)
{
  if (NULL == current_shaveapp) {
    zconf_error("entrypoints option encountered without current shaveapp entry");
    zconfnerrs++;
    fprintf(stderr, "%s:%d: entrypoints option encountered withoud current shaveapp", current_menu->file->name, current_menu->lineno);
    return;
  }
  const char *SYMBOL_FORMAT = "SHAVEAPP_%s_ENTRY_POINTS";
  const char *PROMPT_FORMAT = "%s shaveapp entry points list";
  shaveapp_create_config(SYMBOL_FORMAT, PROMPT_FORMAT,
      current_shaveapp, S_STRING, entrypoints);
}

const char *SYMBOL_FORMAT_SHAVEAPP_TYPE_STATIC = "SHAVEAPP_%s_TYPE_STATIC";

void shaveapp_generate_type_choice(const char *shaveapp_id)
{
  {
    const char *SYMBOL_FORMAT = "SHAVEAPP_%s_TYPE";
    char *symbol_id = shaveapp_alloc_format_string(SYMBOL_FORMAT, shaveapp_id);

    const char *PROMPT_FORMAT = "%s shaveapp type";
    char *prompt = shaveapp_alloc_format_string(PROMPT_FORMAT, shaveapp_id);

    struct symbol *sym = sym_lookup(symbol_id, SYMBOL_CHOICE);
    sym->flags |= SYMBOL_AUTO;
    menu_add_entry(sym);
    menu_add_expr(P_CHOICE, NULL, NULL);
    menu_add_prompt(P_PROMPT, prompt, NULL);
    menu_add_menu();
  }

  {
    const char *PROMPT_FORMAT = "Make %s shaveapp be static";
    shaveapp_create_config(SYMBOL_FORMAT_SHAVEAPP_TYPE_STATIC, PROMPT_FORMAT, shaveapp_id, S_BOOLEAN, NULL);
  }
  {
    const char *SYMBOL_FORMAT = "SHAVEAPP_%s_TYPE_DYNAMIC";
    const char *PROMPT_FORMAT = "Make %s shaveapp be dynamic";
    shaveapp_create_config(SYMBOL_FORMAT, PROMPT_FORMAT, shaveapp_id, S_BOOLEAN, NULL);
  }

  // close the choice we started above
  menu_end_menu();
}

void shaveapp_generate_placement(const char *shaveapp_id)
{
  {
    const char *PROMPT_FORMAT = "%s shaveapp placement";
    char *prompt = shaveapp_alloc_format_string(PROMPT_FORMAT, shaveapp_id);
    menu_add_entry(NULL);
    menu_add_prompt(P_MENU, prompt, NULL);
    menu_add_menu();
  }
  const int SHAVE_CORE_COUNT=12; // TODO lookup the specially defined symbol and get it's value here
  int i;
  for (i=0; i<SHAVE_CORE_COUNT;i++) {
    const char *SYMBOL_FORMAT = "SHAVEAPP_%s_PLACE_CORE%d";
    size_t sym_len = strlen(SYMBOL_FORMAT)+strlen(shaveapp_id);
    char *sym_name = xcalloc(sizeof(char), sym_len);
    snprintf(sym_name, sym_len, SYMBOL_FORMAT, shaveapp_id, i);
    struct symbol *sym = sym_lookup(sym_name, 0);
    sym->flags |= SYMBOL_OPTIONAL;
    sym->type = S_BOOLEAN;

    const char *PROMPT_FORMAT = "Place the %s shaveapp on SHAVE core %d";
    size_t prompt_len = strlen(PROMPT_FORMAT)+strlen(shaveapp_id)+10;
    char *prompt = xcalloc(sizeof(char), prompt_len);
    snprintf(prompt, prompt_len, PROMPT_FORMAT, shaveapp_id, i);

    size_t dep_symbol_len = strlen(SYMBOL_FORMAT_SHAVEAPP_TYPE_STATIC)+strlen(shaveapp_id);
    char *dep_symbol = xcalloc(sizeof(char), dep_symbol_len);
    snprintf(dep_symbol, dep_symbol_len, SYMBOL_FORMAT_SHAVEAPP_TYPE_STATIC, shaveapp_id);
    struct expr *dep_expr = expr_alloc_symbol(sym_lookup(dep_symbol,0));

    menu_add_entry(sym);
    menu_add_prompt(P_PROMPT, prompt, NULL);
    menu_add_expr(P_DEFAULT, expr_alloc_symbol(sym_lookup(i == 0 ? "y" : "n",0)), NULL);
    menu_add_dep(dep_expr);
    menu_end_entry();
  }

  menu_end_menu();
}

void shavegroup_add_to_current_shaveapp(const char *groupid)
{
  if (NULL == current_shaveapp) {
    zconf_error("shavegroup attribute must be applied to a shaveapp");
    zconfnerrs++;
    fprintf(stderr, "%s:%d shavegroup attribute must be applied to a shaveapp", current_menu->file->name, current_menu->lineno);
    return;
  }

  const char *CONFIG_FORMAT = "SHAVEAPP_%s_GROUP";
  const char *PROMPT_FORMAT = "%s shaveapp group";

  /* dirty HACK: add an extra space at the end of the groupid in order to
  avoid it getting expanded to the actual's group value */
  size_t groupid_len = strlen(groupid);
  char fake_groupid[groupid_len+2];
  memset(fake_groupid, 0, groupid_len+2);
  strncpy(fake_groupid, groupid, groupid_len);
  fake_groupid[groupid_len] = ' ';

  shaveapp_create_config(CONFIG_FORMAT, PROMPT_FORMAT, current_shaveapp,
      S_STRING, fake_groupid);
}

void conf_parse(const char *name)
{
	struct symbol *sym;
	int i;

	zconf_initscan(name);

	sym_init();
	_menu_init();
	rootmenu.prompt = menu_add_prompt(P_MENU, ROOTMENU, NULL);

	if (getenv("ZCONF_DEBUG"))
		zconfdebug = 1;
	zconfparse();
	if (zconfnerrs)
		exit(1);
	if (!modules_sym)
		modules_sym = sym_find( "n" );

	rootmenu.prompt->text = _(rootmenu.prompt->text);
	rootmenu.prompt->text = sym_expand_string_value(rootmenu.prompt->text);

	menu_finalize(&rootmenu);
	for_all_symbols(i, sym) {
		if (sym_check_deps(sym))
			zconfnerrs++;
        }
	if (zconfnerrs)
		exit(1);
	sym_set_change_count(1);
}

static const char *zconf_tokenname(int token)
{
	switch (token) {
	case T_MENU:		return "menu";
	case T_ENDMENU:		return "endmenu";
	case T_CHOICE:		return "choice";
	case T_ENDCHOICE:	return "endchoice";
	case T_IF:		return "if";
	case T_ENDIF:		return "endif";
	case T_DEPENDS:		return "depends";
	case T_VISIBLE:		return "visible";
  case T_SHAVEAPP: return "shaveapp";
	}
	return "<token>";
}

static bool zconf_endtoken(const struct kconf_id *id, int starttoken, int endtoken)
{
	if (id->token != endtoken) {
		zconf_error("unexpected '%s' within %s block",
			kconf_id_strings + id->name, zconf_tokenname(starttoken));
		zconfnerrs++;
		return false;
	}
	if (current_menu->file != current_file) {
		zconf_error("'%s' in different file than '%s'",
			kconf_id_strings + id->name, zconf_tokenname(starttoken));
		fprintf(stderr, "%s:%d: location of the '%s'\n",
			current_menu->file->name, current_menu->lineno,
			zconf_tokenname(starttoken));
		zconfnerrs++;
		return false;
	}
	return true;
}

static void zconfprint(const char *err, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: ", zconf_curname(), zconf_lineno());
	va_start(ap, err);
	vfprintf(stderr, err, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

static void zconf_error(const char *err, ...)
{
	va_list ap;

	zconfnerrs++;
	fprintf(stderr, "%s:%d: ", zconf_curname(), zconf_lineno());
	va_start(ap, err);
	vfprintf(stderr, err, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

static void zconferror(const char *err)
{
	fprintf(stderr, "%s:%d: %s\n", zconf_curname(), zconf_lineno() + 1, err);
}

static void print_quoted_string(FILE *out, const char *str)
{
	const char *p;
	int len;

	putc('"', out);
	while ((p = strchr(str, '"'))) {
		len = p - str;
		if (len)
			fprintf(out, "%.*s", len, str);
		fputs("\\\"", out);
		str = p + 1;
	}
	fputs(str, out);
	putc('"', out);
}

static void print_symbol(FILE *out, struct menu *menu)
{
	struct symbol *sym = menu->sym;
	struct property *prop;

	if (sym_is_choice(sym))
		fprintf(out, "\nchoice\n");
	else
		fprintf(out, "\nconfig %s\n", sym->name);
	switch (sym->type) {
	case S_BOOLEAN:
		fputs("  boolean\n", out);
		break;
	case S_TRISTATE:
		fputs("  tristate\n", out);
		break;
	case S_STRING:
		fputs("  string\n", out);
		break;
	case S_INT:
		fputs("  integer\n", out);
		break;
	case S_HEX:
		fputs("  hex\n", out);
		break;
  case S_SHAVEAPP:
    fputs("  shaveapp\n", out);
    break;
	default:
		fputs("  ???\n", out);
		break;
	}
	for (prop = sym->prop; prop; prop = prop->next) {
		if (prop->menu != menu)
			continue;
		switch (prop->type) {
		case P_PROMPT:
			fputs("  prompt ", out);
			print_quoted_string(out, prop->text);
			if (!expr_is_yes(prop->visible.expr)) {
				fputs(" if ", out);
				expr_fprint(prop->visible.expr, out);
			}
			fputc('\n', out);
			break;
		case P_DEFAULT:
			fputs( "  default ", out);
			expr_fprint(prop->expr, out);
			if (!expr_is_yes(prop->visible.expr)) {
				fputs(" if ", out);
				expr_fprint(prop->visible.expr, out);
			}
			fputc('\n', out);
			break;
		case P_CHOICE:
			fputs("  #choice value\n", out);
			break;
		case P_SELECT:
			fputs( "  select ", out);
			expr_fprint(prop->expr, out);
			fputc('\n', out);
			break;
		case P_RANGE:
			fputs( "  range ", out);
			expr_fprint(prop->expr, out);
			fputc('\n', out);
			break;
		case P_MENU:
			fputs( "  menu ", out);
			print_quoted_string(out, prop->text);
			fputc('\n', out);
			break;
		default:
			fprintf(out, "  unknown prop %d!\n", prop->type);
			break;
		}
	}
	if (menu->help) {
		int len = strlen(menu->help);
		while (menu->help[--len] == '\n')
			menu->help[len] = 0;
		fprintf(out, "  help\n%s\n", menu->help);
	}
}

void zconfdump(FILE *out)
{
	struct property *prop;
	struct symbol *sym;
	struct menu *menu;

	menu = rootmenu.list;
	while (menu) {
		if ((sym = menu->sym))
			print_symbol(out, menu);
		else if ((prop = menu->prompt)) {
			switch (prop->type) {
			case P_COMMENT:
				fputs("\ncomment ", out);
				print_quoted_string(out, prop->text);
				fputs("\n", out);
				break;
			case P_MENU:
				fputs("\nmenu ", out);
				print_quoted_string(out, prop->text);
				fputs("\n", out);
				break;
			default:
				;
			}
			if (!expr_is_yes(prop->visible.expr)) {
				fputs("  depends ", out);
				expr_fprint(prop->visible.expr, out);
				fputc('\n', out);
			}
		}

		if (menu->list)
			menu = menu->list;
		else if (menu->next)
			menu = menu->next;
		else while ((menu = menu->parent)) {
			if (menu->prompt && menu->prompt->type == P_MENU)
				fputs("\nendmenu\n", out);
			if (menu->next) {
				menu = menu->next;
				break;
			}
		}
	}
}

#include "lconf.c"
#include "util.c"
#include "confdata.c"
#include "expr.c"
#include "symbol.c"
#include "menu.c"
