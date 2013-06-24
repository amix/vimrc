/*
*   $Id: ruby.c 571 2007-06-24 23:32:14Z elliotth $
*
*   Copyright (c) 2000-2001, Thaddeus Covert <sahuagin@mediaone.net>
*   Copyright (c) 2002 Matthias Veit <matthias_veit@yahoo.de>
*   Copyright (c) 2004 Elliott Hughes <enh@acm.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Ruby language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DECLARATIONS
*/
typedef enum {
	K_UNDEFINED = -1, K_CLASS, K_METHOD, K_MODULE, K_SINGLETON
} rubyKind;

/*
*   DATA DEFINITIONS
*/
static kindOption RubyKinds [] = {
	{ TRUE, 'c', "class",  "classes" },
	{ TRUE, 'f', "method", "methods" },
	{ TRUE, 'm', "module", "modules" },
	{ TRUE, 'F', "singleton method", "singleton methods" }
};

static stringList* nesting = 0;

/*
*   FUNCTION DEFINITIONS
*/

/*
* Returns a string describing the scope in 'list'.
* We record the current scope as a list of entered scopes.
* Scopes corresponding to 'if' statements and the like are
* represented by empty strings. Scopes corresponding to
* modules and classes are represented by the name of the
* module or class.
*/
static vString* stringListToScope (const stringList* list)
{
	unsigned int i;
	unsigned int chunks_output = 0;
	vString* result = vStringNew ();
	const unsigned int max = stringListCount (list);
	for (i = 0; i < max; ++i)
	{
	    vString* chunk = stringListItem (list, i);
	    if (vStringLength (chunk) > 0)
	    {
	        vStringCatS (result, (chunks_output++ > 0) ? "." : "");
	        vStringCatS (result, vStringValue (chunk));
	    }
	}
	return result;
}

/*
* Attempts to advance 's' past 'literal'.
* Returns TRUE if it did, FALSE (and leaves 's' where
* it was) otherwise.
*/
static boolean canMatch (const unsigned char** s, const char* literal)
{
	const int literal_length = strlen (literal);
	const unsigned char next_char = *(*s + literal_length);
	if (strncmp ((const char*) *s, literal, literal_length) != 0)
	{
	    return FALSE;
	}
	/* Additionally check that we're at the end of a token. */
	if ( ! (next_char == 0 || isspace (next_char) || next_char == '('))
	{
	    return FALSE;
	}
	*s += literal_length;
	return TRUE;
}

/*
* Attempts to advance 'cp' past a Ruby operator method name. Returns
* TRUE if successful (and copies the name into 'name'), FALSE otherwise.
*/
static boolean parseRubyOperator (vString* name, const unsigned char** cp)
{
	static const char* RUBY_OPERATORS[] = {
	    "[]", "[]=",
	    "**",
	    "!", "~", "+@", "-@",
	    "*", "/", "%",
	    "+", "-",
	    ">>", "<<",
	    "&",
	    "^", "|",
	    "<=", "<", ">", ">=",
	    "<=>", "==", "===", "!=", "=~", "!~",
	    "`",
	    0
	};
	int i;
	for (i = 0; RUBY_OPERATORS[i] != 0; ++i)
	{
	    if (canMatch (cp, RUBY_OPERATORS[i]))
	    {
	        vStringCatS (name, RUBY_OPERATORS[i]);
	        return TRUE;
	    }
	}
	return FALSE;
}

/*
* Emits a tag for the given 'name' of kind 'kind' at the current nesting.
*/
static void emitRubyTag (vString* name, rubyKind kind)
{
	tagEntryInfo tag;
	vString* scope;

	vStringTerminate (name);
	scope = stringListToScope (nesting);

	initTagEntry (&tag, vStringValue (name));
	if (vStringLength (scope) > 0) {
	    tag.extensionFields.scope [0] = "class";
	    tag.extensionFields.scope [1] = vStringValue (scope);
	}
	tag.kindName = RubyKinds [kind].name;
	tag.kind = RubyKinds [kind].letter;
	makeTagEntry (&tag);

	stringListAdd (nesting, vStringNewCopy (name));

	vStringClear (name);
	vStringDelete (scope);
}

/* Tests whether 'ch' is a character in 'list'. */
static boolean charIsIn (char ch, const char* list)
{
	return (strchr (list, ch) != 0);
}

/* Advances 'cp' over leading whitespace. */
static void skipWhitespace (const unsigned char** cp)
{
	while (isspace (**cp))
	{
	    ++*cp;
	}
}

/*
* Copies the characters forming an identifier from *cp into
* name, leaving *cp pointing to the character after the identifier.
*/
static rubyKind parseIdentifier (
		const unsigned char** cp, vString* name, rubyKind kind)
{
	/* Method names are slightly different to class and variable names.
	 * A method name may optionally end with a question mark, exclamation
	 * point or equals sign. These are all part of the name.
	 * A method name may also contain a period if it's a singleton method.
	 */
	const char* also_ok = (kind == K_METHOD) ? "_.?!=" : "_";

	skipWhitespace (cp);

	/* Check for an anonymous (singleton) class such as "class << HTTP". */
	if (kind == K_CLASS && **cp == '<' && *(*cp + 1) == '<')
	{
		return K_UNDEFINED;
	}

	/* Check for operators such as "def []=(key, val)". */
	if (kind == K_METHOD || kind == K_SINGLETON)
	{
		if (parseRubyOperator (name, cp))
		{
			return kind;
		}
	}

	/* Copy the identifier into 'name'. */
	while (**cp != 0 && (isalnum (**cp) || charIsIn (**cp, also_ok)))
	{
		char last_char = **cp;

		vStringPut (name, last_char);
		++*cp;

		if (kind == K_METHOD)
		{
			/* Recognize singleton methods. */
			if (last_char == '.')
			{
				vStringTerminate (name);
				vStringClear (name);
				return parseIdentifier (cp, name, K_SINGLETON);
			}

			/* Recognize characters which mark the end of a method name. */
			if (charIsIn (last_char, "?!="))
			{
				break;
			}
		}
	}
	return kind;
}

static void readAndEmitTag (const unsigned char** cp, rubyKind expected_kind)
{
	if (isspace (**cp))
	{
		vString *name = vStringNew ();
		rubyKind actual_kind = parseIdentifier (cp, name, expected_kind);

		if (actual_kind == K_UNDEFINED || vStringLength (name) == 0)
		{
			/*
			* What kind of tags should we create for code like this?
			*
			*    %w(self.clfloor clfloor).each do |name|
			*        module_eval <<-"end;"
			*            def #{name}(x, y=1)
			*                q, r = x.divmod(y)
			*                q = q.to_i
			*                return q, r
			*            end
			*        end;
			*    end
			*
			* Or this?
			*
			*    class << HTTP
			*
			* For now, we don't create any.
			*/
		}
		else
		{
			emitRubyTag (name, actual_kind);
		}
		vStringDelete (name);
	}
}

static void enterUnnamedScope (void)
{
	stringListAdd (nesting, vStringNewInit (""));
}

static void findRubyTags (void)
{
	const unsigned char *line;
	boolean inMultiLineComment = FALSE;

	nesting = stringListNew ();

	/* FIXME: this whole scheme is wrong, because Ruby isn't line-based.
	* You could perfectly well write:
	*
	*  def
	*  method
	*   puts("hello")
	*  end
	*
	* if you wished, and this function would fail to recognize anything.
	*/
	while ((line = fileReadLine ()) != NULL)
	{
		const unsigned char *cp = line;

		if (canMatch (&cp, "=begin"))
		{
			inMultiLineComment = TRUE;
			continue;
		}
		if (canMatch (&cp, "=end"))
		{
			inMultiLineComment = FALSE;
			continue;
		}

		skipWhitespace (&cp);

		/* Avoid mistakenly starting a scope for modifiers such as
		*
		*   return if <exp>
		*
		* FIXME: this is fooled by code such as
		*
		*   result = if <exp>
		*               <a>
		*            else
		*               <b>
		*            end
		*
		* FIXME: we're also fooled if someone does something heinous such as
		*
		*   puts("hello") \
		*       unless <exp>
		*/
		if (canMatch (&cp, "case") || canMatch (&cp, "for") ||
			canMatch (&cp, "if") || canMatch (&cp, "unless") ||
			canMatch (&cp, "while"))
		{
			enterUnnamedScope ();
		}

		/*
		* "module M", "class C" and "def m" should only be at the beginning
		* of a line.
		*/
		if (canMatch (&cp, "module"))
		{
			readAndEmitTag (&cp, K_MODULE);
		}
		else if (canMatch (&cp, "class"))
		{
			readAndEmitTag (&cp, K_CLASS);
		}
		else if (canMatch (&cp, "def"))
		{
			readAndEmitTag (&cp, K_METHOD);
		}

		while (*cp != '\0')
		{
			/* FIXME: we don't cope with here documents,
			* or regular expression literals, or ... you get the idea.
			* Hopefully, the restriction above that insists on seeing
			* definitions at the starts of lines should keep us out of
			* mischief.
			*/
			if (inMultiLineComment || isspace (*cp))
			{
				++cp;
			}
			else if (*cp == '#')
			{
				/* FIXME: this is wrong, but there *probably* won't be a
				* definition after an interpolated string (where # doesn't
				* mean 'comment').
				*/
				break;
			}
			else if (canMatch (&cp, "begin") || canMatch (&cp, "do"))
			{
				enterUnnamedScope ();
			}
			else if (canMatch (&cp, "end") && stringListCount (nesting) > 0)
			{
				/* Leave the most recent scope. */
				vStringDelete (stringListLast (nesting));
				stringListRemoveLast (nesting);
			}
			else if (*cp == '"')
			{
				/* Skip string literals.
				 * FIXME: should cope with escapes and interpolation.
				 */
				do {
					++cp;
				} while (*cp != 0 && *cp != '"');
			}
			else if (*cp != '\0')
			{
				do
					++cp;
				while (isalnum (*cp) || *cp == '_');
			}
		}
	}
	stringListDelete (nesting);
}

extern parserDefinition* RubyParser (void)
{
	static const char *const extensions [] = { "rb", "ruby", NULL };
	parserDefinition* def = parserNew ("Ruby");
	def->kinds      = RubyKinds;
	def->kindCount  = KIND_COUNT (RubyKinds);
	def->extensions = extensions;
	def->parser     = findRubyTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
