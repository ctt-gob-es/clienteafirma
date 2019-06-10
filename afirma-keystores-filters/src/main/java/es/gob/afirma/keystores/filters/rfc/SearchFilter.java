/*
 * Copyright 1999 Sun Microsystems, Inc.  All Rights Reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * CA 95054 USA or visit www.sun.com if you need additional information or
 * have any questions.
 */
package es.gob.afirma.keystores.filters.rfc;

import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.OperationNotSupportedException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttributes;
import javax.naming.directory.InvalidSearchFilterException;

/** A class for parsing LDAP search filters (defined in RFC 1960, 2254).
  * @author Jon Ruiz.
  * @author Rosanna Lee. */
final class SearchFilter implements AttrFilter {

    interface StringFilter extends AttrFilter {
        public void parse() throws InvalidSearchFilterException;
    }

    // %%% "filter" and "pos" are not declared "private" due to bug 4064984.
    String                      filter;
    int                         pos;
    private final StringFilter        rootFilter;

    protected static final char         BEGIN_FILTER_TOKEN = '(';
    protected static final char         END_FILTER_TOKEN = ')';
    protected static final char         AND_TOKEN = '&';
    protected static final char         OR_TOKEN = '|';
    protected static final char         NOT_TOKEN = '!';
    protected static final char         EQUAL_TOKEN = '=';
    protected static final char         APPROX_TOKEN = '~';
    protected static final char         LESS_TOKEN = '<';
    protected static final char         GREATER_TOKEN = '>';
    protected static final char         EXTEND_TOKEN = ':';
    protected static final char         WILDCARD_TOKEN = '*';

    SearchFilter(final String filter) throws InvalidSearchFilterException {
        this.filter = filter;
        this.pos = 0;
        normalizeFilter();
        this.rootFilter = createNextFilter();
    }

    // Returns true if targetAttrs passes the filter
    @Override
	public boolean check(final Attributes targetAttrs) throws NamingException {
        if (targetAttrs == null) {
			return false;
		}

        return this.rootFilter.check(targetAttrs);
    }

    /*
     * Utility routines used by member classes
     */

    // does some pre-processing on the string to make it look exactly lik
    // what the parser expects. This only needs to be called once.
    protected void normalizeFilter() {
        skipWhiteSpace(); // get rid of any leading whitespaces

        // Sometimes, search filters don't have "(" and ")" - add them
        if(getCurrentChar() != BEGIN_FILTER_TOKEN) {
            this.filter = BEGIN_FILTER_TOKEN + this.filter + END_FILTER_TOKEN;
        }
        // this would be a good place to strip whitespace if desired
    }

    void skipWhiteSpace() {
        while (Character.isWhitespace(getCurrentChar())) {
            consumeChar();
        }
    }

    protected StringFilter createNextFilter()
        throws InvalidSearchFilterException {
        StringFilter stringFilter;

        skipWhiteSpace();

        try {
            // make sure every filter starts with "("
            if(getCurrentChar() != BEGIN_FILTER_TOKEN) {
                throw new InvalidSearchFilterException("expected \"" + //$NON-NLS-1$
                                                       BEGIN_FILTER_TOKEN +
                                                       "\" at position " + //$NON-NLS-1$
                                                       this.pos);
            }

            // skip past the "("
            consumeChar();

            skipWhiteSpace();

            // use the next character to determine the type of filter
            switch(getCurrentChar()) {
            case AND_TOKEN:
                stringFilter = new CompoundFilter(true);
                stringFilter.parse();
                break;
            case OR_TOKEN:
                stringFilter = new CompoundFilter(false);
                stringFilter.parse();
                break;
            case NOT_TOKEN:
                stringFilter = new NotFilter();
                stringFilter.parse();
                break;
            default:
                stringFilter = new AtomicFilter();
                stringFilter.parse();
                break;
            }

            skipWhiteSpace();

            // make sure every filter ends with ")"
            if(getCurrentChar() != END_FILTER_TOKEN) {
                throw new InvalidSearchFilterException("expected \"" + //$NON-NLS-1$
                                                       END_FILTER_TOKEN +
                                                       "\" at position " + //$NON-NLS-1$
                                                       this.pos);
            }

            // skip past the ")"
            consumeChar();
        }
        catch (final InvalidSearchFilterException e) {
            throw e; // just rethrow these

        // catch all - any uncaught exception while parsing will end up here
        }
        catch  (final Exception e) {
            throw new InvalidSearchFilterException("Unable to parse " + //$NON-NLS-1$
                "character " + this.pos + " in \""+ //$NON-NLS-1$ //$NON-NLS-2$
                    this.filter + "\": " + e); //$NON-NLS-1$
        }

        return stringFilter;
    }

    protected char getCurrentChar() {
        return this.filter.charAt(this.pos);
    }

    protected char relCharAt(final int i) {
        return this.filter.charAt(this.pos + i);
    }

    protected void consumeChar() {
        this.pos++;
    }

    protected void consumeChars(final int i) {
        this.pos += i;
    }

    protected int relIndexOf(final int ch) {
        return this.filter.indexOf(ch, this.pos) - this.pos;
    }

    protected String relSubstring(final int beginIndex, final int endIndex){
        return this.filter.substring(beginIndex+this.pos, endIndex+this.pos);
    }


   /**
     * A class for dealing with compound filters ("and" &amp; "or" filters).
     */
    final class CompoundFilter implements StringFilter {
        private final Vector<StringFilter>  subFilters;
        private final boolean polarity;

        CompoundFilter(final boolean polarity) {
            this.subFilters = new Vector<>();
            this.polarity = polarity;
        }

        @Override
		public void parse() throws InvalidSearchFilterException {
            consumeChar(); // consume the "&"
            while(getCurrentChar() != END_FILTER_TOKEN) {
                this.subFilters.addElement(createNextFilter());
                skipWhiteSpace();
            }
        }

        @Override
		public boolean check(final Attributes targetAttrs) throws NamingException {
            for(int i = 0; i<this.subFilters.size(); i++) {
                final StringFilter fltr = this.subFilters.elementAt(i);
                if(fltr.check(targetAttrs) != this.polarity) {
                    return !this.polarity;
                }
            }
            return this.polarity;
        }
    } /* CompoundFilter */

    /** A class for dealing with NOT filters. */
    final class NotFilter implements StringFilter {
        private StringFilter fltr;

        @Override
		public void parse() throws InvalidSearchFilterException {
            consumeChar(); // consume the "!"
            this.fltr = createNextFilter();
        }

        @Override
		public boolean check(final Attributes targetAttrs) throws NamingException {
            return !this.fltr.check(targetAttrs);
        }
    } /* notFilter */

    // note: declared here since member classes can't have static variables
    static final int EQUAL_MATCH = 1;
    static final int APPROX_MATCH = 2;
    static final int GREATER_MATCH = 3;
    static final int LESS_MATCH = 4;

    /**
     * A class for dealing wtih atomic filters
     */
    final class AtomicFilter implements StringFilter {
        private String attrID;
        private String value;
        private int    matchType;

        @Override
		public void parse() throws InvalidSearchFilterException {

            skipWhiteSpace();

            try {
                // find the end
                final int endPos = relIndexOf(END_FILTER_TOKEN);

                //determine the match type
                final int i = relIndexOf(EQUAL_TOKEN);
                final int qualifier = relCharAt(i-1);
                switch(qualifier) {
                case APPROX_TOKEN:
                    this.matchType = APPROX_MATCH;
                    this.attrID = relSubstring(0, i-1);
                    this.value = relSubstring(i+1, endPos);
                    break;

                case GREATER_TOKEN:
                    this.matchType = GREATER_MATCH;
                    this.attrID = relSubstring(0, i-1);
                    this.value = relSubstring(i+1, endPos);
                    break;

                case LESS_TOKEN:
                    this.matchType = LESS_MATCH;
                    this.attrID = relSubstring(0, i-1);
                    this.value = relSubstring(i+1, endPos);
                    break;

                case EXTEND_TOKEN:
                    throw new OperationNotSupportedException("Extensible match not supported"); //$NON-NLS-1$

                default:
                    this.matchType = EQUAL_MATCH;
                    this.attrID = relSubstring(0,i);
                    this.value = relSubstring(i+1, endPos);
                    break;
                }

                this.attrID = this.attrID.trim();
                this.value = this.value.trim();

                //update our position
                consumeChars(endPos);

            }
            catch (final Exception e) {
                final InvalidSearchFilterException sfe =
                    new InvalidSearchFilterException("Unable to parse " + //$NON-NLS-1$
                    "character " + SearchFilter.this.pos + " in \""+ //$NON-NLS-1$ //$NON-NLS-2$
                    SearchFilter.this.filter + "\""); //$NON-NLS-1$
                sfe.setRootCause(e);
                throw sfe;
            }
        }

        @Override
		public boolean check(final Attributes targetAttrs) {
            Enumeration<?> candidates;

            try {
                final Attribute attr = targetAttrs.get(this.attrID);
                if(attr == null) {
                    return false;
                }
                candidates = attr.getAll();
            }
            catch (final NamingException ne) {
                return false;
            }

            while(candidates.hasMoreElements()) {
                final String val = candidates.nextElement().toString();
                switch(this.matchType) {
	                case APPROX_MATCH:
	                case EQUAL_MATCH:
	                    if(substringMatch(this.value, val)) {
	                        return true;
	                    }
	                    break;
	                case GREATER_MATCH:
	                    if(val.compareTo(this.value) >= 0) {
	                        return true;
	                    }
	                    break;
	                case LESS_MATCH:
	                    if(val.compareTo(this.value) <= 0) {
	                        return true;
	                    }
	                    break;
	                default:
                }
            }
            return false;
        }

        // used for substring comparisons (where proto has "*" wildcards
        private boolean substringMatch(final String proto, final String val) {
            // simple case 1: "*" means attribute presence is being tested
            if(proto.equals(Character.valueOf(WILDCARD_TOKEN).toString())) {
                return true;
            }

            // simple case 2: if there are no wildcards, call String.equals()
            if(proto.indexOf(WILDCARD_TOKEN) == -1) {
                return proto.equalsIgnoreCase(val);
            }

            // do the work: make sure all the substrings are present
            int currentPos = 0;
            final StringTokenizer subStrs = new StringTokenizer(proto, "*", false); //$NON-NLS-1$

            // do we need to begin with the first token?
            if(proto.charAt(0) != WILDCARD_TOKEN &&
               !val.toString().toLowerCase().startsWith(
                      subStrs.nextToken().toLowerCase())) {
                return false;
            }


            while(subStrs.hasMoreTokens()) {
                final String currentStr = subStrs.nextToken();
                currentPos = val.toLowerCase().indexOf(
                       currentStr.toLowerCase(), currentPos);
                if(currentPos == -1) {
                    return false;
                }
                currentPos += currentStr.length();
            }

            // do we need to end with the last token?
            if(proto.charAt(proto.length() - 1) != WILDCARD_TOKEN &&
               currentPos != val.length() ) {
                return false;
            }

            return true;
        }

    } /* AtomicFilter */

    // ----- static methods for producing string filters given attribute set
    // ----- or object array


    /** Creates an LDAP filter as a conjuction of the attributes supplied.
     * @param attrs Attributes.
     * @return Created LDAP filter.
     * @throws NamingException Is there is any error. */
    static String format(final Attributes attrs) throws NamingException {
        if (attrs == null || attrs.size() == 0) {
            return "objectClass=*"; //$NON-NLS-1$
        }

        String answer;
        answer = "(& "; //$NON-NLS-1$
        Attribute attr;
        for (final NamingEnumeration<? extends Attribute> e = attrs.getAll(); e.hasMore(); ) {
            attr = e.next();
            if (attr.size() == 0 || attr.size() == 1 && attr.get() == null) {
                // only checking presence of attribute
                answer += "(" + attr.getID() + "=" + "*)"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            } else {
                for (final NamingEnumeration<?> ve = attr.getAll();
                     ve.hasMore();
                        ) {
                    final String val = getEncodedStringRep(ve.next());
                    if (val != null) {
                        answer += "(" + attr.getID() + "=" + val + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }
                }
            }
        }

        answer += ")"; //$NON-NLS-1$
        //System.out.println("filter: " + answer);
        return answer;
    }

    // Writes the hex representation of a byte to a StringBuffer.
    private static void hexDigit(final StringBuffer buf, final byte x) {
        char c;

        c = (char) (x >> 4 & 0xf);
        if (c > 9) {
			c = (char) (c-10 + 'A');
		} else {
			c = (char)(c + '0');
		}

        buf.append(c);
        c = (char) (x & 0xf);
        if (c > 9) {
			c = (char)(c-10 + 'A');
		} else {
			c = (char)(c + '0');
		}
        buf.append(c);
    }


    /** Returns the string representation of an object (such as an attr value).
      * If obj is a byte array, encode each item as \xx, where xx is hex encoding
      * of the byte value.
      * Else, if obj is not a String, use its string representation (toString()).
      * Special characters in obj (or its string representation) are then
      * encoded appropriately according to RFC 2254:
      * <pre>
      *         *       \2a
      *         (       \28
      *         )       \29
      *         \       \5c
      *         NUL     \00
      * </pre>
      * @param obj The object.
      * @return The string representation of the object. */
    private static String getEncodedStringRep(final Object obj) {
        String str;
        if (obj == null) {
			return null;
		}

        if (obj instanceof byte[]) {
            // binary data must be encoded as \hh where hh is a hex char
            final byte[] bytes = (byte[])obj;
            final StringBuffer b1 = new StringBuffer(bytes.length*3);
            for (final byte b : bytes) {
                b1.append('\\');
                hexDigit(b1, b);
            }
            return b1.toString();
        }
        if (!(obj instanceof String)) {
            str = obj.toString();
        } else {
            str = (String)obj;
        }
        final int len = str.length();
        final StringBuffer buf = new StringBuffer(len);
        char ch;
        for (int i = 0; i < len; i++) {
            switch (ch=str.charAt(i)) {
            case '*':
                buf.append("\\2a"); //$NON-NLS-1$
                break;
            case '(':
                buf.append("\\28"); //$NON-NLS-1$
                break;
            case ')':
                buf.append("\\29"); //$NON-NLS-1$
                break;
            case '\\':
                buf.append("\\5c"); //$NON-NLS-1$
                break;
            case 0:
                buf.append("\\00"); //$NON-NLS-1$
                break;
            default:
                buf.append(ch);
            }
        }
        return buf.toString();
    }


    /** Finds the first occurrence of <code>ch</code> in <code>val</code> starting
      * from position <code>start</code>. It doesn't count if <code>ch</code>
      * has been escaped by a backslash (\).
      * @param ch Character to find.
      * @param val String to search on.
      * @param st Start position.
      * @return First occurrence of <code>ch</code> in <code>val</code> starting
      *         from <code>start</code>. */
    static int findUnescaped(final char ch, final String val, final int st) {
        final int len = val.length();
        int start = st;
        while (start < len) {
            final int where = val.indexOf(ch, start);
            // if at start of string, or not there at all, or if not escaped
            if (where == start || where == -1 || val.charAt(where-1) != '\\') {
				return where;
			}

            // start search after escaped star
            start = where + 1;
        }
        return -1;
    }

    /** Formats the expression <code>expr</code> using arguments from the array
     * <code>args</code>.
     * <code>{i}</code> specifies the <code>i</code>'th element from
     * the array <code>args</code> is to be substituted for the
     * string "<code>{i}</code>".
     * To escape '{' or '}' (or any other character), use '\'.
     * Uses getEncodedStringRep() to do encoding.
     * @param expr Expression to format.
     * @param args Arguments for the array.
     * @return Formatted expression.
     * @throws NamingException If there is any error. */
    static String format(final String expr, final Object[] args) throws NamingException {

         int param;
         int where = 0, start = 0;
         final StringBuffer answer = new StringBuffer(expr.length());

         while ((where = findUnescaped('{', expr, start)) >= 0) {
             final int pstart = where + 1; // skip '{'
             final int pend = expr.indexOf('}', pstart);

             if (pend < 0) {
                 throw new InvalidSearchFilterException("unbalanced {: " + expr); //$NON-NLS-1$
             }

             // at this point, pend should be pointing at '}'
             try {
                 param = Integer.parseInt(expr.substring(pstart, pend));
             }
             catch (final NumberFormatException e) {
                 throw new InvalidSearchFilterException(
                     "integer expected inside {}: '" + expr + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
             }

             if (param >= args.length) {
                 throw new InvalidSearchFilterException(
                     "number exceeds argument list: " + param); //$NON-NLS-1$
             }

             answer.append(expr.substring(start, where)).append(getEncodedStringRep(args[param]));
             start = pend + 1; // skip '}'
         }

         if (start < expr.length()) {
			answer.append(expr.substring(start));
		}

        return answer.toString();
    }

    /** Returns an Attributes instance containing only attributeIDs given in
     * "attributeIDs" whose values come from the given DSContext.
     * @param originals Original attributes.
     * @param attrIDs Attribute identifiers.
     * @return Attributes instance containing only given attributeIDs. */
    static Attributes selectAttributes(final Attributes originals, final String[] attrIDs) {

        if (attrIDs == null) {
			return originals;
		}

        final Attributes result = new BasicAttributes();

        for (final String attrID : attrIDs) {
            final Attribute attr = originals.get(attrID);
            if(attr != null) {
                result.put(attr);
            }
        }

        return result;
    }

}
