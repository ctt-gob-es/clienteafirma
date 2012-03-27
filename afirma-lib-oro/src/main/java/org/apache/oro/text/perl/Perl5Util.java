/*
 * $Id: Perl5Util.java,v 1.19 2003/11/07 20:16:25 dfs Exp $
 *
 * ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Apache" and "Apache Software Foundation", "Jakarta-Oro"
 *    must not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    or "Jakarta-Oro", nor may "Apache" or "Jakarta-Oro" appear in their
 *    name, without prior written permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */


package org.apache.oro.text.perl;

import org.apache.oro.text.PatternCache;
import org.apache.oro.text.PatternCacheLRU;
import org.apache.oro.text.regex.MalformedPatternException;
import org.apache.oro.text.regex.MatchResult;
import org.apache.oro.text.regex.Pattern;
import org.apache.oro.text.regex.Perl5Compiler;
import org.apache.oro.text.regex.Perl5Matcher;
import org.apache.oro.util.Cache;
import org.apache.oro.util.CacheLRU;

/**
 * This is a utility class implementing the 3 most common Perl5 operations
 * involving regular expressions:
 * <ul>
 * <li> [m]/pattern/[i][m][s][x],
 * <li> s/pattern/replacement/[g][i][m][o][s][x],
 * <li> and split().
 * </ul>
 * As with Perl, any non-alphanumeric character can be used in lieu of
 * the slashes.
 *  <p>
 * The objective of the class is to minimize the amount of code a Java
 * programmer using Jakarta-ORO
 * has to write to achieve the same results as Perl by
 * transparently handling regular expression compilation, caching, and
 * matching.  A second objective is to use the same Perl pattern matching
 * syntax to ease the task of Perl programmers transitioning to Java
 * (this also reduces the number of parameters to a method).
 * All the state affecting methods are synchronized to avoid
 * the maintenance of explicit locks in multithreaded programs.  This
 * philosophy differs from the
 * {@link org.apache.oro.text.regex} package, where
 * you are expected to either maintain explicit locks, or more preferably
 * create separate compiler and matcher instances for each thread.
 * <p>
 * To use this class, first create an instance using the default constructor
 * or initialize the instance with a PatternCache of your choosing using
 * the alternate constructor.  The default cache used by Perl5Util is a
 * PatternCacheLRU of capacity GenericPatternCache.DEFAULT_CAPACITY.  You may
 * want to create a cache with a different capacity, a different
 * cache replacement policy, or even devise your own PatternCache
 * implementation.  The PatternCacheLRU is probably the best general purpose
 * pattern cache, but your specific application may be better served by
 * a different cache replacement policy.  You should remember that you can
 * front-load a cache with all the patterns you will be using before
 * initializing a Perl5Util instance, or you can just let Perl5Util
 * fill the cache as you use it.
 * <p>
 * You might use the class as follows:
 * <pre>
 * Perl5Util util = new Perl5Util();
 * String line;
 * DataInputStream input;
 * PrintStream output;
 *
 * // Initialization of input and output omitted
 * while((line = input.readLine()) != null) {
 *     // First find the line with the string we want to substitute because
 *     // it is cheaper than blindly substituting each line.
 *     if(util.match("/HREF=\"description1.html\"/")) {
 *        line = util.substitute("s/description1\\.html/about1.html/", line);
 *     }
 *    output.println(line);
 * }
 * </pre>
 * <p>
 * A couple of things to remember when using this class are that the
 * {@link #match match()} methods have the same meaning as
 * {@link org.apache.oro.text.regex.Perl5Matcher#contains
 *  Perl5Matcher.contains()}
 * and <code>=~ m/pattern/</code> in Perl.  The methods are named match
 * to more closely associate them with Perl and to differentiate them
 * from {@link org.apache.oro.text.regex.Perl5Matcher#matches
 * Perl5Matcher.matches()}.
 * A further thing to keep in mind is that the
 * {@link MalformedPerl5PatternException} class is derived from
 * RuntimeException which means you DON'T have to catch it.  The reasoning
 * behind this is that you will detect your regular expression mistakes
 * as you write and debug your program when a MalformedPerl5PatternException
 * is thrown during a test run.  However, we STRONGLY recommend that you
 * ALWAYS catch MalformedPerl5PatternException whenever you deal with a
 * DYNAMICALLY created pattern.  Relying on a fatal
 * MalformedPerl5PatternException being thrown to detect errors while
 * debugging is only useful for dealing with static patterns, that is, actual
 * pregenerated strings present in your program.  Patterns created from user
 * input or some other dynamic method CANNOT be relied upon to be correct
 * and MUST be handled by catching MalformedPerl5PatternException for your
 * programs to be robust.
 * <p>
 * Finally, as a convenience Perl5Util implements
 * the {@link org.apache.oro.text.regex.MatchResult MatchResult} interface.
 * The methods are merely wrappers which call the corresponding method of
 * the last {@link org.apache.oro.text.regex.MatchResult MatchResult}
 * found (which can be accessed with {@link #getMatch()}) by a match or
 * substitution (or even a split, but this isn't particularly useful).
 * At the moment, the
 * {@link org.apache.oro.text.regex.MatchResult MatchResult} returned
 * by {@link #getMatch()} is not stored in a thread-local variable.  Therefore
 * concurrent calls to {@link #getMatch()} will produce unpredictable
 * results.  So if your concurrent program requires the match results,
 * you must protect the matching and the result retrieval in a critical
 * section.  If you do not need match results, you don't need to do anything
 * special.  If you feel the J2SE implementation of {@link #getMatch()}
 * should use a thread-local variable and obviate the need for a critical
 * section, please express your views on the oro-dev mailing list.
 *
 * @version @version@
 * @since 1.0
 * @see MalformedPerl5PatternException
 * @see org.apache.oro.text.PatternCache
 * @see org.apache.oro.text.PatternCacheLRU
 * @see org.apache.oro.text.regex.MatchResult
 */
public final class Perl5Util implements MatchResult {
  /** The regular expression to use to parse match expression. */
  private static final String __matchExpression = "m?(\\W)(.*)\\1([imsx]*)"; //$NON-NLS-1$

  /** The pattern cache to compile and store patterns */
  private final PatternCache __patternCache;
  /** The hashtable to cache higher-level expressions */
  private final Cache __expressionCache;
  /** The pattern matcher to perform matching operations. */
  private final Perl5Matcher __matcher;
  /** The compiled match expression parsing regular expression. */
  private Pattern __matchPattern;
  /** The last match from a successful call to a matching method. */
  private MatchResult __lastMatch;

  /**
   * A secondary constructor for Perl5Util.  It initializes the Perl5Matcher
   * used by the class to perform matching operations, but requires the
   * programmer to provide a PatternCache instance for the class
   * to use to compile and store regular expressions.  You would want to
   * use this constructor if you want to change the capacity or policy
   * of the cache used.  Example uses might be:
   * <pre>
   * // We know we're going to use close to 50 expressions a whole lot, so
   * // we create a cache of the proper size.
   * util = new Perl5Util(new PatternCacheLRU(50));
   * </pre>
   * or
   * <pre>
   * // We're only going to use a few expressions and know that second-chance
   * // fifo is best suited to the order in which we are using the patterns.
   * util = new Perl5Util(new PatternCacheFIFO2(10));
   * </pre>
   */
  public Perl5Util(final PatternCache cache) {
    this.__matcher      = new Perl5Matcher();
    this.__patternCache = cache;
    this.__expressionCache = new CacheLRU(cache.capacity());
    __compilePatterns();
  }

  /**
   * Default constructor for Perl5Util.  This initializes the Perl5Matcher
   * used by the class to perform matching operations and creates a
   * default PatternCacheLRU instance to use to compile and cache regular
   * expressions.  The size of this cache is
   * GenericPatternCache.DEFAULT_CAPACITY.
   */
  public Perl5Util() {
    this(new PatternCacheLRU());
  }

  /**
   * Compiles the patterns (currently only the match expression) used to
   * parse Perl5 expressions.  Right now it initializes __matchPattern.
   */
  private void __compilePatterns() {
    final Perl5Compiler compiler = new Perl5Compiler();

    try {
      this.__matchPattern =
	compiler.compile(__matchExpression, Perl5Compiler.SINGLELINE_MASK);
    } catch(final MalformedPatternException e) {
      // This should only happen during debugging.
      //e.printStackTrace();
      throw new RuntimeException(e.getMessage());
    }
  }

  /**
   * Parses a match expression and returns a compiled pattern.
   * First checks the expression cache and if the pattern is not found,
   * then parses the expression and fetches a compiled pattern from the
   * pattern cache.  Otherwise, just uses the pattern found in the
   * expression cache.  __matchPattern is used to parse the expression.
   * <p>
   * @param pattern  The Perl5 match expression to parse.
   * @exception MalformedPerl5PatternException If there is an error parsing
   *            the expression.
   */
  private Pattern __parseMatchExpression(final String pattern)
       throws MalformedPerl5PatternException
  {
    int index, compileOptions;
    String options, regex;
    MatchResult result;
    Object obj;
    Pattern ret;

    obj = this.__expressionCache.getElement(pattern);

    // Must catch ClassCastException because someone might incorrectly
    // pass an s/// expression.  try block is cheaper than checking
    // instanceof
    try {
      if(obj != null) {
		return (Pattern)obj;
	}
    } catch(final ClassCastException e) {
      // Fall through and parse expression
    }

    if(!this.__matcher.matches(pattern, this.__matchPattern)) {
		throw new
		MalformedPerl5PatternException("Invalid expression: " + //$NON-NLS-1$
					       pattern);
	}

    result = this.__matcher.getMatch();

    regex = result.group(2);
    compileOptions = Perl5Compiler.DEFAULT_MASK;

    options = result.group(3);

    if(options != null) {
      index = options.length();

      while(index-- > 0) {
	switch(options.charAt(index)) {
	case 'i' :
	  compileOptions |= Perl5Compiler.CASE_INSENSITIVE_MASK;
	  break;
	case 'm' : compileOptions |= Perl5Compiler.MULTILINE_MASK; break;
	case 's' : compileOptions |= Perl5Compiler.SINGLELINE_MASK; break;
	case 'x' : compileOptions |= Perl5Compiler.EXTENDED_MASK; break;
	default  :
	  throw new
	    MalformedPerl5PatternException("Invalid options: " + options); //$NON-NLS-1$
	}
      }
    }

    ret = this.__patternCache.getPattern(regex, compileOptions);
    this.__expressionCache.addElement(pattern, ret);

    return ret;
  }

  /**
   * Searches for the first pattern match somewhere in a character array
   * taking a pattern specified in Perl5 native format:
   * <blockquote><pre>
   * [m]/pattern/[i][m][s][x]
   * </pre></blockquote>
   * The <code>m</code> prefix is optional and the meaning of the optional
   * trailing options are:
   * <dl compact>
   * <dt> i <dd> case insensitive match
   * <dt> m <dd> treat the input as consisting of multiple lines
   * <dt> s <dd> treat the input as consisting of a single line
   * <dt> x <dd> enable extended expression syntax incorporating whitespace
   *             and comments
   * </dl>
   * As with Perl, any non-alphanumeric character can be used in lieu of
   * the slashes.
   * <p>
   * If the input contains the pattern, the org.apache.oro.text.regex.MatchResult
   * can be obtained by calling {@link #getMatch()}.
   * However, Perl5Util implements the MatchResult interface as a wrapper
   * around the last MatchResult found, so you can call its methods to
   * access match information.
   * <p>
   * @param pattern  The pattern to search for.
   * @param input    The char[] input to search.
   * @return True if the input contains the pattern, false otherwise.
   * @exception MalformedPerl5PatternException  If there is an error in
   *            the pattern.  You are not forced to catch this exception
   *            because it is derived from RuntimeException.
   */
  public synchronized boolean match(final String pattern, final char[] input)
       throws MalformedPerl5PatternException
  {
    boolean result;
    __parseMatchExpression(pattern);

    result = this.__matcher.contains(input, __parseMatchExpression(pattern));

    if(result) {
      this.__lastMatch        = this.__matcher.getMatch();
    }

    return result;
  }


  /**
   * Searches for the first pattern match in a String taking
   * a pattern specified in Perl5 native format:
   * <blockquote><pre>
   * [m]/pattern/[i][m][s][x]
   * </pre></blockquote>
   * The <code>m</code> prefix is optional and the meaning of the optional
   * trailing options are:
   * <dl compact>
   * <dt> i <dd> case insensitive match
   * <dt> m <dd> treat the input as consisting of multiple lines
   * <dt> s <dd> treat the input as consisting of a single line
   * <dt> x <dd> enable extended expression syntax incorporating whitespace
   *             and comments
   * </dl>
   * As with Perl, any non-alphanumeric character can be used in lieu of
   * the slashes.
   * <p>
   * If the input contains the pattern, the
   * {@link org.apache.oro.text.regex.MatchResult MatchResult}
   * can be obtained by calling {@link #getMatch()}.
   * However, Perl5Util implements the MatchResult interface as a wrapper
   * around the last MatchResult found, so you can call its methods to
   * access match information.
   * <p>
   * @param pattern  The pattern to search for.
   * @param input    The String input to search.
   * @return True if the input contains the pattern, false otherwise.
   * @exception MalformedPerl5PatternException  If there is an error in
   *            the pattern.  You are not forced to catch this exception
   *            because it is derived from RuntimeException.
   */
  public synchronized boolean match(final String pattern, final String input)
       throws MalformedPerl5PatternException
  {
    return match(pattern, input.toCharArray());
  }


  /**
   * Returns the last match found by a call to a match(), substitute(), or
   * split() method.  This method is only intended for use to retrieve a match
   * found by the last match found by a match() method.  This method should
   * be used when you want to save MatchResult instances.  Otherwise, for
   * simply accessing match information, it is more convenient to use the
   * Perl5Util methods implementing the MatchResult interface.
   * <p>
   * @return The org.apache.oro.text.regex.MatchResult instance containing the
   *         last match found.
   */
  public synchronized MatchResult getMatch() {
    return this.__lastMatch;
  }

  //
  // MatchResult interface methods.
  //

  /**
   * Returns the length of the last match found.
   * <p>
   * @return The length of the last match found.
   */
  public synchronized int length() {
    return this.__lastMatch.length();
  }

  /**
   * @return The number of groups contained in the last match found.
   *         This number includes the 0th group.  In other words, the
   *         result refers to the number of parenthesized subgroups plus
   *         the entire match itself.
   */
  public synchronized int groups() {
    return this.__lastMatch.groups();
  }


  /**
   * Returns the contents of the parenthesized subgroups of the last match
   * found according to the behavior dictated by the MatchResult interface.
   * <p>
   * @param group The pattern subgroup to return.
   * @return A string containing the indicated pattern subgroup.  Group
   *         0 always refers to the entire match.  If a group was never
   *         matched, it returns null.  This is not to be confused with
   *         a group matching the null string, which will return a String
   *         of length 0.
   */
  public synchronized String group(final int group) {
    return this.__lastMatch.group(group);
  }

  /**
   * Returns the begin offset of the subgroup of the last match found
   * relative the beginning of the match.
   * <p>
   * @param group The pattern subgroup.
   * @return The offset into group 0 of the first token in the indicated
   *         pattern subgroup.  If a group was never matched or does
   *         not exist, returns -1.  Be aware that a group that matches
   *         the null string at the end of a match will have an offset
   *         equal to the length of the string, so you shouldn't blindly
   *         use the offset to index an array or String.
   */
  public synchronized int begin(final int group) {
    return this.__lastMatch.begin(group);
  }


  /**
   * Returns the end offset of the subgroup of the last match found
   * relative the beginning of the match.
   * <p>
   * @param group The pattern subgroup.
   * @return Returns one plus the offset into group 0 of the last token in
   *         the indicated pattern subgroup.  If a group was never matched
   *         or does not exist, returns -1.  A group matching the null
   *         string will return its start offset.
   */
  public synchronized int end(final int group) {
    return this.__lastMatch.end(group);
  }


  /**
   * Returns an offset marking the beginning of the last pattern match
   * found relative to the beginning of the input from which the match
   * was extracted.
   * <p>
   * @param group The pattern subgroup.
   * @return The offset of the first token in the indicated
   *         pattern subgroup.  If a group was never matched or does
   *         not exist, returns -1.
   */
  public synchronized int beginOffset(final int group) {
    return this.__lastMatch.beginOffset(group);
  }

  /**
   * Returns an offset marking the end of the last pattern match found
   * relative to the beginning of the input from which the match was
   * extracted.
   * <p>
   * @param group The pattern subgroup.
   * @return Returns one plus the offset of the last token in
   *         the indicated pattern subgroup.  If a group was never matched
   *         or does not exist, returns -1.  A group matching the null
   *         string will return its start offset.
   */
  public synchronized int endOffset(final int group) {
    return this.__lastMatch.endOffset(group);
  }

  /**
   * Returns the same as group(0).
   * <p>
   * @return A string containing the entire match.
   */
  @Override
  public synchronized String toString() {
    if(this.__lastMatch == null) {
		return null;
	}
    return this.__lastMatch.toString();
  }

}

