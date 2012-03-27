/*
jMimeMagic(TM) is a Java library for determining the content type of files or
streams.

Copyright (C) 2004 David Castro

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

For more information, please email arimus@users.sourceforge.net
*/
package net.sf.jmimemagic;

/**
 * Basic JMimeMagic parse exception. This is simply a holder to identify a parsing problem. It
 * should be extended to identify more specific issues.
 *
 * @author $Author: arimus $
 * @version $Revision: 1.1 $
 */
public class MagicParseException extends Exception
{

	private static final long serialVersionUID = 8784355416847841008L;

	/**
     * Default constructor
     */
    public MagicParseException()
    {
        super();
    }

    /**
     * Create exception with error message
     *
     * @param message The error message for this exception
     */
    public MagicParseException(final String message)
    {
        super(message);
    }

    /**
     * Create exception based on an existing Throwable
     *
     * @param cause The throwable on which we'll base this exception
     */
    public MagicParseException(final Throwable cause)
    {
        super(cause);
    }

    /**
     * Create an exception with custom message and throwable info
     *
     * @param message The message
     * @param cause The target Throwable
     */
    public MagicParseException(final String message, final Throwable cause)
    {
        super(message, cause);
    }
}
