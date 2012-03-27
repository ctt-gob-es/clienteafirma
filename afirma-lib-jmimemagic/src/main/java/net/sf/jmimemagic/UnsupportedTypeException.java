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
 * JMimeMagic unsupported test type exception. This exception is thrown when an unsupported
 * test type is specified in a mime magic test
 *
 * @author $Author: arimus $
 * @version $Revision: 1.1 $
 */
public class UnsupportedTypeException extends Exception
{

	private static final long serialVersionUID = 4225590514782456873L;

	/**
     * Default constructor
     */
    public UnsupportedTypeException()
    {
        super();
    }

    /**
     * Create exception with error message
     *
     * @param message The error message for this exception
     */
    public UnsupportedTypeException(final String message)
    {
        super(message);
    }

    /**
     * Create exception based on an existing Throwable
     *
     * @param cause The throwable on which we'll base this exception
     */
    public UnsupportedTypeException(final Throwable cause)
    {
        super(cause);
    }

    /**
     * Create an exception with custom message and throwable info
     *
     * @param message The message
     * @param cause The target Throwable
     */
    public UnsupportedTypeException(final String message, final Throwable cause)
    {
        super(message, cause);
    }
}
