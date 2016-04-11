package xmlwise;

/**
 * Generic exception when parsing XML.
 *
 * @author Christoffer Lerno
 */
public class XmlParseException extends Exception
{
	private static final long serialVersionUID = -3246260520113823143L;

	XmlParseException(final Throwable cause)
	{
		super(cause);
	}

	XmlParseException(final String message)
	{
		super(message);
	}

	XmlParseException(final String message, final Throwable cause)
	{
		super(message, cause);
	}

}
