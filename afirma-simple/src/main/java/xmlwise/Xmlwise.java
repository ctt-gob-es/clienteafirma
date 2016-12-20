package xmlwise;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;

/**
 * Xmlwise convenience methods for loading xml documents and render them into
 * XmlElement trees.
 *
 * @author Christoffer Lerno
 */
public class Xmlwise
{
	private Xmlwise() {}

	/**
	 * Loads an XML document ignoring DTD-validation.
	 *
	 * @param file the file to read from.
	 * @return an XML document.
	 * @throws IOException if we fail to load the file.
	 * @throws XmlParseException if there is a problem parsing the xml in the file.
	 */
	public static Document loadDocument(final File file) throws IOException, XmlParseException
	{
		return loadDocument(file, false);
	}

	/**
	 * Loads an XML document.
	 *
	 * @param file the file to read from.
	 * @param validate if we should validate the document or not.
	 * @return an XML document.
	 * @throws IOException if we fail to load the file.
	 * @throws XmlParseException if there is a problem parsing the xml in the file.
	 */
	public static Document loadDocument(final File file, final boolean validate) throws IOException, XmlParseException
	{
		try
		{
			final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
			final DocumentBuilder builder = documentBuilderFactory.newDocumentBuilder();
			return builder.parse(file);
		}
		catch (final IOException e)
		{
			throw e;
		}
		catch (final Exception e)
		{
			throw new XmlParseException(e);
		}
	}

	/**
	 * Creates a DOM Document from the specified XML string, ignoring DTD-validation.
	 *
	 * @param xml a valid XML document, ie the String can't be null or empty
	 * @param validate if we should validate the document or not.
	 * @return the <code>Document</code> object for the specified string.
	 * @throws XmlParseException if we fail to parse the XML.
	 */
	public static Document createDocument(final String xml, final boolean validate) throws XmlParseException
	{
		try
		{
			final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
			final DocumentBuilder builder = documentBuilderFactory.newDocumentBuilder();
			return builder.parse(new ByteArrayInputStream(xml.getBytes()));
		}
		catch (final Exception e)
		{
			throw new XmlParseException(e);
		}
	}

	/**
	 * Creates a DOM Document from the specified XML string, ignoring DTD-validation.
	 *
	 * @param xml a valid XML document, ie the String can't be null or empty
	 * @return the <code>Document</code> object for the specified string.
	 * @throws XmlParseException if we fail to parse the XML.
	 */
	public static Document createDocument(final String xml) throws XmlParseException
	{
		return createDocument(xml, false);
	}

	/**
	 * Escapes a string to be used in an xml document.
	 * <p>
	 * The following replacements are made:
	 * <table summary="Replacements">
	 * <tr><td>&lt;</td><td>&amp;lt;</td></tr>
	 * <tr><td>&gt;</td><td>&amp;gt;</td></tr>
	 * <tr><td>&amp;</td><td>&amp;amp;</td></tr>
	 * <tr><td>&quot;</td><td>&amp;quot;</td></tr>
	 * <tr><td>'</td><td>&amp;apos;</td></tr>
	 * </table>
	 *
	 * @param stringToEscape the string to escape.
	 * @return an escaped string suitable for use in an xml document.
	 */
	public static String escapeXML(final String stringToEscape)
	{
		final int size = stringToEscape.length();
		if (size == 0)
		 {
			return ""; //$NON-NLS-1$
		}
		final StringBuilder s = new StringBuilder(size);
		for (int i = 0; i < size; i++)
		{
			final char c = stringToEscape.charAt(i);
			switch (c)
			{
				case '<':
					s.append("&lt;"); //$NON-NLS-1$
					break;
				case '>':
					s.append("&gt;"); //$NON-NLS-1$
					break;
				case '&':
					s.append("&amp;"); //$NON-NLS-1$
					break;
				case '"':
					s.append("&quot;"); //$NON-NLS-1$
					break;
				case '\'':
					s.append("&apos;"); //$NON-NLS-1$
					break;
				default: s.append(c);
			}
		}
		return s.toString();
	}

	/**
	 * Loads a document from file and transforms it into an XmlElement tree.
	 *
	 * @param file the file to load.
	 * @return an XmlElement tree rendered from the file.
	 * @throws XmlParseException if parsing the file failed for some reason.
	 * @throws IOException if there were any problems reading from the file.
	 */
	public static XmlElement loadXml(final File file) throws XmlParseException, IOException
	{
		return new XmlElement(loadDocument(file).getDocumentElement());
	}

	/**
	 * Loads a document from file and transforms it into an XmlElement tree.
	 *
	 * @param filename the path to the file.
	 * @return an XmlElement tree rendered from the file.
	 * @throws XmlParseException if parsing the file failed for some reason.
	 * @throws IOException if there were any problems reading from the file.
	 */
	public static XmlElement loadXml(final String filename) throws XmlParseException, IOException
	{
		return loadXml(new File(filename));
	}

	/**
	 * Creates a document from a string and transforms it into an XmlElement tree.
	 *
	 * @param xml the xml as a string.
	 * @return an XmlElement tree rendered from the file.
	 * @throws XmlParseException if parsing the xml failed to validate for some reason.
	 */
	public static XmlElement createXml(final String xml) throws XmlParseException
	{
		return new XmlElement(createDocument(xml).getDocumentElement());
	}

}
