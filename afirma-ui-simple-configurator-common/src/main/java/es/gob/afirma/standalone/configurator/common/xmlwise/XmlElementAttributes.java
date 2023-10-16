package es.gob.afirma.standalone.configurator.common.xmlwise;


import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

/**
 * This is a hash map containing all attributes of a single
 * element.
 * <p>
 * Aside from the hash map methods, it also has convenience
 * methods for extracting integers, booleans and doubles.
 *
 * @author Christoffer Lerno
 */
@SuppressWarnings({"serial"})
public class XmlElementAttributes extends HashMap<String, String>
{

	/**
	 * Creates an empty element attribute map.
	 */
	XmlElementAttributes()
	{
	}

	/**
	 * Creates an object given an Element object.
	 *
	 * @param element the element to read from.
	 */
	public XmlElementAttributes(final Element element)
	{
		super(element.getAttributes().getLength());
		final NamedNodeMap map = element.getAttributes();
		final int attributesLength = map.getLength();
		for (int i = 0; i < attributesLength; i++)
		{
			put(map.item(i).getNodeName(), map.item(i).getNodeValue());
		}
	}

	/**
	 * Get an integer attribute.
	 *
	 * @param attribute the name of the attribute.
	 * @return the integer value of the attribute.
	 * @throws XmlParseException if we fail to parse this attribute as an int, or the attribute is missing.
	 */
	public int getInt(final String attribute) throws XmlParseException
	{
		final String value = get(attribute);
		if (value == null)
		{
			throw new XmlParseException("Could not find attribute " + attribute); //$NON-NLS-1$
		}
		try
		{
			return Integer.parseInt(value);
		}
		catch (final NumberFormatException e)
		{
			throw new XmlParseException("Failed to parse int attribute " + attribute, e); //$NON-NLS-1$
		}
	}

	/**
	 * Get a double attribute.
	 *
	 * @param attribute the name of the attribute.
	 * @return the double value of the attribute.
	 * @throws XmlParseException if we fail to parse this attribute as an double, or the attribute is missing.
	 */
	public double getDouble(final String attribute) throws XmlParseException
	{
		final String value = get(attribute);
		if (value == null)
		{
			throw new XmlParseException("Could not find attribute " + attribute); //$NON-NLS-1$
		}
		try
		{
			return Double.parseDouble(value);
		}
		catch (final NumberFormatException e)
		{
			throw new XmlParseException("Failed to parse double attribute " + attribute, e); //$NON-NLS-1$
		}
	}

	/**
	 * Get an boolean attribute.
	 * <p>
	 * "true", "yes" and "y" are all interpreted as true. (Case-independent)
	 * <p>
	 * "false", "no" and "no" are all interpreted at false. (Case-independent)
	 *
	 * @param attribute the name of the attribute.
	 * @return the boolean value of the attribute.
	 * @throws XmlParseException if the attribute value does match true or false as defined, or the attribute is missing.
	 */
	public boolean getBoolean(final String attribute) throws XmlParseException
	{
		String value = get(attribute);
		if (value == null)
		{
			throw new XmlParseException("Could not find attribute " + attribute); //$NON-NLS-1$
		}
		value = value.toLowerCase();
		if ("true".equals(value) || "yes".equals(value) || "y".equals(value)) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		{
			return true;
		}
		if ("false".equals(value) || "no".equals(value) || "n".equals(value)) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		{
			return false;
		}
		throw new XmlParseException("Attribute " + attribute + " did not have boolean value (was: " + value + ')'); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Renders the content of the attributes as Xml. Does not do proper XML-escaping.
	 *
	 * @return this attribute suitable for xml, in the format " attribute1='value1' attribute2='value2' ..."
	 */
	public String toXml()
	{
		final StringBuilder builder = new StringBuilder(10 * size());
		for (final Map.Entry<String, String> entry : entrySet())
		{
			builder.append(' ').append(entry.getKey()).append("=").append("'"); //$NON-NLS-1$ //$NON-NLS-2$
			builder.append(Xmlwise.escapeXML(entry.getValue())).append("'"); //$NON-NLS-1$
		}
		return builder.toString();
	}
}
