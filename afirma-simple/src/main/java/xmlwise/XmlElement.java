package xmlwise;

import java.util.LinkedList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * A simplified XML Element that only has an attribute map, a list of sub elements a text value and
 * a name.
 * <p>
 * This is the basic entity of the xmlwise library.
 *
 * @author Christoffer Lerno
 */
@SuppressWarnings({"serial"})
public class XmlElement extends LinkedList<XmlElement>
{
	private final XmlElementAttributes m_attributes;
	private final String m_value;
	private final String m_name;

	/**
	 * Creates a new XmlElement given an Element object.
	 *
	 * @param element the document element to construct this object from.
	 */
	public XmlElement(final Element element)
	{
		this.m_attributes = new XmlElementAttributes(element);
		final NodeList children = element.getChildNodes();
		this.m_name = element.getNodeName();
		final StringBuilder textValue = new StringBuilder();
		for (int i = 0; i < children.getLength(); i++)
		{
			final Node node = children.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE)
			{
				add(new XmlElement((Element) node));
			}
			if (node.getNodeType() == Node.TEXT_NODE)
			{
				textValue.append(node.getNodeValue());
			}
		}
		this.m_value = textValue.toString();
	}

	/**
	 * Creates a new XmlElement with the given name and inner text value.
	 *
	 * @param name the name of the node.
	 * @param value the inner text value of the node.
	 */
	public XmlElement(final String name, final String value)
	{
		this.m_attributes = new XmlElementAttributes();
		this.m_name = name;
		this.m_value = value;
	}

	/**
	 * Creates a new XmlElement with the given name with no inner text.
	 *
	 * @param name the name of the node.
	 */
	public XmlElement(final String name)
	{
		this.m_attributes = new XmlElementAttributes();
		this.m_name = name;
		this.m_value = ""; //$NON-NLS-1$
	}

	/**
	 * Get the single direct sub-element with the given name.
	 *
	 * @param name the name of the sub-element.
	 * @return the sub element.
	 * @throws XmlParseException if there are more than one of the sub element, or if no such element was found.
	 */
	public XmlElement getUnique(final String name) throws XmlParseException
	{
		final LinkedList<XmlElement> matches = get(name);
		if (matches.size() != 1)
		{
			throw new XmlParseException("Unexpected number of elements of type " + name + " in element <" + getName() + ">"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		return matches.getFirst();
	}

	/**
	 * Get an integer attribute for this element.
	 *
	 * @param attribute the name of the attribute.
	 * @return the integer value of the attribute.
	 * @throws XmlParseException if we fail to parse this attribute as an int, or the attribute is missing.
	 */
	public int getIntAttribute(final String attribute) throws XmlParseException
	{
		return getAttributes().getInt(attribute);
	}

	/**
	 * Get an integer attribute for this element, defaulting to a default value if the attribute is missing.
	 *
	 * @param attribute the name of the attribute.
	 * @param defaultValue the default value for the attribute, returned if the attribute is missing.
	 * @return the integer value of the attribute or the default value if the attribute is missing.
	 * @throws XmlParseException if we fail to parse this attribute as an int.
	 */
	public int getIntAttribute(final String attribute, final int defaultValue) throws XmlParseException
	{
		return containsAttribute(attribute) ? getIntAttribute(attribute) : defaultValue;
	}

	/**
	 * Get a double attribute for this element.
	 *
	 * @param attribute the name of the attribute.
	 * @return the double value of the attribute.
	 * @throws XmlParseException if we fail to parse this attribute as an double, or the attribute is missing.
	 */
	public double getDoubleAttribute(final String attribute) throws XmlParseException
	{
		return getAttributes().getDouble(attribute);
	}

	/**
	 * Get an double attribute for this element, defaulting to a default value if the attribute is missing.
	 *
	 * @param attribute the name of the attribute.
	 * @param defaultValue the default value for the attribute, returned if the attribute is missing.
	 * @return the double value of the attribute or the default value if the attribute is missing.
	 * @throws XmlParseException if we fail to parse this attribute as an double.
	 */
	public double getDoubleAttribute(final String attribute, final double defaultValue) throws XmlParseException
	{
		return containsAttribute(attribute) ? getDoubleAttribute(attribute) : defaultValue;
	}

	/**
	 * Get a (string) attribute for this element, defaulting to a default value if the attribute is missing.
	 *
	 * @param attribute the name of the attribute.
	 * @param defaultValue the default value for the attribute, returned if the attribute is missing.
	 * @return the value of the attribute or the default value if the attribute is missing.
	 */
	public String getAttribute(final String attribute, final String defaultValue)
	{
		final String value = getAttribute(attribute);
		return value == null ? defaultValue : value;
	}

	/**
	 * Returns the (string) value of an attribute.
	 *
	 * @param attribute the attribute name.
	 * @return the value of the attribute.
	 */
	public String getAttribute(final String attribute)
	{
		return getAttributes().get(attribute);
	}

	/**
	 * Get an boolean attribute for this element.
	 * <p>
	 * "true", "yes" and "y" are all interpreted as true. (Case-independent)
	 * <p>
	 * "false", "no" and "no" are all interpreted at false. (Case-independent)
	 *
	 * @param attribute the name of the attribute.
	 * @return the boolean value of the attribute.
	 * @throws XmlParseException if the attribute value does match true or false as defined, or the attribute is missing.
	 */
	public boolean getBoolAttribute(final String attribute) throws XmlParseException
	{
		return getAttributes().getBoolean(attribute);
	}

	/**
	 * Get an boolean attribute for this element, defaulting to the default value if missing.
	 * <p>
	 * "true", "yes" and "y" are all interpreted as true. (Case-independent)
	 * <p>
	 * "false", "no" and "no" are all interpreted at false. (Case-independent)
	 *
	 * @param attribute the name of the attribute.
	 * @param defaultValue the default value of the attribute.
	 * @return the boolean value of the attribute or the default value if the attribute is missing.
	 * @throws XmlParseException if the attribute value does match true or false as defined
	 */
	public boolean getBoolAttribute(final String attribute, final boolean defaultValue) throws XmlParseException
	{
		return containsAttribute(attribute) ? getBoolAttribute(attribute) : defaultValue;
	}

	/**
	 * Get all elements matching the given key.
	 *
	 * @param name the key to match.
	 * @return a linked list of matching xml elements
	 */
	public LinkedList<XmlElement> get(final String name)
	{
		final LinkedList<XmlElement> list = new LinkedList<>();
		for (final XmlElement element : this)
		{
			if (element.getName().equals(name))
			{
				list.add(element);
			}
		}
		return list;
	}

	/**
	 * Convenience method to set an attribute on this
	 * element.
	 *
	 * @param attribute the attribute to set.
	 * @param value the new value of the attribute.
	 */
	public void setAttribute(final String attribute, final Object value)
	{
		this.m_attributes.put(attribute, value.toString());
	}

	/**
	 * Convenience method to remove an attribute from this element.
	 *
	 * @param attribute the attribute to remove.
	 * @return true if this attribute existed before it was removed,
	 * false otherwise.
	 */
	public boolean removeAttribute(final String attribute)
	{
		return this.m_attributes.remove(attribute) != null;
	}

	/**
	 * Determines if a direct sub-element exists.
	 *
	 * @param key the name of the sub-element.
	 * @return true if the element exists, false otherwise.
	 */
	public boolean contains(final String key)
	{
		for (final XmlElement element : this)
		{
			if (element.getName().equals(key))
			{
				return true;
			}
		}
		return false;
	}
	/**
	 * Renders this as XML.
	 *
	 * @return an xml string based on this element and its sub-elements.
	 */
	public String toXml()
	{
		final StringBuilder builder = new StringBuilder("<").append(this.m_name); //$NON-NLS-1$
		if (this.m_attributes.size() > 0)
		{
			builder.append(this.m_attributes.toXml());
		}
		if (isEmpty() && this.m_value.length() == 0)
		{
			builder.append("/>"); //$NON-NLS-1$
		}
		else
		{
			builder.append('>');
			builder.append(Xmlwise.escapeXML(this.m_value));
			for (final XmlElement element : this)
			{
				builder.append(element.toXml());
			}
			builder.append("</").append(this.m_name).append('>'); //$NON-NLS-1$
		}
		return builder.toString();
	}

	/**
	 * Get the string value contained in this element.
	 * <p>
	 * E.g. the element for <code>&lt;node&gt;foo&lt;/node&gt;</code>
	 * would return "foo" as its value.
	 * <p>
	 * Note that this value will be a concatenation of all strings
	 * directly inside the element, even if the element contains
	 * sub elements.
	 *
	 * @return the string value contained inside this element.
	 */
	public String getValue()
	{
		return this.m_value;
	}

	/**
	 * Returns a map with all attributes of this element.
	 *
	 * @return a map with the attributes for this element.
	 */
	public XmlElementAttributes getAttributes()
	{
		return this.m_attributes;
	}

	/**
	 * Returns the name of this element.
	 * <p>
	 * E.g. the element for <code>&lt;node&gt;foo&lt;/node&gt;</code>
	 * would return "node" as its name.
	 *
	 * @return the name of this element.
	 */
	public String getName()
	{
		return this.m_name;
	}

	/**
	 * Determines if an attribute exists.
	 * @param attribute the attribute to check.
	 * @return true if the attribute exists on this element, false otherwise.
	 */
	public boolean containsAttribute(final String attribute)
	{
		return getAttributes().containsKey(attribute);
	}
}
