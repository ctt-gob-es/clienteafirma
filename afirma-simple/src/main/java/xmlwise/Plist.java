package xmlwise;

import java.io.Closeable;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

/** Plist xml handling (serialization and deserialization).
 * The xml plist dtd can be found at <a href="http://www.apple.com/DTDs/PropertyList-1.0.dtd">http://www.apple.com/DTDs/PropertyList-1.0.dtd</a>.
 * @author Christoffer Lerno. */
public final class Plist
{
	/**
	 * Singleton instance.
	 */
	private final static Plist PLIST = new Plist();

	/**
	 * All element types possible for a plist.
	 */
	private static enum ElementType
	{
		INTEGER,
		STRING,
		REAL,
		DATA,
		DATE,
		DICT,
		ARRAY,
		TRUE,
		FALSE,
	}

	private static final String BASE64_STRING
			= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"; //$NON-NLS-1$
	private static final char[] BASE64_CHARS = BASE64_STRING.toCharArray();
	private final DateFormat m_dateFormat;
	private final Map<Class<?>, ElementType> m_simpleTypes;

	/**
	 * Utility method to close a closeable.
	 *
	 * @param closeable or null.
	 */
	static void silentlyClose(final Closeable closeable)
	{
		try
			{
				if (closeable != null) {
					closeable.close();
				}
		}
		catch (final IOException e)
		{
			// Ignore
		}
	}

	/**
	 * Create a nested {@code map<String, Object>} from a plist xml string using the default mapping.
	 *
	 * @param xml the plist xml data as a string.
	 * @return the resulting map as read from the plist data.
	 * @throws XmlParseException if the plist could not be properly parsed.
	 */
	public static Map<String, Object> fromXml(final String xml) throws XmlParseException
	{
		return PLIST.parse(Xmlwise.createXml(xml));
	}

	/**
	 * Create a plist handler.
	 */
	Plist()
	{
		this.m_dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"); //$NON-NLS-1$
		this.m_dateFormat.setTimeZone(TimeZone.getTimeZone("Z")); //$NON-NLS-1$
		this.m_simpleTypes = new HashMap<>();
		this.m_simpleTypes.put(Integer.class, ElementType.INTEGER);
		this.m_simpleTypes.put(Byte.class, ElementType.INTEGER);
		this.m_simpleTypes.put(Short.class, ElementType.INTEGER);
		this.m_simpleTypes.put(Short.class, ElementType.INTEGER);
		this.m_simpleTypes.put(Long.class, ElementType.INTEGER);
		this.m_simpleTypes.put(String.class, ElementType.STRING);
		this.m_simpleTypes.put(Float.class, ElementType.REAL);
		this.m_simpleTypes.put(Double.class, ElementType.REAL);
		this.m_simpleTypes.put(byte[].class, ElementType.DATA);
		this.m_simpleTypes.put(Boolean.class, ElementType.TRUE);
		this.m_simpleTypes.put(Date.class, ElementType.DATE);
	}

	/**
	 * Convert an object to its plist representation.
	 *
	 * @param o the object to convert, must be Integer, Double, String, Date, Boolean, byte[],
	 * Map or List.
	 * @return an <tt>XmlElement</tt> containing the serialized version of the object.
	 */
	XmlElement objectToXml(final Object o)
	{
		final ElementType type = this.m_simpleTypes.get(o.getClass());
		if (type != null)
		{
			switch (type) {
				case REAL:
					return new XmlElement("real", o.toString()); //$NON-NLS-1$
				case INTEGER:
					return new XmlElement("integer", o.toString()); //$NON-NLS-1$
				case TRUE:
					return new XmlElement(((Boolean) o).booleanValue() ? "true" : "false"); //$NON-NLS-1$ //$NON-NLS-2$
				case DATE:
					return new XmlElement("date", this.m_dateFormat.format((Date) o)); //$NON-NLS-1$
				case STRING:
					return new XmlElement("string", (String) o); //$NON-NLS-1$
				case DATA:
					return new XmlElement("data", base64encode((byte[]) o)); //$NON-NLS-1$
			}
		}
		if (o instanceof Map)
		{
			return toXmlDict((Map) o);
		}
		else if (o instanceof List)
		{
			return toXmlArray((List) o);
		}
		else {
			throw new RuntimeException("Cannot use " + o.getClass() + " in plist."); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/**
	 * Convert a list to its plist representation.
	 *
	 * @param list the list to convert.
	 * @return an <tt>XmlElement</tt> representing the list.
	 */
	private XmlElement toXmlArray(final List<Object> list) {
		final XmlElement array = new XmlElement("array"); //$NON-NLS-1$
		for (final Object o : list) {
			array.add(objectToXml(o));
		}
		return array;
	}

	/**
	 * Convert a map to its plist representation.
	 *
	 * @param map the map to convert, assumed to have string keys.
	 * @return an <tt>XmlElement</tt> representing the map.
	 */
	private XmlElement toXmlDict(final Map<String, Object> map) {
		final XmlElement dict = new XmlElement("dict"); //$NON-NLS-1$
		for (final Map.Entry<String, Object> entry : map.entrySet()) {
			dict.add(new XmlElement("key", entry.getKey())); //$NON-NLS-1$
			dict.add(objectToXml(entry.getValue()));
		}
		return dict;
	}

	/**
	 * Parses a plist top element into a map dictionary containing all the data
	 * in the plist.
	 *
	 * @param element the top plist element.
	 * @return the resulting data tree structure.
	 * @throws XmlParseException if there was any error parsing the xml.
	 */
	Map<String, Object> parse(final XmlElement element) throws XmlParseException {
		if (!"plist".equalsIgnoreCase(element.getName())) { //$NON-NLS-1$
			throw new XmlParseException("Expected plist top element, was: " + element.getName()); //$NON-NLS-1$
		}
		return (Map<String, Object>)parseElement(element.get("dict").getFirst()); //$NON-NLS-1$
	}

	/**
	 * Parses a (non-top) xml element.
	 *
	 * @param element the element to parse.
	 * @return the resulting object.
	 * @throws XmlParseException if there was some error in the xml.
	 */
	private Object parseElement(final XmlElement element) throws XmlParseException {
		try {
			return parseElementRaw(element);
		}
		catch (final Exception e) {
			throw new XmlParseException("Failed to parse: " + element.toXml(), e); //$NON-NLS-1$
		}
	}


	/**
	 * Parses a (non-top) xml element.
	 *
	 * @param element the element to parse.
	 * @return the resulting object.
	 * @throws Exception if there was some error parsing the xml.
	 */
	private Object parseElementRaw(final XmlElement element) throws Exception {
		final ElementType type = ElementType.valueOf(element.getName().toUpperCase());
		switch (type) {
			case INTEGER:
				return parseInt(element.getValue());
			case REAL:
				return Double.valueOf(element.getValue());
			case STRING:
				return element.getValue();
			case DATE:
				return this.m_dateFormat.parse(element.getValue());
			case DATA:
				return base64decode(element.getValue());
			case ARRAY:
				return parseArray(element);
			case TRUE:
				return Boolean.TRUE;
			case FALSE:
				return Boolean.FALSE;
			case DICT:
				return parseDict(element);
			default:
				throw new RuntimeException("Unexpected type: " + element.getName()); //$NON-NLS-1$
		}
	}

	/** Parses a string into a Long or Integer depending on size.
	 * @param value the value as a string.
	 * @return the long value of this string is the value doesn't fit in an integer,
	 * otherwise the int value of the string. */
	private static Number parseInt(final String value)
	{
		final Long l = Long.valueOf(value);
		if (l.intValue() == l.longValue()) {
			return Integer.valueOf(l.intValue());
		}
		return l;
	}

	/**
	 * Parse a list of xml elements as a plist dict.
	 *
	 * @param elements the elements to parse.
	 * @return the dict deserialized as a map.
	 * @throws Exception if there are any problems deserializing the map.
	 */
	private Map<String, Object> parseDict(final List<XmlElement> elements) throws Exception {
		final Iterator<XmlElement> element = elements.iterator();
		final HashMap<String, Object> dict = new HashMap<>();
		while (element.hasNext()) {
			final XmlElement key = element.next();
			if (!"key".equals(key.getName())) { //$NON-NLS-1$
				throw new Exception("Expected key but was " + key.getName()); //$NON-NLS-1$
			}
			final Object o = parseElementRaw(element.next());
			dict.put(key.getValue(), o);
		}
		return dict;
	}

	/**
	 * Parse a list of xml elements as a plist array.
	 *
	 * @param elements the elements to parse.
	 * @return the array deserialized as a list.
	 * @throws Exception if there are any problems deserializing the list.
	 */
	private List<Object> parseArray(final List<XmlElement> elements) throws Exception
	{
		final ArrayList<Object> list = new ArrayList<>(elements.size());
		for (final XmlElement element : elements)
		{
			list.add(parseElementRaw(element));
		}
		return list;
	}

	/**
	 * Encode an array of bytes to a string using base64 encoding.
	 *
	 * @param bytes the bytes to convert.
	 * @return the base64 representation of the bytes.
	 */
	static String base64encode(final byte[] bytes) {
		final StringBuilder builder = new StringBuilder((bytes.length + 2)/ 3 * 4);
		for (int i = 0; i < bytes.length; i += 3) {
			final byte b0 = bytes[i];
			final byte b1 = i < bytes.length - 1 ? bytes[i + 1] : 0;
			final byte b2 = i < bytes.length - 2 ? bytes[i + 2] : 0;
			builder.append(BASE64_CHARS[(b0 & 0xFF) >> 2]);
			builder.append(BASE64_CHARS[(b0 & 0x03) << 4 | (b1  & 0xF0) >> 4]);
			builder.append(i < bytes.length - 1 ? Character.valueOf(BASE64_CHARS[(b1 & 0x0F) << 2 | (b2 & 0xC0) >> 6]) : "="); //$NON-NLS-1$
			builder.append(i < bytes.length - 2 ? Character.valueOf(BASE64_CHARS[b2 & 0x3F]) : "="); //$NON-NLS-1$
		}
		return builder.toString();
	}

	/**
	 * Converts a string to a byte array assuming the string uses base64-encoding.
	 *
	 * @param b64 the string to convert.
	 * @return the resulting byte array.
	 */
	static byte[] base64decode(final String b64) {
		String base64 = b64.trim();
		final int endTrim = base64.endsWith("==") ? 2 : base64.endsWith("=") ? 1 : 0; //$NON-NLS-1$ //$NON-NLS-2$
		final int length = base64.length() / 4 * 3 - endTrim;
		base64 = base64.replace('=', 'A');
		final byte[] result = new byte[length];
		final int stringLength = base64.length();
		int index = 0;
		for (int i = 0; i < stringLength; i += 4) {
			final int i0 = BASE64_STRING.indexOf(base64.charAt(i));
			final int i1 = BASE64_STRING.indexOf(base64.charAt(i + 1));
			final int i2 = BASE64_STRING.indexOf(base64.charAt(i + 2));
			final int i3 = BASE64_STRING.indexOf(base64.charAt(i + 3));
			final byte b0 = (byte) (i0 << 2 | i1 >> 4);
			final byte b1 = (byte) (i1 << 4 | i2 >> 2);
			final byte b2 = (byte) (i2 << 6 | i3);
			result[index++] = b0;
			if (index < length) {
				result[index++] = b1;
				if (index < length) {
					result[index++] = b2;
				}
			}
		}
		return result;
	}


}
