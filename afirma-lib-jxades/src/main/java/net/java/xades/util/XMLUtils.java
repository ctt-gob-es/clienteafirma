package net.java.xades.util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.CDATASection;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;

import com.sun.org.apache.xalan.internal.xsltc.trax.TransformerFactoryImpl;
import com.sun.org.apache.xerces.internal.dom.DOMOutputImpl;

/**
 * Common XML Tasks
 * 
 * @author Miroslav Nachev
 */
public class XMLUtils
{
    private static final Logger logger = Logger.getLogger(XMLUtils.class.getName());

    private static Charset charset = Charset.forName("UTF-8");

    /**
     * Get the attribute with given name's value
     * 
     * @param node
     *            the node which attribute's value is returned
     * @param name
     *            name of the attribute
     * @return the value af the attribute
     */
    public static String getAttributeByName(Node node, String name)
    {
        if (node == null)
        {
            return null;
        }

        Node attribute = node.getAttributes().getNamedItem(name);
        if (attribute == null)
        {
            return null;
        }
        else
        {
            return attribute.getNodeValue().trim();
        }
    }

    /**
     * Get the data of the element , no matter whether it is TXT ot CDATA
     * 
     * @param parentNode
     *            the node which data is returned
     * @return the TEXT or CDATA of the parentNode
     */
    public static String getElementTextValueDeprecated(Element parentNode)
    {
        Text text = getElementTextNode(parentNode);
        if (text != null)
        {
            return text.getData();
        }
        else
        {
            return null;
        }
    }

    /**
     * Sets element TEXT data
     * 
     * @param e
     *            the element
     * @param data
     *            the new data
     */
    public static void setElementTextValue(Element e, String data)
    {
        Text txt = getElementTextNode(e);
        if (txt != null)
        {
            txt.setData(data);
        }
        else
        {
            txt = e.getOwnerDocument().createTextNode(data);
            e.appendChild(txt);
        }
    }

    /**
     * Sets element CDATA data
     * 
     * @param e
     *            the lement
     * @param data
     *            the new data
     */
    public static void setElementCDataValue(Element e, String data)
    {
        CDATASection txt = getElementCDataNode(e);
        if (txt != null)
        {
            txt.setData(data);
        }
        else
        {
            txt = e.getOwnerDocument().createCDATASection(data);
            e.appendChild(txt);
        }
    }

    /**
     * Gets CDATA value of an element
     * 
     * @param e
     *            the element
     * @return CDATA value of element e
     */
    public static String getElementCDataValue(Element e)
    {
        CDATASection text = getElementCDataNode(e);
        if (text != null)
        {
            return text.getData().trim();
        }
        else
        {
            return null;
        }
    }

    /**
     * Returns element's CDATA Node
     * 
     * @param element
     *            the element which CDATA node is returned
     * @return CDATA node
     */
    public static CDATASection getElementCDataNode(Element element)
    {
        return (CDATASection) getChildNodeByType(element, Node.CDATA_SECTION_NODE);
    }

    /**
     * Returns element's TEXT Node
     * 
     * @param element
     *            the element which TEXT node is returned
     * @return TEXT node
     */
    public static Text getElementTextNode(Element element)
    {
        return (Text) getChildNodeByType(element, Node.TEXT_NODE);
    }

    private static Node getChildNodeByType(Element element, short nodeType)
    {
        if (element == null)
        {
            return null;
        }

        NodeList nodes = element.getChildNodes();
        if (nodes == null || nodes.getLength() < 1)
        {
            return null;
        }

        Node node;
        String data;
        for (int i = 0; i < nodes.getLength(); i++)
        {
            node = nodes.item(i);
            short type = node.getNodeType();
            if (type == nodeType)
            {
                if (type == Node.TEXT_NODE || type == Node.CDATA_SECTION_NODE)
                {
                    data = ((Text) node).getData();
                    if (data == null || data.trim().length() < 1)
                    {
                        continue;
                    }
                }

                return node;
            }
        }

        return null;
    }

    public static void writeXML(File file, Node node) throws FileNotFoundException
    {
        writeXML(new FileOutputStream(file), node);
    }

    public static void writeXML(OutputStream outStream, Node node)
    {
        writeXML(new BufferedWriter(new OutputStreamWriter(outStream, charset)), node, true);
    }

    public static void writeXML(OutputStream outStream, Node node, boolean indent)
    {
        writeXML(new BufferedWriter(new OutputStreamWriter(outStream, charset)), node, indent);
    }

    /**
     * Writes the specified document to the given file. The default encoding is UTF-8.
     * 
     * @param out
     *            the output File
     * @param document
     *            the document to be writen
     */
    public static void writeXML(Writer writer, Node node, boolean indent)
    {
        // TODO: This section only works with XALAN transformation!!!
        // Result with JDK transformation:
        // <xades:QualifyingProperties xmlns:xades="http://uri.etsi.org/01903/v1.3.2#"
        // xmlns:ns0="http://uri.etsi.org/01903/v1.3.2#"
        // ns0:Id="S0-QualifyingProperties"
        // xmlns:ns1="http://uri.etsi.org/01903/v1.3.2#"
        // ns1:Target="#S0-Signature">
        //        	
        // DOMSource domSource = new DOMSource(node);
        // StreamResult streamResult = new StreamResult(writer);
        // TransformerFactoryImpl tf = new TransformerFactoryImpl();
        // Transformer serializer = tf.newTransformer();
        // serializer.setOutputProperty(OutputKeys.ENCODING, charset.name());
        //
        // if (indent)
        // {
        // serializer.setOutputProperty(OutputKeys.INDENT, "yes");
        // }
        //
        // serializer.transform(domSource, streamResult);

        Document document = node.getOwnerDocument();
        DOMImplementationLS domImplLS = (DOMImplementationLS) document.getImplementation();
        LSSerializer serializer = domImplLS.createLSSerializer();
        serializer.getDomConfig().setParameter("namespaces", false);

        DOMOutputImpl output = new DOMOutputImpl();
        output.setCharacterStream(writer);

        serializer.write(node, output);
    }

    public static void writeXML(Writer writer, Document document, String doctypeSystem,
            String doctypePublic)
    {
        DOMImplementationLS domImplLS = (DOMImplementationLS) document.getImplementation();
        LSSerializer serializer = domImplLS.createLSSerializer();

        DOMOutputImpl output = new DOMOutputImpl();
        output.setCharacterStream(writer);

        serializer.write(document.getDocumentElement(), output);
    }

    /**
     * Returns the element which is at the end of the specified chain <parent><child><grandchild>...
     * 
     * @param element
     * @param chain
     * @return
     */
    public static Element getChildElementByChain(Element element, String[] chain, boolean create)
    {
        if (chain == null)
        {
            return null;
        }
        Element e = element;
        for (int i = 0; i < chain.length; i++)
        {
            if (e == null)
            {
                return null;
            }
            e = getChildElementByTagName(e, chain[i]);
        }
        return e;
    }

    /**
     * Creates (only if necessary) and returns the element which is at the end of the specified
     * path.
     * 
     * @param doc
     *            the target document where the specified path should be created
     * @param path
     *            a dot separated string indicating the path to be created
     * @return the component at the end of the newly created path.
     */
    public static Element createLastPathComponent(Document doc, String[] path)
    {
        Element parent = (Element) doc.getFirstChild();
        if (path == null || parent == null || doc == null)
        {
            throw new IllegalArgumentException("Document parent and path must not be null");
        }

        Element e = parent;
        for (int i = 0; i < path.length; i++)
        {
            Element newEl = getChildElementByTagName(e, path[i]);
            if (newEl == null)
            {
                newEl = doc.createElement(path[i]);
                e.appendChild(newEl);
            }
            e = newEl;
        }
        return e;
    }

    public static Element getChildElementByTagNameNS(Element parent, String tagName, String nsName)
    {
        NodeList nl = parent.getChildNodes();
        int size = nl.getLength();
        for (int i = 0; i < size; i++)
        {
            Node node = nl.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE)
            {
                if (tagName.equals(node.getLocalName()))
                {
                    String ns = node.getNamespaceURI();
                    if (ns != null && ns.equals(nsName))
                    {
                        return (Element) node;
                    }
                }
            }
        }

        return null;
    }

    /**
     * Returns the child element with the specified tagName for the specified parent element
     * 
     * @param parent
     * @param tagName
     * @return
     */
    public static Element getChildElementByTagName(Element parent, String tagName)
    {
        if (parent == null || tagName == null)
        {
            return null;
        }

        NodeList nodes = parent.getChildNodes();
        Node node;
        int len = nodes.getLength();
        for (int i = 0; i < len; i++)
        {
            node = nodes.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE
                    && ((Element) node).getNodeName().equals(tagName))
            {
                return (Element) node;
            }
        }

        return null;
    }

    public static List<Element> getChildElementsByTagNameNS(Element parent, String tagName,
            String nsName)
    {
        if (parent == null || tagName == null)
            return Collections.<Element> emptyList();

        NodeList nl = parent.getChildNodes();
        int size = nl.getLength();
        ArrayList<Element> childElements = new ArrayList<Element>(size);
        for (int i = 0; i < size; i++)
        {
            Node node = nl.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE)
            {
                if (tagName.equals(node.getLocalName()))
                {
                    String ns = node.getNamespaceURI();
                    if (ns != null && ns.equals(nsName))
                    {
                        childElements.add((Element) node);
                    }
                }
            }
        }

        return childElements;
    }

    public static List<Element> getChildElementsByTagName(Element parent, String tagName)
    {
        if (parent == null || tagName == null)
            return Collections.<Element> emptyList();

        NodeList nodes = parent.getChildNodes();
        Node node;
        int len = nodes.getLength();
        ArrayList<Element> childElements = new ArrayList<Element>(len);
        for (int i = 0; i < len; i++)
        {
            node = nodes.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE
                    && ((Element) node).getNodeName().equals(tagName))
            {
                childElements.add((Element) node);
            }
        }

        return childElements;
    }

    /**
     * Used for debuging
     * 
     * @param parent
     *            Element
     * @param out
     *            PrintStream
     * @param deep
     *            boolean
     * @param prefix
     *            String
     */
    public static void printChildElements(Element parent, PrintStream out, boolean deep,
            String prefix)
    {
        out.print(prefix + "<" + parent.getNodeName());
        NamedNodeMap attrs = parent.getAttributes();
        Node node;
        for (int i = 0; i < attrs.getLength(); i++)
        {
            node = attrs.item(i);
            out.print(" " + node.getNodeName() + "=\"" + node.getNodeValue() + "\"");
        }
        out.println(">");

        // String data = getElementTextValueDeprecated(parent);
        String data = parent.getNodeValue();
        if (data != null && data.trim().length() > 0)
        {
            out.println(prefix + "\t" + data);
        }

        data = getElementCDataValue(parent);
        if (data != null && data.trim().length() > 0)
        {
            out.println(prefix + "\t<![CDATA[" + data + "]]>");
        }

        NodeList nodes = parent.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++)
        {
            node = nodes.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE)
            {
                if (deep)
                {
                    printChildElements((Element) node, out, deep, prefix + "\t");
                }
                else
                {
                    out.println(prefix + node.getNodeName());
                }
            }
        }

        out.println(prefix + "</" + parent.getNodeName() + ">");
    }

}
