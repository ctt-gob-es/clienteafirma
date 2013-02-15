/*
 * Copyright 1999-2005 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.lowagie.text.xml;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 *
 * @author psoares
 */
public class XmlDomWriter {

    /** Print writer. */
    private PrintWriter fOut;

    /** Canonical output. */
    private boolean fCanonical;

    /** Processing XML 1.1 document. */
    private boolean fXML11;

    //
    // Constructors
    //

    /** Default constructor. */
    public XmlDomWriter() {
    } // <init>()



    //
    // Public methods
    //

    /** Sets whether output is canonical. */
    public void setCanonical(final boolean canonical) {
        this.fCanonical = canonical;
    } // setCanonical(boolean)

    /** Sets the output stream for printing. */
    public void setOutput(final OutputStream stream, String encoding)
    throws UnsupportedEncodingException {

        if (encoding == null) {
            encoding = "UTF8";
        }

        final java.io.Writer writer = new OutputStreamWriter(stream, encoding);
        this.fOut = new PrintWriter(writer);

    } // setOutput(OutputStream,String)

    /** Sets the output writer. */
    public void setOutput(final java.io.Writer writer) {

        this.fOut = writer instanceof PrintWriter
                ? (PrintWriter)writer : new PrintWriter(writer);

    } // setOutput(java.io.Writer)

    /** Writes the specified node, recursively. */
    public void write(final Node node) {

        // is there anything to do?
        if (node == null) {
            return;
        }

        final short type = node.getNodeType();
        switch (type) {
            case Node.DOCUMENT_NODE: {
                final Document document = (Document)node;
                this.fXML11 = false; //"1.1".equals(getVersion(document));
                if (!this.fCanonical) {
                    if (this.fXML11) {
                        this.fOut.println("<?xml version=\"1.1\" encoding=\"UTF-8\"?>");
                    } else {
                        this.fOut.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
                    }
                    this.fOut.flush();
                    write(document.getDoctype());
                }
                write(document.getDocumentElement());
                break;
            }

            case Node.DOCUMENT_TYPE_NODE: {
                final DocumentType doctype = (DocumentType)node;
                this.fOut.print("<!DOCTYPE ");
                this.fOut.print(doctype.getName());
                final String publicId = doctype.getPublicId();
                final String systemId = doctype.getSystemId();
                if (publicId != null) {
                    this.fOut.print(" PUBLIC '");
                    this.fOut.print(publicId);
                    this.fOut.print("' '");
                    this.fOut.print(systemId);
                    this.fOut.print('\'');
                } else if (systemId != null) {
                    this.fOut.print(" SYSTEM '");
                    this.fOut.print(systemId);
                    this.fOut.print('\'');
                }
                final String internalSubset = doctype.getInternalSubset();
                if (internalSubset != null) {
                    this.fOut.println(" [");
                    this.fOut.print(internalSubset);
                    this.fOut.print(']');
                }
                this.fOut.println('>');
                break;
            }

            case Node.ELEMENT_NODE: {
                this.fOut.print('<');
                this.fOut.print(node.getNodeName());
                final Attr attrs[] = sortAttributes(node.getAttributes());
                for (final Attr attr : attrs) {
                    this.fOut.print(' ');
                    this.fOut.print(attr.getNodeName());
                    this.fOut.print("=\"");
                    normalizeAndPrint(attr.getNodeValue(), true);
                    this.fOut.print('"');
                }
                this.fOut.print('>');
                this.fOut.flush();

                Node child = node.getFirstChild();
                while (child != null) {
                    write(child);
                    child = child.getNextSibling();
                }
                break;
            }

            case Node.ENTITY_REFERENCE_NODE: {
                if (this.fCanonical) {
                    Node child = node.getFirstChild();
                    while (child != null) {
                        write(child);
                        child = child.getNextSibling();
                    }
                } else {
                    this.fOut.print('&');
                    this.fOut.print(node.getNodeName());
                    this.fOut.print(';');
                    this.fOut.flush();
                }
                break;
            }

            case Node.CDATA_SECTION_NODE: {
                if (this.fCanonical) {
                    normalizeAndPrint(node.getNodeValue(), false);
                } else {
                    this.fOut.print("<![CDATA[");
                    this.fOut.print(node.getNodeValue());
                    this.fOut.print("]]>");
                }
                this.fOut.flush();
                break;
            }

            case Node.TEXT_NODE: {
                normalizeAndPrint(node.getNodeValue(), false);
                this.fOut.flush();
                break;
            }

            case Node.PROCESSING_INSTRUCTION_NODE: {
                this.fOut.print("<?");
                this.fOut.print(node.getNodeName());
                final String data = node.getNodeValue();
                if (data != null && data.length() > 0) {
                    this.fOut.print(' ');
                    this.fOut.print(data);
                }
                this.fOut.print("?>");
                this.fOut.flush();
                break;
            }

            case Node.COMMENT_NODE: {
                if (!this.fCanonical) {
                    this.fOut.print("<!--");
                    final String comment = node.getNodeValue();
                    if (comment != null && comment.length() > 0) {
                        this.fOut.print(comment);
                    }
                    this.fOut.print("-->");
                    this.fOut.flush();
                }
            }
        }

        if (type == Node.ELEMENT_NODE) {
            this.fOut.print("</");
            this.fOut.print(node.getNodeName());
            this.fOut.print('>');
            this.fOut.flush();
        }

    } // write(Node)

    /** Returns a sorted list of attributes. */
    private Attr[] sortAttributes(final NamedNodeMap attrs) {

        final int len = attrs != null ? attrs.getLength() : 0;
        final Attr array[] = new Attr[len];
        for (int i = 0; i < len; i++) {
            array[i] = (Attr)attrs.item(i);
        }
        for (int i = 0; i < len - 1; i++) {
            String name = array[i].getNodeName();
            int index = i;
            for (int j = i + 1; j < len; j++) {
                final String curName = array[j].getNodeName();
                if (curName.compareTo(name) < 0) {
                    name = curName;
                    index = j;
                }
            }
            if (index != i) {
                final Attr temp = array[i];
                array[i] = array[index];
                array[index] = temp;
            }
        }

        return array;

    } // sortAttributes(NamedNodeMap):Attr[]

    //
    // Protected methods
    //

    /** Normalizes and prints the given string. */
    private void normalizeAndPrint(final String s, final boolean isAttValue) {

        final int len = s != null ? s.length() : 0;
        for (int i = 0; i < len; i++) {
            final char c = s.charAt(i);
            normalizeAndPrint(c, isAttValue);
        }

    } // normalizeAndPrint(String,boolean)

    /** Normalizes and print the given character. */
    private void normalizeAndPrint(final char c, final boolean isAttValue) {

        switch (c) {
            case '<': {
                this.fOut.print("&lt;");
                break;
            }
            case '>': {
                this.fOut.print("&gt;");
                break;
            }
            case '&': {
                this.fOut.print("&amp;");
                break;
            }
            case '"': {
                // A '"' that appears in character data
                // does not need to be escaped.
                if (isAttValue) {
                    this.fOut.print("&quot;");
                } else {
                    this.fOut.print("\"");
                }
                break;
            }
            case '\r': {
                // If CR is part of the document's content, it
                // must not be printed as a literal otherwise
                // it would be normalized to LF when the document
                // is reparsed.
                this.fOut.print("&#xD;");
                break;
            }
            case '\n': {
                if (this.fCanonical) {
                    this.fOut.print("&#xA;");
                    break;
                }
                // else, default print char
            }
            default: {
                // In XML 1.1, control chars in the ranges [#x1-#x1F, #x7F-#x9F] must be escaped.
                //
                // Escape space characters that would be normalized to #x20 in attribute values
                // when the document is reparsed.
                //
                // Escape NEL (0x85) and LSEP (0x2028) that appear in content
                // if the document is XML 1.1, since they would be normalized to LF
                // when the document is reparsed.
                if (this.fXML11 && (c >= 0x01 && c <= 0x1F && c != 0x09 && c != 0x0A
                || c >= 0x7F && c <= 0x9F || c == 0x2028)
                || isAttValue && (c == 0x09 || c == 0x0A)) {
                    this.fOut.print("&#x");
                    this.fOut.print(Integer.toHexString(c).toUpperCase());
                    this.fOut.print(";");
                } else {
                    this.fOut.print(c);
                }
            }
        }
    } // normalizeAndPrint(char,boolean)

    /** Extracts the XML version from the Document. */
//    protected String getVersion(Document document) {
//        if (document == null) {
//            return null;
//        }
//        String version = null;
//        Method getXMLVersion = null;
//        try {
//            getXMLVersion = document.getClass().getMethod("getXmlVersion", new Class[]{});
//            // If Document class implements DOM L3, this method will exist.
//            if (getXMLVersion != null) {
//                version = (String) getXMLVersion.invoke(document, (Object[]) null);
//            }
//        } catch (Exception e) {
//            // Either this locator object doesn't have
//            // this method, or we're on an old JDK.
//        }
//        return version;
//    } // getVersion(Document)
}
