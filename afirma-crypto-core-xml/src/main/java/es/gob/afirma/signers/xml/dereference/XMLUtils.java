/*
 * Copyright  1999-2004 The Apache Software Foundation.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */
package es.gob.afirma.signers.xml.dereference;

import java.util.Set;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/** DOM and XML accessibility and comfort functions.
 * @author Christian Geuer-Pollmann. */
final class XMLUtils {

   static void getSet(final Node rootNode,final Set<Node> result,final Node exclude ,final boolean com) {
          if (exclude!=null && isDescendantOrSelf(exclude,rootNode)){
                return;
      }
      getSetRec(rootNode,result,exclude,com);
   }

   static final void getSetRec(final Node rootNode,final Set<Node> result,
        final Node exclude ,final boolean com) {
           //Set result = new HashSet();
       if (rootNode==exclude) {
          return;
       }
       switch (rootNode.getNodeType()) {
            case Node.ELEMENT_NODE:
                result.add(rootNode);
                final Element el=(Element)rootNode;
                if (el.hasAttributes()) {
                    final NamedNodeMap nl = ((Element)rootNode).getAttributes();
                    for (int i=0;i<nl.getLength();i++) {
                            result.add(nl.item(i));
                    }
                }
            //no return keep working
            //$FALL-THROUGH$
            case Node.DOCUMENT_NODE:
                for (Node r=rootNode.getFirstChild();r!=null;r=r.getNextSibling()){
                    if (r.getNodeType()==Node.TEXT_NODE) {
                            result.add(r);
                            while (r!=null && r.getNodeType()==Node.TEXT_NODE) {
                                    r=r.getNextSibling();
                            }
                            if (r==null) {
								return;
							}
                    }
                    getSetRec(r,result,exclude,com);
                }
                return;
            case Node.COMMENT_NODE:
                if (com) {
                    result.add(rootNode);
                }
                return;
            case Node.DOCUMENT_TYPE_NODE:
                return;
            default:
                result.add(rootNode);
       }
       return;
   }

   /** This method returns the owner document of a particular node.
    * This method is necessary because it <I>always</I> returns a
    * {@link Document}. {@link Node#getOwnerDocument} returns <CODE>null</CODE>
    * if the {@link Node} is a {@link Document}.
    * @param node The node.
    * @return the owner document of the node. */
   public static Document getOwnerDocument(final Node node) {
      if (node.getNodeType() == Node.DOCUMENT_NODE) {
         return (Document) node;
      }
      return node.getOwnerDocument();
   }

   /** This method spreads all namespace attributes in a DOM document to their
    * children. This is needed because the XML Signature XPath transform
    * must evaluate the XPath against all nodes in the input, even against
    * XPath namespace nodes. Through a bug in XalanJ2, the namespace nodes are
    * not fully visible in the Xalan XPath model, so we have to do this by
    * hand in DOM spaces so that the nodes become visible in XPath space.
    * @param doc The DOM document.
    * @see <A HREF="http://nagoya.apache.org/bugzilla/show_bug.cgi?id=2650">Namespace axis resolution is not XPath compliant </A> */
   public static void circumventBug2650(final Document doc) {

      final Element documentElement = doc.getDocumentElement();

      // if the document element has no xmlns definition, we add xmlns=""
      final Attr xmlnsAttr =
         documentElement.getAttributeNodeNS("http://www.w3.org/2000/xmlns/", "xmlns"); //$NON-NLS-1$ //$NON-NLS-2$

      if (xmlnsAttr == null) {
         documentElement.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }

      XMLUtils.circumventBug2650internal(doc);
   }

   /** This is the work horse for {@link #circumventBug2650}.
    * @param node The node.
    * @see <A HREF="http://nagoya.apache.org/bugzilla/show_bug.cgi?id=2650">Namespace axis resolution is not XPath compliant </A> */
   private static void circumventBug2650internal(Node node) {
           Node parent=null;
           Node sibling=null;
           final String namespaceNs="http://www.w3.org/2000/xmlns/"; //$NON-NLS-1$
           do {
        	   switch (node.getNodeType()) {
        	   	case Node.ELEMENT_NODE :
                 final Element element = (Element) node;
                 if (!element.hasChildNodes()) {
                	 break;
                 }
	             if (element.hasAttributes()) {
		             final NamedNodeMap attributes = element.getAttributes();
		             final int attributesLength = attributes.getLength();

		             for (Node child = element.getFirstChild(); child!=null;
		                child=child.getNextSibling()) {

		                if (child.getNodeType() != Node.ELEMENT_NODE) {
		                        continue;
		                }
		                final Element childElement = (Element) child;

		                for (int i = 0; i < attributesLength; i++) {
		                        final Attr currentAttr = (Attr) attributes.item(i);
		                        if (namespaceNs!=currentAttr.getNamespaceURI()) {
									continue;
								}
		                        if (childElement.hasAttributeNS(namespaceNs, currentAttr.getLocalName())) {
	                                continue;
		                        }
		                        childElement.setAttributeNS(
	                        		namespaceNs,
		                            currentAttr.getName(),
		                            currentAttr.getNodeValue()
	                            );
		                }
		             }
	             }
        	   	//$FALL-THROUGH$
        	   	case Node.ENTITY_REFERENCE_NODE :
        	   	case Node.DOCUMENT_NODE :
                 parent=node;
                 sibling=node.getFirstChild();
                 break;
        	   }
	    	   while (sibling==null && parent!=null) {
	                         sibling=parent.getNextSibling();
	                         parent=parent.getParentNode();
	    	   }
	    	   if (sibling==null) {
	    		   return;
	    	   }

	    	   node = sibling;
	    	   sibling=node.getNextSibling();
           } while (true);
   }

   /** Returns true if the descendantOrSelf is on the descendant-or-self axis
    * of the context node.
    * @param ctx Context node.
    * @param descendantOrSelf Node to check.
    * @return true if the node is descendant. */
   static public boolean isDescendantOrSelf(final Node ctx, final Node descendantOrSelf) {

      if (ctx == descendantOrSelf) {
         return true;
      }

      Node parent = descendantOrSelf;

      while (true) {
         if (parent == null) {
            return false;
         }

         if (parent == ctx) {
            return true;
         }

         if (parent.getNodeType() == Node.ATTRIBUTE_NODE) {
            parent = ((Attr) parent).getOwnerElement();
         }
         else {
            parent = parent.getParentNode();
         }
      }
   }

}