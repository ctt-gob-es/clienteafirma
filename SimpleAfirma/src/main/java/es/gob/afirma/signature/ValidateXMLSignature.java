
package es.gob.afirma.signature;

import java.io.ByteArrayInputStream;
import java.security.Key;
import java.security.KeyException;
import java.security.Provider;
import java.security.PublicKey;
import java.util.List;

import javax.xml.crypto.AlgorithmMethod;
import javax.xml.crypto.KeySelector;
import javax.xml.crypto.KeySelectorException;
import javax.xml.crypto.KeySelectorResult;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMValidateContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyValue;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

/**
 * This is a simple example of validating an XML 
 * Signature using the JSR 105 API. It assumes the key needed to
 * validate the signature is contained in a KeyValue KeyInfo. 
 */
public final class ValidateXMLSignature {

    /**
     * Valida una firma XML.
     * @param sign Firma a validar
     * @return <code>true</code> si la firma es v&aacute;lida, <code>false</code> en caso contrario
     * @throws Exception Si ocurre cualquier problema durante la validaci&oacute;n
     */
    public static boolean validate(final byte[] sign) throws Exception {
        // Instantiate the document to be validated
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        final Document doc =
            dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));

        // Find Signature element
        final NodeList nl = 
            doc.getElementsByTagNameNS(XMLSignature.XMLNS, "Signature");
        if (nl.getLength() == 0) {
            throw new Exception("Cannot find Signature element");
        }

        // Create a DOM XMLSignatureFactory that will be used to unmarshal the 
        // document containing the XMLSignature 
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM",
                (Provider) Class.forName(System.getProperty
                        ("jsr105Provider", "org.jcp.xml.dsig.internal.dom.XMLDSigRI")).newInstance());

        // Create a DOMValidateContext and specify a KeyValue KeySelector
        // and document context
        final DOMValidateContext valContext = new DOMValidateContext
        (new KeyValueKeySelector(), nl.item(0));

        // Validate the XMLSignature (generated above)
        return fac.unmarshalXMLSignature(valContext).validate(valContext);
    }
    
    /**
     * KeySelector which retrieves the public key out of the
     * KeyValue element and returns it.
     * NOTE: If the key algorithm doesn't match signature algorithm,
     * then the public key will be ignored.
     */
    private static final class KeyValueKeySelector extends KeySelector {
        @Override
		public KeySelectorResult select(final KeyInfo keyInfo,
                final KeySelector.Purpose purpose,
                final AlgorithmMethod method,
                final XMLCryptoContext context)
        throws KeySelectorException {
            if (keyInfo == null) {
                throw new KeySelectorException("Null KeyInfo object!");
            }
            final List<?> list = keyInfo.getContent();

            for (int i = 0; i < list.size(); i++) {
                final XMLStructure xmlStructure = (XMLStructure) list.get(i);
                if (xmlStructure instanceof KeyValue) {
                    final PublicKey pk;
                    try {
                        pk = ((KeyValue)xmlStructure).getPublicKey();
                    } 
                    catch (final KeyException ke) {
                        throw new KeySelectorException(ke);
                    }
                    // make sure algorithm is compatible with method
                    if (algEquals(((SignatureMethod) method).getAlgorithm(), pk.getAlgorithm())) {
                        return new SimpleKeySelectorResult(pk);
                    }
                }
            }
            throw new KeySelectorException("No KeyValue element found!");
        }

        //@@@FIXME: this should also work for key types other than DSA/RSA
        static boolean algEquals(final String algURI, final String algName) {
            if (algName.equalsIgnoreCase("DSA") &&
                    algURI.equalsIgnoreCase(SignatureMethod.DSA_SHA1)) {
                return true;
            } 
            else if (algName.equalsIgnoreCase("RSA") &&
                    algURI.equalsIgnoreCase(SignatureMethod.RSA_SHA1)) {
                return true;
            } 
            else {
                return false;
            }
        }
    }

    private static class SimpleKeySelectorResult implements KeySelectorResult {
        private PublicKey pk;
        SimpleKeySelectorResult(PublicKey pk) {
            this.pk = pk;
        }

        @Override
		public Key getKey() { return this.pk; }
    }
}
