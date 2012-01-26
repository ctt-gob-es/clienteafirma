
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

import es.gob.afirma.signature.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signature.SignValidity.VALIDITY_ERROR;


/**
 * Validador de firmas XML. Basado en la documentaci&oacute;n y los ejemplo de la JSR 105.
 */
public final class ValidateXMLSignature {

    /**
     * Valida una firma XML.
     * @param sign Firma a validar
     * @return <code>true</code> si la firma es v&aacute;lida, <code>false</code> en caso contrario
     */
    public static SignValidity validate(final byte[] sign) {
        // Instantiate the document to be validated
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        Document doc;
        try {
            doc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
        } catch (final Exception e) {
            return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CORRUPTED_SIGN);
        }

        // Find Signature element
        final NodeList nl =
            doc.getElementsByTagNameNS(XMLSignature.XMLNS, "Signature"); //$NON-NLS-1$
        if (nl.getLength() == 0) {
            return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_SIGN);
        }

        try {
            // Create a DOM XMLSignatureFactory that will be used to unmarshal the
            // document containing the XMLSignature
            final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM", //$NON-NLS-1$
                    (Provider) Class.forName(System.getProperty
                            ("jsr105Provider", "org.jcp.xml.dsig.internal.dom.XMLDSigRI")).newInstance()); //$NON-NLS-1$ //$NON-NLS-2$

            // Create a DOMValidateContext and specify a KeyValue KeySelector
            // and document context
            final DOMValidateContext valContext = new DOMValidateContext
            (new KeyValueKeySelector(), nl.item(0));

            // Validate the XMLSignature (generated above)
            return fac.unmarshalXMLSignature(valContext).validate(valContext) ?
                    new SignValidity(SIGN_DETAIL_TYPE.OK, null) :
                    new SignValidity(SIGN_DETAIL_TYPE.KO, null);
        } catch (final Exception e) {
            return new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, null);
        }
    }

    /**
     * KeySelector which retrieves the public key out of the
     * KeyValue element and returns it.
     * NOTE: If the key algorithm doesn't match signature algorithm,
     * then the public key will be ignored.
     */
    static final class KeyValueKeySelector extends KeySelector {
        @Override
		public KeySelectorResult select(final KeyInfo keyInfo,
                final KeySelector.Purpose purpose,
                final AlgorithmMethod method,
                final XMLCryptoContext context)
        throws KeySelectorException {
            if (keyInfo == null) {
                throw new KeySelectorException("Objeto KeyInfo nulo"); //$NON-NLS-1$
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
            throw new KeySelectorException("No se ha encontrado el elemento KeyValue"); //$NON-NLS-1$
        }

        //@@@FIXME: this should also work for key types other than DSA/RSA
        static boolean algEquals(final String algURI, final String algName) {
            if (algName.equalsIgnoreCase("DSA") && //$NON-NLS-1$
                    algURI.equalsIgnoreCase(SignatureMethod.DSA_SHA1)) {
                return true;
            }
            return algName.equalsIgnoreCase("RSA") && //$NON-NLS-1$
                    algURI.equalsIgnoreCase(SignatureMethod.RSA_SHA1);
        }
    }

    private static final class SimpleKeySelectorResult implements KeySelectorResult {
        private final PublicKey pk;
        SimpleKeySelectorResult(final PublicKey pk) {
            this.pk = pk;
        }

        @Override
		public Key getKey() { return this.pk; }
    }
}
