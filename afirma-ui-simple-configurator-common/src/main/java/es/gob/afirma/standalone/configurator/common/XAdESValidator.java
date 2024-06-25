package es.gob.afirma.standalone.configurator.common;

import java.io.ByteArrayInputStream;
import java.security.Key;
import java.security.KeyException;
import java.security.PublicKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.crypto.AlgorithmMethod;
import javax.xml.crypto.KeySelector;
import javax.xml.crypto.KeySelectorException;
import javax.xml.crypto.KeySelectorResult;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMValidateContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyValue;
import javax.xml.crypto.dsig.keyinfo.X509Data;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.dereference.CustomUriDereferencer;

public class XAdESValidator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Valida una firma XML y, opcionalmente, la vigencia de sus certificados.
     * @param sign Firma a validar.
     * @param checkCertificates Indica si debe validarse la vigencia de los certificados.
     * @throws CertificateException Cuando se valide la vigencia del certificado no lo est&eacute;.
	 * @throws InvalidSignatureException Cuando la firma no sea v&aacute;lida.
	 * @throws AOInvalidFormatException Cuando los datos no est&eacute;n firmados con XAdES.
     */
	public static void validate(final byte[] sign, final boolean checkCertificates)
			throws CertificateException, InvalidSignatureException, AOInvalidFormatException {

        final Document doc;
        try {
            doc = SecureXmlBuilder.getSecureDocumentBuilder().parse(new ByteArrayInputStream(sign));
        }
        catch (final Exception e) {
        	throw new AOInvalidFormatException("La firma no es XML", e); //$NON-NLS-1$
        }

        // Obtenemos el elemento Signature
        final NodeList nl = doc.getElementsByTagNameNS(XMLSignature.XMLNS, "Signature"); //$NON-NLS-1$
        if (nl.getLength() == 0) {
        	throw new AOInvalidFormatException("No se encontro la firma de los datos"); //$NON-NLS-1$
        }

        try {
        	for (int i = 0; i < nl.getLength(); i++) {
        		final DOMValidateContext valContext = new DOMValidateContext(
        				new KeyValueKeySelector(),
        				nl.item(i)
        				);

        		final XMLSignature signature = Utils.getDOMFactory().unmarshalXMLSignature(valContext);
        		if (!signature.validate(valContext)) {
        			throw new InvalidSignatureException("La firma es invalida"); //$NON-NLS-1$
        		}
        		if (!signature.getSignatureValue().validate(valContext)) {
        			throw new InvalidSignatureException("El valor de la firma es invalido"); //$NON-NLS-1$
        		}

        		if (checkCertificates) {
        			final XMLSignatureFactory signerFactory = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
        			final XMLSignature signature2 = signerFactory.unmarshalXMLSignature(valContext);
        			final KeyInfo keyInfo = signature2.getKeyInfo();
        			X509Certificate certImpl = null;
        			final Iterator<?> iter = keyInfo.getContent().iterator();
        			while (iter.hasNext()) {
        				final XMLStructure kiType = (XMLStructure) iter.next();
        				//Validamos la fecha de expiracion y emision de los certificados
        				if (kiType instanceof X509Data) {
        					final X509Data xData = (X509Data) kiType;
        					final List<?> x509DataContent = xData.getContent();
        					for (int i1 = 0; i1 < x509DataContent.size(); i1++) {
        						if (x509DataContent.get(i1) instanceof X509Certificate) {
        							certImpl = (X509Certificate) x509DataContent.get(i1);
        							certImpl.checkValidity();
        						}
        					}
        				}
        			}
        		}

        		// Ahora miramos las referencias una a una
        		final Iterator<?> it = signature.getSignedInfo().getReferences().iterator();
        		while (it.hasNext()) {
        			final Reference iNext = (Reference) it.next();
        			if (!iNext.validate(valContext)) {
        				throw new InvalidSignatureException("La referencia '" + iNext.getURI() + "' de la firma es invalida"); //$NON-NLS-1$ //$NON-NLS-2$
        			}
        		}
        	}
        }
        catch (final Exception e) {
        	throw new InvalidSignatureException("No se ha podido validar la firma", e); //$NON-NLS-1$
        }
	}

	static final class KeyValueKeySelector extends KeySelector {
        @Override
		public KeySelectorResult select(final KeyInfo keyInfo,
                                        final KeySelector.Purpose purpose,
                                        final AlgorithmMethod method,
                                        final XMLCryptoContext context) throws KeySelectorException {
            if (keyInfo == null) {
                throw new KeySelectorException("Objeto KeyInfo nulo"); //$NON-NLS-1$
            }
            final List<?> list = keyInfo.getContent();

            try {
            	// Instalamos un dereferenciador nuevo que solo actua cuando falla el por defecto
            	context.setURIDereferencer(
            			new CustomUriDereferencer()
    			);
            }
            catch (final Exception e) {
            	LOGGER.warning(
        			"No se ha podido instalar un dereferenciador a medida: " + e //$NON-NLS-1$
    			);
            }

            for (int i = 0; i < list.size(); i++) {
                final XMLStructure xmlStructure = (XMLStructure) list.get(i);
                if (xmlStructure instanceof KeyValue) {
                    final PublicKey publicKey;
                    try {
                        publicKey = ((KeyValue)xmlStructure).getPublicKey();
                    }
                    catch (final KeyException ke) {
                        throw new KeySelectorException(ke);
                    }
                    return new SimpleKeySelectorResult(publicKey);
                }

                // Si no hay KeyValue intentamos sacar la clave publica del primer Certificado
                // que encontramos en X509Data
                else if (xmlStructure instanceof X509Data) {
                	final List<?> x509DataObjects = ((X509Data)xmlStructure).getContent();
                	for (final Object o : x509DataObjects) {
                		if (o instanceof Certificate) {
                            return new SimpleKeySelectorResult(((Certificate)o).getPublicKey());
                		}
                	}
                }
            }
            throw new KeySelectorException("No se ha encontrado la clave publica dentro del XML firmado"); //$NON-NLS-1$
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
