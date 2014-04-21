package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.security.GeneralSecurityException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.TransformException;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import net.java.xades.security.xml.XAdES.SignatureProductionPlace;
import net.java.xades.security.xml.XAdES.SignatureProductionPlaceImpl;
import net.java.xades.security.xml.XAdES.SignerRole;
import net.java.xades.security.xml.XAdES.SignerRoleImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_BES;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.SAXException;

import es.gob.afirma.signers.xml.XMLConstants;

/** Firmador XAdES OOXML.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class OOXMLXAdESSigner {

	private static final String ID_PACKAGE_OBJECT = "idPackageObject"; //$NON-NLS-1$
	private static final String ID_OFFICE_OBJECT = "idOfficeObject"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

    /** URI que define la versi&oacute;n por defecto de XAdES. */
    private static final String XADESNS = "http://uri.etsi.org/01903/v1.3.2#"; //$NON-NLS-1$

    private static final String XADES_SIGNATURE_PREFIX = "xd"; //$NON-NLS-1$
    private static final String XML_SIGNATURE_PREFIX = "ds"; //$NON-NLS-1$

    private OOXMLXAdESSigner() {
    	// No permitimos la instanciacion
    }

    /** Obtiene el XML de firma XAdES <i>enveloping</i> OOXML.
     * @param ooXmlDocument Documento OOXML original
     * @param algorithm Algoritmo de firma
     * @param pk Clave privada para la firma
     * @param certChain Cadena de certificados del firmante
     * @param xParams Par&aacute;metros adicionales de la firma
     * @return XML de firma
     * @throws ParserConfigurationException Si hay problemas con el analizador XML por defecto.
     * @throws GeneralSecurityException Si ocurre alg&uacute;n problema de seguridad.
     * @throws TransformerException Si hay problemas con los motores de transformadas.
     * @throws SAXException Si hay problemas en XML SAX.
     * @throws IOException Si hay problemas gen&eacute;ricos en el tratamiento de datos.
     * @throws TransformException Si no se puede aplicar alguna transformaci&oacute;n necesaria.
     * @throws XMLSignatureException Si hay problemas con la firma XML.
     * @throws MarshalException Si hay problemas con la envoltura de la firma XML. */
    static byte[] getSignedXML(final byte[] ooXmlDocument,
    		                   final String algorithm,
    						   final PrivateKey pk,
    						   final X509Certificate[] certChain,
    						   final Properties xParams) throws ParserConfigurationException,
    								                            GeneralSecurityException,
    								                            IOException,
    								                            SAXException,
    								                            TransformerException,
    								                            MarshalException,
    								                            XMLSignatureException,
    								                            TransformException  {

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		final Document docSignature = dbf.newDocumentBuilder().newDocument();

		// Instancia XADES_EPES
		final XAdES_BES xades = XAdES.newInstance(
			XAdES.BES, // XAdES
			XADESNS, // XAdES NameSpace
			XADES_SIGNATURE_PREFIX, // XAdES Prefix
			XML_SIGNATURE_PREFIX, // XMLDSig Prefix
			DigestMethod.SHA512, // DigestMethod
			docSignature, // Document
			docSignature.getDocumentElement() // Element
		);

		//*******************************************************************
		//************* ATRIBUTOS XAdES *************************************

		// SigningCertificate
		xades.setSigningCertificate(certChain[0]);

		// SignatureProductionPlace
		final SignatureProductionPlace spp = getSignatureProductionPlace(
			extraParams.getProperty("signatureProductionCity"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionProvince"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionPostalCode"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionCountry") //$NON-NLS-1$
		);
		if (spp != null) {
			xades.setSignatureProductionPlace(spp);
		}

		// SignerRole
		SignerRole signerRole = null;
		try {
			final String claimedRole = extraParams.getProperty("signerClaimedRole"); //$NON-NLS-1$
			final String certifiedRole = extraParams.getProperty("signerCertifiedRole"); //$NON-NLS-1$
			signerRole = new SignerRoleImpl();
			if (claimedRole != null) {
				signerRole.addClaimedRole(claimedRole);
			}
			if (certifiedRole != null) {
				signerRole.addCertifiedRole(certifiedRole);
			}
		}
		catch (final Exception e) {
			// Se ignoran los errores, el parametro es opcional
		}
		if (signerRole != null) {
			xades.setSignerRole(signerRole);
		}

		// SigningTime
		xades.setSigningTime(new Date());

		//************* FIN ATRIBUTOS XAdES *********************************
		//*******************************************************************

		// Creamos el objeto final de firma
		final OOXMLAdvancedSignature xmlSignature = OOXMLAdvancedSignature.newInstance(xades, ooXmlDocument);

		// Lista de referencias a firmar
		final List<Reference> referenceList = new ArrayList<Reference>();

		// Identificador de primer nivel de la firma
		final String signatureId = "xmldsig-" + UUID.randomUUID().toString(); //$NON-NLS-1$

		// Creamos la factoria de firma XML
	    XMLSignatureFactory fac;

		try {
			// Primero comprobamos si hay una version nueva de XMLSec accesible, en cuyo caso, podria
			// provocar un error el no usarla. Normalmente, ClassCastException al recuperar la factoria.
			fac =  XMLSignatureFactory.getInstance(
				"DOM", //$NON-NLS-1$
				(Provider) Class.forName("org.apache.jcp.xml.dsig.internal.dom.XMLDSigRI").newInstance() //$NON-NLS-1$
			);
			LOGGER.info("Se usara la factoria XML del XMLSec instalado"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
		}

	    // Huella digital para las referencias
	    final DigestMethod digestMethod = fac.newDigestMethod(DigestMethod.SHA512, null);

		// Anadimos los nodos especificos OOXML y las referencias a ellos

		// idPackageObject
		xmlSignature.addXMLObject(
			OOXMLPackageObjectHelper.getPackageObject(
				ID_PACKAGE_OBJECT,
				fac,
				ooXmlDocument,
				docSignature,
				signatureId
			)
		);
		referenceList.add(
			fac.newReference(
				"#" + ID_PACKAGE_OBJECT, //$NON-NLS-1$
				digestMethod,
				null,
				"http://www.w3.org/2000/09/xmldsig#Object", //$NON-NLS-1$
				null
			)
		);

		// idOfficeObject
		xmlSignature.addXMLObject(
			OOXMLOfficeObjectHelper.getOfficeObject(
				ID_OFFICE_OBJECT,
				fac,
				docSignature,
				signatureId,
				extraParams.getProperty("signatureComments"), //$NON-NLS-1$
				extraParams.getProperty("signatureAddress1"), //$NON-NLS-1$
				extraParams.getProperty("signatureAddress2") //$NON-NLS-1$
			)
		);
		referenceList.add(
			fac.newReference(
				"#" + ID_OFFICE_OBJECT, //$NON-NLS-1$
				digestMethod,
				null,
				"http://www.w3.org/2000/09/xmldsig#Object", //$NON-NLS-1$
				null
			)
		);

		xmlSignature.sign(
			certChain,
			pk,
			XMLConstants.SIGN_ALGOS_URI.get(algorithm),
			referenceList,
			signatureId
		);

		return writeXML(
			docSignature.getDocumentElement()
		);
    }

    /** Escribe un XML como texto.
     * @param node Nodo XML que queremos pasar a texto
     * @return Cadena de texto con el XML en forma de array de octetos */
    private static byte[] writeXML(final Node node) {

        // La codificacion por defecto sera UTF-8
        final String xmlEncoding = "UTF-8"; //$NON-NLS-1$

        // Primero creamos un writer
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Writer writer = null;
        try {
            writer = new OutputStreamWriter(baos, xmlEncoding);
        }
        catch (final UnsupportedEncodingException e) {
            LOGGER.warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            writer = new OutputStreamWriter(baos);
        }

        // Ahora escribimos el XML usando XALAN
        writeXMLwithXALAN(writer, node, xmlEncoding);

        try {
            DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(baos.toByteArray()));
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido recargar el XML para insertar los atributos de la cabecera, quizas la codificacion se vea afectada: " + e); //$NON-NLS-1$
            return baos.toByteArray();
        }

        // Y devolvemos el resultado como array de bytes, insertando antes la
        // cabecera de hoja de estilo
        try {
            return new String(baos.toByteArray(), xmlEncoding).getBytes(xmlEncoding);
        }
        catch (final Exception e) {
            LOGGER.warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto del sistema: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return new String(baos.toByteArray()).getBytes();
        }
    }

	private static void writeXMLwithXALAN(final Writer writer, final Node node, final String xmlEncoding) {
        final LSSerializer serializer = ((DOMImplementationLS) node.getOwnerDocument().getImplementation()).createLSSerializer();
        serializer.getDomConfig().setParameter("namespaces", Boolean.FALSE); //$NON-NLS-1$
        final com.sun.org.apache.xerces.internal.dom.DOMOutputImpl output = new com.sun.org.apache.xerces.internal.dom.DOMOutputImpl();
        output.setCharacterStream(writer);
        if (xmlEncoding != null) {
            output.setEncoding(xmlEncoding);
        }
        serializer.write(node, output);
    }

    private static SignatureProductionPlace getSignatureProductionPlace(final String city,
                                                                        final String province,
                                                                        final String postalCode,
                                                                        final String country) {
    	if (city == null && province == null && postalCode == null && country == null) {
    		return null;
    	}
    	return new SignatureProductionPlaceImpl(city, province, postalCode, country);
    }

}
