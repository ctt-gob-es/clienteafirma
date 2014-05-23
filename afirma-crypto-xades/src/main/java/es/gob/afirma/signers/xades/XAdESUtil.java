package es.gob.afirma.signers.xades;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import net.java.xades.security.xml.XAdES.CommitmentTypeIdImpl;
import net.java.xades.security.xml.XAdES.CommitmentTypeIndication;
import net.java.xades.security.xml.XAdES.CommitmentTypeIndicationImpl;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;

final class XAdESUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_ORIGIN = "urn:oid:1.2.840.113549.1.9.16.6.1"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_RECEIPT = "urn:oid:1.2.840.113549.1.9.16.6.2"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_DELIVERY = "urn:oid:1.2.840.113549.1.9.16.6.3"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_SENDER = "urn:oid:1.2.840.113549.1.9.16.6.4"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_APPROVAL = "urn:oid:1.2.840.113549.1.9.16.6.5"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_CREATION = "urn:oid:1.2.840.113549.1.9.16.6.6"; //$NON-NLS-1$
	private static final Map<String, String> COMMITMENT_TYPE_IDENTIFIERS = new HashMap<String, String>(6);
	static {
		COMMITMENT_TYPE_IDENTIFIERS.put("1", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_ORIGIN); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("2", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_RECEIPT); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("3", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_DELIVERY); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("4", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_SENDER); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("5", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_APPROVAL); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("6", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_CREATION); //$NON-NLS-1$
	}

	private XAdESUtil() {
		// No permitimos la instanciacion
	}

	static Element getFirstElmentFromXPath(final String xpathExpression, final Element sourceElement) throws AOException {
		final NodeList nodeList;
		try {
			 nodeList = (NodeList)XPathFactory.newInstance().newXPath().evaluate(xpathExpression, sourceElement, XPathConstants.NODESET);
		}
		catch (final XPathExpressionException e1) {
			throw new AOException(
				"No se ha podido evaluar la expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "'): " + e1, //$NON-NLS-1$ //$NON-NLS-2$
				e1
			);
		}
		if (nodeList.getLength() < 1) {
			throw new AOException(
				"La expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "') no ha devuelto ningun nodo" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		if (nodeList.getLength() > 1) {
			LOGGER.warning(
				"La expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "') ha devuelto varios nodos, se usara el primero" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		return (Element) nodeList.item(0);
	}

	static List<CommitmentTypeIndication> parseCommitmentTypeIndications(final Properties xParams, final String signedDataId) {

		final List<CommitmentTypeIndication> ret = new ArrayList<CommitmentTypeIndication>();

		if (xParams == null) {
			return ret;
		}
		String tmpStr = xParams.getProperty("commitmentTypeIndications"); //$NON-NLS-1$
		if (tmpStr == null) {
			return ret;
		}
		final int nCtis;
		try {
			nCtis = Integer.parseInt(tmpStr);
			if (nCtis < 1) {
				throw new NumberFormatException();
			}
		}
		catch(final Exception e) {
			LOGGER.severe(
				"El parametro adicional 'CommitmentTypeIndications' debe contener un valor numerico entero (el valor actual es " + tmpStr + "), no se anadira el CommitmentTypeIndication: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return ret;
		}

		String identifier;
		String description;
		ArrayList<String> documentationReferences;
		ArrayList<String> commitmentTypeQualifiers;

		for(int i=0;i<=nCtis;i++) {

			// Identifier
			tmpStr = xParams.getProperty("commitmentTypeIndication" + Integer.toString(i) + "Identifier"); //$NON-NLS-1$ //$NON-NLS-2$
			if (tmpStr == null) {
				continue;
			}
			identifier = COMMITMENT_TYPE_IDENTIFIERS.get(tmpStr);
			if (identifier == null)  {
				LOGGER.severe(
					"El identificador del CommitmentTypeIndication " + i + " no es un tipo soportado (" + tmpStr + "), se omitira y se continuara con el siguiente" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				);
				continue;
			}

			// Description
			description = xParams.getProperty("commitmentTypeIndication" + Integer.toString(i) + "Description"); //$NON-NLS-1$ //$NON-NLS-2$

			// DocumentationReferences
			tmpStr = xParams.getProperty("commitmentTypeIndication" + Integer.toString(i) + "DocumentationReferences"); //$NON-NLS-1$ //$NON-NLS-2$
			if (tmpStr == null) {
				documentationReferences = new ArrayList<String>(0);
			}
			else {
				documentationReferences = new ArrayList<String>();
				final String[] docRefs = tmpStr.split(Pattern.quote("|")); //$NON-NLS-1$
				for (final String docRef : docRefs) {
					try {
						documentationReferences.add(new URL(docRef).toString());
					}
					catch (final MalformedURLException e) {
						LOGGER.severe(
							"La referencia documental '" + docRef + "' del CommitmentTypeIndication " + i + " no es una URL, se omitira y se continuara con la siguiente referencia documental: " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						);
						continue;
					}
				}
			}

			// CommitmentTypeQualifiers
			tmpStr = xParams.getProperty("commitmentTypeIndication" + Integer.toString(i) + "CommitmentTypeQualifiers"); //$NON-NLS-1$ //$NON-NLS-2$
			if (tmpStr == null) {
				commitmentTypeQualifiers = new ArrayList<String>(0);
			}
			else {
				commitmentTypeQualifiers = new ArrayList<String>();
				final String[] ctqs = tmpStr.split(Pattern.quote("|")); //$NON-NLS-1$
				for (final String ctq : ctqs) {
					commitmentTypeQualifiers.add(ctq);
				}
			}

			ret.add(
				new CommitmentTypeIndicationImpl(
					new CommitmentTypeIdImpl(
						"OIDAsURN",				// OID como URI u OID como URN //$NON-NLS-1$
						identifier,				// Un OID
						description,			// Descripcion textual (opcional)
						documentationReferences	// Lista de URL (opcional)
					),
					"#" + signedDataId,			// Una URI //$NON-NLS-1$
					commitmentTypeQualifiers	// Lista de elementos textuales (opcional)
				)
			);
		}
		return ret;
	}

}
