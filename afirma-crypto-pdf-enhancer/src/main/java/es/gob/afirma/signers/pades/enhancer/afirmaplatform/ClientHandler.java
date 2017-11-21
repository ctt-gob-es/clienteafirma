/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades.enhancer.afirmaplatform;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.axis.AxisFault;
import org.apache.axis.MessageContext;
import org.apache.axis.SOAPPart;
import org.apache.axis.handlers.BasicHandler;
import org.apache.ws.security.WSConstants;
import org.apache.ws.security.components.crypto.Crypto;
import org.apache.ws.security.components.crypto.CryptoFactory;
import org.apache.ws.security.message.WSSecHeader;
import org.apache.ws.security.message.WSSecSignature;
import org.apache.ws.security.message.WSSecUsernameToken;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

final class ClientHandler extends BasicHandler {

	private static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Opci&oacute;n de seguridad <code>UserNameToken</code>. */
	static final String USERNAMEOPTION = WSConstants.USERNAME_TOKEN_LN;

	/** Opci&oacute;n de seguridad <code>BinarySecurityToken</code>. */
	static final String CERTIFICATEOPTION = WSConstants.BINARY_TOKEN_LN;

	/** Sin seguridad. */
	static final String NONEOPTION = "none"; //$NON-NLS-1$

	static final String DIGESTPASSWORD = "DIGEST"; //$NON-NLS-1$
	static final String TEXTPASSWORD = "TEXT"; //$NON-NLS-1$

	private static final long serialVersionUID = 1L;

	// Opciones de seguridad

	/** Opci&oacute;n de seguridad del objeto actual. */
	private String securityOption = null;

	/** Usuario para el <i>token</i> de seguridad <code>UserNameToken</code>. */
	private String usernameTokenName = null;

	/** Password para la etiqueta de seguridad <code>UserNameToken</code>. */
	private String usernameTokenPassword = null;

	/** Tipo de contrase&ntilde;a para el <code>UserNameTokenPassword</code>. */
	private String usernameTokenPasswordType = null;

	/** Localizaci&oacute;n del <code>KeyStore</code> con certificado y clave privada de usuario. */
	private String keystoreLocation = null;

	/** Tipo de <code>KeyStore</code>. */
	private String keystoreType = null;

	/** Clave del <code>KeyStore</code>. */
	private String keystorePassword = null;

	/** Alias del certificado usado para firmar la etiqueta <code>soapBody</code> de la petici&oacute;n y
	 * que ser&aacute; alojado en la etiqueta <code>BinarySecurityToken</code>. */
	private String keystoreCertAlias = null;

	/** Contrase&ntilde;a del certificado usado para firmar la etiqueta <code>soapBody</code> de la
	 * petici&oacute;n y que ser&aacute; alojado en la etiqueta <code>BinarySecurityToken</code>. */
	private String keystoreCertPassword = null;

	/** Inicializa el atributo <code>securityOption</code>.
	 * @param config Propiedades de configuraci&oacute;n. */
	ClientHandler(final Properties config) {
		if (config == null) {
			throw new IllegalArgumentException("Fichero de configuracion de propiedades nulo"); //$NON-NLS-1$
		}
		try {
			this.securityOption = config.getProperty("security.mode").toUpperCase(); //$NON-NLS-1$
			this.usernameTokenName = config.getProperty("security.usertoken.user"); //$NON-NLS-1$
			this.usernameTokenPassword = config.getProperty("security.usertoken.password"); //$NON-NLS-1$
			this.usernameTokenPasswordType = config.getProperty("security.usertoken.passwordType"); //$NON-NLS-1$
			this.keystoreLocation = config.getProperty("security.keystore.location"); //$NON-NLS-1$
			this.keystoreType = config.getProperty("security.keystore.type"); //$NON-NLS-1$
			this.keystorePassword = config.getProperty("security.keystore.password"); //$NON-NLS-1$
			this.keystoreCertAlias = config.getProperty("security.keystore.cert.alias"); //$NON-NLS-1$
			this.keystoreCertPassword = config.getProperty("security.keystore.cert.password"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new IllegalArgumentException("Error leyendo el fichero de configuracion de seguridad: " + e, e); //$NON-NLS-1$
		}
		if (
			!this.securityOption.equals(USERNAMEOPTION.toUpperCase()) &&
			!this.securityOption.equals(CERTIFICATEOPTION.toUpperCase()) &&
			!this.securityOption.equals(NONEOPTION.toUpperCase())
		) {
			throw new IllegalArgumentException("Opcion de seguridad no valida: " + this.securityOption); //$NON-NLS-1$
		}
	}

	@Override
	public void invoke(final MessageContext msgContext) throws AxisFault {

		try {
			//Obtencion del documento XML que representa la peticion SOAP
			final SOAPMessage msg = msgContext.getCurrentMessage();

			final Document doc = ((org.apache.axis.message.SOAPEnvelope) msg.getSOAPPart().getEnvelope()).getAsDocument();

			//Aseguramiento de la peticion SOAP segon la opcion de seguridad configurada
			final SOAPMessage secMsg;
			if (this.securityOption.equals(USERNAMEOPTION.toUpperCase())) {
				secMsg = createUserNameToken(doc);
			}
			else if (this.securityOption.equals(CERTIFICATEOPTION.toUpperCase())) {
				secMsg = createBinarySecurityToken(doc);
			}
			else {
				secMsg = msgContext.getMessage();
			}

			if (!this.securityOption.equals(NONEOPTION.toUpperCase())) {
				//Modificacion de la peticion SOAP
				((SOAPPart) msgContext.getRequestMessage().getSOAPPart()).setCurrentMessage(secMsg.getSOAPPart().getEnvelope(), SOAPPart.FORM_SOAPENVELOPE);
			}
		}
		catch (final Exception e) {
			LOGGER.severe("Excepcion al invocar al servicio de mejora de la firma: " + e); //$NON-NLS-1$
			AxisFault.makeFault(e);
		}
		catch (final Error e) {
			LOGGER.severe("Error al invocar al servicio de mejora de la firma: " + e); //$NON-NLS-1$
			// Encapsulamos para permitir su uso con makeFault(Exception)
			AxisFault.makeFault(new IOException(e));
		}
	}

	/** Asegura, mediante la etiqueta <code>userNameToken</code>, una petici&oacute;n SOAP no segura.
	 * @param soapEnvelopeRequest Documento XML que representa la petici&oacute;n SOAP no segura.
	 * @return Un mensaje SOAP que contiene la petici&oacute;n SOAP de entrada asegurada
	 *         mediante la etiqueta <code>userNameToken</code>.
	 * @throws TransformerException Si hay problemas en el tratamiento del XML.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 * @throws SOAPException Si hay problemas en el manejo SOAP. */
	private SOAPMessage createUserNameToken(final Document soapEnvelopeRequest) throws TransformerException, IOException, SOAPException {

		// Insercion del tag wsse:Security y userNameToken
		final WSSecHeader wsSecHeader = new WSSecHeader(null, false);
		final WSSecUsernameToken wsSecUsernameToken = new WSSecUsernameToken();
		if (TEXTPASSWORD.equalsIgnoreCase(this.usernameTokenPasswordType)) {
			wsSecUsernameToken.setPasswordType(WSConstants.PASSWORD_TEXT);
		}
		else if (DIGESTPASSWORD.equalsIgnoreCase(this.usernameTokenPasswordType)) {
			wsSecUsernameToken.setPasswordType(WSConstants.PASSWORD_DIGEST);
		}
		else {
			LOGGER.severe("Tipo de contrasena no valida: " + this.usernameTokenPasswordType); //$NON-NLS-1$
			throw new SOAPException("No se ha especificado un tipo de contrasena valida"); //$NON-NLS-1$
		}
		wsSecUsernameToken.setUserInfo(this.usernameTokenName, this.usernameTokenPassword);
		wsSecHeader.insertSecurityHeader(soapEnvelopeRequest);
		wsSecUsernameToken.prepare(soapEnvelopeRequest);

		// Aoadimos una marca de tiempo inidicando la fecha de creacion del tag
		wsSecUsernameToken.addCreated();
		wsSecUsernameToken.addNonce();

		// Modificacion de la peticion
		final Document secSOAPReqDoc = wsSecUsernameToken.build(soapEnvelopeRequest, wsSecHeader);
		final Element element = secSOAPReqDoc.getDocumentElement();

		// Transformacion del elemento DOM a String
		final DOMSource source = new DOMSource(element);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final StreamResult streamResult = new StreamResult(baos);
		TransformerFactory.newInstance().newTransformer().transform(source, streamResult);
		final String secSOAPReq = new String(baos.toByteArray());

		//Creacion de un nuevo mensaje SOAP a partir del mensaje SOAP securizado formado
		final MessageFactory mf = new org.apache.axis.soap.MessageFactoryImpl();
		return mf.createMessage(null, new ByteArrayInputStream(secSOAPReq.getBytes()));
	}

	/** Asegura, mediante la etiqueta BinarySecurityToken y firma, una petici&oacute;n SOAP no securizada.
	 * @param soapEnvelopeRequest Documento XML que representa la petici&oacute;n SOAP sin securizar.
	 * @return Un mensaje SOAP que contiene la petici&oacute;n SOAP de entrada securizada
	 *         mediante la etiqueta BinarySecurityToken.
     * @throws TransformerException Si hay problemas con las transformaciones XML.
     * @throws IOException Si hay problemas en el tratamiento de datos.
     * @throws SOAPException Si hay problemas en el servicio Web. */
	private SOAPMessage createBinarySecurityToken(final Document soapEnvelopeRequest) throws TransformerException,
	                                                                                         IOException,
	                                                                                         SOAPException {
		//Insercion del tag wsse:Security y BinarySecurityToken
		final WSSecHeader wsSecHeader = new WSSecHeader(null, false);
		final WSSecSignature wsSecSignature = new WSSecSignature();
		final Crypto crypto = CryptoFactory.getInstance("org.apache.ws.security.components.crypto.Merlin", initializateCryptoProperties()); //$NON-NLS-1$

		//Indicacion para que inserte el tag BinarySecurityToken
		wsSecSignature.setKeyIdentifierType(WSConstants.BST_DIRECT_REFERENCE);
		wsSecSignature.setUserInfo(this.keystoreCertAlias, this.keystoreCertPassword);
		wsSecHeader.insertSecurityHeader(soapEnvelopeRequest);
		wsSecSignature.prepare(soapEnvelopeRequest, crypto, wsSecHeader);

		//Modificacion y firma de la peticion
		final Document secSOAPReqDoc = wsSecSignature.build(soapEnvelopeRequest, crypto, wsSecHeader);
		final Element element = secSOAPReqDoc.getDocumentElement();

		//Transformacion del elemento DOM a String
		final DOMSource source = new DOMSource(element);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final StreamResult streamResult = new StreamResult(baos);
		TransformerFactory.newInstance().newTransformer().transform(source, streamResult);
		final String secSOAPReq = new String(baos.toByteArray());
		LOGGER.info("SOAP Request: " + secSOAPReq); //$NON-NLS-1$

		//Creacion de un nuevo mensaje SOAP a partir del mensaje SOAP securizado formado
		final MessageFactory mf = new org.apache.axis.soap.MessageFactoryImpl();
		return mf.createMessage(null, new ByteArrayInputStream(secSOAPReq.getBytes()));
	}

	/** Establece el conjunto de propiedades con el que ser&aacute; inicializado el
	 * gestor criptogr&aacute;fico de WSS4J.
	 * @return Devuelve el conjunto de propiedades con el que ser&aacute; inicializado
	 *        el gestor criptogr&aacute;fico de WSS4J. */
	private Properties initializateCryptoProperties() {
		final Properties res = new Properties();
		res.put("org.apache.ws.security.crypto.provider", "org.apache.ws.security.components.crypto.Merlin"); //$NON-NLS-1$ //$NON-NLS-2$
		res.put("org.apache.ws.security.crypto.merlin.keystore.type", this.keystoreType); //$NON-NLS-1$
		res.put("org.apache.ws.security.crypto.merlin.keystore.password", this.keystorePassword); //$NON-NLS-1$
		res.put("org.apache.ws.security.crypto.merlin.keystore.alias", this.keystoreCertAlias); //$NON-NLS-1$
		res.put("org.apache.ws.security.crypto.merlin.alias.password", this.keystoreCertPassword); //$NON-NLS-1$
		res.put("org.apache.ws.security.crypto.merlin.file", this.keystoreLocation); //$NON-NLS-1$
		return res;
	}
}