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
import java.security.InvalidParameterException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
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
import org.apache.ws.security.components.crypto.CredentialException;
import org.apache.ws.security.components.crypto.Crypto;
import org.apache.ws.security.components.crypto.CryptoFactory;
import org.apache.ws.security.message.WSSecHeader;
import org.apache.ws.security.message.WSSecSignature;
import org.apache.ws.security.message.WSSecUsernameToken;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


class ClientHandler extends BasicHandler {
	private static Logger logger = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	/** Opcion de seguridad UserNameToken */
	static final String USERNAMEOPTION = WSConstants.USERNAME_TOKEN_LN;
	/** Opcion de seguridad BinarySecurityToken */
	static final String CERTIFICATEOPTION = WSConstants.BINARY_TOKEN_LN;
	/** Sin seguridad */
	static final String NONEOPTION = "none"; //$NON-NLS-1$
	static final String DIGESTPASSWORD = "DIGEST"; //$NON-NLS-1$
	static final String TEXTPASSWORD = "TEXT"; //$NON-NLS-1$
	private static final long serialVersionUID = 1L;
	// Opciones de seguridad
	// Opcion de seguridad del objeto actual
	private String securityOption = null;
	// Usuario para el token de seguridad UserNameToken.
	private String usernameTokenName = null;
	// Password para el token de seguridad UserNameToken
	private String usernameTokenPassword = null;
	// Tipo de password para el UserNameTokenPassword
	private String usernameTokenPasswordType = null;
	// Localizacion del keystore con certificado y clave privada de usuario
	private String keystoreLocation = null;
	// Tipo de keystore
	private String keystoreType = null;
	// Clave del keystore
	private String keystorePassword = null;
	// Alias del certificado usado para firmar el tag soapBody de la peticion y que sero alojado en el token BinarySecurityToken
	private String keystoreCertAlias = null;
	// Password del certificado usado para firmar el tag soapBody de la peticion y que sero alojado en el token BinarySecurityToken
	private String keystoreCertPassword = null;

	/**
	 * Inicializa el atributo securityOption
	 * @param securityOption opcion de seguridad.
	 * @throws AxisFault
	 * @throws Exception
	 */
	ClientHandler(final Properties config) throws AxisFault {
		if (config == null) {
			logger.severe("Fichero de configuracion de propiedades nulo"); //$NON-NLS-1$
			System.exit(-1);
			return;
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
		} catch (final Exception e) {
			logger.severe("Error leyendo el fichero de configuracion de securizacion: " + e); //$NON-NLS-1$
			System.exit(-1);
		}
		if (!this.securityOption.equals(USERNAMEOPTION.toUpperCase()) && !this.securityOption.equals(CERTIFICATEOPTION.toUpperCase()) && !this.securityOption.equals(NONEOPTION.toUpperCase())) {
			logger.severe("Opcion de seguridad no valida: " + this.securityOption); //$NON-NLS-1$
			AxisFault.makeFault(new InvalidParameterException("Opcion de seguridad no valida: " + this.securityOption)); //$NON-NLS-1$
		}
	}

	@Override
	public void invoke(final MessageContext msgContext) throws AxisFault {
		SOAPMessage msg,secMsg;
		Document doc = null;
		secMsg = null;
		try {
			//Obtencion del documento XML que representa la peticion SOAP
			msg = msgContext.getCurrentMessage();
			doc = ((org.apache.axis.message.SOAPEnvelope) msg.getSOAPPart().getEnvelope()).getAsDocument();
			//Securizacion de la peticion SOAP segon la opcion de seguridad configurada
			if (this.securityOption.equals(USERNAMEOPTION.toUpperCase())) {
				secMsg = createUserNameToken(doc);
			} else if (this.securityOption.equals(CERTIFICATEOPTION.toUpperCase())) {
				secMsg = createBinarySecurityToken(doc);
			} else {
				secMsg = msgContext.getMessage();
			}

			if (!this.securityOption.equals(NONEOPTION.toUpperCase())) {
				//Modificacion de la peticion SOAP
				((SOAPPart) msgContext.getRequestMessage().getSOAPPart()).setCurrentMessage(secMsg.getSOAPPart().getEnvelope(), SOAPPart.FORM_SOAPENVELOPE);
			}
		} catch (final Exception e) {
			logger.severe("Excepcion al invocar al servicio de mejora de la firma: " + e); //$NON-NLS-1$
			AxisFault.makeFault(e);
		}
		catch (final Error e) {
			logger.severe("Error al invocar al servicio de mejora de la firma: " + e); //$NON-NLS-1$
			// Encapsulamos para permitir su uso con makeFault(Exception)
			AxisFault.makeFault(new IOException(e));
		}
	}

	/**
	 * Securiza, mediante el tag userNameToken, una peticion SOAP no securizada.
	 * @param soapRequest Documento xml que representa la peticion SOAP sin securizar.
	 * @return Un mensaje SOAP que contiene la peticion SOAP de entrada securizada
	 * mediante el tag userNameToken.
	 * @throws TransformerException
	 * @throws javax.xml.transform.TransformerFactoryConfigurationError
	 * @throws IOException
	 * @throws SOAPException
	 */
	private SOAPMessage createUserNameToken(final Document soapEnvelopeRequest) throws TransformerException, IOException, SOAPException {
		ByteArrayOutputStream baos;
		Document secSOAPReqDoc;
		DOMSource source;
		Element element;
		SOAPMessage res;
		StreamResult streamResult;
		String secSOAPReq;
		WSSecUsernameToken wsSecUsernameToken;
		WSSecHeader wsSecHeader;
		// Insercion del tag wsse:Security y userNameToken
		wsSecHeader = new WSSecHeader(null, false);
		wsSecUsernameToken = new WSSecUsernameToken();
		if (TEXTPASSWORD.equalsIgnoreCase(this.usernameTokenPasswordType)) {
			wsSecUsernameToken.setPasswordType(WSConstants.PASSWORD_TEXT);
		} else if (DIGESTPASSWORD.equalsIgnoreCase(this.usernameTokenPasswordType)) {
			wsSecUsernameToken.setPasswordType(WSConstants.PASSWORD_DIGEST);
		}
		else {
			logger.severe("Tipo de password no valido: " + this.usernameTokenPasswordType); //$NON-NLS-1$
			throw new SOAPException("No se ha especificado un tipo de password valido"); //$NON-NLS-1$
		}
		wsSecUsernameToken.setUserInfo(this.usernameTokenName, this.usernameTokenPassword);
		wsSecHeader.insertSecurityHeader(soapEnvelopeRequest);
		wsSecUsernameToken.prepare(soapEnvelopeRequest);
		// Aoadimos una marca de tiempo inidicando la fecha de creacion del tag
		wsSecUsernameToken.addCreated();
		wsSecUsernameToken.addNonce();
		// Modificacion de la peticion
		secSOAPReqDoc = wsSecUsernameToken.build(soapEnvelopeRequest, wsSecHeader);
		element = secSOAPReqDoc.getDocumentElement();
		// Transformacion del elemento DOM a String
		source = new DOMSource(element);
		baos = new ByteArrayOutputStream();
		streamResult = new StreamResult(baos);
		TransformerFactory.newInstance().newTransformer().transform(source, streamResult);
		secSOAPReq = new String(baos.toByteArray());
		//Creacion de un nuevo mensaje SOAP a partir del mensaje SOAP securizado formado
		final MessageFactory mf = new org.apache.axis.soap.MessageFactoryImpl();
		res = mf.createMessage(null, new ByteArrayInputStream(secSOAPReq.getBytes()));
		return res;
	}

	/**
	 * Securiza, mediante el tag BinarySecurityToken y firma, una peticion SOAP no securizada.
	 * @param soapEnvelopeRequest Documento xml que representa la peticion SOAP sin securizar.
	 * @return Un mensaje SOAP que contiene la peticion SOAP de entrada securizada
	 * mediante el tag BinarySecurityToken.
	 * @throws javax.xml.transform.TransformerFactoryConfigurationError
	 * @throws TransformerException
	 * @throws javax.xml.transform.TransformerConfigurationException
	 * @throws SOAPException
	 * @throws IOException
	 * @throws KeyStoreException
	 * @throws CredentialException
	 * @throws CertificateException
	 * @throws NoSuchAlgorithmException
	 */
	private SOAPMessage createBinarySecurityToken(final Document soapEnvelopeRequest) throws TransformerException, IOException, SOAPException, KeyStoreException, CredentialException, NoSuchAlgorithmException, CertificateException {
		ByteArrayOutputStream baos;
		Crypto crypto;
		Document secSOAPReqDoc;
		DOMSource source;
		Element element;
		StreamResult streamResult;
		String secSOAPReq;
		SOAPMessage res;
		WSSecSignature wsSecSignature;
		WSSecHeader wsSecHeader;
		crypto = null;
		wsSecHeader = null;
		wsSecSignature = null;
		//Insercion del tag wsse:Security y BinarySecurityToken
		wsSecHeader = new WSSecHeader(null, false);
		wsSecSignature = new WSSecSignature();
		crypto = CryptoFactory.getInstance("org.apache.ws.security.components.crypto.Merlin", initializateCryptoProperties()); //$NON-NLS-1$
		//Indicacion para que inserte el tag BinarySecurityToken
		wsSecSignature.setKeyIdentifierType(WSConstants.BST_DIRECT_REFERENCE);
		wsSecSignature.setUserInfo(this.keystoreCertAlias, this.keystoreCertPassword);
		wsSecHeader.insertSecurityHeader(soapEnvelopeRequest);
		wsSecSignature.prepare(soapEnvelopeRequest, crypto, wsSecHeader);
		//Modificacion y firma de la peticion
		secSOAPReqDoc = wsSecSignature.build(soapEnvelopeRequest, crypto, wsSecHeader);
		element = secSOAPReqDoc.getDocumentElement();
		//Transformacion del elemento DOM a String
		source = new DOMSource(element);
		baos = new ByteArrayOutputStream();
		streamResult = new StreamResult(baos);
		TransformerFactory.newInstance().newTransformer().transform(source, streamResult);
		secSOAPReq = new String(baos.toByteArray());
		logger.info("SOAP Request: " + secSOAPReq); //$NON-NLS-1$
		//Creacion de un nuevo mensaje SOAP a partir del mensaje SOAP securizado formado
		final MessageFactory mf = new org.apache.axis.soap.MessageFactoryImpl();
		res = mf.createMessage(null, new ByteArrayInputStream(secSOAPReq.getBytes()));
		return res;
	}

	/**
	 * Establece el conjunto de propiedades con el que sero inicializado el gestor criptogrofico de WSS4J.
	 * @return Devuelve el conjunto de propiedades con el que sero inicializado el gestor criptogrofico de WSS4J.
	 */
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