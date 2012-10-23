package es.gob.afirma.platform.wsclientoriginal;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.InvalidParameterException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.util.Properties;

import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.axis.AxisFault;
import org.apache.axis.MessageContext;
import org.apache.axis.SOAPPart;
import org.apache.axis.handlers.BasicHandler;
import org.apache.log4j.Logger;
import org.apache.ws.security.WSConstants;
import org.apache.ws.security.components.crypto.CredentialException;
import org.apache.ws.security.components.crypto.Crypto;
import org.apache.ws.security.components.crypto.CryptoFactory;
import org.apache.ws.security.message.WSSecHeader;
import org.apache.ws.security.message.WSSecSignature;
import org.apache.ws.security.message.WSSecUsernameToken;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


public class ClientHandler extends BasicHandler {
	private static Logger logger = Logger.getLogger(ClientHandler.class);
	/** Opcion de seguridad UserNameToken */
	public static final String USERNAMEOPTION = WSConstants.USERNAME_TOKEN_LN;
	/** Opcion de seguridad BinarySecurityToken */
	public static final String CERTIFICATEOPTION = WSConstants.BINARY_TOKEN_LN;
	/** Sin seguridad */
	public static final String NONEOPTION = "none";
	public static final String DIGESTPASSWORD = "DIGEST";
	public static final String TEXTPASSWORD = "TEXT";
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
	public ClientHandler(final Properties config) throws AxisFault {
		if (config == null) {
			System.err.println("Fichero de configuracion de propiedades nulo");
			System.exit(-1);
		}
		try {
			this.securityOption = config.getProperty("security.mode").toUpperCase();
			this.usernameTokenName = config.getProperty("security.usertoken.user");
			this.usernameTokenPassword = config.getProperty("security.usertoken.password");
			this.usernameTokenPasswordType = config.getProperty("security.usertoken.passwordType");
			this.keystoreLocation = config.getProperty("security.keystore.location");
			this.keystoreType = config.getProperty("security.keystore.type");
			this.keystorePassword = config.getProperty("security.keystore.password");
			this.keystoreCertAlias = config.getProperty("security.keystore.cert.alias");
			this.keystoreCertPassword = config.getProperty("security.keystore.cert.password");
		} catch (final Exception e) {
			logger.error("Error leyendo el fichero de configuracion de securizacion", e);
			System.exit(-1);
		}
		if (!this.securityOption.equals(USERNAMEOPTION.toUpperCase()) && !this.securityOption.equals(CERTIFICATEOPTION.toUpperCase()) && !this.securityOption.equals(NONEOPTION.toUpperCase())) {
			logger.error("Opcion de seguridad no valida: " + this.securityOption);
			AxisFault.makeFault(new InvalidParameterException("Opcion de seguridad no valida: " + this.securityOption));
		}
	}

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
				secMsg = this.createUserNameToken(doc);
			} else if (this.securityOption.equals(CERTIFICATEOPTION.toUpperCase())) {
				secMsg = this.createBinarySecurityToken(doc);
			} else {
				secMsg = msgContext.getMessage();
			}
			/* SOLO PARA DEBUG */
			String textRequest = "";
			final ByteArrayOutputStream baos = new ByteArrayOutputStream();
			secMsg.writeTo(baos);
			textRequest = new String(baos.toByteArray());
			//logger.error(textRequest);
			/* FIN */
			if (!this.securityOption.equals(NONEOPTION.toUpperCase())) {
				//Modificacion de la peticion SOAP
				((SOAPPart) msgContext.getRequestMessage().getSOAPPart()).setCurrentMessage(secMsg.getSOAPPart().getEnvelope(), SOAPPart.FORM_SOAPENVELOPE);
			}
		} catch (final Exception e) {
			e.printStackTrace();
			AxisFault.makeFault(e);
		}
	}

	/**
	 * Securiza, mediante el tag userNameToken, una peticion SOAP no securizada.
	 * @param soapRequest Documento xml que representa la peticion SOAP sin securizar.
	 * @return Un mensaje SOAP que contiene la peticion SOAP de entrada securizada
	 * mediante el tag userNameToken.
	 * @throws TransformerConfigurationException
	 * @throws TransformerException
	 * @throws TransformerFactoryConfigurationError
	 * @throws IOException
	 * @throws SOAPException
	 */
	private SOAPMessage createUserNameToken(final Document soapEnvelopeRequest) throws TransformerConfigurationException, TransformerException, TransformerFactoryConfigurationError, IOException, SOAPException {
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
		} else {
			System.err.println("Tipo de password no valido: " + this.usernameTokenPasswordType);
			throw new SOAPException("No se ha especificado un tipo de password valido");
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
	 * @throws TransformerFactoryConfigurationError
	 * @throws TransformerException
	 * @throws TransformerConfigurationException
	 * @throws SOAPException
	 * @throws IOException
	 * @throws KeyStoreException
	 * @throws CredentialException
	 * @throws CertificateException
	 * @throws NoSuchAlgorithmException
	 */
	private SOAPMessage createBinarySecurityToken(final Document soapEnvelopeRequest) throws TransformerConfigurationException, TransformerException, TransformerFactoryConfigurationError, IOException, SOAPException, KeyStoreException, CredentialException, NoSuchAlgorithmException, CertificateException {
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
		crypto = CryptoFactory.getInstance("org.apache.ws.security.components.crypto.Merlin", this.initializateCryptoProperties());
		//Indicacion para que inserte el tag BinarySecurityToken
		wsSecSignature.setKeyIdentifierType(WSConstants.BST_DIRECT_REFERENCE);
		// wsSecSignature.setUserInfo(this.usernameTokenName, this.usernameTokenPassword);
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
		System.out.println(secSOAPReq);
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
		res.setProperty("org.apache.ws.security.crypto.provider", "org.apache.ws.security.components.crypto.Merlin");
		res.setProperty("org.apache.ws.security.crypto.merlin.keystore.type", this.keystoreType);
		res.setProperty("org.apache.ws.security.crypto.merlin.keystore.password", this.keystorePassword);
		res.setProperty("org.apache.ws.security.crypto.merlin.keystore.alias", this.keystoreCertAlias);
		res.setProperty("org.apache.ws.security.crypto.merlin.alias.password", this.keystoreCertPassword);
		res.setProperty("org.apache.ws.security.crypto.merlin.file", this.keystoreLocation);
		return res;
	}
}