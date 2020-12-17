/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades.enhancer.afirmaplatform;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.apache.axis.Handler;
import org.apache.axis.client.Call;
import org.apache.axis.client.Service;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** A&ntilde;ade un sello de tiempo a un PDF firmado mediante la Plataforma &#64;firma. */
final class AFirmaPlatformPdfEnhancer {

   private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
   private static final String CABECERA = "[TestClient]:"; //$NON-NLS-1$
   //Ruta donde se encuentran los ficheros de entrada a los servicios web
   private static String servicio;
   private static String endPoint;
   private static String rutaTrustedCacerts;
   private static String passwordTrustedCacerts;
   private static String typeTrustedCacerts;
   private static String authorizationMethod = null;
   private static String authorizationKeyStorePath = null;
   private static String authorizationKeyStoreType = null;
   private static String authorizationKeyStorePassword = null;
   private static String authorizationKeyStoreCertAlias = null;
   private static String authorizationKeyStoreCertPassword = null;
   private static String authorizationName = null;
   private static String authorizationPassword = null;
   private static String authorizationPasswordType;


   private AFirmaPlatformPdfEnhancer() {
	   // Impedimos la construccion de objetos
   }

   private static void init(final Properties p) {

	  servicio = p.getProperty("webservices.servicio"); //$NON-NLS-1$
	  endPoint = p.getProperty("webservices.endpoint"); //$NON-NLS-1$
      rutaTrustedCacerts = p.getProperty("com.trustedstore.path"); //$NON-NLS-1$
      passwordTrustedCacerts = p.getProperty("com.trustedstore.password"); //$NON-NLS-1$
      typeTrustedCacerts = p.getProperty("com.trustedstore.type"); //$NON-NLS-1$
      authorizationMethod = p.getProperty("webservices.authorization.method"); //$NON-NLS-1$
      authorizationKeyStorePath = p.getProperty("webservices.authorization.ks.path"); //$NON-NLS-1$
      authorizationKeyStoreType = p.getProperty("webservices.authorization.ks.type"); //$NON-NLS-1$
      authorizationKeyStorePassword = p.getProperty("webservices.authorization.ks.password"); //$NON-NLS-1$
      authorizationKeyStoreCertAlias = p.getProperty("webservices.authorization.ks.cert.alias"); //$NON-NLS-1$
      authorizationKeyStoreCertPassword = p.getProperty("webservices.authorization.ks.cert.password"); //$NON-NLS-1$
      authorizationName = p.getProperty("webservices.authorization.name"); //$NON-NLS-1$
      authorizationPassword = p.getProperty("webservices.authorization.password"); //$NON-NLS-1$
      authorizationPasswordType = p.getProperty("webservices.authorization.passwordType"); //$NON-NLS-1$
   }

   private static void setSystemParameters() {
      System.setProperty("javax.net.ssl.trustStore", rutaTrustedCacerts); //$NON-NLS-1$
      System.setProperty("javax.net.ssl.trustStorePassword", passwordTrustedCacerts); //$NON-NLS-1$
      System.setProperty("javax.net.ssl.trustStoreType", typeTrustedCacerts); //$NON-NLS-1$
   }

   private static Properties generateHandlerProperties() {
      final Properties config = new Properties();
      config.setProperty("security.mode", authorizationMethod); //$NON-NLS-1$
      config.setProperty("security.usertoken.user", authorizationName); //$NON-NLS-1$
      config.setProperty("security.usertoken.password", authorizationPassword); //$NON-NLS-1$
      config.setProperty("security.usertoken.passwordType", authorizationPasswordType); //$NON-NLS-1$
      config.setProperty("security.keystore.location", authorizationKeyStorePath); //$NON-NLS-1$
      config.setProperty("security.keystore.type", authorizationKeyStoreType); //$NON-NLS-1$
      config.setProperty("security.keystore.password", authorizationKeyStorePassword); //$NON-NLS-1$
      config.setProperty("security.keystore.cert.alias", authorizationKeyStoreCertAlias); //$NON-NLS-1$
      config.setProperty("security.keystore.cert.password", authorizationKeyStoreCertPassword); //$NON-NLS-1$
      return config;
   }

   private static final String APP_NAME_FIELD = "%%APPNAME%%"; //$NON-NLS-1$
   private static final String SIGN_FIELD = "%%SIGN%%"; //$NON-NLS-1$
   private static final String RETURN_TYPE_FIELD = "%%RETURNTYPE%%"; //$NON-NLS-1$

   static byte[] upgradeSign(final byte[] sign,
		                     final String applicationName,
		                     final String returnSignType) throws IOException {

	   if (applicationName == null || applicationName.isEmpty()) {
		   throw new IllegalArgumentException(
			   "El nombre de aplicacion en la plataforma AFirma no puede ser nulo" //$NON-NLS-1$
		   );
	   }
	   if (sign == null) {
		   throw new IllegalArgumentException(
			   "La firma a mejorar no puede ser nula" //$NON-NLS-1$
		   );
	   }
	   if (returnSignType == null || returnSignType.isEmpty()) {
		   throw new IllegalArgumentException(
			   "El formato de destino de la mejora de firma no puede ser nulo ni vacio" //$NON-NLS-1$
		   );
	   }

	   final String xmlTemplate;
	   try (
		   final InputStream is = AFirmaPlatformPdfEnhancer.class.getResourceAsStream("signUpgradeTemplate.xml") //$NON-NLS-1$
	   ) {
		   xmlTemplate = new String(
			   AOUtil.getDataFromInputStream(is)
		   );
	   }

	   final String responseXml = upgradeSign(
		   xmlTemplate.replace(APP_NAME_FIELD, applicationName).replace(SIGN_FIELD, Base64.encode(sign)
	   ).replace(RETURN_TYPE_FIELD, returnSignType));

	   if (responseXml == null || responseXml.isEmpty()) {
		   throw new IOException("Error en la invocacion al servicio de mejora, la respuesta es nula o vacia"); //$NON-NLS-1$
	   }
	   if (!responseXml.contains("urn:oasis:names:tc:dss:1.0:resultmajor:Success")) { //$NON-NLS-1$
		   throw new IOException("Error devuelto por el servidor: " + responseXml); //$NON-NLS-1$
	   }

	   final String signFindTagStart = "<![CDATA["; //$NON-NLS-1$
	   final String signFindTagEnd = "]]>"; //$NON-NLS-1$
	   final int indexStart = responseXml.indexOf(signFindTagStart) + signFindTagStart.length();
	   final int indexEnd = responseXml.indexOf(signFindTagEnd);

	   final String base64Sign = responseXml.substring(indexStart, indexEnd);

	   return Base64.decode(base64Sign);
   }

   static String upgradeSign(final String dssXml) {

      try {

         final Properties prop = new Properties();
         try (
    		 final InputStream is = AFirmaPlatformPdfEnhancer.class.getResourceAsStream("webservices.properties") //$NON-NLS-1$
		 ) {
            prop.load(is);
         }
         catch (final IOException e) {
            final String msgError = "No se han podido cargar las propiedades de WebServices: " + e.getMessage(); //$NON-NLS-1$
            throw new Exception("ERR-01: " + msgError, e); //$NON-NLS-1$
         }
         init(prop);
         setSystemParameters();

         //Se configura del endponit del servicio
         final String endpoint = endPoint + servicio;

         final Properties clientHandlerInitProperties = generateHandlerProperties();
         final Handler reqHandler = new ClientHandler(clientHandlerInitProperties);

         String servicioDSS=servicio;

		 //Configuracion especifica para los perfiles DSS
		 //Servicio de Validacion y Actualizacion de Firma
		 if (servicio.equals("DSSAfirmaVerify")) { //$NON-NLS-1$
				servicioDSS = "verify"; //$NON-NLS-1$
		 }
		 //Servicio de Firma Delegada
     	 else if (servicio.equals("DSSAfirmaSign")) { //$NON-NLS-1$
				servicioDSS = "sign"; //$NON-NLS-1$
     	 }
		 //Servicio de Registro de Firmas
		 else if (servicio.equals("DSSAfirmaArchiveSubmit")) { //$NON-NLS-1$
				servicioDSS = "archiveSubmit"; //$NON-NLS-1$
		 }
		 //Servicio de obtencion de firmas
		 else if (servicio.equals("DSSAfirmaArchiveRetrieval")) { //$NON-NLS-1$
			    servicioDSS = "archiveRetrieval"; //$NON-NLS-1$
		 }

		//Se crea el servicio
		final Service  service = new Service();
		final Call call = (Call) service.createCall();
		call.setTargetEndpointAddress( new java.net.URL(endpoint) );
		call.setOperationName(new QName("http://soapinterop.org/", servicioDSS)); //$NON-NLS-1$
		call.setTimeout(Integer.valueOf(prop.getProperty("webservices.timeout"))); //$NON-NLS-1$
        call.setClientHandlers(reqHandler, null);

        return (String) call.invoke(new Object[] {dssXml});
      }
      catch (final Exception e) {
    	 LOGGER.severe(AFirmaPlatformPdfEnhancer.CABECERA + e.toString());
         return null;
      }

   }

}