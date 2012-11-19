/**
 * <p>Fichero: TestClient.java</p>
 * <p>Descripci�n: </p>
 * <p>Empresa: Telvent Interactiva </p>
 * <p>Fecha creaci�n: 02-dic-2005</p>
 * @author SERYS
 * @version 1.0
 */

package es.gob.afirma.platform.wsclientoriginal;

import java.io.IOException;
import java.util.Properties;

import javax.xml.namespace.QName;

import org.apache.axis.Handler;
import org.apache.axis.client.Call;
import org.apache.axis.client.Service;
import org.apache.log4j.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

public class TestClient {
   private static final Logger logger = Logger.getLogger(TestClient.class);
   private static final String CABECERA = "[TestClient]:";
   //Ruta donde se encuentran los ficheros de entrada a los servicios web
   private static final String RUTA_XML_ENTRADA = "webservices.rutaXml";
   private static final String XML_ENTRADA_PREFIX = "webservices.";
   private static String servicio;
   private static String end_point;
   private static String ruta_trusted_cacerts;
   private static String password_trusted_cacerts;
   private static String type_trusted_cacerts;
   private static String authorizationMethod = null;
   private static String authorizationKeyStorePath = null;
   private static String authorizationKeyStoreType = null;
   private static String authorizationKeyStorePassword = null;
   private static String authorizationKeyStoreCertAlias = null;
   private static String authorizationKeyStoreCertPassword = null;
   private static String authorizationName = null;
   private static String authorizationPassword = null;
   private static String authorizationPasswordType;

   private static void init(final Properties p) {

	  servicio = p.getProperty("webservices.servicio");
	  end_point = p.getProperty("webservices.endpoint");
      ruta_trusted_cacerts = p.getProperty("com.trustedstore.path");
      password_trusted_cacerts = p.getProperty("com.trustedstore.password");
      type_trusted_cacerts = p.getProperty("com.trustedstore.type");
      authorizationMethod = p.getProperty("webservices.authorization.method");
      authorizationKeyStorePath = p.getProperty("webservices.authorization.ks.path");
      authorizationKeyStoreType = p.getProperty("webservices.authorization.ks.type");
      authorizationKeyStorePassword = p.getProperty("webservices.authorization.ks.password");
      authorizationKeyStoreCertAlias = p.getProperty("webservices.authorization.ks.cert.alias");
      authorizationKeyStoreCertPassword = p.getProperty("webservices.authorization.ks.cert.password");
      authorizationName = p.getProperty("webservices.authorization.name");
      authorizationPassword = p.getProperty("webservices.authorization.password");
      authorizationPasswordType = p.getProperty("webservices.authorization.passwordType");
   }

   private static void setSystemParameters() {
      System.setProperty("javax.net.ssl.trustStore", ruta_trusted_cacerts);
      System.setProperty("javax.net.ssl.trustStorePassword", password_trusted_cacerts);
      System.setProperty("javax.net.ssl.trustStoreType", type_trusted_cacerts);
   }

   private static Properties generateHandlerProperties() {
      final Properties config = new Properties();
      config.setProperty("security.mode", authorizationMethod);
      config.setProperty("security.usertoken.user", authorizationName);
      config.setProperty("security.usertoken.password", authorizationPassword);
      config.setProperty("security.usertoken.passwordType", authorizationPasswordType);
      config.setProperty("security.keystore.location", authorizationKeyStorePath);
      config.setProperty("security.keystore.type", authorizationKeyStoreType);
      config.setProperty("security.keystore.password", authorizationKeyStorePassword);
      config.setProperty("security.keystore.cert.alias", authorizationKeyStoreCertAlias);
      config.setProperty("security.keystore.cert.password", authorizationKeyStoreCertPassword);
      return config;
   }

   private static final String APP_NAME_FIELD = "%%APPNAME%%";
   private static final String SIGN_FIELD = "%%SIGN%%";
   private static final String RETURN_TYPE_FIELD = "%%RETURNTYPE%%";

   public static byte[] upgradeSign(final byte[] sign, final String applicationName, final String returnSignType) throws IOException {
	   final String responseXml = upgradeSign(new String(
		   AOUtil.getDataFromInputStream(
			   TestClient.class.getResourceAsStream("signUpgradeTemplate.xml")
		   )
	   ).replace(APP_NAME_FIELD, applicationName).replace(SIGN_FIELD, Base64.encode(sign)).replace(RETURN_TYPE_FIELD, returnSignType));

	   if (!responseXml.contains("urn:oasis:names:tc:dss:1.0:resultmajor:Success")) {
		   throw new IOException("Error devuelto por el servidor");
	   }

	   final String SIGN_FIND_TAG_START = "<![CDATA[";
	   final String SIGN_FIND_TAG_END = "]]>";
	   final int indexStart = responseXml.indexOf(SIGN_FIND_TAG_START) + SIGN_FIND_TAG_START.length();
	   final int indexEnd = responseXml.indexOf(SIGN_FIND_TAG_END);

	   final String base64Sign = responseXml.substring(indexStart, indexEnd);

	   return Base64.decode(base64Sign);

   }

   public static String upgradeSign(final String dssXml) {

      try {

         final Properties prop = new Properties();
         try {
            prop.load(TestClient.class.getResourceAsStream("webservices.properties"));
         }
         catch (final IOException e) {
            final String msgError = "No se han podido cargar las propiedades de WebServices: " + e.getMessage();
            throw new Exception("ERR-01: " + msgError, e);
         }
         init(prop);
         setSystemParameters();

         //Se configura del endponit del servicio
         final String endpoint = end_point + servicio;

         final Properties clientHandlerInitProperties = generateHandlerProperties();
         final Handler reqHandler = new ClientHandler(clientHandlerInitProperties);

         String servicioDSS=servicio;

		 //Configuracion especifica para los perfiles DSS
		 //Servicio de Validacion y Actualizacion de Firma
		 if(servicio.equals("DSSAfirmaVerify")) {
				servicioDSS = "verify";
		 }
		 //Servicio de Firma Delegada
     	 else if(servicio.equals("DSSAfirmaSign")) {
				servicioDSS = "sign";
     	 }
		 //Servicio de Registro de Firmas
		 else if(servicio.equals("DSSAfirmaArchiveSubmit")) {
				servicioDSS = "archiveSubmit";
		 }
		 //Servicio de obtencion de firmas
		 else if(servicio.equals("DSSAfirmaArchiveRetrieval")) {
			    servicioDSS = "archiveRetrieval";
		 }

		//Se crea el servicio
		final Service  service = new Service();
		final Call call = (Call) service.createCall();
		call.setTargetEndpointAddress( new java.net.URL(endpoint) );
		call.setOperationName(new QName("http://soapinterop.org/", servicioDSS));
		call.setTimeout(new Integer(prop.getProperty("webservices.timeout")));
        call.setClientHandlers(reqHandler, null);

        //logger.info(TestClient.CABECERA + paramIn.toString());
        final String ret = (String) call.invoke(new Object[] {dssXml});
        return ret;

      }
      catch (final Exception e) {
         logger.error(TestClient.CABECERA + e.toString());
         e.printStackTrace();
         return null;
      }
   }

}