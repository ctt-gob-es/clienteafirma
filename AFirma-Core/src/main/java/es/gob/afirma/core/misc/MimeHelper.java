/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.core.misc;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;


/** M&eacute;todos de utilidad para la gesti&oacute;n de MimeType y OID
 * identificadores de tipo de contenidos. */
public final class MimeHelper {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    /** MimeType por defecto. */
    public static final String DEFAULT_MIMETYPE = "application/octet-stream";
    
    /** Tabla que asocia Oids y Mimetypes. */
    private static Properties oidMimetypeProp = null;

    /** Tabla que asocia Mimetypes y Oids. */
    private static Properties mimetypeOidProp = null;

    /** Objeto para el almac&eacute;n de la informaci&oacute;n de los datos. */
    private MimeInfo mimeInfo = null;

    /** Datos analizados. */
    private final byte[] data;

    /** MimeType de los datos analizados. */
    private String mimeType = null;

    /** Realiza el an&aacute;lisis de los datos.
     * @param data
     *        Datos que se desean analizar.
     * @throws NullPointerException
     *         Cuando se introducen datos nulos. */
    public MimeHelper(final byte[] data) {

        if (data == null) {
            throw new IllegalArgumentException("No se han indicado los datos que se desean analizar"); //$NON-NLS-1$
        }

        this.data = data.clone();
        this.mimeInfo = new MimeInfo();

        try {
            Method getMagicMatchMethod = Class.forName("net.sf.jmimemagic.Magic")
                .getMethod("getMagicMatch", new Class[] { byte[].class });
            Object magicMatchObject = getMagicMatchMethod.invoke(null, this.data);
        
            Class<?> magicMatchClass = Class.forName("net.sf.jmimemagic.MagicMatch");
            this.mimeInfo.mimeType = (String) magicMatchClass.getMethod("getMimeType", (Class[]) null).invoke(magicMatchObject, (Object[]) null);
            this.mimeInfo.extension = (String) magicMatchClass.getMethod("getExtension", (Class[]) null).invoke(magicMatchObject, (Object[]) null);
            this.mimeInfo.description = (String) magicMatchClass.getMethod("getDescription", (Class[]) null).invoke(magicMatchObject, (Object[]) null);
        }
        catch (final ClassNotFoundException e) {
            LOGGER.warning("No se encontro la biblioteca JMimeMagic para la deteccion del tipo de dato"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            try {
                Class<?> magicMatchNotFoundException = Class.forName("net.sf.jmimemagic.MagicMatchNotFoundException");
                if (magicMatchNotFoundException.isInstance(e)) {
                    LOGGER.warning("No se pudo detectar el formato de los datos"); //$NON-NLS-1$
                } else {
                    LOGGER.warning("Error durante el analisis de la cabecera de los datos: " + e); //$NON-NLS-1$
                }
            } catch (final Exception e2) {
                LOGGER.warning("Error al evaluar el tipo de dato mediante JMimeMagic: " + e2); //$NON-NLS-1$
            }
        }
    }

    /** Obtiene el Oid correspondiente a un Mime Type concreto. Si no conoce el
     * Oid asociado, devuelve <code>null</code>.
     * @param mimetype
     *        del que deseamos obtener el Oid.
     * @return OID asociado al Mime Type. */
    public static String transformMimeTypeToOid(final String mimetype) {
        if (mimetypeOidProp == null) {
            loadMimetypeOidProperties();
        }
        return mimetypeOidProp.getProperty(mimetype);
    }

    /** Obtiene el Mime correspondiente a un Oid concreto. Si no conoce el Mime
     * Type asociado, devuelve <code>null</code>.
     * @param oid
     *        del que deseamos obtener el Mime Type.
     * @return MimeType asociado al OID. */
    public static String transformOidToMimeType(final String oid) {
        if (oidMimetypeProp == null) {
            loadOidMimetypeProperties();
        }
        return oidMimetypeProp.getProperty(oid);
    }

    /** Carga el properties que relaciona OIDs de formato con su mime type
     * correspondiente. */
    private static void loadOidMimetypeProperties() {
        oidMimetypeProp = new Properties();

        final InputStream isProp = MimeHelper.class.getResourceAsStream("/resources/oids_mimetypes.properties"); //$NON-NLS-1$
        try {
            oidMimetypeProp.load(isProp);
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido cargar la tabla de relaciones Oid-Mimetype: " + e); //$NON-NLS-1$
        }
        try {
            isProp.close();
        }
        catch (final Exception e) {
            // Ignoramos errores en el cierre
        }
    }

    /** Carga la tabla de relacion de MimeTypes y Oids. */
    private static void loadMimetypeOidProperties() {
        if (oidMimetypeProp == null) {
            loadOidMimetypeProperties();
        }
        mimetypeOidProp = new Properties();
        for (final String key : oidMimetypeProp.keySet().toArray(new String[0])) {
            mimetypeOidProp.put(oidMimetypeProp.get(key), key);
        }
    }

    /** Recupera el MimeType de los datos analizados, <code>null</code> si no se
     * pudo detectar.
     * @return MimeType de los datos. */
    public String getMimeType() {

        // Comprobamos si ya se calculo previamente el tipo de datos
        if (this.mimeType == null) {

            // Probamos a pasear los datos como si fuesen un XML, si no lanzan
            // una excepcion, entonces son
            // datos XML.
            try {
                DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(this.data));
                this.mimeType = "text/xml"; //$NON-NLS-1$
            }
            catch (final Exception e) {
                // Ignoramos, es porque no es XML
            }

            if (this.mimeType == null && this.mimeInfo != null) {
                this.mimeType = this.mimeInfo.mimeType;
            }

            // Cuando el MimeType sea el de un fichero ZIP, comprobamos si es en
            // realidad alguno de los ficheros ofimaticos soportados (que son ZIP
            // con una estructura concreta)
            if (this.mimeType != null && this.mimeType.equals("application/zip")) { //$NON-NLS-1$
                this.mimeType = OfficeXMLAnalizer.getMimeType(this.data);
            }
            
            if (this.mimeType == null) {
                this.mimeType = MimeHelper.DEFAULT_MIMETYPE;
            }
        }

        return this.mimeType;
    }

    /** Recupera la extensi&oacute;n com&uacute;n para un fichero con los datos analizados, {@code null} si no se conoce. La extensi&oacute;n se
     * devuelve sin el punto separador.
     * @return Extensi&oacute;n para un fichero de datos. */
    public String getExtension() {

        String extension = null;

        // Probamos a pasear los datos como si fuesen un XML, si no lanzan
        // una excepcion, entonces son datos XML.
        try {
            DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(this.data));
            extension = "xml"; //$NON-NLS-1$
        }
        catch (final Exception e) {
         // Ignoramos, es porque no es XML
        }

        if (extension == null && this.mimeInfo != null) {
            extension = this.mimeInfo.extension;
        }

        // Cuando el MimeType sea el de un fichero ZIP, comprobamos si es en
        // realidad alguno de los ficheros ofimaticos soportados (que son ZIP con una
        // estructura concreta)
        if (extension != null && extension.equals("zip")) { //$NON-NLS-1$
            extension = OfficeXMLAnalizer.getExtension(this.data);
        }

        return extension;
    }

    /** Recupera la descripcion de los datos analizados, <code>null</code> si no
     * se pudo detectar.
     * @return Descripci&oacute;n del tipo de dato. */
    public String getDescription() {
        if (this.mimeInfo != null) {
            return this.mimeInfo.description;
        }
        return null;
    }

    /**
     * Almacena la informaci&oacute;n identificada del tipo de datos.
     */
    private class MimeInfo {
        /** MimeType de los datos. */
        String mimeType = null;
        /** Extensi&oacute;n com&uacute;n para el tipo de fichero. */
        String extension = null;
        /** Descripci&oacute;n del tipo de datos. */
        String description = null;
    }
    
    // ************************************************************
    // ***************** SOPORTE DE ADJUNTOS MIME *****************
    // ************************************************************

//    /** Devuelve un binario en forma de Base64 con cabecera MIME tal y como se
//     * define en la especificaci&oacute;n MIME.
//     * @param data
//     *        Datos originales
//     * @param contentID
//     *        Identificador de los datos
//     * @param contentType
//     *        Tipo de los datos, en formato MIMEType. Si se proporciona <code>null</code> se intenta determinar
//     * @param fileName
//     *        Nombre del fichero original en el que se encontraban los datos
//     * @param modificationDate
//     *        Fecha de la &uacute;ltima modificaci&oacute;n de los datos
//     * @return Codificaci&oacute;n MIME de los datos, con cabecera */
//    public static String getMimeEncodedAsAttachment(final byte[] data,
//                                                    final String contentID,
//                                                    String contentType,
//                                                    final String fileName,
//                                                    final java.util.Date modificationDate) {
//
//        final StringBuilder sb = new StringBuilder();
//
//        sb.append("MIME-Version: 1.0\n");
//        if (contentID != null) {
//            sb.append("Content-ID: ");
//            sb.append(contentID);
//            sb.append('\n');
//        }
//
//        if (contentType == null) {
//            contentType = new MimeHelper(data).getMimeType();
//        }
//        if (contentType != null) {
//            sb.append("Content-Type: ");
//            sb.append(contentType);
//            sb.append('\n');
//        }
//
//        sb.append("Content-Disposition: attachment");
//        // De la RFC 2183:
//        // A short (length <= 78 characters)
//        // parameter value containing only non-`tspecials' characters SHOULD be
//        // represented as a single `token'.
//        // A short (length <= 78 characters) parameter value containing
//        // only ASCII characters, but including 'tspecials' characters, SHOULD
//        // be represented as 'quoted-string'.
//
//        if (fileName != null) {
//            if (!isPureAscii(fileName)) {
//                LOGGER.warning("MIME solo soporta nombres de ficheros ASCII, se ignorara el campo 'filename': " + fileName);
//            }
//            else if (fileName.length() > 78) {
//                LOGGER.warning("No se soporta la codificacion RFC 2184 para los nombres de fichero de mas de 78 caracteres, se ignorara el campo 'filename': " + fileName);
//            }
//            else {
//                sb.append("; filename=");
//                if (containsTSpecial(fileName)) {
//                    sb.append('"');
//                }
//                sb.append(fileName);
//                if (containsTSpecial(fileName)) {
//                    sb.append('"');
//                }
//                sb.append(";");
//            }
//        }
//
//        if (modificationDate != null) {
//            sb.append(" modification-date=\"");
//            sb.append(modificationDate.toString());
//            sb.append("\";");
//        }
//        sb.append('\n');
//
//        sb.append('\n');
//
//        sb.append(Base64.encode(data));
//
//        return sb.toString();
//
//    }

//    private static final String[] MIME_T_SPECIALS = new String[] {
//            "(", ")", "<", ">", "@", ",", ";", ":", "\\", "\"", "/", "[", "]", "?", "=", " "
//    };

//    private static boolean containsTSpecial(final String in) {
//        if (in == null) {
//            return false;
//        }
//        for (final String s : MIME_T_SPECIALS) {
//            if (in.indexOf(s) != -1) {
//                return true;
//            }
//        }
//        return false;
//    }

//    /** Comprueba que la cadena de texto s&oacute;lo contenga caracteres ASCII.
//     * @param v
//     *        Cadena que se desea comprobar.
//     * @return Devuelve {@code true} si todos los caracteres de la cadena son
//     *         ASCII, {@code false} en caso contrario. */
//    private static boolean isPureAscii(final String v) {
//        final byte bytearray[] = v.getBytes();
//        final java.nio.charset.CharsetDecoder d = java.nio.charset.Charset.forName("US-ASCII").newDecoder();
//        try {
//            d.decode(java.nio.ByteBuffer.wrap(bytearray)).toString();
//        }
//        catch (final java.nio.charset.CharacterCodingException e) {
//            return false;
//        }
//        return true;
//    }

    // ************************************************************
    // *** FIN SOPORTE DE ADJUNTOS MIME ***************************
    // ************************************************************
}
 