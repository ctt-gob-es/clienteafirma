/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
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
    public static final String DEFAULT_MIMETYPE = "application/octet-stream"; //$NON-NLS-1$
    
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
            Method getMagicMatchMethod = AOUtil.classForName("net.sf.jmimemagic.Magic") //$NON-NLS-1$
                .getMethod("getMagicMatch", new Class[] { byte[].class }); //$NON-NLS-1$
            Object magicMatchObject = getMagicMatchMethod.invoke(null, this.data);
        
            Class<?> magicMatchClass = AOUtil.classForName("net.sf.jmimemagic.MagicMatch"); //$NON-NLS-1$
            this.mimeInfo.setMimeType((String) magicMatchClass.getMethod("getMimeType", (Class[]) null).invoke(magicMatchObject, (Object[]) null)); //$NON-NLS-1$
            this.mimeInfo.setExtension((String)magicMatchClass.getMethod("getExtension", (Class[]) null).invoke(magicMatchObject, (Object[]) null)); //$NON-NLS-1$
            this.mimeInfo.setDescription((String) magicMatchClass.getMethod("getDescription", (Class[]) null).invoke(magicMatchObject, (Object[]) null)); //$NON-NLS-1$
        }
        catch (final ClassNotFoundException e) {
            LOGGER.warning("No se encontro la biblioteca JMimeMagic para la deteccion del tipo de dato"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            try {
                Class<?> magicMatchNotFoundException = AOUtil.classForName("net.sf.jmimemagic.MagicMatchNotFoundException"); //$NON-NLS-1$
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
                this.mimeType = this.mimeInfo.getMimeType();
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
            extension = this.mimeInfo.getExtension();
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
            return this.mimeInfo.getDescription();
        }
        return null;
    }

    /**
     * Almacena la informaci&oacute;n identificada del tipo de datos.
     */
    static class MimeInfo {
        
        /** MimeType de los datos. */
        private String mType = null;
        
        void setMimeType(final String mimeType) {
            this.mType = mimeType;
        }
        
        String getMimeType() {
            return this.mType;
        }
        
        /** Extensi&oacute;n com&uacute;n para el tipo de fichero. */
        private String extension = null;
        
        String getExtension() {
            return this.extension;
        }
        
        void setExtension(final String ext) {
            this.extension = ext;
        }
        
        String getDescription() {
            return this.description;
        }
        
        void setDescription(final String desc) {
            this.description = desc;
        }
        
        /** Descripci&oacute;n del tipo de datos. */
        private String description = null;
    }
    
}
 