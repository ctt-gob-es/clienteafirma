/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/** M&eacute;todos de utilidad para la gesti&oacute;n de MimeType y OID
 * identificadores de tipo de contenido. */
public final class MimeHelper {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Valor que devuelve JMimeMagic por defecto cuando no sabe identificar la extensi&oacute;n
     * o el MimeType de unos datos. */
    public static final String UNKNOWN_JMIMEMAGIC_VALUE = "???"; //$NON-NLS-1$

    /** MimeType por defecto. */
    public static final String DEFAULT_MIMETYPE = "application/octet-stream"; //$NON-NLS-1$

    /** Descriptor por defecto del tipo de contenido. */
    public static final String DEFAULT_CONTENT_DESCRIPTION = "binary"; //$NON-NLS-1$

    /** OID del tipo de datos gen&eacute;rico. */
    public static final String DEFAULT_CONTENT_OID_DATA ="1.2.840.113549.1.7.1"; //$NON-NLS-1$

    /** Mimetype de ficheros ZIP. */
    private static final String ZIP_MIMETYPE = "application/zip"; //$NON-NLS-1$

    /** Extensi&oacute;n asignada a los ficheros XML. */
    private static final String XML_EXTENSION = "xml"; //$NON-NLS-1$

    /** Descripci&oacute;n asignada por JMimeMagic a los ficheros XML. */
    private static final String XML_DESCRIPTION = "Documento XML"; //$NON-NLS-1$

    /** Mimetype de ficheros XML. */
    private static final String XML_MIMETYPE = "text/xml"; //$NON-NLS-1$

    /** Mimetype asignado por JMimeMagic a algunos ficheros Word. */
    private static final String DOC_OFFICE_MIMETYPE = "application/msword"; //$NON-NLS-1$

    /** Tabla que asocia Oids y Mimetypes. */
    private static Properties oidMimetypeProp = null;

    /** Tabla que asocia MimeType y OID. */
    private static Properties mimetypeOidProp = null;

    /** Objeto para el almac&eacute;n de la informaci&oacute;n de los datos. */
    private MimeInfo mimeInfo = null;

    /** Datos analizados. */
    private final byte[] data;

    /** Realiza el an&aacute;lisis de los datos.
     * @param data Datos que se desean analizar.
     * @throws NullPointerException Cuando se introducen datos nulos. */
    public MimeHelper(final byte[] data) {

        if (data == null) {
            throw new IllegalArgumentException("No se han indicado los datos que se desean analizar"); //$NON-NLS-1$
        }

        this.data = data;
        this.mimeInfo = new MimeInfo();

        try {
            final Method getMagicMatchMethod = Class.forName("net.sf.jmimemagic.Magic") //$NON-NLS-1$
                .getMethod("getMagicMatch", byte[].class); //$NON-NLS-1$
            final Object magicMatchObject = getMagicMatchMethod.invoke(null, this.data);

            final Class<?> magicMatchClass = Class.forName("net.sf.jmimemagic.MagicMatch"); //$NON-NLS-1$
            String mt = (String) magicMatchClass.getMethod("getMimeType", (Class[]) null).invoke(magicMatchObject, (Object[]) null); //$NON-NLS-1$
            if (UNKNOWN_JMIMEMAGIC_VALUE.equals(mt)) {
            	mt = null;
            }
            this.mimeInfo.setMimeType(mt);
            String ext = (String)magicMatchClass.getMethod("getExtension", (Class[]) null).invoke(magicMatchObject, (Object[]) null); //$NON-NLS-1$
            if (UNKNOWN_JMIMEMAGIC_VALUE.equals(ext)) {
            	ext = null;
            }
            this.mimeInfo.setExtension(ext);
            String desc = (String) magicMatchClass.getMethod("getDescription", (Class[]) null).invoke(magicMatchObject, (Object[]) null); //$NON-NLS-1$
            if (UNKNOWN_JMIMEMAGIC_VALUE.equals(desc)) {
            	desc = null;
            }
            this.mimeInfo.setDescription(desc);
        }
        catch (final ClassNotFoundException e) {
            LOGGER.warning("No se encontro la biblioteca JMimeMagic para la deteccion del tipo de dato: " + e); //$NON-NLS-1$
        }
        catch (final Exception e) {
            try {
                final Class<?> magicMatchNotFoundException = Class.forName("net.sf.jmimemagic.MagicMatchNotFoundException"); //$NON-NLS-1$
                if (e.getCause() != null && magicMatchNotFoundException.isInstance(e.getCause())) {
                    LOGGER.log(Level.WARNING, "No se pudo detectar el formato de los datos: " + e); //$NON-NLS-1$
                }
                else {
                    LOGGER.log(Level.WARNING, "Error durante el analisis de la cabecera de los datos: " + e, e); //$NON-NLS-1$
                }
            }
            catch (final Exception e2) {
                LOGGER.warning("Error al cargar las bibliotecas de deteccion del tipo de dato: " + e2); //$NON-NLS-1$
            }
        }
    }

    /** Obtiene el OID correspondiente a un MimeType concreto. Si no conoce el
     * OID asociado, devuelve el OID por defecto para datos binarios
     * (DEFAULT_CONTENT_OID_DATA).
     * @param mimetype MimeType del que deseamos obtener el OID.
     * @return OID asociado al MimeType.
     * @throws IOException Si no se puede cargar la tabla de correspondencias entre MimeType y OID. */
    public static String transformMimeTypeToOid(final String mimetype) throws IOException {
        if (mimetypeOidProp == null) {
            loadMimetypeOidProperties();
        }
        return mimetype == null ?
        		DEFAULT_CONTENT_OID_DATA :
        			mimetypeOidProp.getProperty(mimetype, DEFAULT_CONTENT_OID_DATA);
    }

    /** Obtiene el MimeType correspondiente a un OID concreto. Si no conoce el MimeType
     * asociado, devuelve el gen&eacute;rico de datos binarios (DEFAULT_MIMETYPE).
     * @param oid OID del que deseamos obtener el MimeType.
     * @return MimeType asociado al OID.
     * @throws IOException Si no se puede cargar la tabla de correspondencias entre MimeType y OID. */
    public static String transformOidToMimeType(final String oid) throws IOException {
        if (oidMimetypeProp == null) {
            loadOidMimetypeProperties();
        }
        return oid == null ?
        		DEFAULT_MIMETYPE :
        			oidMimetypeProp.getProperty(oid, DEFAULT_MIMETYPE);
    }

    /** Carga el fichero de propiedades que relaciona OID de formato con su MimeType
     * correspondiente.
     * @throws IOException Cuando hay errores en la carga del fichero de propiedades. */
    private static void loadOidMimetypeProperties() throws IOException {
        oidMimetypeProp = new Properties();
        try (
    		final InputStream isProp = MimeHelper.class.getClassLoader().getResourceAsStream(
				"resources/mimetypes_oids.properties" //$NON-NLS-1$
			);
		) {
	        if (isProp == null) {
	        	throw new IOException(
        			"No se ha encontrado el fichero de recursos para la relacion entre OID y MimeType" //$NON-NLS-1$
    			);
	        }
	        oidMimetypeProp.load(isProp);
        }
    }

    /** Carga la tabla de relaci&oacute;n de MimeType y OID.
     * @throws IOException Cuando hay errores en la carga del fichero de propiedades */
    private static void loadMimetypeOidProperties() throws IOException {
        if (oidMimetypeProp == null) {
            loadOidMimetypeProperties();
        }
        mimetypeOidProp = new Properties();
        for (final String key : oidMimetypeProp.keySet().toArray(new String[0])) {
            mimetypeOidProp.put(oidMimetypeProp.get(key), key);
        }
    }

    /** Recupera el MimeType de los datos analizados.
     * @return MimeType de los datos o <code>null</code> si no se
     * pudo detectar.
     * @throws IOException Si no se pueden analizar los datos. */
    public String getMimeType() throws IOException {

        // Comprobamos si ya se verifico previamente el tipo de datos
    	if (this.mimeInfo.isMimeTypeVerified()) {
    		return this.mimeInfo.getMimeType();
    	}

    	String mType = this.mimeInfo.getMimeType();

    	// Si no hubo analisis inicial o este indico que los datos son XML, comprobamos
    	// si los datos son XML en realidad
    	if (mType == null || XML_MIMETYPE.equals(mType)) {
    		if (AOFileUtils.isXML(this.data)) {
    			this.mimeInfo.setMimeType(XML_MIMETYPE);
    			this.mimeInfo.setMimeTypeVerified(true);
    		}
    		else {
    			mType = null;
    		}
    	}

    	// Cuando el MimeType sea el de un fichero ZIP o el de Microsoft Word, comprobamos si es en
    	// realidad alguno de los ficheros ofimaticos soportados
    	if (ZIP_MIMETYPE.equals(mType) || DOC_OFFICE_MIMETYPE.equals(mType)) {
    		this.mimeInfo.setMimeType(OfficeAnalizer.getMimeType(this.data));
    		this.mimeInfo.setMimeTypeVerified(true);
    	}

    	if (mType == null) {
    		this.mimeInfo.setMimeType(DEFAULT_MIMETYPE);
    		this.mimeInfo.setMimeTypeVerified(true);
    	}

        return this.mimeInfo.getMimeType();
    }

    /** Recupera la extensi&oacute;n com&uacute;n para un fichero con los datos analizados.
     * @return Extensi&oacute;n para un fichero de datos o {@code null} si no se conoce el
     *         tipo. La extensi&oacute;n se devuelve sin el punto separador.
     * @throws IOException Cuando no se pueden analizar los datos. */
    public String getExtension() throws IOException {

        // Si ya se verifico previamente la extension, la devolvemos directamente
    	if (this.mimeInfo.isExtensionVerified()) {
    		return this.mimeInfo.getExtension();
    	}

    	// Obtenemos la extension asociada al tipo de datos indicado o, si no se identifica,
    	// confiamos en el valor inicial
   		final String mType = getMimeType();
    	final String ext = verifyExtension(mType);
    	if (ext != null) {
    		this.mimeInfo.setExtension(ext);
    	}
		this.mimeInfo.setExtensionVerified(true);

		return this.mimeInfo.getExtension();
    }

    /**
     * Obtiene la extensi&oacute;n que debe corresponder al tipo de dato en base a
     * una serie de comprobaciones no englobadas en el an&aacute;lisis incial.
     * @param mimeType Tipo de dato.
     * @return Extensi&oacute;n que le corresponde a un fichero con esos datos o
     * {@code null} si no se conoce.
     */
    private static String verifyExtension(final String mimeType) {

    	if (DEFAULT_MIMETYPE.equals(mimeType)) {
    		return null;
    	}
    	if (XML_MIMETYPE.equals(mimeType)) {
    		return XML_EXTENSION;
    	}

    	if (mimeType != null) {
    		final String ext = OfficeAnalizer.getExtension(mimeType);
    		if (ext != null) {
    			return ext;
    		}
    	}

		return null;
	}

	/** Recupera la descripci&oacute;n de los datos analizados.
     * @return Descripci&oacute;n del tipo de dato.
	 * @throws IOException Cuando no se pueden analizar los datos. */
    public String getDescription() throws IOException {

    	// Si ya se verifico previamente la descripcion, la devolvemos directamente
    	if (this.mimeInfo.isDescriptionVerified()) {
    		return this.mimeInfo.getDescription();
    	}

    	// Si se verifico el tipo de los datos, identificamos la descripcion que le
    	// correponderia si es alguno de los datos registrados, o usamos el tipo
    	// preasignado si no lo tenemos identificado como tipo a verificar
    	final String mType = getMimeType();
    	String desc = verifyDescription(mType);
    	if (desc == null) {
    		if (this.mimeInfo.getDescription() != null) {
    			desc = this.mimeInfo.getDescription();
    		} else {
    			desc = DEFAULT_CONTENT_DESCRIPTION;
    		}
    	}

		this.mimeInfo.setDescription(desc);
		this.mimeInfo.setDescriptionVerified(true);

		return this.mimeInfo.getDescription();
    }

    /**
     * Obtiene la descripci&oacute;n que debe corresponder al tipo de dato en base a
     * una serie de comprobaciones no englobadas en el an&aacute;lisis incial.
     * @param mimeType Tipo de dato.
     * @return Descripci&oacute;n que le corresponde a un fichero con esos datos o
     * {@code null} si no se conoce.
     */
    private static String verifyDescription(final String mimeType) {

    	if (XML_MIMETYPE.equals(mimeType)) {
    		return XML_DESCRIPTION;
    	}

    	if (mimeType != null && !mimeType.equals(DEFAULT_MIMETYPE)) {
    		final String desc = OfficeAnalizer.getDescription(mimeType);
    		if (desc != null) {
    			return desc;
    		}
    	}

		return null;
	}

	/**
     * Indica si unos datos son un fichero ZIP, independientemente de que ese ZIP se
     * corresponda con otro formato basado en el mismo (como los ficheros DOCX, ODT,...).
     * @return {@code true} si los datos son un ZIP, {@code false} en caso de que no
     * sean un ZIP o que no se puedan analizar.
     */
	public boolean isZipData() {

    	// Comprobamos si JMimeMagic lo detecto como ZIP
    	if (this.mimeInfo != null) {
   			return ZIP_MIMETYPE.equals(this.mimeInfo.getMimeType());
    	}

		if (this.data.length < 4) {
			return false;
		}

		// Los 4 primeros bytes pueden ser 0x504B0304, 0x504B0506 o 0x504B0708
		return this.data[0] == (byte) 0x50 && this.data[1] == (byte) 0x4B && (
				this.data[2] == (byte) 0x03 && this.data[3] == (byte) 0x04 ||
				this.data[2] == (byte) 0x05 && this.data[3] == (byte) 0x06 ||
				this.data[2] == (byte) 0x07 && this.data[3] == (byte) 0x08);
	}

    /** Almacena la informaci&oacute;n identificada del tipo de datos. */
    static class MimeInfo {

        /** MimeType de los datos. */
        private String mType = null;

        /** Indica si se han realizado comprobaciones adicionales para verificar si el
         * tipo de dato es el que se ha establecido. */
        private boolean mTypeVerified = false;

        /** Extensi&oacute;n com&uacute;n para el tipo de fichero. */
        private String extension = null;

        /** Indica si se han realizado comprobaciones adicionales para verificar si la
         * extension es la que se ha establecido. */
        private boolean extensionVerified = false;

        /** Descripci&oacute;n del tipo de datos. */
        private String description = null;

        /** Indica si se han realizado comprobaciones adicionales para verificar si la
         * descripci&oacute;n es la que se ha establecido. */
        private boolean descriptionVerified = false;

        String getMimeType() {
            return this.mType;
        }

        void setMimeType(final String mimeType) {
            this.mType = mimeType;
        }

        boolean isMimeTypeVerified() {
			return this.mTypeVerified;
		}

        void setMimeTypeVerified(final boolean mTypeVerified) {
			this.mTypeVerified = mTypeVerified;
		}

        String getExtension() {
            return this.extension;
        }

        void setExtension(final String ext) {
            this.extension = ext;
        }

        boolean isExtensionVerified() {
			return this.extensionVerified;
		}

        void setExtensionVerified(final boolean extensionVerified) {
			this.extensionVerified = extensionVerified;
		}

        String getDescription() {
            return this.description;
        }

        void setDescription(final String desc) {
            this.description = desc;
        }

        boolean isDescriptionVerified() {
			return this.descriptionVerified;
		}

        void setDescriptionVerified(final boolean descriptionVerified) {
			this.descriptionVerified = descriptionVerified;
		}
    }
}
