/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Logger;

import javax.security.auth.x500.X500Principal;

/** M&eacute;todos generales de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3 */
public final class AOUtil {

    private AOUtil() {
        // No permitimos la instanciacion
    }

    private static final int BUFFER_SIZE = 4096;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String[] SUPPORTED_URI_SCHEMES = new String[] {
            "http", "https", "file", "urn" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    };

    private static final Charset DEFAULT_ENCODING = StandardCharsets.UTF_8;


    /** Crea una URI a partir de un nombre de fichero local o una URL.
     * @param file Nombre del fichero local o URL
     * @return URI (<code>file://</code>) del fichero local o URL
     * @throws URISyntaxException Si no se puede crear una URI soportada a partir de la cadena de entrada */
    public static URI createURI(final String file) throws URISyntaxException {

        if (file == null || file.isEmpty()) {
            throw new IllegalArgumentException("No se puede crear una URI a partir de un nulo"); //$NON-NLS-1$
        }

        String filename = file.trim();

        if (filename.isEmpty()) {
            throw new IllegalArgumentException("La URI no puede ser una cadena vacia"); //$NON-NLS-1$
        }

        // Cambiamos los caracteres Windows
        filename = filename.replace('\\', '/');

        // Realizamos los cambios necesarios para proteger los caracteres no
        // seguros
        // de la URL
        filename =
                filename.replace(" ", "%20") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("<", "%3C") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace(">", "%3E") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("\"", "%22") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("{", "%7B") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("}", "%7D") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("|", "%7C") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("^", "%5E") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("[", "%5B") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("]", "%5D") //$NON-NLS-1$ //$NON-NLS-2$
                        .replace("`", "%60"); //$NON-NLS-1$ //$NON-NLS-2$

        final URI uri = new URI(filename);

        // Comprobamos si es un esquema soportado
        final String scheme = uri.getScheme();
        for (final String element : SUPPORTED_URI_SCHEMES) {
            if (element.equals(scheme)) {
                return uri;
            }
        }

        // Si el esquema es nulo, aun puede ser un nombre de fichero valido
        // El caracter '#' debe protegerse en rutas locales
        if (scheme == null) {
            filename = filename.replace("#", "%23"); //$NON-NLS-1$ //$NON-NLS-2$
            return createURI("file://" + filename); //$NON-NLS-1$
        }

        // Miramos si el esquema es una letra, en cuyo caso seguro que es una
        // unidad de Windows ("C:", "D:", etc.), y le anado el file://
        // El caracter '#' debe protegerse en rutas locales
        if (scheme.length() == 1 && Character.isLetter((char) scheme.getBytes()[0])) {
            filename = filename.replace("#", "%23"); //$NON-NLS-1$ //$NON-NLS-2$
            return createURI("file://" + filename); //$NON-NLS-1$
        }

        throw new URISyntaxException(filename, "Tipo de URI no soportado"); //$NON-NLS-1$

    }

    /** Obtiene el flujo de entrada de un fichero (para su lectura) a partir de su URI.
     * @param uri URI del fichero a leer
     * @return Flujo de entrada hacia el contenido del fichero
     * @throws IOException Cuando no se ha podido abrir el fichero de datos. */
    public static InputStream loadFile(final URI uri) throws IOException {

        if (uri == null) {
            throw new IllegalArgumentException("Se ha pedido el contenido de una URI nula"); //$NON-NLS-1$
        }

        if (uri.getScheme().equals("file")) { //$NON-NLS-1$
            // Es un fichero en disco. Las URL de Java no soportan file://, con
            // lo que hay que diferenciarlo a mano

            // Retiramos el "file://" de la uri
            String path = uri.getSchemeSpecificPart();
            if (path.startsWith("//")) { //$NON-NLS-1$
                path = path.substring(2);
            }
            return new FileInputStream(new File(path));
        }

        // Es una URL
        final InputStream tmpStream = new BufferedInputStream(uri.toURL().openStream());

        // Las firmas via URL fallan en la descarga por temas de Sun, asi que
        // descargamos primero
        // y devolvemos un Stream contra un array de bytes
        final byte[] tmpBuffer = getDataFromInputStream(tmpStream);

        return new java.io.ByteArrayInputStream(tmpBuffer);
    }

    /** Lee un flujo de datos de entrada y los recupera en forma de array de
     * bytes. Este m&eacute;todo consume, pero no cierra el flujo de datos de
     * entrada.
     * @param input Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException Cuando ocurre un problema durante la lectura. */
    public static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes;
        final byte[] buffer = new byte[BUFFER_SIZE];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }

    /** Obtiene el nombre com&uacute;n (Common Name, CN) del titular de un
     * certificado X&#46;509. Si no se encuentra el CN, se devuelve la unidad organizativa
     * (Organization Unit, OU).
     * @param c Certificado X&#46;509 del cual queremos obtener el nombre
     *        com&uacute;n.
     * @return Nombre com&uacute;n (Common Name, CN) del titular de un
     *         certificado X&#46;509. */
    public static String getCN(final X509Certificate c) {
        if (c == null) {
            return null;
        }
        return getCN(c.getSubjectX500Principal().toString());
    }

    /** Obtiene el nombre com&uacute;n (Common Name, CN) de un <i>Principal</i>
     * X&#46;400. Si no se encuentra el CN, se devuelve la unidad organizativa
     * (Organization Unit, OU).
     * @param principal <i>Principal</i> del cual queremos obtener el nombre
     *        com&uacute;n
     * @return Nombre com&uacute;n (Common Name, CN) de un <i>Principal</i>
     *         X&#46;400 */
    public static String getCN(final String principal) {
        if (principal == null) {
            return null;
        }

        String rdn = getRDNvalueFromLdapName("cn", principal); //$NON-NLS-1$
        if (rdn == null) {
            rdn = getRDNvalueFromLdapName("ou", principal); //$NON-NLS-1$
        }

        if (rdn != null) {
            return rdn;
        }

        final int i = principal.indexOf('=');
        if (i != -1) {
            LOGGER .warning("No se ha podido obtener el Common Name ni la Organizational Unit, se devolvera el fragmento mas significativo"); //$NON-NLS-1$
            return getRDNvalueFromLdapName(principal.substring(0, i), principal);
        }

        LOGGER.warning("Principal no valido, se devolvera la entrada"); //$NON-NLS-1$
        return principal;
    }
    
    /** Obtiene las unidades organizativas(Organizational Unit, OU) de un <i>Principal</i>
     * X&#46;400. 
     * @param principal <i>Principal</i> del cual queremos obtener el nombre
     *        com&uacute;n
     * @return Unidad organizativa (Organizational Unit, OU) de un <i>Principal</i>
     *         X&#46;400 */
    public static String[] getOUS(final String principal) {
        if (principal == null) {
            return null;
        }

        final ArrayList<String> ousList = new ArrayList<String>();
        
        String ou = getRDNvalueFromLdapName("ou", principal); //$NON-NLS-1$
        String principalAux = principal;
        while (ou != null) {
        	ousList.add(ou);
        	principalAux = principalAux.replace("OU=" + ou, "");  //$NON-NLS-1$//$NON-NLS-2$
        	ou = getRDNvalueFromLdapName("ou", principalAux); //$NON-NLS-1$
        }
        
        return ousList.toArray(new String[0]);
    }

    /** Recupera el valor de un RDN (<i>Relative Distinguished Name</i>) de un principal. El valor de retorno no incluye
     * el nombre del RDN, el igual, ni las posibles comillas que envuelvan el valor.
     * La funci&oacute;n no es sensible a la capitalizaci&oacute;n del RDN. Si no se
     * encuentra, se devuelve {@code null}.
     * @param rdn RDN que deseamos encontrar.
     * @param principal Principal del que extraer el RDN (seg&uacute;n la <a href="http://www.ietf.org/rfc/rfc4514.txt">RFC 4514</a>).
     * @return Valor del RDN indicado o {@code null} si no se encuentra. */
    public static String getRDNvalueFromLdapName(final String rdn, final String principal) {

        int offset1 = 0;
        while ((offset1 = principal.toLowerCase(Locale.US).indexOf(rdn.toLowerCase(), offset1)) != -1) {

            if (offset1 > 0 && principal.charAt(offset1-1) != ',' && principal.charAt(offset1-1) != ' ') {
                offset1++;
                continue;
            }

            offset1 += rdn.length();
            while (offset1 < principal.length() && principal.charAt(offset1) == ' ') {
                offset1++;
            }

            if (offset1 >= principal.length()) {
                return null;
            }

            if (principal.charAt(offset1) != '=') {
                continue;
            }

            offset1++;
            while (offset1 < principal.length() && principal.charAt(offset1) == ' ') {
                offset1++;
            }

            if (offset1 >= principal.length()) {
                return ""; //$NON-NLS-1$
            }

            int offset2;
            if (principal.charAt(offset1) == ',') {
                return ""; //$NON-NLS-1$
            }
            else if (principal.charAt(offset1) == '"') {
                offset1++;
                if (offset1 >= principal.length()) {
                    return ""; //$NON-NLS-1$
                }

                offset2 = principal.indexOf('"', offset1);
                if (offset2 == offset1) {
                    return ""; //$NON-NLS-1$
                }
                else if (offset2 != -1) {
                    return principal.substring(offset1, offset2);
                }
                else {
                    return principal.substring(offset1);
                }
            }
            else {
                offset2 = principal.indexOf(',', offset1);
                if (offset2 != -1) {
                    return principal.substring(offset1, offset2).trim();
                }
                return principal.substring(offset1).trim();
            }
        }

        return null;
    }

    /** Identifica si un certificado es de seud&oacute;nimo.
     * @param cert Certificado que hay que comprobar.
     * @return Devuelve {@code true} si es un certificado de seud&oacute;nimo, {@code false} en caso contrario. */
    public static boolean isPseudonymCert(final X509Certificate cert) {
    	// El certificado es de seudonimo si declara la extension 2.5.4.65
    	return getRDNvalueFromLdapName("2.5.4.65", //$NON-NLS-1$
    			cert.getSubjectX500Principal().getName(X500Principal.RFC2253)) != null;
    }

    /** Equivalencias de hexadecimal a texto por la posici&oacute;n del vector.
     * Para ser usado en <code>hexify()</code> */
    private static final char[] HEX_CHARS = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
    };

    /** Convierte un vector de octetos en una cadena de caracteres que contiene
     * la representaci&oacute;n hexadecimal. Copiado directamente de
     * opencard.core.util.HexString
     * @param abyte0
     *        Vector de octetos que deseamos representar textualmente
     * @param separator
     *        Indica si han o no de separarse los octetos con un
     *        gui&oacute;n y en l&iacute;neas de 16
     * @return Representaci&oacute;n textual del vector de octetos de entrada */
    public static String hexify(final byte abyte0[], final boolean separator) {
        if (abyte0 == null) {
            return "null"; //$NON-NLS-1$
        }

        final StringBuffer stringbuffer = new StringBuffer(256);
        int i = 0;
        for (int j = 0; j < abyte0.length; j++) {
            if (separator && i > 0) {
                stringbuffer.append('-');
            }
            stringbuffer.append(HEX_CHARS[abyte0[j] >> 4 & 0xf]);
            stringbuffer.append(HEX_CHARS[abyte0[j] & 0xf]);
            ++i;
            if (i == 16) {
                if (separator && j < abyte0.length - 1) {
                    stringbuffer.append('\n');
                }
                i = 0;
            }
        }
        return stringbuffer.toString();
    }

    /** Convierte un vector de octetos en una cadena de caracteres que contiene
     * la representaci&oacute;n hexadecimal. Copiado directamente de
     * opencard.core.util.HexString
     * @param abyte0
     *        Vector de octetos que deseamos representar textualmente
     * @param separator
     *        Indica si han o no de separarse los octetos con un
     *        gui&oacute;n y en l&iacute;neas de 16
     * @return Representaci&oacute;n textual del vector de octetos de entrada */
    public static String hexify(final byte abyte0[], final String separator) {
        if (abyte0 == null) {
            return "null"; //$NON-NLS-1$
        }

        final StringBuffer stringbuffer = new StringBuffer(256);
        for (int j = 0; j < abyte0.length; j++) {
            if (separator != null && j > 0) {
                stringbuffer.append(separator);
            }
            stringbuffer.append(HEX_CHARS[abyte0[j] >> 4 & 0xf]);
            stringbuffer.append(HEX_CHARS[abyte0[j] & 0xf]);
        }
        return stringbuffer.toString();
    }

    /** Carga una librer&iacute;a nativa del sistema.
     * @param path Ruta a la libreria de sistema.
     * @throws IOException Si ocurre alg&uacute;n problema durante la carga */
    public static void loadNativeLibrary(final String path) throws IOException {
        if (path == null) {
            LOGGER.warning("No se puede cargar una biblioteca nula"); //$NON-NLS-1$
            return;
        }
        final int pos = path.lastIndexOf('.');
        final File file = new File(path);
        final File tempLibrary =
            File.createTempFile(pos < 1 ? file.getName() : file.getName().substring(0, file.getName().indexOf('.')),
                pos < 1 || pos == path.length() - 1 ? null : path.substring(pos));

        // Copiamos el fichero
        copyFile(file, tempLibrary);

        // Pedimos borrar los temporales cuando se cierre la JVM
        tempLibrary.deleteOnExit();

        LOGGER.info("Cargamos " + LoggerUtil.getCleanUserHomePath(tempLibrary.getAbsolutePath())); //$NON-NLS-1$
        System.load(tempLibrary.getAbsolutePath());
    }

    /** Copia un fichero.
     * @param source Fichero origen con el contenido que queremos copiar.
     * @param dest Fichero destino de los datos.
     * @throws IOException SI ocurre algun problema durante la copia */
    public static void copyFile(final File source, final File dest) throws IOException {
        if (source == null || dest == null) {
            throw new IllegalArgumentException("Ni origen ni destino de la copia pueden ser nulos"); //$NON-NLS-1$
        }
        try (
	        final FileInputStream is = new FileInputStream(source);
	        final FileOutputStream os = new FileOutputStream(dest);
	        final FileChannel in = is.getChannel();
	        final FileChannel out = os.getChannel();
    	) {
        	final MappedByteBuffer buf = in.map(FileChannel.MapMode.READ_ONLY, 0, in.size());
        	out.write(buf);
        }
    }

    /** Genera una lista de cadenas compuesta por los fragmentos de texto
     * separados por la cadena de separaci&oacute;n indicada. No soporta
     * expresiones regulares. Por ejemplo:<br>
     * <ul>
     * <li><b>Texto:</b> foo$bar$foo$$bar$</li>
     * <li><b>Separado:</b> $</li>
     * <li><b>Resultado:</b> "foo", "bar", "foo", "", "bar", ""</li>
     * </ul>
     * @param text
     *        Texto que deseamos dividir.
     * @param sp
     *        Separador entre los fragmentos de texto.
     * @return Listado de fragmentos de texto entre separadores.
     * @throws NullPointerException
     *         Cuando alguno de los par&aacute;metros de entrada es {@code null}. */
    public static String[] split(final String text, final String sp) {

        final ArrayList<String> parts = new ArrayList<>();
        int i = 0;
        int j = 0;
        while (i != text.length() && (j = text.indexOf(sp, i)) != -1) {
            if (i == j) {
                parts.add(""); //$NON-NLS-1$
            }
            else {
                parts.add(text.substring(i, j));
            }
            i = j + sp.length();
        }
        if (i == text.length()) {
            parts.add(""); //$NON-NLS-1$
        }
        else {
            parts.add(text.substring(i));
        }

        return parts.toArray(new String[0]);
    }

	/** Convierte un objeto de propiedades en una cadena Base64 URL SAFE.
	 * @param p Objeto de propiedades a convertir.
	 * @return Base64 URL SAFE que descodificado es un fichero de propiedades en texto plano.
	 * @throws IOException Si hay problemas en la conversi&oacute;n a Base64. */
	public static String properties2Base64(final Properties p) throws IOException {
		if (p == null) {
			return ""; //$NON-NLS-1$
		}
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final OutputStreamWriter osw = new OutputStreamWriter(baos, DEFAULT_ENCODING);
		p.store(osw, ""); //$NON-NLS-1$
		return Base64.encode(baos.toByteArray(), true);
	}

	/** Convierte una cadena Base64 en un objeto de propiedades.
	 * @param base64 Base64 que descodificado es un fichero de propiedades en texto plano.
	 * @return Objeto de propiedades.
	 * @throws IOException Si hay problemas en el proceso. */
    public static Properties base642Properties(final String base64) throws IOException {
    	final Properties p = new Properties();
    	if (base64 == null || base64.isEmpty()) {
    		return p;
    	}
    	p.load(new InputStreamReader(
    					new ByteArrayInputStream(Base64.decode(base64.replace('-', '+').replace('_', '/'))), DEFAULT_ENCODING)
    			);

    	return p;
    }

    /** Convierte un objeto de propiedades en una cadena Base64 URL SAFE.
	 * @param p Objeto de propiedades a convertir.
	 * @return Base64 URL SAFE que descodificado es un fichero de propiedades en texto plano.
	 * @throws IOException Si hay problemas en la conversi&oacute;n a Base64. */
	public static String propertiesAsString(final Properties p) throws IOException {
		if (p == null) {
			return ""; //$NON-NLS-1$
		}
		final StringBuilder buffer = new StringBuilder();
    	for (final String k : p.keySet().toArray(new String[0])) {
    		buffer.append(k).append("=").append(p.getProperty(k)).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
    	}
		return buffer.toString();
	}

    /** Indica si el JRE actual es Java 9 o superior.
     * @return <code>true</code> si el JRE actual es Java 9 o superior,
     *         <code>false</code> si es Java 8 o inferior. */
	public static boolean isJava9orNewer() {
		final String ver = System.getProperty("java.version");  //$NON-NLS-1$
		if (ver == null || ver.isEmpty()) {
			LOGGER.warning("No se ha podido determinar la version de Java"); //$NON-NLS-1$
			return false;
		}
		try {
			// Valoramos si la version tiene el patron antiguo (1.X)
			if (ver.startsWith("1.")) { //$NON-NLS-1$
				return Integer.parseInt(ver.substring(2, 3)) > 8;
			}

			// En el nuevo esquema de versionado de Java se sigue el patron [1-9][0-9]*((\.0)*\.[1-9][0-9]*)*,
			// en el que tenemos $MAJOR.$MINOR.$SECURITY (http://openjdk.java.net/jeps/223)
			String majorVer = ver;
			if (majorVer.indexOf(".") > -1) { //$NON-NLS-1$
				majorVer = majorVer.substring(0, majorVer.indexOf(".")); //$NON-NLS-1$
			}

			if (isOnlyNumber(majorVer)) {
				return Integer.parseInt(majorVer) > 8;
			}
		}
		catch(final Exception e) {
			LOGGER.warning("No se ha podido determinar la version de Java (" + ver + "):" + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return false;
	}


	/** Comprueba si el texto es un n&uacute;mero.
	 * @param value Texto a comprobar.
	 * @return <code>true</code> si el texto es un n&uacute;mero, <code>false</code>
	 *         en caso contrario. */
	public static boolean isOnlyNumber(final String value) {
		if (value == null || value.isEmpty()) {
			return false;
		}
	    return value.matches("^[0-9]+$"); //$NON-NLS-1$
	}

}

