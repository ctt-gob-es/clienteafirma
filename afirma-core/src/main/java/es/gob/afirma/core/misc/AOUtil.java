/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.misc;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Locale;
import java.util.logging.Logger;

import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;

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

    /** Crea una URI a partir de un nombre de fichero local o una URL.
     * @param file Nombre del fichero local o URL
     * @return URI (<code>file://</code>) del fichero local o URL
     * @throws URISyntaxException Si no se puede crear una URI soportada a partir de la cadena de entrada */
    public static URI createURI(final String file) throws URISyntaxException {

        if (file == null || "".equals(file)) { //$NON-NLS-1$
            throw new IllegalArgumentException("No se puede crear una URI a partir de un nulo"); //$NON-NLS-1$
        }

        String filename = file.trim();

        if ("".equals(filename)) { //$NON-NLS-1$
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
     * bytes. Este m&eacute;todo consume pero no cierra el flujo de datos de
     * entrada.
     * @param input
     *        Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException
     *         Cuando ocurre un problema durante la lectura */
    public static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes = 0;
        final byte[] buffer = new byte[BUFFER_SIZE];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }

    /** Obtiene el nombre com&uacute;n (Common Name, CN) del titular de un
     * certificado X.509. Si no se encuentra el CN, se devuelve la unidad organizativa
     * (Organization Unit, OU).
     * @param c
     *        Certificado X.509 del cual queremos obtener el nombre
     *        com&uacute;n
     * @return Nombre com&uacute;n (Common Name, CN) del titular de un
     *         certificado X.509 */
    public static String getCN(final X509Certificate c) {
        if (c == null) {
            return null;
        }
        return getCN(c.getSubjectX500Principal().toString());
    }

    /** Obtiene el nombre com&uacute;n (Common Name, CN) de un <i>Principal</i>
     * X.400. Si no se encuentra el CN, se devuelve la unidad organizativa
     * (Organization Unit, OU).
     * @param principal
     *        <i>Principal</i> del cual queremos obtener el nombre
     *        com&uacute;n
     * @return Nombre com&uacute;n (Common Name, CN) de un <i>Principal</i>
     *         X.400 */
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

    /** Caracterres aceptados en una codificaci&oacute;n Base64 seg&uacute;n la
     * <a href="http://www.faqs.org/rfcs/rfc3548.html">RFC 3548</a>. Importante:
     * A&ntilde;adimos el car&aacute;cter &tilde; porque en ciertas
     * codificaciones de Base64 est&aacute; aceptado, aunque no es nada
     * recomendable */
    private static final String BASE_64_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz=_-\n+/0123456789\r~"; //$NON-NLS-1$

    /** @param data Datos a comprobar si podr6iacute;an o no ser Base64.
     * @return <code>true</code> si los datos proporcionado pueden ser una
     *         codificaci&oacute;n base64 de un original binario (que no tiene
     *         necesariamente porqu&eacute; serlo), <code>false</code> en caso
     *         contrario. */
    public static boolean isBase64(final byte[] data) {

        int count = 0;

        // Comprobamos que todos los caracteres de la cadena pertenezcan al
        // alfabeto base 64
        for (final byte b : data) {
        	if (BASE_64_ALPHABET.indexOf((char) b) == -1) {
        		return false;
        	}

        	if (b != '\n' && b != '\r') {
        		count++;
        	}
        }
        // Comprobamos que la cadena tenga una longitud multiplo de 4 caracteres
        return count % 4 == 0;
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

    /** Genera una cadena representativa del &aacute;rbol que recibe.
     * @param tree
     *        &Aacute;rbol que se desea representar.
     * @param linePrefx
     *        Prefijo de cada l&iacute;nea de firma (por defecto, cadena
     *        vac&iacute;a).
     * @param identationString
     *        Cadena para la identaci&oacute;n de los nodos de firma (por
     *        defecto, tabulador).
     * @return Cadena de texto. */
    public static String showTreeAsString(final AOTreeModel tree, final String linePrefx, final String identationString) {

        if (tree == null || tree.getRoot() == null) {
            LOGGER.severe("Se ha proporcionado un arbol de firmas vacio"); //$NON-NLS-1$
            return null;
        }

        if (!(tree.getRoot() instanceof AOTreeNode)) {
            LOGGER.severe("La raiz del arbol de firmas no es de tipo DafaultMutableTreeNode"); //$NON-NLS-1$
            return null;
        }

        final StringBuilder buffer = new StringBuilder();

        // Transformamos en cadenas de texto cada rama que surja del nodo raiz
        // del arbol
        final AOTreeNode root = (AOTreeNode) tree.getRoot();
        for (int i = 0; i < root.getChildCount(); i++) {
            archiveTreeNode(root.getChildAt(i), 0, linePrefx != null ? linePrefx : "", identationString != null ? identationString : "\t", buffer);  //$NON-NLS-1$//$NON-NLS-2$
        }

        return buffer.toString();
    }

    /** Transforma en cadena una rama completa de un &aacute;rbol. Para una
     * correcta indexaci&oacute; es necesario indicar la profundidad en la que
     * esta el nodo del que pende la rama. En el caso de las ramas que penden
     * del nodo ra&iacute;z del &aacute;rbol la profundidad es cero (0).
     * @param node
     *        Nodo del que cuelga la rama.
     * @param depth
     *        Profundidad del nodo del que pende la rama.
     * @param linePrefx
     *        Prefijo de cada l&iacute;nea de firma (por defecto, cadena
     *        vac&iacute;a).
     * @param identationString
     *        Cadena para la identaci&oacute;n de los nodos de firma (por
     *        defecto, tabulador).
     * @param buffer
     *        Buffer en donde se genera la cadena de texto. */
    private static void archiveTreeNode(final AOTreeNode node,
                                              final int depth,
                                              final String linePrefx,
                                              final String identationString,
                                              final StringBuilder buffer) {
        buffer.append('\n').append(linePrefx);
        for (int i = 0; i < depth; i++) {
            buffer.append(identationString);
        }
        buffer.append(node.getUserObject());
        for (int i = 0; i < node.getChildCount(); i++) {
            archiveTreeNode(node.getChildAt(i), depth + 1, linePrefx, identationString, buffer);
        }
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
        if (tempLibrary != null) {
            tempLibrary.deleteOnExit();
        }

        LOGGER.info("Cargamos " + (tempLibrary == null ? path : tempLibrary.getAbsolutePath())); //$NON-NLS-1$
        System.load(tempLibrary != null ? tempLibrary.getAbsolutePath() : path);

    }

    /** Copia un fichero.
     * @param source Fichero origen con el contenido que queremos copiar.
     * @param dest Fichero destino de los datos.
     * @throws IOException SI ocurre algun problema durante la copia */
    public static void copyFile(final File source, final File dest) throws IOException {
        if (source == null || dest == null) {
            throw new IllegalArgumentException("Ni origen ni destino de la copia pueden ser nulos"); //$NON-NLS-1$
        }

        final FileInputStream is = new FileInputStream(source);
        final FileOutputStream os = new FileOutputStream(dest);
        final FileChannel in = is.getChannel();
        final FileChannel out = os.getChannel();
        final MappedByteBuffer buf = in.map(FileChannel.MapMode.READ_ONLY, 0, in.size());
        out.write(buf);

        in.close();
        out.close();
        is.close();
        os.close();

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

        final ArrayList<String> parts = new ArrayList<String>();
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

}

