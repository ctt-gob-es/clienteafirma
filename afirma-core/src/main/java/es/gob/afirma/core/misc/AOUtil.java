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

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.core.util.windows.WinRegistryWrapper;

/** M&eacute;todos generales de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3 */
public final class AOUtil {

    private AOUtil() {
        // No permitimos la instanciacion
    }
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String[] SUPPORTED_URI_SCHEMES = new String[] {
            "http", "https", "file", "urn" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    };

    /** Crea una URI a partir de un nombre de fichero local o una URL.
     * @param filename
     *        Nombre del fichero local o URL
     * @return URI (<code>file://</code>) del fichero local o URL
     * @throws AOException
     *         cuando ocurre cualquier problema creando la URI */
    public final static URI createURI(String filename) throws AOException {

        if (filename == null) {
            throw new AOException("No se puede crear una URI a partir de un nulo"); //$NON-NLS-1$
        }

        filename = filename.trim();

        if ("".equals(filename)) { //$NON-NLS-1$
            throw new AOException("La URI no puede ser una cadena vacia"); //$NON-NLS-1$
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

        final URI uri;
        try {
            uri = new URI(filename);
        }
        catch (final Exception e) {
            throw new AOException("Formato de URI (" + filename + ") incorrecto", e); //$NON-NLS-1$ //$NON-NLS-2$
        }

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

        throw new AOException("Formato de URI valido pero no soportado '" + filename + "'"); //$NON-NLS-1$ //$NON-NLS-2$

    }

    /** Obtiene el flujo de entrada de un fichero (para su lectura) a partir de
     * su URI.
     * @param uri
     *        URI del fichero a leer
     * @return Flujo de entrada hacia el contenido del fichero
     * @throws FileNotFoundException
     *         Cuando no se encuentra el fichero indicado
     * @throws AOException
     *         Cuando ocurre cualquier problema obteniendo el flujo */
    public final static InputStream loadFile(final URI uri) throws FileNotFoundException, AOException {

        if (uri == null) {
            throw new IllegalArgumentException("Se ha pedido el contenido de una URI nula"); //$NON-NLS-1$
        }

        if (uri.getScheme().equals("file")) { //$NON-NLS-1$
            // Es un fichero en disco. Las URL de Java no soportan file://, con
            // lo que hay que diferenciarlo a mano
            try {
                // Retiramos el "file://" de la uri
                String path = uri.getSchemeSpecificPart();
                if (path.startsWith("//")) { //$NON-NLS-1$
                    path = path.substring(2);
                }
                return new FileInputStream(new File(path));
            }
            catch (final NullPointerException e) {
                throw e;
            }
            catch (final FileNotFoundException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOException("Error intentando abrir un archivo en almacenamiento local", e); //$NON-NLS-1$
            }
        }

        // Es una URL
        InputStream tmpStream;
        try {
            tmpStream = new BufferedInputStream(uri.toURL().openStream());
        }
        catch (final Exception e) {
            throw new AOException("Error intentando abrir la URI '" + uri.toASCIIString() + "' como URL", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        // Las firmas via URL fallan en la descarga por temas de Sun, asi que
        // descargamos primero
        // y devolvemos un Stream contra un array de bytes
        final byte[] tmpBuffer;
        try {
            tmpBuffer = getDataFromInputStream(tmpStream);
        }
        catch (final Exception e) {
            throw new AOException("Error leyendo el fichero remoto '" + uri.toString() + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // LOGGER.info(
        // "Leido fichero de " + tmpBuffer.length + " bytes:\n" + new
        // String(tmpBuffer)
        // );
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
    public final static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes = 0;
        final byte[] buffer = new byte[4096];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }

    /** Obtiene el directorio principal del sistema operativo del sistema.
     * @return Directorio principal del sistema operativo */
    private final static String getSystemRoot() {
        if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
            return File.separator;
        }
        String systemRoot = null;
        final String defaultSystemRoot = "C:\\WINDOWS"; //$NON-NLS-1$
        try {
            systemRoot =
                    WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_LOCAL_MACHINE,
                                                 "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion", //$NON-NLS-1$
                                                 "SystemRoot"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER
                  .severe("No se ha podido obtener el directorio principal de Windows accediendo al registro, " + "se probara con 'C:\\WINDOWS': " //$NON-NLS-1$ //$NON-NLS-2$
                          + e);
        }
        if (systemRoot == null) {
            final File winSys32 = new File(defaultSystemRoot + "\\SYSTEM32"); //$NON-NLS-1$
            if (winSys32.exists() && winSys32.isDirectory()) {
                return defaultSystemRoot;
            }
        }
        if (systemRoot == null) {
            LOGGER
                  .warning("No se ha encontrado el directorio ra&iacute;z del sistema, se devolver&aacute;: " + File.separator); //$NON-NLS-1$
            systemRoot = File.separator;
        }
        return systemRoot;
    }

    /** Devuelve el directorio principal de bibliotecas del sistema. Importante:
     * No funciona correctamente en Windows de 64 bits //FIXME
     * @return Directorio principal de bibliotecas */
    public final static String getSystemLibDir() {
        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            String systemRoot = AOUtil.getSystemRoot();
            if (systemRoot == null) {
                LOGGER
                      .warning("No se ha podido determinar el directorio de Windows accediendo al registro, " + "se establecera 'C:\\WINDOWS\\' por defecto"); //$NON-NLS-1$ //$NON-NLS-2$
                systemRoot = "c:\\windows\\"; //$NON-NLS-1$
            }
            if (!systemRoot.endsWith("\\")) { //$NON-NLS-1$
                systemRoot += "\\"; //$NON-NLS-1$
            }
            return systemRoot + "System32"; //$NON-NLS-1$
        }
        return "/usr/lib"; //$NON-NLS-1$
    }

    /** Obtiene el nombre com&uacute;n (Common Name, CN) del titular de un
     * certificado X.509. Si no se encuentra el CN, se devuelve la unidad organizativa
     * (Organization Unit, OU).
     * @param c
     *        Certificado X.509 del cual queremos obtener el nombre
     *        com&uacute;n
     * @return Nombre com&uacute;n (Common Name, CN) del titular de un
     *         certificado X.509 */
    public final static String getCN(final X509Certificate c) {
        if (c == null) {
            return null;
        }
        return getCN(c.getSubjectDN().toString());
    }

    /** Obtiene el nombre com&uacute;n (Common Name, CN) de un <i>Principal</i>
     * X.400. Si no se encuentra el CN, se devuelve la unidad organizativa
     * (Organization Unit, OU).
     * @param principal
     *        <i>Principal</i> del cual queremos obtener el nombre
     *        com&uacute;n
     * @return Nombre com&uacute;n (Common Name, CN) de un <i>Principal</i>
     *         X.400 */
    public final static String getCN(final String principal) {
        if (principal == null) {
            return null;
        }
        
        String rdn = getRDNvalue("cn", principal); //$NON-NLS-1$
        if (rdn == null) {
            rdn = getRDNvalue("ou", principal); //$NON-NLS-1$
        }
        
        if (rdn != null) {
            return rdn;
        }
        
        int i = principal.indexOf('=');
        if (i != -1) {
            LOGGER .warning("No se ha podido obtener el Common Name ni la Organizational Unit, se devolvera el fragmento mas significativo"); //$NON-NLS-1$
            return getRDNvalue(principal.substring(0, i), principal);
        }
        
        LOGGER.warning("Principal no valido, se devolvera la entrada"); //$NON-NLS-1$
        return principal;
    }
    
    /**
     * Recupera el valor de un RDN de un principal. El valor de retorno no incluye
     * el nombre del RDN, el igual, ni las posibles comillas que envuelvan el valor.
     * La funci&oacute;n no es sensible a la capitalizaci&oacute;n del RDN. Si no se
     * encuentra, se devuelve {@code null}.
     * @param rdn RDN que deseamos encontrar.
     * @param principal Principal del que extraer el RDN
     * @return Valor del RDN indicado o {@code null} si no se encuentra.
     */
    private final static String getRDNvalue(final String rdn, final String principal) {
        
        int offset1 = 0;
        while ((offset1 = principal.toLowerCase().indexOf(rdn.toLowerCase(), offset1)) != -1) {

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
            } else if (principal.charAt(offset1) == '"') {
                offset1++;
                if (offset1 >= principal.length()) {
                    return ""; //$NON-NLS-1$
                }
                
                offset2 = principal.indexOf("\"", offset1); //$NON-NLS-1$
                if (offset2 == offset1) {
                    return ""; //$NON-NLS-1$
                } else if (offset2 != -1) {
                    return principal.substring(offset1, offset2);
                } else {
                    return principal.substring(offset1);
                }
            } 
            else {
                offset2 = principal.indexOf(",", offset1); //$NON-NLS-1$
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

    /** @param data
     *        Datos a comprobar si podr6iacute;an o no ser Base64
     * @return <code>true</code> si los datos proporcionado pueden ser una
     *         codificaci&oacute;n base64 de un original binario (que no tiene
     *         necesariamente porqu&eacute; serlo), <code>false</code> en caso
     *         contrario */
    public final static boolean isBase64(final byte[] data) {

        // Comprobamos que la cadena tenga una longitud multiplo de 4 caracteres
        final String b64String = new String(data).trim();
        if (b64String.replace("\r\n", "").replace("\n", "").length() % 4 != 0) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            return false;
        }

        // Comprobamos que todos los caracteres de la cadena pertenezcan al
        // alfabeto base 64
        for (final byte b : data) {
            if (!BASE_64_ALPHABET.contains(new String(new byte[] {
                b
            }))) {
                return false;
            }
        }
        return true;
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
    public final static String hexify(final byte abyte0[], final boolean separator) {
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
    public static final String hexify(final byte abyte0[], final String separator) {
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

    /** Recupera el texto con un identificador de versi&oacute;n a partir de un
     * properties indicado a trav&eacute;s de un <code>InputStream</code>. Las
     * propiedades del properties que definen la versi&oacute;n son:<br/>
     * <code><ul><li>version.mayor: Versi&oacute;n.</li>
     * <li>version.minor: Versi&oacute;n menor.</li>
     * <li>version.build: Contrucci&oacute;n</li>
     * <li>version.description: Descripci&oacute;n</li></ul></code> El formato
     * en el que se devuelve la versi&oacute;n ser&aacute; siempre:<br/>
     * <code>X.Y.Z Descripci&oacute;n</code><br/>
     * En donde <code>X</code>, <code>Y</code> y <code>Z</code> son la
     * versi&oacute;n, subversi&oacute;n y contrucci&oacute;n del cliente y
     * puede tener uno o m&aacute;s d&iacute;gitos; y <code>Descripci&oacute;n</code> es un texto libre opcional que puede
     * completar la identificaci&oacute;n de la versi&oacute;n del cliente.</br>
     * Si no se indica alg&uacute;n de los n&uacute;meros de versi&oacute;n se
     * indicar&aacute; cero ('0') y si no se indica la descripci&oacute;n no se
     * mostrar&aacute; nada.
     * @param is
     *        Datos del properties con la versi&oacute;n.
     * @return Identificador de la versi&oacute;n. */
    public final static String getVersion(final InputStream is) {
        final Properties p = new Properties();
        try {
            p.load(is);
        }
        catch (final Exception e) {
            LOGGER.warning("No se han podido obtener los datos de version del cliente de firma"); //$NON-NLS-1$
        }
        final StringBuilder version = new StringBuilder();
        version.append(p.getProperty("version.mayor", "0")) //$NON-NLS-1$ //$NON-NLS-2$
               .append(".") //$NON-NLS-1$
               .append(p.getProperty("version.minor", "0")) //$NON-NLS-1$ //$NON-NLS-2$
               .append(".") //$NON-NLS-1$
               .append(p.getProperty("version.build", "0")); //$NON-NLS-1$ //$NON-NLS-2$

        final String desc = p.getProperty("version.description"); //$NON-NLS-1$
        if (desc != null && !desc.trim().equals("")) { //$NON-NLS-1$
            version.append(" ").append(desc); //$NON-NLS-1$
        }
        return version.toString();

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
    public final static String showTreeAsString(final AOTreeModel tree, String linePrefx, String identationString) {

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
            archiveTreeNode(root.getChildAt(i), 0, (linePrefx != null) ? linePrefx : "", (identationString != null) ? identationString : "\t", buffer);  //$NON-NLS-1$//$NON-NLS-2$
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
    private final static void archiveTreeNode(final AOTreeNode node,
                                              final int depth,
                                              final String linePrefx,
                                              final String identationString,
                                              final StringBuilder buffer) {
        buffer.append('\n').append(linePrefx);
        for (int i = 0; i < depth; i++) {
            buffer.append(identationString);
        }
        buffer.append((node).getUserObject());
        for (int i = 0; i < node.getChildCount(); i++) {
            archiveTreeNode(node.getChildAt(i), depth + 1, linePrefx, identationString, buffer);
        }
    }

    /** Carga una librer&iacute;a nativa del sistema.
     * @param path
     *        Ruta a la libreria de sistema. */
    public static void loadNativeLibrary(final String path) {
        if (path == null) {
            LOGGER.warning("No se puede cargar una biblioteca nula"); //$NON-NLS-1$
            return;
        }
        boolean copyOK = false;
        final int pos = path.lastIndexOf('.');
        final File file = new File(path);
        File tempLibrary = null;
        try {
            tempLibrary =
                    File.createTempFile(pos < 1 ? file.getName() : file.getName().substring(0, file.getName().indexOf('.')),
                                        pos < 1 || pos == path.length() - 1 ? null : path.substring(pos));

            // Copiamos el fichero
            copyOK = copyFile(file, tempLibrary);
        }
        catch (final Exception e) {
            LOGGER.warning("Error al generar una nueva instancia de la libreria " + path + " para su carga: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Pedimos borrar los temporales cuando se cierre la JVM
        if (tempLibrary != null) {
            try {
                tempLibrary.deleteOnExit();
            }
            catch (final Exception e) {
                // Ignoramos los errores, el usuario debe limpiar los temporales regularmente
            }
        }

        LOGGER.info("Cargamos " + (tempLibrary == null ? path : tempLibrary.getAbsolutePath())); //$NON-NLS-1$
        System.load((copyOK && tempLibrary != null) ? tempLibrary.getAbsolutePath() : path);

    }

    /** Copia un fichero.
     * @param source
     *        Fichero origen con el contenido que queremos copiar.
     * @param dest
     *        Fichero destino de los datos.
     * @return Devuelve <code>true</code> si la operac&oacute;n finaliza
     *         correctamente, <code>false</code> en caso contrario. */
    public static boolean copyFile(final File source, final File dest) {
        if (source == null || dest == null) {
            return false;
        }
        try {
            final FileInputStream is = new FileInputStream(source);
            final FileOutputStream os = new FileOutputStream(dest);
            final FileChannel in = is.getChannel();
            final FileChannel out = os.getChannel();
            final MappedByteBuffer buf = in.map(FileChannel.MapMode.READ_ONLY, 0, in.size());
            out.write(buf);

            // Cerramos los canales sin preocuparnos de que lo haga
            // correctamente
            try {
                in.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores de cierre
            }
            try {
                out.close();
            }
            catch (final Exception e) {
             // Ignoramos los errores de cierre
            }
            try {
                is.close();
            }
            catch (final Exception e) {
             // Ignoramos los errores de cierre
            }
            try {
                os.close();
            }
            catch (final Exception e) {
             // Ignoramos los errores de cierre
            }

        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido copiar el fichero origen '" + source.getName() //$NON-NLS-1$
                                                     + "' al destino '" //$NON-NLS-1$
                                                     + dest.getName()
                                                     + "': " //$NON-NLS-1$
                                                     + e);
            return false;
        }
        return true;
    }

    /** Genera una lista de cadenas compuesta por los fragmentos de texto
     * separados por la cadena de separaci&oacute;n indicada. No soporta
     * expresiones regulares. Por ejemplo:<br/>
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

        final Vector<String> parts = new Vector<String>();
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
    
    /** Carga una clase excluyendo de la ruta de b&uacute;squeda de clases las URL que no correspondan con JAR.
     * @param className Nombre de la clase a cargar
     * @return Clase cargada
     * @throws ClassNotFoundException cuando no se encuentra la clase a cargar
     */
    public static Class<?> classForName(final String className) throws ClassNotFoundException {
        getCleanClassLoader().loadClass(className);
        return Class.forName(className);
    }
    
    /** Obtiene un ClassLoader que no incluye URL que no referencien directamente a ficheros JAR.
     * @return ClassLoader sin URL adicionales a directorios sueltos Web
     */
    public static ClassLoader getCleanClassLoader() {
        ClassLoader classLoader = AOUtil.class.getClassLoader();
        if (classLoader instanceof URLClassLoader) {
            Vector<URL> urls = new Vector<URL>();
            for (URL url : ((URLClassLoader)classLoader).getURLs()) {
                if (url.toString().endsWith(".jar")) { //$NON-NLS-1$
                    urls.add(url);
                }
                classLoader = new URLClassLoader(urls.toArray(new URL[0]));
            }
        }
        return classLoader;
    }
}

