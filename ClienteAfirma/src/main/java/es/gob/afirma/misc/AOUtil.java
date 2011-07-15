/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.awt.Component;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import javax.swing.ProgressMonitorInputStream;

import org.apache.commons.codec.binary.Base64;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.misc.tree.TreeNode;

/** M&eacute;todos generales de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3 */
public final class AOUtil {

    private AOUtil() {}

    private static final String[] SUPPORTED_URI_SCHEMES = new String[] {
            "http", "https", "file", "urn"
    };

    /** Crea una URI a partir de un nombre de fichero local o una URL.
     * @param filename
     *        Nombre del fichero local o URL
     * @return URI (<code>file://</code>) del fichero local o URL
     * @throws AOException
     *         cuando ocurre cualquier problema creando la URI */
    public final static URI createURI(String filename) throws AOException {

        if (filename == null) {
            throw new AOException("No se puede crear una URI a partir de un nulo");
        }

        filename = filename.trim();

        if ("".equals(filename)) {
            throw new AOException("La URI no puede ser una cadena vacia");
        }

        // Cambiamos los caracteres Windows
        filename = filename.replace('\\', '/');

        // Realizamos los cambios necesarios para proteger los caracteres no
        // seguros
        // de la URL
        filename =
                filename.replace(" ", "%20")
                        .replace("<", "%3C")
                        .replace(">", "%3E")
                        .replace("\"", "%22")
                        .replace("{", "%7B")
                        .replace("}", "%7D")
                        .replace("|", "%7C")
                        .replace("^", "%5E")
                        .replace("[", "%5B")
                        .replace("]", "%5D")
                        .replace("`", "%60");

        final URI uri;
        try {
            uri = new URI(filename);
        }
        catch (final Exception e) {
            throw new AOException("Formato de URI (" + filename + ") incorrecto", e);
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
            filename = filename.replace("#", "%23");
            return createURI("file://" + filename);
        }

        // Miramos si el esquema es una letra, en cuyo caso seguro que es una
        // unidad de Windows ("C:", "D:", etc.), y le anado el file://
        // El caracter '#' debe protegerse en rutas locales
        if (scheme.length() == 1 && Character.isLetter((char) scheme.getBytes()[0])) {
            filename = filename.replace("#", "%23");
            return createURI("file://" + filename);
        }

        throw new AOException("Formato de URI valido pero no soportado '" + filename + "'");

    }

    /** Obtiene el flujo de entrada de un fichero (para su lectura) a partir de
     * su URI.
     * @param uri
     *        URI del fichero a leer
     * @param c
     *        Componente grafico que invoca al m&eacute;todo (para la
     *        modalidad del di&aacute;logo de progreso)
     * @param waitDialog
     *        Indica si debe mostrarse o no un di&aacute;logo de progreso de
     *        la carga y lectura
     * @return Flujo de entrada hacia el contenido del fichero
     * @throws FileNotFoundException
     *         Cuando no se encuentra el fichero indicado
     * @throws AOException
     *         Cuando ocurre cualquier problema obteniendo el flujo */
    public final static InputStream loadFile(final URI uri, final Component c, final boolean waitDialog) throws FileNotFoundException, AOException {

        // Cuidado: Repinta mal el dialogo de espera, hay que tratar con hilos
        // nuevos
        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4209604

        if (uri == null) {
            throw new NullPointerException("Se ha pedido el contenido de una URI nula");
        }

        javax.swing.ProgressMonitor pm = null;

        if (uri.getScheme().equals("file")) {
            // Es un fichero en disco. Las URL de Java no soportan file://, con
            // lo que hay que diferenciarlo a mano
            try {
                // Retiramos el "file://" de la uri
                String path = uri.getSchemeSpecificPart();
                if (path.startsWith("//")) {
                    path = path.substring(2);
                }
                // Cuidado, el ProgressMonitor no se entera del tamano de los
                // ficheros grandes:
                // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6445283
                if (waitDialog) {
                    return new ProgressMonitorInputStream(c, "Leyendo " + path, new FileInputStream(new File(path)));
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
                throw new AOException("Error intentando abrir un archivo en almacenamiento local", e);
            }
        }

        // Es una URL
        InputStream tmpStream;
        try {
            if (waitDialog) {
                final ProgressMonitorInputStream pmis =
                        new ProgressMonitorInputStream(c, "Leyendo " + uri.toURL().toString(), uri.toURL().openStream());
                pm = pmis.getProgressMonitor();
                // pm.setMillisToDecideToPopup(0);
                // pm.setMillisToPopup(0);

                // Las URL pocas veces informan del tamano del fichero, asi que
                // ponemos un valor alto
                // por defecto para segurarnos de que el dialogo se muestra
                pm.setMaximum(10000000);

                tmpStream = new BufferedInputStream(pmis);
            }
            else {
                tmpStream = new BufferedInputStream(uri.toURL().openStream());
            }
        }
        catch (final Exception e) {
            if (pm != null) {
                pm.close();
            }
            throw new AOException("Error intentando abrir la URI '" + uri.toASCIIString() + "' como URL", e);
        }
        // Las firmas via URL fallan en la descarga por temas de Sun, asi que
        // descargamos primero
        // y devolvemos un Stream contra un array de bytes
        final byte[] tmpBuffer;
        try {
            tmpBuffer = getDataFromInputStream(tmpStream);
        }
        catch (final Exception e) {
            if (pm != null) {
                pm.close();
            }
            throw new AOException("Error leyendo el fichero remoto '" + uri.toString() + "'", e);
        }

        // Hay que cerrar el ProgressMonitor a mano:
        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4174850
        if (pm != null) {
            pm.close();
        }

        // Logger.getLogger("es.gob.afirma").info(
        // "Leido fichero de " + tmpBuffer.length + " bytes:\n" + new
        // String(tmpBuffer)
        // );
        return new java.io.ByteArrayInputStream(tmpBuffer);
    }

    /** Obtiene el flujo de entrada de un fichero (para su lectura) a partir de
     * su URI. Si se introduce una ruta nula, se devuelve nulo.
     * @param uri
     *        URI del fichero a leer
     * @param parent
     *        Componente grafico que invoca al m&eacute;todo (para la
     *        modalidad del di&aacute;logo de progreso)
     * @param waitDialog
     *        Indica si debe mostrarse o no un di&aacute;logo de progreso de
     *        la carga y lectura
     * @param b64Content
     *        Indica si el contenido es base 64 para que se descodifique.
     * @return Flujo de entrada hacia el contenido del fichero
     * @throws FileNotFoundException
     *         Cuando no se encuentra el fichero indicado
     * @throws AOException
     *         Cuando ocurre cualquier problema obteniendo el flujo */
    public final static InputStream loadFile(final URI uri, final Component parent, final boolean waitDialog, final boolean b64Content) throws FileNotFoundException,
                                                                                                                                       AOException {

        if (uri == null) {
            return null;
        }

        InputStream dataStream = AOUtil.loadFile(uri, parent, true);
        if (b64Content) {
            try {
                dataStream = new ByteArrayInputStream(Base64.decodeBase64(AOUtil.getDataFromInputStream(dataStream)));
            }
            catch (final IOException e) {
                throw new AOException("Error en la lectura del fichero de entrada de datos: " + uri, e); //$NON-NLS-1$
            }
        }

        return dataStream;
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
        final String defaultSystemRoot = "C:\\WINDOWS";
        try {
            systemRoot =
                    WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_LOCAL_MACHINE,
                                                 "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion",
                                                 "SystemRoot");
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma")
                  .severe("No se ha podido obtener el directorio principal de Windows accediendo al registro, " + "se probara con 'C:\\WINDOWS': "
                          + e);
        }
        if (systemRoot == null) {
            final File winSys32 = new File(defaultSystemRoot + "\\SYSTEM32");
            if (winSys32.exists() && winSys32.isDirectory()) {
                return defaultSystemRoot;
            }
        }
        if (systemRoot == null) {
            Logger.getLogger("es.gob.afirma")
                  .warning("No se ha encontrado el directorio ra&iacute;z del sistema, se devolver&aacute;: " + File.separator);
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
                Logger.getLogger("es.gob.afirma")
                      .warning("No se ha podido determinar el directorio de Windows accediendo al registro, " + "se establecera 'C:\\WINDOWS\\' por defecto");
                systemRoot = "c:\\windows\\";
            }
            if (!systemRoot.endsWith("\\")) {
                systemRoot += "\\";
            }
            return systemRoot + "System32";
        }
        return "/usr/lib";
    }

    /** Obtiene el nombre com&uacute;n (Common Name, CN) del titular de un
     * certificado X.509.
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
     * X.400.
     * @param principal
     *        <i>Principal</i> del cual queremos obtener el nombre
     *        com&uacute;n
     * @return Nombre com&uacute;n (Common Name, CN) de un <i>Principal</i>
     *         X.400 */
    public final static String getCN(final String principal) {
        if (principal == null) {
            return null;
        }
        final List<Rdn> rdns;
        try {
            rdns = new LdapName(principal).getRdns();
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido obtener el Common Name, se devolvera la cadena de entrada: " + e);
            return principal;
        }
        if (rdns != null && (!rdns.isEmpty())) {
            String ou = null;
            for (int j = 0; j < rdns.size(); j++) {
                if (rdns.get(j).toString().startsWith("cn=") || rdns.get(j).toString().startsWith("CN=")) {
                    return rdns.get(j).toString().substring(3);
                }
                else if (rdns.get(j).toString().startsWith("ou=") || rdns.get(j).toString().startsWith("OU=")) {
                    ou = rdns.get(j).toString().substring(3);
                }
            }

            // En caso de no haber encontrado el Common Name y si la
            // Organizational Unit,
            // devolvemos esta ultima
            if (ou != null) {
                return ou;
            }

            Logger.getLogger("es.gob.afirma")
                  .warning("No se ha podido obtener el Common Name ni la Organizational Unit, se devolvera el fragmento mas significativo");
            return rdns.get(rdns.size() - 1).toString().substring(3);
        }
        Logger.getLogger("es.gob.afirma").warning("Principal no valido, se devolvera la entrada");
        return principal;
    }

    /** Caracterres aceptados en una codificaci&oacute;n Base64 seg&uacute;n la
     * <a href="http://www.faqs.org/rfcs/rfc3548.html">RFC 3548</a>. Importante:
     * A&ntilde;adimos el car&aacute;cter &tilde; porque en ciertas
     * codificaciones de Base64 est&aacute; aceptado, aunque no es nada
     * recomendable */
    private static final String BASE_64_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz=_-\n+/0123456789\r~";

    /** @param data
     *        Datos a comprobar si podr6iacute;an o no ser Base64
     * @return <code>true</code> si los datos proporcionado pueden ser una
     *         codificaci&oacute;n base64 de un original binario (que no tiene
     *         necesariamente porqu&eacute; serlo), <code>false</code> en caso
     *         contrario */
    public final static boolean isBase64(final byte[] data) {

        // if (!new String(data).endsWith("=")) return false;

        // Comprobamos que la cadena tenga una longitud multiplo de 4 caracteres
        final String b64String = new String(data).trim();
        if (b64String.replace("\r\n", "").replace("\n", "").length() % 4 != 0) {
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
            return "null";
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
            return "null";
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

    /** Obtiene la ruta absoluta del fichero de almac&eacute;n de las claves de
     * cifrado.
     * @return Ruta absoluta del fichero. */
    public final static String getCipherKeystore() {
        return AOInstallParameters.getUserHome() + "ciphkeys.jceks";
    }

    /** Recupera un algoritmo de hash a partir del algoritmo de firma introducido
     * s&oacute;lo para su uso para la generaci&oacute;n de hashes. Si el
     * algoritmo no respeta el formato "ALGORITHM_HASH"+with+"AGORITHM_SIGN" se
     * devuelve nulo.
     * @param signatureAlgorithm
     *        Nombre del algoritmo de firma
     * @return Algoritmo de hash. */
    public final static String getDigestAlgorithm(final String signatureAlgorithm) {

        final int withPos = signatureAlgorithm.indexOf("with");
        if (withPos == -1) {
            return null;
        }

        String digestAlg = signatureAlgorithm.substring(0, withPos);
        if (digestAlg.startsWith("SHA")) {
            digestAlg = "SHA-" + digestAlg.substring(3);
        }
        return digestAlg;
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
            Logger.getLogger("es.gob.afirma").warning("No se han podido obtener los datos de version del cliente de firma");
        }
        final StringBuilder version = new StringBuilder();
        version.append(p.getProperty("version.mayor", "0"))
               .append(".")
               .append(p.getProperty("version.minor", "0"))
               .append(".")
               .append(p.getProperty("version.build", "0"));

        final String desc = p.getProperty("version.description");
        if (desc != null && !desc.trim().equals("")) {
            version.append(" ").append(desc);
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
    public final static String showTreeAsString(final TreeModel tree, String linePrefx, String identationString) {

        if (tree == null || tree.getRoot() == null) {
            Logger.getLogger("es.gob.afirma").severe("Se ha proporcionado un arbol de firmas vacio"); //$NON-NLS-1$
            return null;
        }

        if (!(tree.getRoot() instanceof TreeNode)) {
            Logger.getLogger("es.gob.afirma").severe("La raiz del arbol de firmas no es de tipo DafaultMutableTreeNode"); //$NON-NLS-1$
            return null;
        }

        if (linePrefx == null) {
            linePrefx = "";
        }
        if (identationString == null) {
            identationString = "\t";
        }

        final StringBuilder buffer = new StringBuilder();

        // Transformamos en cadenas de texto cada rama que surja del nodo raiz
        // del arbol
        final TreeNode root = (TreeNode) tree.getRoot();
        for (int i = 0; i < root.getChildCount(); i++) {
            archiveTreeNode(root.getChildAt(i), 0, linePrefx, identationString, buffer);
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
    private final static void archiveTreeNode(final TreeNode node,
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
            Logger.getLogger("es.gob.afirma").warning("No se puede cargar una biblioteca nula");
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
            Logger.getLogger("es.gob.afirma").warning("Error al generar una nueva instancia de la libreria " + path + " para su carga: " + e);
        }

        // Pedimos borrar los temporales cuando se cierre la JVM
        if (tempLibrary != null) {
            try {
                tempLibrary.deleteOnExit();
            }
            catch (final Exception e) {}
        }

        Logger.getLogger("es.gob.afirma").info("Cargamos " + (tempLibrary == null ? path : tempLibrary.getAbsolutePath()));
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
            catch (final Exception e) {}
            try {
                out.close();
            }
            catch (final Exception e) {}
            try {
                is.close();
            }
            catch (final Exception e) {}
            try {
                os.close();
            }
            catch (final Exception e) {}

        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se ha podido copiar el fichero origen '" + source.getName()
                                                     + "' al destino '"
                                                     + dest.getName()
                                                     + "': "
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
                parts.add("");
            }
            else {
                parts.add(text.substring(i, j));
            }
            i = j + sp.length();
        }
        if (i == text.length()) {
            parts.add("");
        }
        else {
            parts.add(text.substring(i));
        }

        return parts.toArray(new String[0]);
    }
}
