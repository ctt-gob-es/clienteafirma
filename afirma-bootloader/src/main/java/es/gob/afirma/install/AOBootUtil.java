/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.install;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;
import java.util.zip.ZipFile;

/** M&eacute;todos generales de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3 */
final class AOBootUtil {
    
    /** Gestor de registro. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;

    private AOBootUtil() {
        // No permitimos la instanciacion
    }

    /** Esquemas de ruta soportados. */
    private static final String[] SUPPORTED_URI_SCHEMES = new String[] {
                                                                        "http", //$NON-NLS-1$
                                                                        "https", //$NON-NLS-1$
                                                                        "file" //$NON-NLS-1$
    };

    /** Crea una URI a partir de un nombre de fichero local o una URL.
     * @param filename Nombre del fichero local o URL
     * @return URI (<code>file://</code>) del fichero local o URL
     * @throws URISyntaxException 
     * @throws URISyntaxException cuando ocurre cualquier problema creando la URI */
    static URI createURI(final String file) throws URISyntaxException {

        if (file == null) {
            throw new IllegalArgumentException("No se puede crear una URI a partir de un nulo"); //$NON-NLS-1$
        }

        // Cambiamos los caracteres Windows
        String filename = file.replace('\\', '/');

        // Cambiamos los espacios por %20
        filename = filename.replace(" ", "%20"); //$NON-NLS-1$ //$NON-NLS-2$

        final URI uri = new URI(filename);

        // Comprobamos si es un esquema soportado
        final String scheme = uri.getScheme();
        for (final String element : SUPPORTED_URI_SCHEMES) {
            if (element.equals(scheme)) {
                return uri;
            }
        }

        // Si el esquema es nulo, aun puede ser un nombre de fichero valido
        if (scheme == null) {
            return createURI("file://" + filename); //$NON-NLS-1$
        }

        // Miramos si el esquema es una letra, en cuyo caso seguro que es una
        // unidad de Windows ("C:", "D:", etc.), y le anado el file://
        if (scheme.length() == 1 && Character.isLetter((char) scheme.getBytes()[0])) {
            return createURI("file://" + filename); //$NON-NLS-1$
        }
        
        throw new IllegalArgumentException("Formato de URI valido pero no soportado '" + filename + "'"); //$NON-NLS-1$ //$NON-NLS-2$

    }

    /** Obtiene el flujo de entrada de un fichero (para su lectura) a partir de su URI.
     * @param uri URI del fichero a leer
     * @return Flujo de entrada hacia el contenido del fichero
     * @throws IOException Cuando ocurre cualquier problema obteniendo el flujo*/
    static InputStream loadFile(final URI uri) throws IOException {

        // Cuidado: Repinta mal el dialogo de espera, hay que tratar con hilos nuevos
        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4209604

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
            return new BufferedInputStream(new FileInputStream(new File(path)));

        }
        // Es una URL
        final InputStream tmpStream = new BufferedInputStream(uri.toURL().openStream());

        // Las firmas via URL fallan en la descarga por temas de Sun, asi que descargamos primero
        // y devolvemos un Stream contra un array de bytes
        final byte[] tmpBuffer = getDataFromInputStream(tmpStream);

        return new java.io.ByteArrayInputStream(tmpBuffer);

    }

    /** Lee un flujo de datos de entrada y los recupera en forma de array de bytes. Este
     * m&eacute;todo consume pero no cierra el flujo de datos.
     * No cierra el flujo de datos de entrada.
     * @param input Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException Si ocurre cualquier error durante la lectura de datos */
    static byte[] getDataFromInputStream(final InputStream input) throws IOException {

        int nBytes = 0;
        final byte[] buffer = new byte[1024];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }

        return baos.toByteArray();
    }

    /** Crea una URL a partir de una URL base y un nombre de fichero.
     * @param urlBase URL base del fichero.
     * @param filename Nombre del fichero.
     * @return URL de referencia directa al fichero. */
    static URL createURLFile(final URL urlBase, final String filename) {
        try {
            // TODO: Tratar el caso de urls con caracteres especiales
            String codeBase = urlBase.toString();
            if (!codeBase.endsWith("/") && !codeBase.endsWith("\\")) { //$NON-NLS-1$ //$NON-NLS-2$
                codeBase = codeBase + "/"; //$NON-NLS-1$
            }
            return AOBootUtil.createURI(codeBase + filename).toURL();
        }
        catch (final Exception e) {
            LOGGER.severe("No se pudo crear la referencia al fichero '" + filename + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
    }

    /** Copia un fichero.
     * @param source Fichero origen con el contenido que queremos copiar.
     * @param dest Fichero destino de los datos.
     * @return Devuelve <code>true</code> si la operac&oacute;n finaliza correctamente, <code>false</code> en caso contrario. */
    static boolean copyFile(final File source, final File dest) {
        if (source == null || dest == null) {
            return false;
        }

        // Si no existe el directorio del fichero destino, lo creamos
        if (!dest.getParentFile().exists()) {
            dest.getParentFile().mkdirs();
        }

        // Copiamos el directorio
        try {
            final FileChannel in = new FileInputStream(source).getChannel();
            final FileChannel out = new FileOutputStream(dest).getChannel();
            final MappedByteBuffer buf = in.map(FileChannel.MapMode.READ_ONLY, 0, in.size());
            out.write(buf);

            // Cerramos los canales sin preocuparnos de que lo haga correctamente
            try {
                in.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores en el cierre
            }
            try {
                out.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores en el cierre
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

    /** Recupera el texto con un identificador de versi&oacute;n a partir de un properties indicado
     * a trav&eacute;s de un <code>InputStream</code>. Las propiedades del properties que definen la
     * versi&oacute;n son:<br/>
     * <code><ul><li>version.mayor: Versi&oacute;n.</li>
     * <li>version.minor: Versi&oacute;n menor.</li>
     * <li>version.build: Contrucci&oacute;n</li>
     * <li>version.description: Descripci&oacute;n</li></ul></code> El formato en el que se devuelve la versi&oacute;n ser&aacute; siempre:<br/>
     * <code>X.Y.Z Descripci&oacute;n</code><br/>
     * En donde <code>X</code>, <code>Y</code> y <code>Z</code> son la versi&oacute;n, subversi&oacute;n
     * y contrucci&oacute;n del cliente y puede tener uno o m&aacute;s d&iacute;gitos; y <code>Descripci&oacute;n</code> es un texto libre opcional
     * que puede completar la
     * identificaci&oacute;n de la versi&oacute;n del cliente.</br>
     * Si no se indica alg&uacute;n de los n&uacute;meros de versi&oacute;n se indicar&aacute; cero ('0')
     * y si no se indica la descripci&oacute;n no se mostrar&aacute; nada.
     * @param is Datos del properties cobn la versi&oacute;n.
     * @return Identificador de la versi&oacute;n. */
    static String getVersion(final InputStream is) {
        final Properties p = new Properties();
        try {
            p.load(is);
        }
        catch (final Exception e) {
            LOGGER.warning("No se han podido obtener los datos de version"); //$NON-NLS-1$
        }
        final StringBuilder version = new StringBuilder();
        version.append(p.getProperty("version.mayor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        .append(p.getProperty("version.minor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        .append(p.getProperty("version.build", "0")).append(" ") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        .append(p.getProperty("build", "")); //$NON-NLS-1$ //$NON-NLS-2$
        if (p.containsKey("java")) {//$NON-NLS-1$
            version.append(" para Java ").append(p.getProperty("java")).append(" o superior"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        if (p.containsKey("description")) {//$NON-NLS-1$
            version.append(" (").append(p.getProperty("description")).append(")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        return version.toString().trim();
    }

    /** Recupera la versi&oacute;n indicada en un fichero "version.properties" en el directorio
     * ra&iacute;z de un fichero ZIP.
     * @param zipFilePath Ruta del fichero Zip.
     * @return Versi&oacute;n del m&oacute;dulo que contiene el Zip. */
    static String getVersionFromZip(final String zipFilePath) {

        String idVersion;
        try {
            final ZipFile zipFile = new ZipFile(zipFilePath);
            final InputStream is = zipFile.getInputStream(zipFile.getEntry("version.properties")); //$NON-NLS-1$
            idVersion = AOBootUtil.getVersion(is);
            try {
                is.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores en el cierre
            }
            try {
                zipFile.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores en el cierre
            }
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido identificar el cliente de firma instalado"); //$NON-NLS-1$
            idVersion = "0.0.0"; //$NON-NLS-1$
        }
        return idVersion;
    }
    
    /** Obtiene un ClassLoader que no incluye URL que no referencien directamente a ficheros JAR.
     * @return ClassLoader sin URL adicionales a directorios sueltos Web
     */
    static ClassLoader getCleanClassLoader() {
        ClassLoader classLoader = AOBootUtil.class.getClassLoader();
        if (classLoader instanceof URLClassLoader) {
            final Vector<URL> urls = new Vector<URL>();
            for (final URL url : ((URLClassLoader)classLoader).getURLs()) {
                if (url.toString().endsWith(".jar")) { //$NON-NLS-1$
                    urls.add(url);
                }
                classLoader = new URLClassLoader(urls.toArray(new URL[0]));
            }
        }
        return classLoader;
    }

}
