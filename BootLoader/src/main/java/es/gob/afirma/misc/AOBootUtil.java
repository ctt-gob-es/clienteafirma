/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.misc;

import java.awt.Component;
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
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Properties;
import java.util.zip.ZipFile;

import javax.swing.ProgressMonitorInputStream;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.install.AfirmaBootLoader;
import es.gob.afirma.install.Messages;

/** M&eacute;todos generales de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3 */
public final class AOBootUtil {

    private AOBootUtil() {}

    /** Esquemas de ruta soportados. */
    private static final String[] SUPPORTED_URI_SCHEMES = new String[] {
                                                                        "http", //$NON-NLS-1$
                                                                        "https", //$NON-NLS-1$
                                                                        "file" //$NON-NLS-1$
    };

    /** Crea una URI a partir de un nombre de fichero local o una URL.
     * @param filename Nombre del fichero local o URL
     * @return URI (<code>file://</code>) del fichero local o URL
     * @throws AOException cuando ocurre cualquier problema creando la URI */
    public static URI createURI(String filename) throws AOException {

        if (filename == null) {
            throw new AOException("No se puede crear una URI a partir de un nulo"); //$NON-NLS-1$
        }

        // Cambiamos los caracteres Windows
        filename = filename.replace('\\', '/');

        // Cambiamos los espacios por %20
        filename = filename.replace(" ", "%20"); //$NON-NLS-1$ //$NON-NLS-2$

        final URI uri;
        try {
            uri = new URI(filename);
        }
        catch (final Exception e) {
            throw new AOException("Formato de URI incorrecto: " + e); //$NON-NLS-1$
        }

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
        if (scheme.length() == 1) {
            if (Character.isLetter((char) scheme.getBytes()[0])) {
                return createURI("file://" + filename); //$NON-NLS-1$
            }
        }

        throw new AOException("Formato de URI valido pero no soportado '" + filename + "'"); //$NON-NLS-1$ //$NON-NLS-2$

    }

    /** Obtiene el flujo de entrada de un fichero (para su lectura) a partir de su URI.
     * @param uri URI del fichero a leer
     * @param c Componente grafico que invoca al m&eacute;todo (para la modalidad
     *        del di&aacute;logo de progreso)
     * @param waitDialog <code>true</code> si deseamos que se muestre un di&aacute;logo
     *        gr&aacute;fico de espera si la operaci&oacute;n dura mucho, <code>false</code> en caso contrario
     * @return Flujo de entrada hacia el contenido del fichero
     * @throws FileNotFoundException Si el fichero no existe
     * @throws AOException Cuando ocurre cualquier problema obteniendo el flujo */
    public static InputStream loadFile(final URI uri, final Component c, final boolean waitDialog) throws FileNotFoundException, AOException {

        // Cuidado: Repinta mal el dialogo de espera, hay que tratar con hilos nuevos
        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4209604

        if (uri == null) {
            throw new IllegalArgumentException("Se ha pedido el contenido de una URI nula"); //$NON-NLS-1$
        }

        javax.swing.ProgressMonitor pm = null;

        if (uri.getScheme().equals("file")) { //$NON-NLS-1$
            // Es un fichero en disco. Las URL de Java no soportan file://, con
            // lo que hay que diferenciarlo a mano
            try {
                // Retiramos el "file://" de la uri
                String path = uri.getSchemeSpecificPart();
                if (path.startsWith("//")) {
                    path = path.substring(2);
                }
                // Cuidado, el ProgressMonitor no se entera del tamano de los ficheros grandes:
                // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6445283
                if (waitDialog) {
                    return new BufferedInputStream(new ProgressMonitorInputStream(c, Messages.getString("AOBootUtil.0") + " " + path, //$NON-NLS-1$ //$NON-NLS-2$
                                                                                  new FileInputStream(new File(path))));
                }
                return new BufferedInputStream(new FileInputStream(new File(path)));
            }
            catch (final Exception e) {
                throw new AOException("Ocurrio un error intentando abrir un archivo en almacenamiento local: " + e); //$NON-NLS-1$
            }
        }
        // Es una URL
        final InputStream tmpStream;
        try {
            if (waitDialog) {
                final ProgressMonitorInputStream pmis = new ProgressMonitorInputStream(c, Messages.getString("AOBootUtil.0") + uri.toURL().toString(), //$NON-NLS-1$
                                                                                       uri.toURL().openStream());
                pm = pmis.getProgressMonitor();
                // pm.setMillisToDecideToPopup(0);
                // pm.setMillisToPopup(0);

                // Las URL pocas veces informan del tamano del fichero, asi que ponemos un valor alto
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
            throw new AOException("Error intentando abrir la URI '" + uri.toASCIIString() + "' como URL: " + e //$NON-NLS-1$ //$NON-NLS-2$
            );
        }
        // Las firmas via URL fallan en la descarga por temas de Sun, asi que descargamos primero
        // y devolvemos un Stream contra un array de bytes
        final byte[] tmpBuffer;
        try {
            tmpBuffer = getDataFromInputStream(tmpStream);
        }
        catch (final Exception e) {
            if (pm != null) {
                pm.close();
            }
            throw new AOException("Error leyendo el fichero remoto '" + uri.toString() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Hay que cerrar el ProgressMonitor a mano:
        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4174850
        if (pm != null) {
            pm.close();
        }

        return new java.io.ByteArrayInputStream(tmpBuffer);

    }

    /** Lee un flujo de datos de entrada y los recupera en forma de array de bytes. Este
     * m&eacute;todo consume pero no cierra el flujo de datos.
     * No cierra el flujo de datos de entrada.
     * @param input Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException Si ocurre cualquier error durante la lectura de datos */
    public static byte[] getDataFromInputStream(final InputStream input) throws IOException {

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
    public static URL createURLFile(final URL urlBase, final String filename) {
        try {
            // TODO: Tratar el caso de urls con caracteres especiales
            String codeBase = urlBase.toString();
            if (!codeBase.endsWith("/") && !codeBase.endsWith("\\")) { //$NON-NLS-1$ //$NON-NLS-2$
                codeBase = codeBase + "/"; //$NON-NLS-1$
            }
            return AOBootUtil.createURI(codeBase + filename).toURL();
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER.severe("No se pudo crear la referencia al fichero '" + filename + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            return null;
        }
    }

    /** Copia un fichero.
     * @param source Fichero origen con el contenido que queremos copiar.
     * @param dest Fichero destino de los datos.
     * @return Devuelve <code>true</code> si la operac&oacute;n finaliza correctamente, <code>false</code> en caso contrario. */
    public static boolean copyFile(final File source, final File dest) {
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
            catch (final Exception e) {}
            try {
                out.close();
            }
            catch (final Exception e) {}
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER.severe("No se ha podido copiar el fichero origen '" + source.getName()
                                                     + "' al destino '"
                                                     + dest.getName()
                                                     + "': "
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
    public static String getVersion(final InputStream is) {
        final Properties p = new Properties();
        try {
            p.load(is);
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER.warning("No se han podido obtener los datos de version"); //$NON-NLS-1$ //$NON-NLS-2$
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
    public static String getVersionFromZip(final String zipFilePath) {

        String idVersion;
        try {
            final ZipFile zipFile = new ZipFile(zipFilePath);
            final InputStream is = zipFile.getInputStream(zipFile.getEntry("version.properties")); //$NON-NLS-1$
            idVersion = AOBootUtil.getVersion(is);
            try {
                is.close();
            }
            catch (final Exception e) {}
            try {
                zipFile.close();
            }
            catch (final Exception e) {}
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER.warning("No se ha podido identificar el cliente de firma instalado"); //$NON-NLS-1$ //$NON-NLS-2$
            idVersion = "0.0.0"; //$NON-NLS-1$
        }
        return idVersion;
    }

    /** Carga una librer&iacute;a nativa del sistema.
     * @param path Ruta a la libreria de sistema. */
    public static void loadNativeLibrary(final String path) {

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
            AfirmaBootLoader.LOGGER.warning("Error al generar una nueva instancia de la libreria " + path + " para su carga: " + e);
        }
        AfirmaBootLoader.LOGGER.info("Cargamos " + (tempLibrary == null ? path : tempLibrary.getAbsolutePath()));
        System.load((copyOK && tempLibrary != null) ? tempLibrary.getAbsolutePath() : path);
        if (tempLibrary != null) {
            tempLibrary.deleteOnExit();
        }
    }

    /** Obtiene los par&aacuter;metros de una funci&oacute; JavaScript a trav&eacute;s
     * de el objeto que los almacena.
     * @param jsMethodParams Conjunto de par&aacute;metros como array o como cadenas separadas por ','.
     * @return Listado de par&aacute;metros. */
    public static Object[] prepareJSParams(final Object jsMethodParams) {
        if (jsMethodParams == null) {
            return new Object[0];
        }
        if (jsMethodParams instanceof Object[]) {
            return (Object[]) jsMethodParams;
        }
        if (jsMethodParams instanceof String) {
            return ((String) jsMethodParams).split(",");
        }
        return new Object[0];
    }

}
