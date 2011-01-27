/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
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
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Properties;
import java.util.jar.JarOutputStream;
import java.util.jar.Pack200.Unpacker;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipFile;

import javax.swing.ProgressMonitorInputStream;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.install.Messages;

/** 
 * M&eacute;todos generales de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3 
 */
public final class AOBootUtil {

	private AOBootUtil() {}

	private static final String[] SUPPORTED_URI_SCHEMES = new String[] {
		"http", //$NON-NLS-1$
		"https", //$NON-NLS-1$
		"file" //$NON-NLS-1$
	};

	/**
	 * Crea una URI a partir de un nombre de fichero local o una URL.
	 * @param filename Nombre del fichero local o URL
	 * @return URI (<code>file://</code>) del fichero local o URL
	 * @throws AOException cuando ocurre cualquier problema creando la URI
	 */
	public static URI createURI(String filename) throws AOException {

		if (filename == null) throw new AOException("No se puede crear una URI a partir de un nulo"); //$NON-NLS-1$

		// Cambiamos los caracteres Windows
		filename = filename.replace('\\', '/');

		// Cambiamos los espacios por %20
		filename = filename.replace(" ", "%20"); //$NON-NLS-1$ //$NON-NLS-2$

		URI uri;
		try {
			uri = new URI(filename);
		}
		catch(Throwable e) {
			throw new AOException("Formato de URI incorrecto: " + e); //$NON-NLS-1$
		}

		// Comprobamos si es un esquema soportado
		String scheme = uri.getScheme();
		for(int i=0; i<SUPPORTED_URI_SCHEMES.length; i++) if (SUPPORTED_URI_SCHEMES[i].equals(scheme)) return uri;

		// Si el esquema es nulo, aun puede ser un nombre de fichero valido
		if(scheme == null) return createURI("file://" + filename); //$NON-NLS-1$

		// Miramos si el esquema es una letra, en cuyo caso seguro que es una
		// unidad de Windows ("C:", "D:", etc.), y le anado el file://
		if (scheme.length() == 1) if (Character.isLetter((char)scheme.getBytes()[0])) return createURI("file://" + filename);		 //$NON-NLS-1$

		throw new AOException("Formato de URI valido pero no soportado '" + filename + "'"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	/**
	 * Obtiene el flujo de entrada de un fichero (para su lectura) a partir de su URI.
	 * @param uri URI del fichero a leer
	 * @param c Componente grafico que invoca al m&eacute;todo (para la modalidad
	 *          del di&aacute;logo de progreso)
	 * @param waitDialog <code>true</code> si deseamos que se muestre un di&aacute;logo
	 *                   gr&aacute;fico de espera si la operaci&oacute;n dura mucho,
	 *                   <code>false</code> en caso contrario
	 * @return Flujo de entrada hacia el contenido del fichero
	 * @throws FileNotFoundException Si el fichero no existe
	 * @throws AOException Cuando ocurre cualquier problema obteniendo el flujo
	 */
	public static InputStream loadFile(URI uri, Component c, boolean waitDialog) throws FileNotFoundException, AOException {
		
		// Cuidado: Repinta mal el dialogo de espera, hay que tratar con hilos nuevos
		// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4209604

		if (uri == null) {
			throw new NullPointerException("Se ha pedido el contenido de una URI nula"); //$NON-NLS-1$
		}
		
		javax.swing.ProgressMonitor pm = null;
		
		if (uri.getScheme().equals("file")) { //$NON-NLS-1$
			// Es un fichero en disco. Las URL de Java no soportan file://, con
			// lo que hay que diferenciarlo a mano
			try {
				// Retiramos el "file://" de la uri
				String path = uri.getSchemeSpecificPart();
				if(path.startsWith("//")) path = path.substring(2); //$NON-NLS-1$
				// Cuidado, el ProgressMonitor no se entera del tamano de los ficheros grandes:
				// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6445283
				if (waitDialog) return new BufferedInputStream(new ProgressMonitorInputStream(
					c,
					Messages.getString("AOBootUtil.0") + " " + path, //$NON-NLS-1$ //$NON-NLS-2$
					new FileInputStream(new File(path))
				));
				
				else return new BufferedInputStream(new FileInputStream(new File(path)));
			}
			catch (NullPointerException e) {
				throw e;
			}
			catch (FileNotFoundException e) {
				throw e;
			}
			catch(Throwable e) {
				throw new AOException("Ocurrio un error intentando abrir un archivo en almacenamiento local: " + e); //$NON-NLS-1$
			}
		}
		// Es una URL
		else {
			InputStream tmpStream;
			try {
				if (waitDialog) {
					ProgressMonitorInputStream pmis = new ProgressMonitorInputStream(
						c,
						Messages.getString("AOBootUtil.0") + uri.toURL().toString(), //$NON-NLS-1$
						uri.toURL().openStream()
					);
					pm = pmis.getProgressMonitor(); 
//					pm.setMillisToDecideToPopup(0);
//					pm.setMillisToPopup(0);
					
					// Las URL pocas veces informan del tamano del fichero, asi que ponemos un valor alto
					// por defecto para segurarnos de que el dialogo se muestra
					pm.setMaximum(10000000);
					
					tmpStream = new BufferedInputStream(pmis);
				}
				else tmpStream = new BufferedInputStream(uri.toURL().openStream());
			}
			catch(Throwable e) {
				if (pm != null) pm.close();
				throw new AOException(
					"Ocurrio un error intentando abrir la URI '" + uri.toASCIIString() + "' como URL: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
			// Las firmas via URL fallan en la descarga por temas de Sun, asi que descargamos primero
			// y devolvemos un Stream contra un array de bytes
			byte[] tmpBuffer = new byte[0];
			try {
				tmpBuffer = getDataFromInputStream(tmpStream);
			}
			catch(Throwable e) {
				if (pm != null) pm.close();
				throw new AOException("Error leyendo el fichero remoto '" + uri.toString() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
			
			// Hay que cerrar el ProgressMonitor a mano:
			// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4174850
			if (pm != null) pm.close();
			
			return new java.io.ByteArrayInputStream(tmpBuffer);
		}
	}

	/**
	 * Lee un flujo de datos de entrada y los recupera en forma de array de bytes. Este
	 * m&eacute;todo consume pero no cierra el flujo de datos.
	 * No cierra el flujo de datos de entrada.
	 * @param input Flujo de donde se toman los datos.
	 * @return Los datos obtenidos del flujo.
	 * @throws IOException Si ocurre cualquier error durante la lectura de datos
	 */
	public static byte[] getDataFromInputStream(InputStream input) throws IOException {

		int nBytes = 0;
		byte[] buffer = new byte[1024];
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		while((nBytes = input.read(buffer)) != -1) {
			baos.write(buffer, 0, nBytes);
		}

		return baos.toByteArray();
	}


	/** 
	 * Obtiene el directorio de instalaci&oacute;n del entorno de ejecuci&oacute;n de Java
	 * actualmente en uso.
	 * Copiado de com.sun.deploy.config.Config. 
	 * @return Directorio del entorno de ejecuci&oacute;n de Java
	 * @throws AOException Si ocurre cualquier error durante el proceso
	 */
	public static String getJavaHome() throws AOException {
		String ret = null;
		try {
			ret = System.getProperty("jnlpx.home"); //$NON-NLS-1$
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No se ha podido leer 'jnlp.home', se intentara 'java.home'" //$NON-NLS-1$
			);
		}
		if (ret != null) {
			// use jnlpx.home if available
			// jnlpx.home always point to the location of the
			// jre bin directory (where javaws is installed)
			return ret.substring(0, ret.lastIndexOf(File.separator));
		}
		else {
			try {
			return System.getProperty("java.home"); //$NON-NLS-1$
			} catch (Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"No se ha podido leer 'java.home'" //$NON-NLS-1$
					);
			}
		}
		throw new AOException("No ha podido recuperar el directorio de Java"); //$NON-NLS-1$
	}
		
	/**
	 * Obtiene el directorio principal del sistema operativo del sistema.
	 * @return Directorio principal del sistema operativo
	 */
	public final static String getSystemRoot() {
		if (!System.getProperty("os.name").contains("indows")) return File.separator;
		String systemRoot = null;
		final String defaultSystemRoot = "C:\\WINDOWS";
		try {
			systemRoot = WinRegistryWrapper.getString(
				WinRegistryWrapper.HKEY_LOCAL_MACHINE, 
				"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion", 
				"SystemRoot"
			);
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe(
				"No se ha podido obtener el directorio principal de Windows accediendo al registro, " + 
				"se probara con 'C:\\WINDOWS': " + e
			);
		}
		if (systemRoot == null) {
			final File winSys32 = new File(defaultSystemRoot + "\\SYSTEM32");
			if (winSys32.exists() && winSys32.isDirectory()) return defaultSystemRoot;
		}
		if (systemRoot == null) {
			Logger.getLogger("es.gob.afirma").warning("No se ha encontrado el directorio ra&iacute;z del sistema, se devolver&aacute;: "+File.separator);
			systemRoot = File.separator; 
		}
		return systemRoot;
	}
	
	/**
     * Comprueba que se pueda obtener una URL v&aacute;lida a partir de una URl base y un nombre de fichero.
     * @param urlBase URL base de la nueva URL.
     * @param filename Nombre del fichero que queremos referenciar.
     * @return Devuelve <code>true</code> si es posible crear una URL v&aacute;lida, <code>false</code> en caso contrario.
     */
    public static boolean existRemoteFile(URL urlBase, String filename) {
        if(urlBase == null || filename == null) {
            Logger.getLogger("es.gob.afirma").severe("Ni la URL base ni el nombre de fichero pueden ser nulos"); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }

        URI uri;
        try {
            uri = AOBootUtil.createURI(urlBase.toString()+ "/" + filename); //$NON-NLS-1$
        } catch (Throwable e) {
            return false;
        }

        if(uri == null) {
            return false;
        }
         

        // Es un fichero
        if (uri.getScheme().equals("file")) { //$NON-NLS-1$
            // Retiramos el "file://" de la uri
            String path = uri.getSchemeSpecificPart();
            if(path.startsWith("//")) path = path.substring(2); //$NON-NLS-1$
            return new File(path).exists();
        }
        // Es una URL
        else {
            boolean result = false;
            try {
                InputStream in = uri.toURL().openStream();
                if(in != null) {
                    result = in.read() != -1;
                    in.close();
                }
            } catch (Exception e) {
                result = false;
            }
            return result;
        }
    }

    /**
     * Comprueba que un archivo exista en local y sea un fichero accesible.
     * @param filename Ruta del archivo que se desea comprobar.
     * @return Devuelve <code>true</code> si el fichero existe y <code>false</code> en caso contrario.
     */
    public static boolean existFile(String filename) {
        File file = new File(filename);
        return file.exists() && file.isFile() && file.canRead();
    }
    
    /**
     * Crea una URL a partir de una URL base y un nombre de fichero.
     * @param urlBase URL base del fichero.
     * @param filename Nombre del fichero.
     * @return URL de referencia directa al fichero.
     */
    public static URL createURLFile(URL urlBase, String filename) {
        try {
            //TODO: Tratar el caso de urls con caracteres especiales
            String codeBase = urlBase.toString();
            if(!codeBase.endsWith("/") && !codeBase.endsWith("\\")) codeBase = codeBase + "/"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            return AOBootUtil.createURI(codeBase + filename).toURL();
        } catch (Throwable e) {
            Logger.getLogger("es.gob.afirma").severe("No se pudo crear la referencia al fichero '"+filename+"': "+e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            return null;
        }
    }

    /**
     * Copia un fichero remoto indicado por una URL en un directorio local del sistema.
     * @param file Fichero que se desea descargar.
     * @param dirDest Directorio local.
     * @throws Exception Ocurri&oacute; un error durante la copia del fichero.
     */
    public static void copyRemoteFile(URL file, String dirDest) throws Exception {
        copyRemoteFile(file, dirDest, null);
    }
    
    /**
     * Copia un fichero remoto indicado por una URL en un directorio local del sistema.
     * @param file Fichero que se desea descargar.
     * @param dirDest Directorio local.
     * @param newFilename Nombre con el que se almacenar&aacute; el fichero. Si se indica <code>null</code>
     * se almacena con el mismo nombre que el fichero remoto.
     * @throws Exception Ocurri&oacute; un error durante la copia del fichero.
     */
    public static void copyRemoteFile(URL file, String dirDest, String newFilename) throws Exception {

        if(file == null) {
            throw new NullPointerException("La URL al fichero remoto no puede ser nula"); //$NON-NLS-1$
        }
        if(dirDest == null) {
            throw new NullPointerException("El directorio destino no puede ser nulo"); //$NON-NLS-1$
        }

        // Obtenemos el nombre del fichero destino a partir del directorio de destino y el nombre del nuevo
        // fichero o el del fichero remoto si no se indico uno nuevo
        File outFile = new File(dirDest+(newFilename != null ? "/"+newFilename : file.getPath().substring(file.getPath().lastIndexOf("/")))); //$NON-NLS-1$ //$NON-NLS-2$
        if(!outFile.getParentFile().exists()) {
            outFile.getParentFile().mkdirs();
        }

        InputStream is = AOBootUtil.loadFile(new URI(file.toString()), null, false);
        
        int nBytes = 0;
        byte[] buffer = new byte[1024];
        FileOutputStream fos = new FileOutputStream(outFile);
        while((nBytes = is.read(buffer)) != -1) {
            fos.write(buffer, 0, nBytes);
        }

        try {is.close(); } catch (Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo cerrar el fichero remoto: "+e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        try {fos.close(); } catch (Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo cerrar el fichero local: "+e); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }
    
    /**
     * Borra un directorio y todos sus subdirectorios y ficheros.
     * @param dir Directorio a borrar.
     * @return Devuelve <code>true</code> si el directorio se borr&oacute; completamente,
     * <code>false</code> si no se pudo borrar el directorio o alguno de los ficheros que contiene. 
     */
    public static boolean deleteDir(File dir) {
        if(dir == null || !dir.exists()) {
            return true;
        }

        boolean success = true;
        
        // Si es un directorio, borramos su contenido
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
                if (!deleteDir(new File(dir, children[i]))) {
                    success = false;
                }
            }
        }
    
        // Borramos el propio fichero o directorio
        if(!dir.delete()) {
            Logger.getLogger("es.gob.afirma").severe("No se ha podido eliminar el archivo: "+dir); //$NON-NLS-1$ //$NON-NLS-2$
            success = false;
        }
        return success;
    }
        
    /**
     * Copia un fichero.
     * @param source Fichero origen con el contenido que queremos copiar.
     * @param dest Fichero destino de los datos.
     * @return Devuelve <code>true</code> si la operac&oacute;n  finaliza correctamente,
     * <code>false</code> en caso contrario.
     */
    public static boolean copyFile(File source, File dest) {
    	if (source == null || dest == null) return false;
    	
    	// Si no existe el directorio del fichero destino, lo creamos
    	if(!dest.getParentFile().exists()) {
    		dest.getParentFile().mkdirs();
    	}
    	
    	// Copiamos el directorio
    	try {
    	 FileChannel in = new FileInputStream(source).getChannel();
    	 FileChannel out = new FileOutputStream(dest).getChannel();
         MappedByteBuffer buf = in.map(FileChannel.MapMode.READ_ONLY, 0, in.size());
         out.write(buf);
         
         // Cerramos los canales sin preocuparnos de que lo haga correctamente 
         try { in.close(); } catch (Exception e) {}
         try { out.close(); } catch (Exception e) {}
    	}
    	catch(Throwable e) {
    		Logger.getLogger("es.gob.afirma").severe(
				"No se ha podido copiar el fichero origen '" +
				source.getName() +
				"' al destino '" +
				dest.getName() +
				"': " +
				e
			);
    		return false;
    	}
    	return true;
    }
    
    /**
     * Recupera el texto con un identificador de versi&oacute;n a partir de un properties indicado
     * a trav&eacute;s de un <code>InputStream</code>. Las propiedades del properties que definen la
     * versi&oacute;n son:<br/>
     * <code><ul><li>version.mayor: Versi&oacute;n.</li>
     * <li>version.minor: Versi&oacute;n menor.</li>
     * <li>version.build: Contrucci&oacute;n</li>
     * <li>version.description: Descripci&oacute;n</li></ul></code>
     * El formato en el que se devuelve la versi&oacute;n ser&aacute; siempre:<br/>
     * <code>X.Y.Z Descripci&oacute;n</code><br/>
     * En donde <code>X</code>, <code>Y</code> y <code>Z</code> son la versi&oacute;n, subversi&oacute;n
     * y contrucci&oacute;n del cliente y puede tener uno o m&aacute;s d&iacute;gitos; y 
     * <code>Descripci&oacute;n</code> es un texto libre opcional que puede completar la
     * identificaci&oacute;n de la versi&oacute;n del cliente.</br>
     * Si no se indica alg&uacute;n de los n&uacute;meros de versi&oacute;n se indicar&aacute; cero ('0')
     * y si no se indica la descripci&oacute;n no se mostrar&aacute; nada.
     *  
     * @param is Datos del properties cobn la versi&oacute;n.
     * @return Identificador de la versi&oacute;n.
     */
    public static String getVersion(InputStream is) {
        Properties p = new Properties();
        try {
            p.load(is);
        } catch (Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se han podido obtener los datos de version"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        StringBuilder version = new StringBuilder();
        version.append(p.getProperty("version.mayor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            .append(p.getProperty("version.minor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            .append(p.getProperty("version.build", "0")).append(" ") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            .append(p.getProperty("build", "")); //$NON-NLS-1$ //$NON-NLS-2$
            if(p.containsKey("java")) //$NON-NLS-1$
                version.append(" para Java ").append(p.getProperty("java")).append(" o superior"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            if(p.containsKey("description")) //$NON-NLS-1$
            	version.append(" (").append(p.getProperty("description")).append(")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        return version.toString().trim();
    }
    
    /**
     * Recupera la versi&oacute;n indicada en un fichero "version.properties" en el directorio
     * ra&iacute;z de un fichero ZIP.
     * @param zipFilePath Ruta del fichero Zip.
     * @return Versi&oacute;n del m&oacute;dulo que contiene el Zip.
     */
    public static String getVersionFromZip(String zipFilePath) {
        
        String idVersion;
        try {
            ZipFile zipFile = new ZipFile(zipFilePath);
            InputStream is = zipFile.getInputStream(zipFile.getEntry("version.properties")); //$NON-NLS-1$
            idVersion = AOBootUtil.getVersion(is);
            try {is.close();} catch (Exception e) {}
            try {zipFile.close();} catch (Exception e) {}
        } catch (Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido identificar el cliente de firma instalado"); //$NON-NLS-1$ //$NON-NLS-2$
            idVersion = "0.0.0"; //$NON-NLS-1$
        }
        return idVersion;
    }
    
    /**
     * Recupera la construcci&oacute;n (LITE, MEDIA o COMPLETA) del n&uacute;cleo del
     * cliente Afirma instalado en el sistema. Si no puede recuperarla, devuelve
     * <code>null</code>.
     * @param jarCorePath Ruta del JAR del n&uacute;cleo.
     * @return Construcci&oacute;n del cliente instalado.
     */
    public static String getBuildVersion(String jarCorePath) {
        
        String buildVersion;
        try {
            ZipFile zipFile = new ZipFile(jarCorePath);
            InputStream is = zipFile.getInputStream(zipFile.getEntry("version.properties")); //$NON-NLS-1$
            Properties p = new Properties();
            p.load(is);
            buildVersion = p.getProperty("build"); //$NON-NLS-1$
            try {is.close();} catch (Exception e) {}
            try {zipFile.close();} catch (Exception e) {}
        } catch (Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido identificar el cliente de firma instalado: "+e); //$NON-NLS-1$ //$NON-NLS-2$
            buildVersion = null;
        }
        return buildVersion;
    }
    
    /**
     * Descomprime un JAR comprimido con Pack200 y GZip.
     * @param packgz <i>jar.pack.gz</i> original
     * @param jar JAR resultante del desempaquetado
     * @throws Throwable Si ocurre cualquier problema durante la descompresi&oacute;n
     */
    public static void unpack200gunzip(final InputStream packgz, final OutputStream jar) throws Throwable {
        if (packgz == null) throw new NullPointerException("El pack.gz es nulo");
        InputStream is = new GZIPInputStream(packgz);
        Unpacker u = java.util.jar.Pack200.newUnpacker();
        JarOutputStream jos = new JarOutputStream(jar);
        u.unpack(is, jos);
        jos.flush();
        jos.close();
    }
    
    /**
     * Carga una librer&iacute;a nativa del sistema. 
     * @param path Ruta a la libreria de sistema.
     */
    public static void loadNativeLibrary(String path) {
        
    	boolean copyOK = false;
        int pos = path.lastIndexOf('.');
        File file = new File(path);
        File tempLibrary = null;
        try {
            tempLibrary = File.createTempFile(
            		pos < 1 ? file.getName() : file.getName().substring(0, file.getName().indexOf('.')),
                    pos < 1 || pos == path.length()-1 ? null : path.substring(pos));

            // Copiamos el fichero
            copyOK = copyFile(file, tempLibrary);
        } catch (Exception e) {
            Logger.getLogger("es.gob.afirma").warning("Ocurrio un error al generar una nueva instancia de la libreria "
                    + path + " para su carga: "+e);
        }
        Logger.getLogger("es.gob.afirma").info("Cargamos "+(tempLibrary == null ? path : tempLibrary.getAbsolutePath()));
        System.load((copyOK && tempLibrary != null) ? tempLibrary.getAbsolutePath() : path);
    }
    
    public static Object[] prepareJSParams(Object jsMethodParams) {
    	
    	if (jsMethodParams == null)
    		return null;
    	
    	Object[] params = null;
    	if (jsMethodParams instanceof Object[]) {
    		params = (Object[]) jsMethodParams;
    	} else if (jsMethodParams instanceof String) {
    		String stringParams = (String) jsMethodParams;
    		params = stringParams.split(",");
    	}
    	
    	return params;
    }
}
