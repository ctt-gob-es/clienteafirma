/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.keystores;

import java.io.File;
import java.io.FileInputStream;
import java.lang.reflect.Field;
import java.security.KeyStore;
import java.security.KeyStoreSpi;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttributes;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import com.sun.jndi.toolkit.dir.SearchFilter;

import es.atosorigin.exe.MatchOParser;
import es.atosorigin.exe.PEParser;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCertificatesNotFoundException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.keystores.AOSecMod.ModuleName;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.AOWinNativeUtil;
import es.gob.afirma.misc.WinRegistryWrapper;

/**
 * Utilidades para le manejo de almacenes de claves y certificados.
 */
public final class KeyStoreUtilities {

	private KeyStoreUtilities() {}

	/** 
	 * Crea las l&iacute;neas de configuraci&oacute;n para el proveedor
	 * PKCS#11 de Sun.
	 * @param lib Nombre (con o sin ruta) de la biblioteca PKCS#11
	 * @param name Nombre que queremos tenga el proveedor. CUIDADO: SunPKCS11 a&ntilde;ade el prefijo <i>SunPKCS11-</i>.
	 * @return Fichero con las propiedades de configuracion del proveedor PKCS#11
	 * de Sun para acceder al KeyStore de un token generico.
	 */
	static String createPKCS11ConfigFile(final String lib, String name) {
		if (name == null) name = "AFIRMA-PKCS11";
		final StringBuilder buffer = new StringBuilder("library=");

		//TODO: Ir uno a uno en el ApplicationPath de Java hasta que
		// encontremos la biblioteca, en vez de mirar directamente en
		// system32 y usr/lib

		// Si la biblioteca no existe directamente es que viene sin Path
		// Mozilla devuelve las bibliotecas sin Path
		if (!new java.io.File(lib).exists()) {
			String sysLibDir = AOUtil.getSystemLibDir();
			if(!sysLibDir.endsWith(java.io.File.separator)) sysLibDir += java.io.File.separator;
			buffer.append(sysLibDir);
		}

		buffer
		.append(lib).append("\r\n")
		// Ignoramos la descripcion que se nos proporciona, ya que el proveedor PKCS#11 de Sun
		// falla si llegan espacios o caracteres raros
		.append("name=").append(name).append("\r\n")
		.append("showInfo=false\r\n");
		
		Logger.getLogger("es.gob.afirma").info("Creada configuracion PKCS#11:\r\n" + buffer.toString());
		return buffer.toString();
	}

	/** 
	 * Crea las l&iacute;neas de configuraci&oacute;n para el uso de las bibliotecas
	 * NSS como m&oacute;dulo PKCS#11 por el proveedor de Sun.
	 * @param userProfileDirectory Directorio donde se encuentra el perfil de usuario de
	 *                             Mozilla Firefox
	 * @param libDir Directorio que contiene las bibliotecas NSS
	 * @return Fichero con las propiedades de configuracion del proveedor PKCS#11
	 * de Sun para acceder al KeyStore de Mozilla v&iacute;a NSS.
	 */
	public static String createPKCS11NSSConfigFile(final String userProfileDirectory, final String libDir) {

//		if (System.getProperty("java.version").startsWith("1.5")) {
			// Comprobar el sistema operativo y usar el nombre apropiado para la
			// libreria 'softkn3'
			String softoknLib = "libsoftokn3.so";
			String os = System.getProperty("os.name");  
			if(os.contains("indows")) {
				softoknLib = "softokn3.dll";
			}
			else if (os.startsWith("Mac OS X")) {
				softoknLib = "libsoftokn3.dylib";
			}
//		}
		
		StringBuilder buffer = new StringBuilder("name=NSSCrypto-AFirma\r\n");

		// Java 1.5 tenia un metodo indocumentado para acceder a NSS,
		// http://docs.sun.com/app/docs/doc/819-3671/gcsoc?a=view
		
//        if (System.getProperty("java.version").startsWith("1.5")) {
			buffer
			.append("library=").append(libDir).append(java.io.File.separator).append(softoknLib).append("\r\n")
			.append("attributes=compatibility\r\n")
			.append("slot=2\r\n")
			.append("showInfo=false\r\n")
			.append("nssArgs=\"")
			  .append("configdir='").append(userProfileDirectory).append("' ")
			  .append("certPrefix='' ")
			  .append("keyPrefix='' ")
			  .append("secmod='secmod.db' ")
			  .append("flags=readOnly")
			.append("\"\n")
			;
//			//.append("omitInitialize=true");
//        }
//        else {
			// Inicializacion segun Java 6
//			buffer
//			.append("nssLibraryDirectory=").append(libDir).append("\r\n")
//			.append("nssSecmodDirectory=\"").append(userProfileDirectory).append("\"\r\n")
//			//.append("nssUseSecmod=true\r\n") // No es necesario, con usar nssLibraryDirectory o nssSecModDirectory ya se activa el modo NSS
//			.append("nssDbMode=readOnly\r\n")
//			.append("attributes=compatibility\r\n")
//			.append("nssModule=keystore\r\n")
//			//.append("allowSingleThreadedModules=true\r\n");
//			//.append("nssNetscapeDbWorkaround=true\r\n") // Solo si necesitamos crear claves privadas
//			//.append("showInfo=true\r\n")
//			
//			;
//        }

		//Logger.getLogger("es.gob.afirma").info("Configuracion SunPKCS11 para NSS:\n" + buffer.toString());

		return buffer.toString();
	}
	
	/**
	 * Obtiene el directorio de las bibliotecas NSS (<i>Netscape Security Services</i>) del sistema.
	 * @return Directorio de las bibliotecas NSS del sistema
	 * @throws AOException Cuando no se ha podido encontrar y cargar una versi&oacute;n v&aacute;lidad de NSS.
	 */
	public static String getSystemNSSLibDir() throws AOException {
		String os = System.getProperty("os.name"); 
		if (os.contains("indows")) {
			// Intentamos exraer la ruta de instalacion de Firefox del registro 
			String dir = WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_CURRENT_USER, "Software\\Classes\\FirefoxURL\\shell\\open\\command", "");
			if (dir == null) {
				dir = WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_LOCAL_MACHINE, "SOFTWARE\\Classes\\FirefoxURL\\shell\\open\\command", "");
				if (dir == null) {
					throw new AOException(
							"No se ha podido localizar el directorio de Firefox a traves del registro de Windows");
				}
			}
			
			String regKeyLowCase = dir.toLowerCase();
			int pos = regKeyLowCase.indexOf("firefox.exe");
			if (pos != -1) {
				dir = dir.substring(0, pos);
				if (dir.startsWith("\"")) dir = dir.substring(1);
				if (dir.endsWith(File.separator)) dir = dir.substring(0, dir.length()-1);
	
				File tmpFile = new File(dir);
				if (tmpFile.exists() && tmpFile.isDirectory()) {
					tmpFile = new File(dir + File.separator + "softokn3.dll");
					if (tmpFile.exists()) {
						System.out.println("Informacion sobre el modulo PKCS#11 de NSS:");
						try {
							new PEParser().parse(AOUtil.getDataFromInputStream(new FileInputStream(tmpFile)));
						}
						catch(final Throwable e) {
							System.out.println("No se ha podido obtener la informacion");
						}
						try {
							dir = tmpFile.getParentFile().getCanonicalPath();
						} catch (final Throwable e) {
							if (dir.contains("\u007E")) {
						        dir = AOWinNativeUtil.getLongPathName(dir);
						    }
						}
						if (dir.contains(")") ||
							dir.contains("(") ||
							dir.contains("\u007E")) {
							// Tenemos una ruta con caracteres ilegales para la
							// configuracion de SunPKCS#11 por el bug 6581254:
							// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6581254
							try {
								// Copiamos las DLL necesarias a un temporal y devolvemos el temporal
								File tmp = File.createTempFile("nss", null);
								tmp.delete();
								if (!tmp.mkdir()) throw new AOException("No se ha creado el directorio temporal");
								String dest = tmp.getCanonicalPath() + File.separator;
								AOUtil.copyFile(new File(dir + File.separator + "softokn3.dll"), new File(dest + "softokn3.dll"));
								AOUtil.copyFile(new File(dir + File.separator + "sqlite3.dll"), new File(dest + "sqlite3.dll"));
								AOUtil.copyFile(new File(dir + File.separator + "plc4.dll"), new File(dest + "plc4.dll"));
								AOUtil.copyFile(new File(dir + File.separator + "plds4.dll"), new File(dest + "plds4.dll"));
								AOUtil.copyFile(new File(dir + File.separator + "nspr4.dll"), new File(dest + "nspr4.dll"));
								AOUtil.copyFile(new File(dir + File.separator + "mozcrt19.dll"), new File(dest + "mozcrt19.dll"));
								AOUtil.copyFile(new File(dir + File.separator + "nssutil3.dll"), new File(dest + "nssutil3.dll"));
								AOUtil.copyFile(new File(dir + File.separator + "nss3.dll"), new File(dest + "nss3.dll"));
								return tmp.getCanonicalPath();
							}
							catch(Throwable e) {
								Logger.getLogger("es.gob.afirma").warning(
									"No se ha podido duplicar NSS en un directorio temporal, si esta version de JRE esta afectada por el error 6581254 es posible que no pueda cargarse: " + e
								);
							}
						}
						return dir;
					}
				}
			}
		}
		else if(os.contains("inux") || os.contains("SunOS") || os.contains("olaris")){
			if (new File("/lib/libsoftokn3.so").exists()) {
				return "/lib";
			}
			if (new File("/usr/lib/libsoftokn3.so").exists()) {
				return "/usr/lib";
			}
			if (new File("/usr/lib/nss/libsoftokn3.so").exists()) {
				return "/usr/lib/nss";
			}
		}
		else if(os.startsWith("Mac OS X")) {
			File tmpFile = new File("/Applications/Firefox.app/Contents/MacOS/libsoftokn3.dylib"); 
			if (tmpFile.exists()) {
				System.out.println("Informacion sobre el modulo PKCS#11 de NSS:");
				try {
					new MatchOParser().parse(AOUtil.getDataFromInputStream(new FileInputStream(tmpFile)));
				}
				catch(final Throwable e) {
					System.out.println("No se ha podido obtener la informacion: " + e);
				}
				return "/Applications/Firefox.app/Contents/MacOS";
			}
			tmpFile = new File("/lib/libsoftokn3.dylib"); 
			if (tmpFile.exists()) {
				System.out.println("Informacion sobre el modulo PKCS#11 de NSS:");
				try {
					new MatchOParser().parse(AOUtil.getDataFromInputStream(new FileInputStream(tmpFile)));
				}
				catch(final Throwable e) {
					System.out.println("No se ha podido obtener la informacion: " + e);
				}
				return "/lib";
			}
			tmpFile = new File("/usr/lib/libsoftokn3.dylib"); 
			if (tmpFile.exists()) {
				System.out.println("Informacion sobre el modulo PKCS#11 de NSS:");
				try {
					new MatchOParser().parse(AOUtil.getDataFromInputStream(new FileInputStream(tmpFile)));
				}
				catch(final Throwable e) {
					System.out.println("No se ha podido obtener la informacion: " + e);
				}
				return "/usr/lib";
			}
			tmpFile = new File("/usr/lib/nss/libsoftokn3.dylib"); 
			if (tmpFile.exists()) {
				System.out.println("Informacion sobre el modulo PKCS#11 de NSS:");
				try {
					new MatchOParser().parse(AOUtil.getDataFromInputStream(new FileInputStream(tmpFile)));
				}
				catch(final Throwable e) {
					System.out.println("No se ha podido obtener la informacion: " + e);
				}
				return "/usr/lib/nss";
			}
			// Las versiones Alpha de Firefox se llaman Minefield
			tmpFile = new File("/Applications/Minefield.app/Contents/MacOS/libsoftokn3.dylib"); 
			if (tmpFile.exists()) {
				System.out.println("Informacion sobre el modulo PKCS#11 de NSS:");
				try {
					new MatchOParser().parse(AOUtil.getDataFromInputStream(new FileInputStream(tmpFile)));
				}
				catch(final Throwable e) {
					System.out.println("No se ha podido obtener la informacion: " + e);
				}
				return "/Applications/Minefield.app/Contents/MacOS";
			}
		}
		throw new AOException("No se han encontrado las bibliotecas NSS instaladas en su sistema operativo");
	}


	/**
	 * Obtiene las rutas completas hacia las bibliotecas (.dll o .so) de los m&oacute;dulos de seguridad externos (PKCS#11)
	 * instalados en Mozilla / Firefox, indexados por su descripci&oacute;n dentro de una
	 * <code>Hashtable</code>. 
	 * @return Nombres de las bibliotecas de los m&oacute;dulos de seguridad de Mozilla / Firefox
	 */
	static Hashtable<String, String> getMozillaPKCS11Modules() {
		try {
			final Hashtable<String, String> modsByDesc = new Hashtable<String, String>();
			for(ModuleName module : AOSecMod.getModules(AOUtil.getMozillaUserProfileDirectory())) {
				modsByDesc.put(module.getDescription(), module.getLib());
			}

			return purgeStoresTable(modsByDesc); // Eliminamos las entradas que usen la misma biblioteca
		}
		catch(Throwable t) {
			Logger.getLogger("es.gob.afirma").severe(
					"No se han podido obtener los modulos externos de Mozilla, se devolvera una lista vacia o unicamente con el DNIe: " + t
			);
			return new Hashtable<String, String>(0);
		}
	}

	
	/**
	 * Obtiene el nombre (<i>commonName</i>) de un m&oacute;dulo externo de Mozilla a partir de su
	 * representaci&oacute;n textual.
	 * Este m&eacute;todo es dependiente de la implementaci&oacute;n de <code>toString()</code>
	 * de la clase <code>sun.security.pkcs11.Secmod.Module</code>, ya que no podemos acceder
	 * directamente al atributo <code>slot</code> por ser de tipo <i>friend</i>:
	 * <code><pre> 
	 * public String toString() {
	 *   return 
	 *   commonName + " (" + type + ", " + libraryName + ", slot " + slot + ")";
	 *  }
	 *  </pre></code>
	 * @param description Resultado de una llamada a <code>sun.security.pkcs11.Secmod.Module.toString()</code>
	 * @return Nombre correspondiente al m&oacute;dulo de seguridad
	 */
	static String getMozModuleName(final String description) {
		final int ini = description.indexOf('(');
		if(ini > 0) return description.substring(0, ini).trim();
		return description;
	}
	
	/**
     * Dada una tabla que indexa por descripci&oacute;n los m&oacute;dulos pkcs11, 
     * eliminamos las entradas necesarias para que aparezca una &uacute;nica vez
     * cada m&oacute;dulo PKCS#11. 
     * @param table Tabla con las descripciones de los m&oacute;dulos pkcs11 y las librer&iacute;as asociadas.
     * @return Tabla con los m&oacute;dulos eliminados.
     */
    private static Hashtable<String, String> purgeStoresTable(Hashtable<String, String> table) {

        if(table == null) {
            return new Hashtable<String, String>();
        }

        Hashtable<String, String> purgedTable = new Hashtable<String, String>();
        Set<String> revisedLibs = new HashSet<String>();
        
        String tmpLib;
        for(String key : table.keySet()) {
            tmpLib = table.get(key);
            if(tmpLib.toLowerCase().endsWith(".dll"))
                tmpLib = tmpLib.toLowerCase();

            if(!revisedLibs.contains(tmpLib) && (!tmpLib.toLowerCase().contains("nssckbi"))) {
                purgedTable.put(key, table.get(key));
                revisedLibs.add(tmpLib);
            } else {
                Logger.getLogger("es.gob.afirma").warning("Se eliminara el modulo '"+key+"' porque ya existe uno con la misma biblioteca o es un modulo de certificados raiz: "+table.get(key));
            }
        }

        return purgedTable;
    }
	
    @SuppressWarnings("unchecked")
	static void cleanCAPIDuplicateAliases(KeyStore keyStore) throws Throwable {
		Field field;
		KeyStoreSpi keyStoreVeritable;

		field = keyStore.getClass().getDeclaredField("keyStoreSpi");
		field.setAccessible(true);
		keyStoreVeritable = (KeyStoreSpi)field.get(keyStore);

		if("sun.security.mscapi.KeyStore$MY".equals(keyStoreVeritable.getClass().getName())) {
			Collection entries;
			String alias, hashCode;
			X509Certificate[] certificates;

			field = keyStoreVeritable.getClass().getEnclosingClass().getDeclaredField("entries");
			field.setAccessible(true);
			entries = (Collection)field.get(keyStoreVeritable);

			for(Object entry : entries) {
				field = entry.getClass().getDeclaredField("certChain");
				field.setAccessible(true);
				certificates = (X509Certificate[])field.get(entry);

				hashCode = Integer.toString(certificates[0].hashCode());

				field = entry.getClass().getDeclaredField("alias");
				field.setAccessible(true);
				alias = (String)field.get(entry);
				
				if(!alias.equals(hashCode)) {
					field.set(entry, alias.concat(" - ").concat(hashCode));
				}
			} // for
		} // if

	}

    static void fixFirefoxNSSPath() {
    	if (!System.getProperty("os.name").contains("indows")) return;
    	try {

    		// Obtenemos el PATH, pero del registro directamente, nunca de "java.library.path", que puede estar
    		// alterado por el JRE

    		// Cuidado!! El Path es un REG_EXPAND_SZ, que WinRegistry no soporta, asi que lo obtenemos con un
    		// get() directamente y haciendo el casting a byte[]
    		// Puede traer un caracter extrano al final de la linea, mejor limpiar con trim()
    		// Leemos con un get y luego diferenciamos si es un REG_SZ o un REG_SZ_EXPAND

    		// Buscamos en que controlSet tenemos el PATH
    		String controlSetName = "CurrentControlSet";
    		Object out = WinRegistryWrapper.get(
    				WinRegistryWrapper.HKEY_LOCAL_MACHINE,
    				"SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment",
    				"Path"
    		);

    		// No esta en el current, buscamos entonces en otros entornos, del 0 al 9
    		if(out == null) {
    			int i = 0;
    			do {
    				i++;
    				out = WinRegistryWrapper.get(
    						WinRegistryWrapper.HKEY_LOCAL_MACHINE,
    						"SYSTEM\\ControlSet00"+ i + "\\Control\\Session Manager\\Environment",
    						"Path"
    				);
    			} while (out == null && i < 9);

    			// Establecemos el ControlSet adecuado
    			if(out != null) controlSetName = "ControlSet00" + i; 
    		}
    		String originalPath;
    		if (out instanceof String) originalPath = out.toString();
    		else if (out instanceof byte[]) originalPath = new String((byte[])out);
    		else return; // Es de un tipo desconocido o nulo, no hago nada

    		String[] paths = originalPath.split(System.getProperty("path.separator"));
    		String firefoxHome = getSystemNSSLibDir();

    		StringBuilder pathWithoutFirefoxHome = new StringBuilder();
    		for(int i=0;i<paths.length;i++) {
    			if(firefoxHome.equals(paths[i]) || (paths[i].endsWith(System.getProperty("file.separator")) && firefoxHome.equals(paths[i].substring(0, paths[i].length()-System.getProperty("file.separator").length())))) {
    				// Si es el primero todo esta OK y salimos
    				if (i==0) return;
    				// Si esta pero no es el primero lo quitamos para anadirlo luego al principio
    				continue;
    			}
    			// Este no es, lo anadimos...
    			pathWithoutFirefoxHome.append(paths[i].replace(File.pathSeparator, ""));
    			pathWithoutFirefoxHome.append(';');
    		}

    		// Anadimos el dir de Firefox al resultado
    		originalPath = firefoxHome + ";" + pathWithoutFirefoxHome;

    		// Y lo reintroducimos en el registro
    		// CUIDADO!!! Convierte los REG_SZ_EXPAND en REG_SZ
    		WinRegistryWrapper.setStringValue(
    				WinRegistryWrapper.HKEY_LOCAL_MACHINE,
    				"SYSTEM\\" + controlSetName + "\\Control\\Session Manager\\Environment",
    				"Path",
    				originalPath
    		);

    		// El usuario deberia reiniciar, pero omitimos notificarle...

    	}
    	catch(Throwable e) {}

    }
    
    private final static int ALIAS_MAX_LENGTH = 120;
    
    /**
     * Obtiene una hashtable con las descripciones usuales de los alias de certificados (como claves de estas &uacute;ltimas).
	 * @param alias Alias de los certificados entre los que el usuario debe seleccionar uno
	 * @param kss Listado de KeyStores de donde se han sacadon los alias (debe ser <code>null</code>
	 *           si se quiere usar el m&eacute;todo para seleccionar otra cosa que no sean certificados
	 *           X.509 (como claves de cifrado)
	 * @param keyUsageFilter Filtro que determina que certificados se van a mostrar seg&uacute;n su <code>KeyUsage</code>
	 * @param checkPrivateKeys Indica si se debe comprobar que el certificado tiene clave privada o no, para no mostrar
	 *                         aquellos que carezcan de ella
	 * @param checkValidity Indica si se debe comprobar la validez temporal de un certificado al ser seleccionado
	 * @param showExpiredCertificates Indica si se deben o no mostrar los certificados caducados o aun no v&aacute;lidos 
	 * @param issuerFilter Filtro seg&uacute;n la RFC2254 para el emisor del certificado 
	 * @param subjectFilter Filtro seg&uacute;n la RFC2254 para el titular del certificado
	 * @return Alias seleccionado por el usuario
	 * @throws AOCancelledOperationException Si el usuario cancela manualmente la operaci&oacute;n
	 * @throws AOCertificatesNotFoundException Si no hay certificados que mostrar al usuario
	 */
	public final static Hashtable<String, String> getAlisasesByFriendlyName(
			final String[] alias, 
            final Vector<KeyStore> kss, 
            final Boolean[] keyUsageFilter, 
            final boolean checkPrivateKeys, 
            final boolean checkValidity, 
            final boolean showExpiredCertificates,
            final String issuerFilter,
            final String subjectFilter) throws AOCertificatesNotFoundException {
		
		String [] trimmedAliases = alias.clone();
		
		// Creamos un HashTable con la relacion Alias-Nombre_a_mostrar de los certificados
		Hashtable<String, String> aliassesByFriendlyName = new Hashtable<String, String>(trimmedAliases.length);
		for(String trimmedAlias : trimmedAliases) {
			aliassesByFriendlyName.put(trimmedAlias, trimmedAlias);
		}

		String tmpCN;
		String issuerTmpCN;
		
		X509Certificate tmpCert;
		if (kss != null && kss.size() > 0) {
		    
		    KeyStore ks = null;
			for(String al : aliassesByFriendlyName.keySet().toArray(new String[aliassesByFriendlyName.size()])) {
				tmpCert = null;
				
				// Seleccionamos el KeyStore en donde se encuentra el alias
				for(KeyStore tmpKs : kss) {
				    try {
				        tmpCert = (X509Certificate) tmpKs.getCertificate(al);
				    }
				    catch(Throwable e) {
				        Logger.getLogger("es.gob.afirma").warning("No se ha inicializado el KeyStore indicado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				        continue;
				    }
				    if(tmpCert != null) {
				        ks = tmpKs;
				        break;
				    }
				}
				
				// Si no tenemos Store para el alias en curso, pasamos al siguiente alias
				if (ks == null) continue;
				
				if (tmpCert == null) Logger.getLogger("es.gob.afirma").warning("El KeyStore no permite extraer el certificado publico para el siguiente alias: " + al); //$NON-NLS-1$ //$NON-NLS-2$
				
				if (!showExpiredCertificates && tmpCert!=null) {
					try {
						tmpCert.checkValidity();
					}
					catch(Throwable e) {
						Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
							"Se ocultara el certificado '" + al + "' por no ser valido: " + e //$NON-NLS-1$ //$NON-NLS-2$
						);
						aliassesByFriendlyName.remove(al);
						continue;
					}
				}
				
				if (checkPrivateKeys && tmpCert!=null) {
					try {
						if (!(ks.getEntry(al, new KeyStore.PasswordProtection(new char[0])) instanceof KeyStore.PrivateKeyEntry)) {
	                        aliassesByFriendlyName.remove(al);
							Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
								"El certificado '" + al + "' no era tipo trusted pero su clave tampoco era de tipo privada, no se mostrara" //$NON-NLS-1$ //$NON-NLS-2$
							);
							continue;
						}
					}
					catch(UnsupportedOperationException e) {
						aliassesByFriendlyName.remove(al);
                        Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
                            "El certificado '" + al + "' no se mostrara por no soportar operaciones de clave privada" //$NON-NLS-1$ //$NON-NLS-2$
                        );
						continue;
					}
					catch(Throwable e) {
						Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
							"Se ha incluido un certificado (" + al + ") con clave privada inaccesible: " + e //$NON-NLS-1$ //$NON-NLS-2$
						);
					}
				}
				
				if (
						AOUtil.matchesKeyUsageFilter(tmpCert, keyUsageFilter) &&
						KeyStoreUtilities.filterIssuerByRFC2254(issuerFilter, tmpCert) &&
						KeyStoreUtilities.filterSubjectByRFC2254(subjectFilter, tmpCert)
				) {
					tmpCN = AOUtil.getCN(tmpCert);
					issuerTmpCN = AOUtil.getCN(tmpCert.getIssuerX500Principal().getName());
					
					if (tmpCN != null && issuerTmpCN != null) {
						aliassesByFriendlyName.put(al, tmpCN + " (" + issuerTmpCN + ")"); //$NON-NLS-1$ //$NON-NLS-2$
					}
					
					else if (tmpCN != null /*&& isValidString(tmpCN)*/) {
						aliassesByFriendlyName.put(al, tmpCN);
					}
					else {
						// Hacemos un trim() antes de insertar, porque los alias de los certificados de las tarjetas
						// ceres terminan con un '\r', que se ve como un caracter extrano
						aliassesByFriendlyName.put(al, al.trim());
					}
				}
				else {
					// Eliminamos aquellos certificados que no hayan encajado
					Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
							"El certificado '" + al + "' no se mostrara por no cumplir el filtro de uso" //$NON-NLS-1$ //$NON-NLS-2$
					);
					aliassesByFriendlyName.remove(al);
				}
			}
		}
		
		else {
			
			// Vamos a ver si en vez de un alias nos llega un Principal X.500 completo,
			// en cuyo caso es muy largo como para mostrase y mostrariamos solo el
			// CN o una version truncada si no nos cuela como X.500.
			// En este bucle usamos la clave tanto como clave como valor porque asi se ha inicializado
			// el HashTable.
			for(String al : aliassesByFriendlyName.keySet().toArray(new String[aliassesByFriendlyName.size()])) {
				String value = aliassesByFriendlyName.get(al);
				if (value.length()>ALIAS_MAX_LENGTH ) {
					tmpCN = AOUtil.getCN(value);
					if (tmpCN != null) aliassesByFriendlyName.put(al, tmpCN);
					else aliassesByFriendlyName.put(al, value.substring(0, ALIAS_MAX_LENGTH-3) + "..."); //$NON-NLS-1$
				}
				// Hacemos un trim() antes de insertar, porque los alias de los certificados de las tarjetas
				// ceres terminan con un '\r', que se ve como un caracter extrano.
				// Esto ya lo habremos hecho anteriormente si teniamos KeyStore
				else aliassesByFriendlyName.put(al, value.trim());
			}
		}
		
		return aliassesByFriendlyName;

    }
	
	private static boolean filterSubjectByRFC2254(final String filter, final X509Certificate cert) {
		if (cert == null || filter == null) return true;
		return filterRFC2254(filter, cert.getSubjectDN().toString());
	}
	
	private static boolean filterIssuerByRFC2254(final String filter, final X509Certificate cert) {
		if (cert == null || filter == null) return true;
		return filterRFC2254(filter, cert.getIssuerDN().toString());
	}
	
	/**
	 * Indica si un nombres LDAP se ajusta a los requisitos de un filtro.
	 * @param f Filtro seg&uacute;n la RFC2254.
	 * @param name Nombre LDAP al que se debe aplicar el filtro.
	 * @return <code>true</code> si el nombre LDAP es nulo o se adec&uacute;a al filtro o este &uacute;ltimo es nulo,
	 *         <code>false</code> en caso contrario 
	 */
	private static boolean filterRFC2254(String f, String name) {
		try {
			return filterRFC2254(f, new LdapName(name));
		}
		catch(Throwable e) {
			e.printStackTrace();
			Logger.getLogger("es.gob.afirma").warning(
				"No ha sido posible filtrar el certificado (filtro: '" +
				f +
				"', nombre: '" +
				name +
				"'), no se eliminara del listado: " +
				e
			);
			return true;
		}
	}
	
	/**
	 * Indica si un nombres LDAP se ajusta a los requisitos de un filtro.
	 * @param f Filtro seg&uacute;n la RFC2254.
	 * @param name Nombre LDAP al que se debe aplicar el filtro.
	 * @return <code>true</code> si el nombre LDAP es nulo o se adec&uacute;a al filtro o este &uacute;ltimo es nulo,
	 *         <code>false</code> en caso contrario 
	 */
	private static boolean filterRFC2254(String f, LdapName name) {
		if (f == null || name == null) return true;
		try {
			List<Rdn> rdns = name.getRdns();
			if (rdns == null || (rdns.isEmpty())) {
				Logger.getLogger("es.gob.afirma").warning(
					"El nombre proporcionado para filtrar no contiene atributos, no se mostrara el certificado en el listado"
				);
				return false;
			}
			Attributes attrs = new BasicAttributes(true);
			for (Rdn rdn : rdns) attrs.put(rdn.getType(), rdn.getValue());
			return new SearchFilter(f).check(attrs);
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").warning(
				"No ha sido posible filtrar el certificado (filtro: '" +
				f +
				"', nombre: '" +
				name +
				"'), no se eliminara del listado: " +
				e
			);
			return true;
		}
	}

	/**
	 * Carga las dependencias de la biblioteca "softokn3" necesaria para acceder
	 * al almac&eacute;n de certificados. Hacemos esto por precauci&oacute;n ya
	 * que esto se har&iacute;a autom&aacute;ticamente si las dependencias estuviesen
	 * en el PATH del sistema.
	 * @param nssDirectory Directorio en donde se encuentran las bibliotecas de NSS.
	 */
	static void loadNSSDependencies(String nssDirectory) {
		
		for(String libPath : getSoftkn3Dependencies()) {			
			System.load(nssDirectory + java.io.File.separator + libPath);
		}
	}
	
	/**
	 * Recupera el listado de dependencias de la biblioteca "softkn3"
	 * para el sistema operativo en el que se est&aacute; ejecutando la
	 * aplicaci&oacute;n. Los nombres apareceran ordenados de tal forma
	 * las bibliotecas no tengan dependencias de otra que no haya aparecido
	 * anterioremente en la lista.
	 * @return Listado con los nombres de las bibliotecas. 
	 */
	private static String[] getSoftkn3Dependencies() {
		
		String[] libs = new String[0];
		
		if (System.getProperty("os.name").contains("indows")) {
			libs = new String[] {
					System.mapLibraryName("mozcrt19"),
					System.mapLibraryName("nspr4"),
					System.mapLibraryName("plds4"),
					System.mapLibraryName("plc4"),
					System.mapLibraryName("nssutil3"),
					System.mapLibraryName("sqlite3")
			};
		}
		
		return libs;
	}
}
