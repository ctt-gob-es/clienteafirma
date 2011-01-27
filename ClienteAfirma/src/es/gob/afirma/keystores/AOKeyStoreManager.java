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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.security.KeyStore;
import java.security.Provider;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.Vector;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCertificateKeyException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOKeyStoreManagerException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;

/** 
 * Clase gestora de claves y certificados.
 * B&aacute;sicamente se encarga de crear KeyStores de distintos tipos, utilizando el
 * proveedor JCA apropiado para cada caso  
 * @version 0.3
 */
public class AOKeyStoreManager {
	
	/**
	 * Instancia del provider de NSS. S&oacute;lo se permite una instancia
	 * de esta clase, as&iacute; que la cacheamos.
	 */
	protected Provider nssProvider = null; 
	
	/**
	 * Instancia del provider CAPI de Sun. Aunque se permite m&aacute;s de una instancia
	 * de este provider, lo cacheamos para evitar problemas.
	 */
	private static Provider sunMSCAPIProvider = null;
		
	/** Almac&eacute;n de claves. */
	private KeyStore ks;
	
	/** Tipo de almac&eacute;n. */
	private AOConstants.AOKeyStore ksType;
	
	/**
	 * Devuelve el tipo de almac&eacute;n de claves.
	 * @return Tipo de almac&eacute;n de claves
	 */
	public AOConstants.AOKeyStore getType() {
		return ksType;
	}
	
	/**
	 * Obtiene un almac&eacute;n de claves ya inicializado.
	 * Se encarga tambi&eacute;n de a&ntilde;adir o retirar los <i>Provider</i> necesarios para
	 * operar con dicho almac&eacute;n
	 * @param type Tipo del almac&eacute;n de claves
	 * @param store Flujo para la lectura directa del almac&acute;n de claves (solo para los almacenes en disco)
	 * @param pssCallBack CallBack encargado de recuperar la contrase¬ntilde;a del Keystore
	 * @param params Par&aacute;metros adicionales
	 * @return Almac&eacute;n de claves solicitado (<b>ya inicializado</b>, pueden ser varios en el caso de Mozilla, el interno y los externos)
	 * @throws AOException Cuando ocurre cualquier problema durante la inicializaci&oacute;n
	 */
	public Vector<KeyStore> init(AOConstants.AOKeyStore type, 
			                     final InputStream store, 
			                     PasswordCallback pssCallBack, 
			                     final Object[] params) throws AOException {
	    
		System.out.println("Inicializamos el almacen de tipo: "+type);
		
		
		
	    final Vector<KeyStore> ret = new Vector<KeyStore>(1);
	    
		if(type == null) {
			Logger.getLogger("es.gob.afirma").severe("Se ha solicitado inicializar un AOKeyStore nulo, se intentara u");
			if(System.getProperty("os.name").contains("indows")) type = AOConstants.AOKeyStore.WINDOWS;
			else type = AOConstants.AOKeyStore.MOZ_UNI;
		}
		ksType = type;

		if(pssCallBack == null) pssCallBack = new NullPasswordCallback();

		if (type == AOConstants.AOKeyStore.SINGLE) {
			
			if (store==null) throw new AOException("Es necesario proporcionar el fichero X.509 o PKCS#7");
			
			final Provider pkcs7Provider = new SingleCertKeyStoreProvider();
			Security.addProvider(pkcs7Provider);
			
			try {
				ks = KeyStore.getInstance(type.getName(), pkcs7Provider);
			}
			catch(final Throwable e) {
				//e.printStackTrace();
				throw new AOKeyStoreManagerException(
					"No se ha podido obtener el almacen " + type.getName() + " solicitado: " + e
				);
			}
			
			try {
				ks.load(
					store,
					pssCallBack.getPassword()
				);
			}
			catch (final AOCancelledOperationException e) {
				throw e;
			}
			catch(final Throwable e) {
                throw new AOKeyStoreManagerException(
                	"No se ha podido abrir el almacen " + type.getName() + " solicitado", e
                );
			}
			ret.add(ks);
			return ret;
			
		}
		
		else if (type == AOConstants.AOKeyStore.JAVA ||
				 type == AOConstants.AOKeyStore.JAVACE ||
				 type == AOConstants.AOKeyStore.JCEKS) {
			// Suponemos que el proveedor SunJSSE esta instalado. Hay que tener cuidado con esto
			// si alguna vez se usa JSS, que a veces lo retira
			if (store==null) throw new AOException("Es necesario proporcionar el fichero KeyStore");

			try {
				ks = KeyStore.getInstance(type.getName());
			}
			catch(final Throwable e) {
			    //e.printStackTrace();
				throw new AOKeyStoreManagerException(
					"No se ha podido obtener el almac\u00E9n de nombre '" + type.getName() + "': " + e
				);
			}

			//TODO: Revisar si el KeyStore de Java requiere contrasena
			try {
				ks.load(
					store,
					pssCallBack.getPassword()
				);
			}
			catch (final AOCancelledOperationException e) {
				throw e;
			}
			catch(final Throwable e) {
			    Logger.getLogger("es.gob.afirma").warning("No se ha podido abrir el almacen " 
                        + type.getName() + " solicitado : " + e);
                throw new AOKeyStoreManagerException(
                        "No se ha podido abrir el almac\u00E9n " + type.getName() + " solicitado.", e
                );
			}
	        ret.add(ks);
	        return ret;
		}

		else if (type == AOConstants.AOKeyStore.PKCS12) {

			// Suponemos que el proveedor SunJSSE esta instalado. Hay que tener cuidado con esto
			// si alguna vez se usa JSS, que a veces lo retira

			if (store==null) throw new AOException("Es necesario proporcionar el fichero PKCS12 / PFX");

			try {
				ks = KeyStore.getInstance(type.getName());
			}
			catch(final Throwable e) {
                //e.printStackTrace();
                throw new AOKeyStoreManagerException(
                    "No se ha podido obtener el almac\u00E9n de nombre '" + type.getName() + "': " + e
                );
			}
			try {
				ks.load(
					store,
					pssCallBack.getPassword()
				);
			}
			catch (final AOCancelledOperationException e) {
				throw e;
			}
			catch(final Throwable e) {
                Logger.getLogger("es.gob.afirma").warning(
            		"No se ha podido abrir el almacen " + type.getName() + " solicitado : " + e
        		);
                throw new AOKeyStoreManagerException(
                    "No se ha podido abrir el almac\u00E9n " + type.getName() + " solicitado.", e
                );
			}
	        ret.add(ks);
	        return ret;
	        
		}
		
		else if (type == AOConstants.AOKeyStore.WINDOWS || type == AOConstants.AOKeyStore.WINROOT) {

			// Si no se ha agregado el proveedor CAPI de Sun, lo anadimos
			// En java 6 viene instalado de serie, pero no pasa nada por
			// reinstalarlo
			if(sunMSCAPIProvider == null) {
				try {
					sunMSCAPIProvider = (Provider)Class.forName("sun.security.mscapi.SunMSCAPI").newInstance();
					Security.insertProviderAt(sunMSCAPIProvider, 1);
				}
				catch(final Throwable e) {
					Logger.getLogger("es.gob.afirma").warning(
						"No se ha podido instanciar el proveedor 'sun.security.mscapi.SunMSCAPI': " + e
					);
				}
			}
			
			// Inicializamos
			try {
				ks = KeyStore.getInstance(type.getName());
			}
			catch(final Throwable e) {
				//e.printStackTrace();
				throw new AOKeyStoreManagerException(
					"No se ha podido obtener el KeyStore de nombre '" + type.getName() +
					"' del Provider 'sun.security.mscapi.SunMSCAPI': " + e
				);
			}

			Logger.getLogger("es.gob.afirma").info("Cargando KeyStore de Windows.");
			try {
				ks.load(store, pssCallBack.getPassword());
			}
			catch (AOCancelledOperationException e) {
				throw e;
			}
			catch(Throwable e) {
				e.printStackTrace();
				throw new AOKeyStoreManagerException(
					"No se ha podido inicializar el KeyStore de nombre '" + type.getName() +
					"' del Provider 'sun.security.mscapi.SunMSCAPI': " + e
				);
			}
			
			// Tratamos los alias repetidos, situacion problematica afectada por el bug
			// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6483657
			// Este solo se da con SunMSCAPI
			try {
				KeyStoreUtilities.cleanCAPIDuplicateAliases(ks);
			}
			catch(Throwable e) {
				Logger.getLogger("es.gob.afirma").warning(
					"No se han podido tratar los alias duplicados: " + e
				);
			}
			
			ret.add(ks);
            return ret;
		}
		
		else if (type == AOConstants.AOKeyStore.WINCA || type == AOConstants.AOKeyStore.WINADDRESSBOOK) {
			
			// Nos aseguramos de que SunMSCAPI este cargado, para que la DLL sunmscapi.dll tambien lo este
			if (Security.getProvider("SunMSCAPI") == null) {
				try {
					Security.addProvider((Provider)Class.forName("sun.security.mscapi.SunMSCAPI").newInstance());
				} catch (Throwable e) {
					throw new AOKeyStoreManagerException(
							"No se ha podido obtener el KeyStore de nombre '" + type.getName() +
							"' por no poderse cargar el Provider 'sun.security.mscapi.SunMSCAPI': " + e
					);
				}
			}
			Provider p = Security.getProvider("SunMSCAPIAddressBook");
			if (p == null) {
				p = new SunMSCAPIAddressBook();
				Security.addProvider(p);
			}
			
			try {
				ks = KeyStore.getInstance(type.getName(), p);
			}
			catch(final Throwable e) {
				throw new AOException("Error obteniendo el almacen '" + type.getName() + "' del proveedor SunMSCAPIAddressBook: " + e);
			}
			
			try {
				ks.load(null, null);
			}
			catch(final Throwable e) {
				throw new AOException("Error inicializando el proveedor SunMSCAPIAddressBook: " + e);
			}

			ret.add(ks);
			return ret;
		}
		
		else if (type == AOConstants.AOKeyStore.PKCS11) {
			// En el "params" debemos traer los parametros:
			// - p11lib: Biblioteca PKCS#11, debe estar en el Path (Windows) o en el
			//           LD_LIBRARY_PATH (UNIX, Linux, Mac OS X)
			//  -desc: Descripcion del token PKCS#11 (opcional)
			
			// Anadimos el proveedor PKCS11 de Sun
			if (params==null || params.length < 2) throw new AOException(
					"No se puede acceder al KeyStore PKCS#11 si no se especifica la biblioteca"
			);
			final String p11lib;
			if (params[0] != null) p11lib = params[0].toString();
			else throw new NullPointerException(
					"No se puede acceder al KeyStore PKCS#11 si se especifica una biblioteca nula"
			);
			
			// Agregamos un nombre a cada PKCS#11 para asegurarnos de no se agregan mas de una vez como provider.
			// Si ya se cargo el PKCS#11 anteriormente, se volvera a instanciar.
			final String p11ProviderName = new File(p11lib).getName().replace('.', '_').replace(' ', '_');
			Provider p11Provider = Security.getProvider("SunPKCS11-" + p11ProviderName);

		    if (p11Provider == null) {
		    	
		    	Constructor<?> sunPKCS11Contructor;
		    	try {
		    		sunPKCS11Contructor =
		    			Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class);
		    	} catch (Throwable e) {
		    		throw new AOKeyStoreManagerException(
		    				"No se ha podido construir un proveedor de tipo 'sun.security.pkcs11.SunPKCS11': " + e
		    		);
		    	}
		    	
		    	//TODO:Borrar
		    	System.out.println(KeyStoreUtilities.createPKCS11ConfigFile(p11lib, p11ProviderName));
		    	System.out.println("---");
		    	
		    	try {
		    		p11Provider = (Provider)sunPKCS11Contructor.newInstance(		//TODO: InvocationTargetException
		    				new ByteArrayInputStream(
		    						KeyStoreUtilities.createPKCS11ConfigFile(p11lib, p11ProviderName).getBytes()
		    				)
		    		);
		    	}
		    	catch(final Throwable e) {
		    		// El PKCS#11 del DNIe a veces falla a la primera pero va correctamente a la segunda
		    		// asi que reintentamos una vez mas
		    		try {
			    		p11Provider = (Provider)sunPKCS11Contructor.newInstance(
		    				new ByteArrayInputStream(
	    						KeyStoreUtilities.createPKCS11ConfigFile(p11lib, p11ProviderName).getBytes()
		    				)
			    		);
		    		}
		    		catch(final Throwable ex) {
			    		throw new AOException(
		    				"Error instanciando el proveedor SunPKCS11 para la la biblioteca " + p11lib + ": " + e
			    		);	
		    		}
		    	}
		    	try {
		    		Security.addProvider(p11Provider);
		    	}
		    	catch(final Throwable e) {
		    		throw new AOException(
		    				"Error instalando el proveedor SunPKCS11 para la la biblioteca " + p11lib + ": " + e
		    		);
		    	}
		    }
		    else {
		    	Logger.getLogger("es.gob.afirma").info("El proveedor PKCS#11 solicitado ya estaba instanciado, se reutilizara esa instancia: " + p11Provider.getName());
		    }
		    
			try {
				ks = KeyStore.getInstance(type.getName(), p11Provider);
			} 
			catch(final Throwable e) {
				Security.removeProvider(p11Provider.getName());
				p11Provider = null;
				//e.printStackTrace();
				throw new AOKeyStoreManagerException(
					"No se ha podido obtener el KeyStore de nombre '" + type.getName() +
					"' del Provider 'sun.security.pkcs11.SunPKCS11': " + e
				);
			}
			
			try {
				ks.load(
					store,
					pssCallBack.getPassword()
				);
			}
			catch (final AOCancelledOperationException e) {
				throw e;
			}
			catch(final Throwable e) {
				//e.printStackTrace();
				Security.removeProvider(p11Provider.getName());
				p11Provider = null;
				throw new AOKeyStoreManagerException(
					"No se ha podido inicializar el KeyStore de nombre '" + type.getName() +
					"' del Provider 'sun.security.pkcs11.SunPKCS11': " + e
				);
			}
			ret.add(ks);
            return ret;
		}

		else if (type == AOConstants.AOKeyStore.MOZILLA) {	

			// Usamos NSS

			// En el "params" debemos traer los parametros:
			// - mozillaProfileDir: Directorio de configuracion de Mozilla / Firefox
			// - NSSLibDir: Directorio con las librerias de NSS
			// Se ha detectado que es necesario disponder de algunas librerias de NSS en el directorio
			// de Mozilla / Firefox.

			// Anadimos el proveedor PKCS11 de Sun
			if (params==null || params.length < 2) throw new AOException(
				"No se puede acceder al KeyStore PKCS#11 de NSS si no se especifica el directorio del perfil de " +
				"usuario y la ubicacion de las bibliotecas"
			);
			String mozillaProfileDir = params[0].toString();
			String NSSLibDir = params[1].toString();
			
			Logger.getLogger("es.gob.afirma").info("Se usaran las bibliotecas NSS del siguiente directorio: "+NSSLibDir);
			
			nssProvider = Security.getProvider("SunPKCS11-NSSCrypto-AFirma");
			
			if(nssProvider == null) {	
				KeyStoreUtilities.loadNSSDependencies(NSSLibDir);
				try {
//					nssProvider = new sun.security.pkcs11.SunPKCS11(new ByteArrayInputStream(KeyStoreUtilities.createPKCS11NSSConfigFile(mozillaProfileDir, NSSLibDir).getBytes()));
					Constructor<?> sunPKCS11Constructor = Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class);
					nssProvider = (Provider)sunPKCS11Constructor.newInstance(
						new ByteArrayInputStream(
								KeyStoreUtilities.createPKCS11NSSConfigFile(mozillaProfileDir, NSSLibDir).getBytes())
					);
				}
				catch(Throwable e) {
					e.printStackTrace();
					//Logger.getLogger("es.gob.afirma").severe("No se ha podido instanciar la clase SunPKCS11, se abortara la operacion: "+e);
					throw new AOException("No se ha podido instanciar la clase SunPKCS11, se abortara la operacion: " + e);
				}
				Security.addProvider(nssProvider);
			}

			try {
				ks = KeyStore.getInstance(type.getName(), nssProvider);
			} 
			catch(Throwable e) {
				e.printStackTrace();
				throw new AOKeyStoreManagerException(
					"No se ha podido obtener el KeyStore de nombre '" + type.getName() +
					"' del Provider 'sun.security.pkcs11.SunPKCS11': " + e
				);
			}

			try {
				ks.load(
					store,
					pssCallBack.getPassword()
				);
			}
			catch (AOCancelledOperationException e) {
				throw e;
			}
			catch(Throwable e) {
			    e.printStackTrace();
				throw new AOKeyStoreManagerException(
					"No se ha podido inicializar el KeyStore de nombre '" + type.getName() +
					"' del Provider 'sun.security.pkcs11.SunPKCS11': " + e
				);
			}
			ret.add(ks);
            return ret;
		}

		else if (type == AOConstants.AOKeyStore.APPLE) {

			//Anadimos el proveedor de Apple
			try {
				Security.insertProviderAt((Provider)Class.forName("com.apple.crypto.provider.Apple").newInstance(), 0);
			} 
			catch(Throwable e) {
				e.printStackTrace();
				throw new AOException("No se ha podido instanciar el proveedor 'com.apple.crypto.provider.Apple': " + e);
			}

			// Inicializamos
			try {
				ks = KeyStore.getInstance(type.getName());
			}
			catch(Throwable e) {
				e.printStackTrace();
				throw new AOKeyStoreManagerException(
						"No se ha podido obtener el KeyStore de nombre '" + type.getName() +
						"' del Provider 'com.apple.crypto.provider.Apple': " + e
				);
			}

			try {
				ks.load(
						store,
						pssCallBack.getPassword()
				);
			}
			catch (AOCancelledOperationException e) {
				throw e;
			}
			catch(Throwable e) {
				e.printStackTrace();
				throw new AOKeyStoreManagerException(
						"No se ha podido inicializar el KeyStore de nombre '" + type.getName() +
						"' del Provider 'com.apple.crypto.provider.Apple': " + e
				);
			}
			ret.add(ks);
            return ret;
		}
		
		throw new UnsupportedOperationException("Tipo de almacen no soportado");
	}
			
	/**
	 * Obtiene la clave privada de un certificado.
	 * @param alias Alias del certificado
	 * @param pssCallback <i>CallBback</i> para obtener la contrase&ntilde;a del certificado que contiene la clave 
	 * @return Clave privada del certificado correspondiente al alias
	 * @throws AOCancelledOperationException Cuando el usuario cancela el proceso antes de que finalice
	 * @throws AOCertificateKeyException Cuando ocurren errores obteniendo la clave privada del certificado
	 */
	public KeyStore.PrivateKeyEntry getKeyEntry(String alias, PasswordCallback pssCallback) throws AOCancelledOperationException, AOCertificateKeyException {
		if (ks == null) throw new NullPointerException("Se han pedido claves a un almacen no inicializado");
		
		KeyStore.PrivateKeyEntry keyEntry;
		try {
			keyEntry = (KeyStore.PrivateKeyEntry)ks.getEntry(
					alias,
					new KeyStore.PasswordProtection(pssCallback.getPassword())
			);
		} catch (AOCancelledOperationException e) {
			throw e;
		} 
		catch(Throwable e) {
			throw new AOCertificateKeyException("Error intentando obtener la clave privada: "+ e);
		}
		
		return keyEntry;
	}
	
	/**
	 * Obtiene el certificado correspondiente a una clave privada.
	 * @param privateKeyEntry Clave privada del certificado
	 * @return Certificado cuya clave privada es la indicada
	 */
	public X509Certificate getCertificate(KeyStore.PrivateKeyEntry privateKeyEntry) {
		return (X509Certificate)privateKeyEntry.getCertificate();
	}
	
	/**
	 * Obtiene un certificado del keystore activo a partir de su alias.
	 * @param alias Alias del certificado.
	 * @return Certificado.
	 */
	public Certificate getCertificate(String alias) {
	    if (ks == null) {
           Logger.getLogger("es.gob.afirma").warning(
               "El KeyStore actual no esta inicializado, por lo que no se pudo recuperar el certificado para el alias '" + alias + "'"
           );
	        return null;
	    }
	    try {
	        return ks.getCertificate(alias);
	    }
	    catch(Throwable e) {
	        Logger.getLogger("es.gob.afirma").warning(
	            "El KeyStore actual no contenia o no pudo recuperar el certificado para el alias '" + alias + "': " + e
	        );
	        return null;
	    }
	}
	
	/**
	 * Obtiene todos los alias de los certificados del almac&eacute;n actual.
	 * @return Todos los alias encontrados en el almac&eacute;n actual
	 */
	public String[] getAliases() {
		
		Enumeration<String> aliases;
		if (ks == null) throw new NullPointerException("Se han pedido los alias de un almacen no inicializado");
		
		Logger.getLogger("es.gob.afirma").info("Solicitando los alias al KeyStore (" + ks.getProvider() + ")");
		
		try {
			aliases = ks.aliases();
		} 
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Ocurrio un error intentando obtener los alias del almacen de claves, se devolvera " +
					"una enumeracion vacia: " + e
			);
			return new String[0];
		}
		
		String currAlias;
		Vector<String> v = new Vector<String>();
		
		Logger.getLogger("es.gob.afirma").info("Componiendo el vector de alias");
		
		for(;aliases.hasMoreElements();) {
			currAlias = aliases.nextElement().toString();
			v.add(currAlias);
		}
	
		return v.toArray(new String[0]);
		
	}

	
	/**
	 * Devualve el <code>keyStore</code> en uso.
	 * @return Almac&eacute;n de claves (<code>KeyStore</code>) actual
	 */
	public Vector<KeyStore> getKeyStores() {
	    Vector<KeyStore> ret = new Vector<KeyStore>(1);
	    ret.add(ks);
		return ret;
	}
	
	/**
	 * Recupera el repositorio que posea la descripci&oacute;n indicada. Si no existe un
	 * keystore con la descripci&oacute;n indicada, se devuleve <code>null</code>.
	 * @param description Descripci&oacute;n del repositorio que se desea recuperar.  
	 * @return KeyStore Repositorio de certificados.
	 */
	public static AOKeyStore getKeyStore(String description) {
	    AOKeyStore keystore = null;
	    for(AOKeyStore tempKs : AOKeyStore.values()) {
	        if(tempKs.getDescription().equals(description)) {
	            return tempKs;
	        }
	    }
	    return keystore;
	}
	
	@Override
	public String toString() {
		StringBuilder ret = new StringBuilder("Gestor de almacenes de claves");
		if (ksType !=null) {
			String tmpStr = ksType.getDescription();
			if (tmpStr != null) {
				ret.append(" de tipo ");
				ret.append(tmpStr);
			}
			tmpStr = ksType.getName();
			if (tmpStr != null) {
				ret.append(" con nombre ");
				ret.append(tmpStr);
			}
			ret.append(" de clase ");
			ret.append(ksType.toString());
		}
		return ret.toString();
	}
}
