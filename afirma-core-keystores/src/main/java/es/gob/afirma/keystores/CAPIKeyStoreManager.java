/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.InputStream;
import java.lang.reflect.Field;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.KeyStoreSpi;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.UnrecoverableEntryException;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Collections;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.InvalidOSException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.callbacks.FirstEmptyThenPinUiPasswordCallback;

/** Clase gestora de claves y certificados en los almacenes <i>ROOT</i> y <i>MY</i> de CAPI.
 * @version 0.1 */
public final class CAPIKeyStoreManager extends AOKeyStoreManager {

	// Se persiste de forma estatica
	private static KeyStore capiKsMy = null;

	CAPIKeyStoreManager() {
		setKeyStoreType(AOKeyStore.WINDOWS);
	}

    /** Obtiene la clave privada de un certificado.
     * @param alias Alias del certificado.
     * @return Clave privada del certificado correspondiente al alias.
     * @throws KeyStoreException Cuando ocurren errores en el tratamiento del almac&eacute;n de claves.
     * @throws NoSuchAlgorithmException Cuando no se puede identificar el algoritmo para la
     *                                  recuperaci&oacute;n de la clave.
     * @throws UnrecoverableEntryException Si la contrase&ntilde;a proporcionada no es v&aacute;lida
     *                                     para obtener la clave privada
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando el usuario cancela el proceso
     *                                                          antes de que finalice. */
    @Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias) throws KeyStoreException,
    		                                                               NoSuchAlgorithmException,
    		                                                               UnrecoverableEntryException {
        if (capiKsMy == null) {
            throw new IllegalStateException("Se han pedido claves a un almacen no inicializado"); //$NON-NLS-1$
        }
        return (KeyStore.PrivateKeyEntry) capiKsMy.getEntry(alias, new KeyStore.PasswordProtection("dummy".toCharArray())); //$NON-NLS-1$
    }

	@Override
	public void init(final AOKeyStore type,
			                   final InputStream store,
			                   final PasswordCallback pssCallBack,
			                   final Object[] params,
			                   final boolean forceReset) throws AOKeyStoreManagerException {
		resetCachedAliases();
		if (AOKeyStore.WINDOWS.equals(type)) {
			if (forceReset) {
				capiKsMy = null;
			}
			setKeyStore(initCapi());
        }
		else {
			throw new AOKeyStoreManagerException(
				"Tipo de almacen no soportado, este gestor es exclusivo CAPI: " + type.getName() //$NON-NLS-1$
			);
		}
	}

	/** Inicializa el almac&eacute;n CAPI a trav&eacute;s del proveedor <code>SunMSCAPI</code>.
	 * La inicializaci&acute;n se realiza proporcionando <code>null</code> como contrase&ntilde;a, lo que, por deficiencias del
	 * proveedor con ciertos CSP que necesitan que el PIN se establezca con la funci&oacute;n de CAPI
	 * <a href="msdn.microsoft.com/en-us/library/aa379858%28v=VS.85%29.aspx">CSPSetProvParam</a>, puede no funcionar adecuadamente.
	 * Un m&eacute;todo alternativo de inicializaci&oacute;n podr&iacute;a ser este:
	 * <pre>
	 *  if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
	 *      throw new InvalidOSException("Microsoft Windows"); //$NON-NLS-1$
	 *  }
     *
	 *  // Si no se ha agregado el proveedor CAPI de Sun, lo anadimos
	 *  // En java 6 viene instalado de serie, pero no pasa nada por reinstalarlo
	 *  Provider provider = Security.getProvider("SunMSCAPI"); //$NON-NLS-1$
	 *  if (provider == null) {
	 *      try {
	 *          provider = (Provider) Class.forName("sun.security.mscapi.SunMSCAPI").newInstance(); //$NON-NLS-1$
	 *              Security.addProvider(provider);
	 *      }
	 *      catch(final Exception e) {
	 *          LOGGER.severe("No se ha podido instanciar 'sun.security.mscapi.SunMSCAPI': " + e); //$NON-NLS-1$
	 *              throw new MissingSunMSCAPIException(e);
	 *      }
	 *  }
     *
	 *  // Inicializamos
	 * 	final KeyStore.Builder keystoreBuilder = KeyStore.Builder.newInstance(
     *      AOKeyStore.WINDOWS.getProviderName(),
     *      provider,
     *      new KeyStore.CallbackHandlerProtection(
     *          new CallbackHandler() {
	 *              &#64;Override
	 *              public void handle(final Callback[] callbacks) throws IOException, UnsupportedCallbackException {
	 *                  for (final Callback callback2 : callbacks) {
	 *                      if (callback2 instanceof TextOutputCallback) {
	 *                          final TextOutputCallback toc = (TextOutputCallback)callback2;
	 *                          switch (toc.getMessageType()) {
	 *                              case TextOutputCallback.INFORMATION:
	 *                                  AOUIFactory.showMessageDialog(
	 *                                      null, // Padre
	 *                                      toc.getMessage(),
	 *                                      "Informacion del almacen externo",
	 *                                      JOptionPane.INFORMATION_MESSAGE
	 *                                  );
	 *                                  break;
	 *                              case TextOutputCallback.ERROR:
	 *                                  AOUIFactory.showErrorMessage(
	 *                                      null,
	 *                                      toc.getMessage(),
	 *                                      "Error del almacen externo",
	 *                                      JOptionPane.ERROR_MESSAGE
	 *                                  );
	 *                                  break;
	 *                              case TextOutputCallback.WARNING:
	 *                                  AOUIFactory.showMessageDialog(
	 *                                      null, // Padre
	 *                                      toc.getMessage(),
	 *                                      "Advertencia del almacen externo",
	 *                                      JOptionPane.WARNING_MESSAGE
	 *                                  );
	 *                                  break;
	 *                              default:
	 *                                  throw new IOException(
	 *                                      "Tipo de mensaje no soportado indicado en la callback: " + toc.getMessageType() //$NON-NLS-1$
	 *                                  );
	 *                          }
	 *                      }
	 *                      else if (callback2 instanceof NameCallback) {
	 *                          // Preguntamos al usuario por un nombre de usuario
	 *                          ((NameCallback)callback2).setName(
	 *                              (String)AOUIFactory.showInputDialog(
	 *                                  null,
	 *                                  "Introduzca el nombre de usuario del almacen externo",
	 *                                  "Nombre de usuario",
	 *                                  JOptionPane.QUESTION_MESSAGE,
	 *                                  null,
	 *                                  null,
	 *                                  null
	 *                              )
	 *                          );
	 *                      }
	 *                      else if (callback2 instanceof PasswordCallback) {
	 *                          // Preguntamos al usuario la contrasena
	 *                          ((PasswordCallback)callback2).setPassword(
	 *                              new UIPasswordCallback("Contrasena del almacen externo").getPassword()
	 *                          );
	 *                      }
	 *                      else {
	 *                          throw new UnsupportedCallbackException(
	 *                              callback2, "Tipo de callback no soportada: " + callback2 != null ? callback2.getClass().getName() : "NULA" //$NON-NLS-1$ //$NON-NLS-2$
	 *                          );
	 *                      }
	 *                  }
	 *              }
	 *          }
	 *      )
	 *  );
	 *  try {
	 *      capiKsMy = keystoreBuilder.getKeyStore();
	 *  }
	 *  catch (final Exception e) {
	 *      throw new AOKeyStoreManagerException("No se ha podido obtener el almacen Windows.MY: " + e, e); //$NON-NLS-1$
	 *  }
	 * </pre>
	 * @return Almac&eacute; inicializado.
	 * @throws AOKeyStoreManagerException Si hay problemas durante la creaci&oacute;n o inicializaci&oacute;n del almac&eacute;n. */
    private static KeyStore initCapi() throws AOKeyStoreManagerException {

    	if (capiKsMy == null) {

	        if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
	            throw new InvalidOSException("Microsoft Windows"); //$NON-NLS-1$
	        }

	        // Si no se ha agregado el proveedor CAPI de Sun, lo anadimos
	        // En java 6 viene instalado de serie, pero no pasa nada por reinstalarlo
	        Provider capiProvider = Security.getProvider("SunMSCAPI"); //$NON-NLS-1$
	        if (capiProvider == null) {
	            try {
	            	capiProvider = (Provider) Class.forName("sun.security.mscapi.SunMSCAPI").getConstructor().newInstance(); //$NON-NLS-1$
	                Security.addProvider(capiProvider);
	            }
	            catch(final Exception e) {
	            	LOGGER.severe("No se ha podido instanciar 'sun.security.mscapi.SunMSCAPI': " + e); //$NON-NLS-1$
	            	throw new MissingSunMSCAPIException(e);
	            }
	        }

	        // Inicializamos
	        try {
	        	capiKsMy = KeyStoreUtilities.getKeyStoreWithPasswordCallbackHandler(
	        		AOKeyStore.WINDOWS,
					new FirstEmptyThenPinUiPasswordCallback(
						KeyStoreMessages.getString("CAPIKeyStoreManager.0") //$NON-NLS-1$
					),
					capiProvider,
					null
				);
			}
	        catch (final AOCancelledOperationException e) {
	        	throw e;
			}
	        catch (final Exception e) {
	        	throw new AOKeyStoreManagerException("No se ha podido obtener el almacen Windows.MY: " + e, e); //$NON-NLS-1$
			}

	        // Tratamos los alias repetidos, situacion problematica afectada por el bug
	        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6483657
	        // Este solo se da con SunMSCAPI
	        try {
	            cleanCAPIDuplicateAliases(capiKsMy);
	        }
	        catch (final Exception e) {
	            LOGGER.warning("No se han podido tratar los alias duplicados: " + e); //$NON-NLS-1$
	        }

    	}
        return capiKsMy;
    }

    /** Obtiene un certificado del almac&eacute;n activo a partir de su alias.
     * @param alias Alias del certificado.
     * @return El certificado o {@code null} si no se pudo recuperar. */
    @Override
	public X509Certificate getCertificate(final String alias) {
        if (alias == null) {
            LOGGER.warning("El alias del certificado es nulo, se devolvera null"); //$NON-NLS-1$
            return null;
        }

        if (capiKsMy == null) {
            LOGGER.warning(
        		"No se ha podido recuperar el certificado con el alias especificado porque el KeyStore no estaba inicializado, se devolvera null" //$NON-NLS-1$
    		);
            return null;
        }

        final X509Certificate cert;
        try {
            cert = (X509Certificate) capiKsMy.getCertificate(alias);
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido recuperar el certificado con el alias especificado', se devolvera null: " + e); //$NON-NLS-1$
            return null;
        }
        if (cert == null) {
            LOGGER.warning("No se ha podido recuperar el certificado con el alias especificado, se devolvera null"); //$NON-NLS-1$
            return null;
        }
        return cert;

    }

    /** Obtiene la cadena de certificaci&oacute;n de un certificado del keystore
     * activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return Certificados de la cadena de certificaci&oacute;n o {@code null} si no se pudo recuperar. */
    @Override
	public X509Certificate[] getCertificateChain(final String alias) {
        if (capiKsMy == null) {
            LOGGER.warning("El KeyStore actual no esta inicializado, por lo que no se pudo recuperar el certificado para el alias especificado"); //$NON-NLS-1$
            return null;
        }
        try {
            return (X509Certificate[]) capiKsMy.getCertificateChain(alias);
        }
        catch (final Exception e) {
            LOGGER.severe(
              "Error al obtener la cadena de certificados para el alias certificado actual, se devolvera una cadena vacia: " + e //$NON-NLS-1$
            );
            return new X509Certificate[0];
        }
    }

    /** Obtiene todos los alias de los certificados del almac&eacute;n actual.
     * @return Todos los alias encontrados en el almac&eacute;n actual */
    @Override
	public String[] getAliases() {

        if (capiKsMy == null) {
            throw new IllegalStateException("Se han pedido los alias de un almacen no inicializado"); //$NON-NLS-1$
        }

        if (getCachedAliases() != null) {
        	return getCachedAliases();
        }

        LOGGER.info("Solicitando los alias al KeyStore (" + capiKsMy.getProvider() + ")"); //$NON-NLS-1$ //$NON-NLS-2$

        try {
        	setCachedAliases(
    			cleanDeactivatedAliases(
        			Collections.list(CAPIKeyStoreManager.capiKsMy.aliases()).toArray(new String[0])
    			)
			);
        }
        catch (final Exception e) {
            LOGGER.severe("Error intentando obtener los alias del almacen de claves, se devolvera una enumeracion vacia: " + e); //$NON-NLS-1$
            return new String[0];
        }
        return getCachedAliases();
    }

    @Override
    public String toString() {
    	return "Gestor del almacen Windows.MY de CAPI via SunMSCAPI"; //$NON-NLS-1$
    }

    private static void cleanCAPIDuplicateAliases(final KeyStore keyStore) throws NoSuchFieldException,
                                                                                  IllegalAccessException {
    	// Java 9 no sufre el problema de Alias duplicados
    	if (AOUtil.isJava9orNewer()) {
    		return;
    	}

    	Field field = keyStore.getClass().getDeclaredField("keyStoreSpi"); //$NON-NLS-1$
    	field.setAccessible(true);
    	final KeyStoreSpi keyStoreVeritable = (KeyStoreSpi) field.get(keyStore);

    	if ("sun.security.mscapi.KeyStore$MY".equals(keyStoreVeritable.getClass().getName())) { //$NON-NLS-1$
    		String alias, hashCode;
    		X509Certificate[] certificates;

    		field = keyStoreVeritable.getClass().getEnclosingClass().getDeclaredField("entries"); //$NON-NLS-1$
    		field.setAccessible(true);

    		final Object entriesCollection = field.get(keyStoreVeritable);
    		if (entriesCollection instanceof Collection<?>) {
    			final Collection<?> entries = (Collection<?>) field.get(keyStoreVeritable);
        		for (final Object entry : entries) {
        			field = entry.getClass().getDeclaredField("certChain"); //$NON-NLS-1$
        			field.setAccessible(true);
        			certificates = (X509Certificate[]) field.get(entry);

        			hashCode = Integer.toString(certificates[0].hashCode());

        			field = entry.getClass().getDeclaredField("alias"); //$NON-NLS-1$
        			field.setAccessible(true);
        			alias = (String) field.get(entry);

        			if (!alias.equals(hashCode)) {
        				field.set(entry, alias.concat(" - ").concat(hashCode)); //$NON-NLS-1$
        			}
        		} // for
    		}
    	} // if
    }
}
