/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.awt.Component;
import java.io.IOException;
import java.security.UnrecoverableKeyException;
import java.util.Map;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.SmartCardLockedException;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/** Gestor simple de <code>KeyStores</code>. Obtiene o un <code>KeyStore</code> de DNIe
 * v&iacute;a controlador 100% Java o el <code>KeyStore</code> por defecto del sistema operativo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SimpleKeyStoreManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private SimpleKeyStoreManager() { /* No permitimos la instanciacion */ }

    /** Obtiene un <code>KeyStore</code>.
     * @param dnie <code>true</code> si desea obtenerse un <code>KeyStore</code> para DNIe, <code>false</code> si desea obtenerse
     *        el <code>KeyStore</code> por defecto del sistema operativo.
     * @param forced Si {@code true}, es obligatorio el uso del DNIe en caso de solicitarlo y no se
     * deber&aacute; cargar un almacen por defecto incluso si el usuario no lo proporciona.
     * @param parent Componente padre para la modalidad.
     * @return <code>KeyStore</code> apropiado.
     * @throws AOKeyStoreManagerException Si ocurre cualquier problema durante la obtenci&oacute;n del <code>KeyStore</code>.
     * @throws NoDnieFoundException Si se obliga al uso de DNIe pero este no se proporciona. */
    static AOKeyStoreManager getKeyStore(final boolean dnie, final boolean forced, final Component parent)
    		throws AOKeyStoreManagerException, NoDnieFoundException {

    	// -- Se ha habilitado el uso de DNIe --

        if (dnie) {

            JMulticardUtilities.configureJMulticard(true);

            LOGGER.info("Cargando certificados del DNIe"); //$NON-NLS-1$

            try {
            	return getKeyStoreManager(AOKeyStore.DNIEJAVA, parent);
            }
            catch(final SmartCardLockedException e) {
            	AOUIFactory.showErrorMessage(
    				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.9"), //$NON-NLS-1$
    				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.10"), //$NON-NLS-1$
    				JOptionPane.WARNING_MESSAGE,
    				e
				);
            }
            catch (final Exception e) {
            	String exClassName = e.getClass().getName();
            	Throwable t = e.getCause();
            	if (t != null) {
            		t = t.getCause();
            		if (t != null) {
            			t = t.getCause();
            			if (t != null) {
            				exClassName = t.getClass().getName();
            			}
            		}
            	}
            	switch(exClassName) {
            		case "es.gob.jmulticard.apdu.connection.CardNotPresentException": //$NON-NLS-1$
                		if (JOptionPane.YES_OPTION == AOUIFactory.showConfirmDialog(
            				parent,
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.1"),  //$NON-NLS-1$
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.2"),  //$NON-NLS-1$
            				JOptionPane.YES_NO_OPTION,
            				JOptionPane.WARNING_MESSAGE
        				)) {
                			return getKeyStore(true, forced, parent);
                		}
                		break;
            		case "es.gob.jmulticard.card.InvalidCardException": //$NON-NLS-1$
            			if (JOptionPane.YES_OPTION == AOUIFactory.showConfirmDialog(
                				parent,
                				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.3"),  //$NON-NLS-1$
                				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.4"),  //$NON-NLS-1$
                				JOptionPane.YES_NO_OPTION,
                				JOptionPane.WARNING_MESSAGE
            				)) {
                    			return getKeyStore(true, forced, parent);
                    		}
                    		break;

            		case "es.gob.jmulticard.card.dnie.BurnedDnieCardException": //$NON-NLS-1$
	            		AOUIFactory.showErrorMessage(
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.5"), //$NON-NLS-1$
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.6"), //$NON-NLS-1$
            				JOptionPane.WARNING_MESSAGE,
            				e
        				);
	            		break;
            		default:
                		Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
            				"No se ha podido inicializar el controlador 100% Java del DNIe: " + e //$NON-NLS-1$
        				);
                		AOUIFactory.showErrorMessage(
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.7"), //$NON-NLS-1$
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.8"), //$NON-NLS-1$
            				JOptionPane.ERROR_MESSAGE,
            				e
        				);
            	}
            }

            // Si era obligatorio el uso de DNIe, devolvemos una excepcion indicando que no se ha proporcionado
            if (forced) {
            	throw new NoDnieFoundException();
            }
        }

        // El por defecto

        // -- Comportamiento por defecto --

        // Configuramos el uso de JMulticard segun lo establecido en el dialogo de preferencias
        final boolean enableJMulticard = PreferencesManager.getBoolean(
        		PreferencesManager.PREFERENCE_GENERAL_ENABLED_JMULTICARD);

        JMulticardUtilities.configureJMulticard(enableJMulticard);

        // Identificamos el almacen por defecto
        final AOKeyStore aoks = getDefaultKeyStoreType();

        LOGGER.info("Cargando almacen por defecto: " + aoks.getName()); //$NON-NLS-1$

        // Cargamos el almacen
        try {
			return getKeyStoreManager(
				aoks,
				parent
			);
		} catch (final IOException ioe) {
			if (ioe.getCause() != null && ioe.getCause().getCause() != null
				&& ioe.getCause().getCause() instanceof UnrecoverableKeyException) {
				AOUIFactory.showMessageDialog(
						parent,
		        		SimpleAfirmaMessages.getString("SimpleKeyStoreManager.12"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
				final boolean stopOperation = false;
				while (!stopOperation) {
					try {
						return getKeyStoreManager(
							aoks,
							parent
						);
					} catch (final AOCancelledOperationException aoce) {
						AOUIFactory.showMessageDialog(
								parent,
				        		SimpleAfirmaMessages.getString("SimpleKeyStoreManager.13"), //$NON-NLS-1$
								SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
								JOptionPane.WARNING_MESSAGE
						);
						return loadSystemAOKSManager(parent);
					} catch (final IOException ioe2) {
						if (ioe.getCause() != null && ioe.getCause().getCause() != null
								&& ioe.getCause().getCause() instanceof UnrecoverableKeyException) {
							AOUIFactory.showMessageDialog(
									parent,
					        		SimpleAfirmaMessages.getString("SimpleKeyStoreManager.12"), //$NON-NLS-1$
									SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
									JOptionPane.ERROR_MESSAGE
							);
							continue;
						}
					} catch (final Exception e) {
			        	AOUIFactory.showErrorMessage(
			            		SimpleAfirmaMessages.getString("SimpleKeyStoreManager.11", aoks.getName()), //$NON-NLS-1$
			    				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
			    				JOptionPane.ERROR_MESSAGE,
			    				e
			    			);
			        	return loadSystemAOKSManager(parent);
					}
				}
			}
			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("SimpleKeyStoreManager.11", aoks.getName()), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					ioe
			);

			return loadSystemAOKSManager(parent);

		} catch (final AOCancelledOperationException aoce) {
			AOUIFactory.showMessageDialog(
					parent,
	        		SimpleAfirmaMessages.getString("SimpleKeyStoreManager.13"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE
			);
			return loadSystemAOKSManager(parent);
		} catch (final Exception e) {
        	AOUIFactory.showErrorMessage(
        		SimpleAfirmaMessages.getString("SimpleKeyStoreManager.11", aoks.getName()), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				e
			);

        	return loadSystemAOKSManager(parent);

		}
    }

    private static AOKeyStoreManager getKeyStoreManager(final AOKeyStore aoks, final Component parent) throws IOException, AOKeystoreAlternativeException {
    	String lib = null;
    	if (AOKeyStore.PKCS12.equals(aoks) || AOKeyStore.PKCS11.equals(aoks)) {
    		lib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
    	}
    	return AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		aoks,
    		lib,
    		null,
    		aoks.getStorePasswordCallback(parent),
    		parent
		);
    }

    /** Indica si est&aacute; disponible el almac&eacute;n de claves de Mozilla Firefox.
     * @return <code>true</code> si est&aacute; disponible el almac&eacute;n de claves de Mozilla Firefox,
     *         <code>false</code> en caso contrario. */
    public static boolean isFirefoxAvailable() {
		final String mozProfileDir;
		final String nssLibDir;
		try {
			mozProfileDir = MozillaKeyStoreUtilities.getMozillaUserProfileDirectory();
			nssLibDir = MozillaKeyStoreUtilities.getSystemNSSLibDir();
		}
		catch(final Exception e) {
			LOGGER.warning("No se ha podido obtener el directorio de NSS del usuario: " + e); //$NON-NLS-1$
			return false;
		}

		return mozProfileDir != null && nssLibDir != null;
    }

    /**
     * Recupera el repositorio con el nombre indicado. Si no existe un <code>KeyStore</code> con
     * ese nombre, se devuelve <code>null</code>.
     * @param name Nombre del repositorio que se desea recuperar.
     * @return KeyStore Repositorio de certificados.
     */
    public static AOKeyStore getKeyStore(final String name) {
    	if (name == null) {
    		return null;
    	}
        for (final AOKeyStore tempKs : AOKeyStore.values()) {
            if (tempKs.getName().equalsIgnoreCase(name.trim())) {
                return tempKs;
            }
        }
        // Buscamos entre los registros de los almacenes de clave PKCS#11
        final Map<String, String> userRegResult = KeyStorePreferencesManager.getUserSmartCardsRegistered();
        final Map<String, String> systemRegResult = KeyStorePreferencesManager.getSystemSmartCardsRegistered();
        final boolean existSmartCard = userRegResult.containsKey(name) || systemRegResult.containsKey(name);
        if (existSmartCard) {
			final AOKeyStore result = AOKeyStore.PKCS11;
			result.setName(name);
			return result;
		}

        try {
        	return AOKeyStore.valueOf(name);
        }
        catch(final Exception e) {
        	Logger.getLogger("es.gob.afirma").warning("Almacen de claves no reconocido (" + name + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        return null;
    }


    /** Obtiene el almac&eacute;n de claves por defecto de la aplicaci&oacute;n.
     * @return Almac&eacute;n de claves por defecto de la aplicaci&oacute;n. */
    public static AOKeyStore getDefaultKeyStoreType() {
    	AOKeyStore ks = null;
    	final String savedStoreName = PreferencesManager.get(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);
    	if (savedStoreName != null && !PreferencesManager.VALUE_KEYSTORE_DEFAULT.equals(savedStoreName)) {
    		ks = getValidKeyStoreType(savedStoreName);
    	}
    	return ks != null ? ks : AOKeyStore.getDefaultKeyStoreTypeByOs(Platform.getOS());
    }

    /** Obtiene el almac&eacute;n de claves por defecto de la aplicaci&oacute;n.
     * @return Almac&eacute;n de claves por defecto de la aplicaci&oacute;n. */
    private static AOKeyStore getValidKeyStoreType(final String keyStoreName) {
    	AOKeyStore ks = null;
    	if (keyStoreName != null && !keyStoreName.isEmpty()) {
    		ks = getKeyStore(keyStoreName);
    		if (ks != null) {
    			final OS os = Platform.getOS();
    			// Si desinstalan Firefox que no se quede una seleccion mala
    			if (AOKeyStore.MOZ_UNI.equals(ks)) {
    				if (isFirefoxAvailable()) {
    					return ks;
    				}
					return AOKeyStore.getDefaultKeyStoreTypeByOs(os);
    			}
    			// No deberia pasar
    			if (AOKeyStore.WINDOWS.equals(ks)) {
    				if (OS.WINDOWS.equals(os)) {
    					return ks;
    				}
    				return AOKeyStore.getDefaultKeyStoreTypeByOs(os);
    			}
    			// No deberia pasar
    			if (AOKeyStore.APPLE.equals(ks)) {
    				if (OS.MACOSX.equals(os)) {
    					return ks;
    				}
    				return AOKeyStore.getDefaultKeyStoreTypeByOs(os);
    			}
    			return ks;
    		}
    	}
    	return ks;
    }

    /**
     * Obtiene el &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario. En caso de que no est&eacute;
     * definido o no sea un almac&eacute;n v&aacute;lido, se haya seleccionado.
     * @return Almac&eacute;n de claves seleccionado por el usuario.
     */
    public static AOKeyStore getLastSelectedKeystore() {
    	final String savedStoreName = KeyStorePreferencesManager.getLastSelectedKeystore();
    	return getValidKeyStoreType(savedStoreName);
    }

    /**
     * Devuelve el gestor del almac&eacute;n de claves del sistema.
     * @param parent Componente padre.
     * @return Gestor del almac&eacute;n de claves del sistema.
     * @throws AOKeyStoreManagerException Error al obtener el gestor.
     */
    private static AOKeyStoreManager loadSystemAOKSManager(final Component parent) throws AOKeyStoreManagerException {
    	final OS os = Platform.getOS();
    	final AOKeyStore osks = AOKeyStore.getDefaultKeyStoreTypeByOs(os);
    	try {
			return getKeyStoreManager(
					osks,
					parent
				);
		} catch (final Exception e1) {
			throw new AOKeyStoreManagerException(
	        		"No se ha podido incializar el almacen del sistema", e1 //$NON-NLS-1$
	    		);
		}
    }

}
