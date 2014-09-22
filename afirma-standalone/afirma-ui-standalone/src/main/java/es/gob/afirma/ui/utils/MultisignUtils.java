/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.ui.principal.Main;

/** Utilidades para las multifirmas */
public final class MultisignUtils {

    private MultisignUtils() {
        // No permitimos la instanciacion
    }

    /** Obtiene el manager del almacen o repositorio de certificados
     * @param kssc Configuracion del almacen o repositorio de certificados
     * @param padre Componente padre
     * @return <code>AOKeyStoreManager</code> solicitado
     * @throws AOException Si ocurre cualquier error al intentar obtener el gestor del almac&eacute;n o repositorio de certificados
     * @throws IOException Cuando ocurre alg&uacute;n error en el acceso al almac&eacute;n. */
    public static AOKeyStoreManager getAOKeyStoreManager(final KeyStoreConfiguration kssc, final Component padre) throws AOException, IOException {
        PasswordCallback pssCallback;

        final AOKeyStore store = kssc.getType();
        String lib = kssc.getLib();
        if (store == AOKeyStore.WINDOWS || store == AOKeyStore.SINGLE) {
            pssCallback = NullPasswordCallback.getInstance();
        }
        else if (store == AOKeyStore.PKCS12) {
            pssCallback = new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getName(), //$NON-NLS-1$ //$NON-NLS-2$
                                                              null,
                                                              Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                                                              KeyEvent.VK_O,
                                                              Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$

            final File selectedFile =
                    SelectionDialog.showFileOpenDialog(null, Messages.getString("Open.repository.pkcs12"), Main.getPreferences().get("dialog.load.repository.pkcs12", null), (ExtFilter) Utils.getRepositoryFileFilterPkcs12()); //$NON-NLS-1$ //$NON-NLS-2$
            if (selectedFile != null) {
                lib = selectedFile.getAbsolutePath();
                Main.getPreferences().put("dialog.load.repository.pkcs12", lib); //$NON-NLS-1$
            }
            else {
                throw new AOCancelledOperationException();
            }
        }
        else if (store == AOKeyStore.PKCS11) {
            pssCallback = new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getName(), //$NON-NLS-1$ //$NON-NLS-2$
                                                              null,
                                                              Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                                                              KeyEvent.VK_O,
                                                              Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$

            final File selectedFile =
                    SelectionDialog.showFileOpenDialog(null, Messages.getString("Open.repository.pkcs11"), Main.getPreferences().get("dialog.load.repository.pkcs11", null), (ExtFilter) Utils.getRepositoryFileFilterPkcs11()); //$NON-NLS-1$ //$NON-NLS-2$
            if (selectedFile != null) {
                lib = selectedFile.getAbsolutePath();
                Main.getPreferences().put("dialog.load.repository.pkcs11", lib); //$NON-NLS-1$
            }
            else {
                throw new AOCancelledOperationException();
            }
        }
        else if (store == AOKeyStore.DNIEJAVA) {
        	pssCallback = null;
        }
        else {
            pssCallback = new UIPasswordCallbackAccessibility(
        		Messages.getString("Msg.pedir.contraenia") + " " + store.getName(), //$NON-NLS-1$ //$NON-NLS-2$
                null,
                Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                KeyEvent.VK_O,
                Messages.getString("CustomDialog.showInputPasswordDialog.title") //$NON-NLS-1$
    		);
        }

        try {
            return AOKeyStoreManagerFactory.getAOKeyStoreManager(store, lib, kssc.toString(), pssCallback, padre);
        }
        catch (final AOKeystoreAlternativeException e) {
        	throw new AOException("Error inicializando el almacen", e); //$NON-NLS-1$
		}

    }

    /** Solicita que se seleccione un certificado del almac&eacute;n indicado y devuelve su clave privada.
     * @param kssc Configuraci&oacute;n del almac&eacute;n o repositorio de certificados
     * @param keyStoreManager Manager del almac&eacute;n o repositorio de certificados
     * @param padre Componente padre
     * @return Entrada que apunta a la clave privada
     * @throws AOException se ha producido un error al intentar obtener la clave privada
     * @throws KeyStoreException Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
	 * @throws NoSuchAlgorithmException Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave
	 * @throws UnrecoverableEntryException Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada */
    public static PrivateKeyEntry getPrivateKeyEntry(final KeyStoreConfiguration kssc, final AOKeyStoreManager keyStoreManager, final Component padre) throws AOException, KeyStoreException, NoSuchAlgorithmException, UnrecoverableEntryException {
        return new CertificateManagerDialog().show(padre, keyStoreManager);
    }
}