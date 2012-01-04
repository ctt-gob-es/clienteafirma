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
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Vector;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.KeyStoreConfiguration;
import es.gob.afirma.keystores.main.filters.CertificateFilter;

/** Utilidades para las multifirmas */
public class MultisignUtils {

    static Logger logger = Logger.getLogger(MultisignUtils.class.toString());

    /** Obtiene el manager del almacen o repositorio de certificados
     * @param kssc Configuracion del almacen o repositorio de certificados
     * @param padre Componente padre
     * @return <code>AOKeyStoreManager</code> solicitado
     * @throws AOException Si ocurre cualquier error al intentar obtener el gestor del almac&eacute;n o repositorio de certificados */
    public static AOKeyStoreManager getAOKeyStoreManager(final KeyStoreConfiguration kssc, final Component padre) throws AOException {
        PasswordCallback pssCallback;

        final AOKeyStore store = kssc.getType();
        String lib = kssc.getLib();
        if (store == AOKeyStore.WINDOWS || store == AOKeyStore.WINROOT || store == AOKeyStore.SINGLE) {
            pssCallback = new NullPasswordCallback();
        }
        else if (store == AOKeyStore.PKCS12) {
            /*pssCallback = new UIPasswordCallback(
            		Messages.getString("Msg.pedir.contraenia", store.getDescription()),  //$NON-NLS-1$
            		null);*/
            pssCallback =
                new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription(), //$NON-NLS-1$
                                                    null,
                                                    Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                                                    KeyEvent.VK_O,
                                                    Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$

            final File selectedFile =
                SelectionDialog.showFileOpenDialog(null, Messages.getString("Open.repository"), (ExtFilter) Utils.getRepositoryFileFilter()); //$NON-NLS-1$
            if (selectedFile != null) {
                lib = selectedFile.getAbsolutePath();
            }
            else {
                throw new AOCancelledOperationException();
            }
        }
        else {
            /*pssCallback = new UIPasswordCallback(
            		Messages.getString("Msg.pedir.contraenia", store.getDescription()),  //$NON-NLS-1$
            		null);*/
            pssCallback =
                new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription(), //$NON-NLS-1$
                                                    null,
                                                    Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                                                    KeyEvent.VK_O,
                                                    Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$
        }

        try {
            return AOKeyStoreManagerFactory.getAOKeyStoreManager(store, lib, kssc.toString(), pssCallback, padre);
        }
        catch (final AOCancelledOperationException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("Error inicializando el almacen", e); //$NON-NLS-1$
        }

    }

    /** Solicita que se seleccione un certificado del almac&eacute;n indicado y devuelve su clave privada.
     * @param kssc Configuraci&oacute;n del almac&eacute;n o repositorio de certificados
     * @param keyStoreManager Manager del almac&eacute;n o repositorio de certificados
     * @param padre Componente padre
     * @return
     * @throws AOException se ha producido un error al intentar obtener la clave privada */
    public static PrivateKeyEntry getPrivateKeyEntry(final KeyStoreConfiguration kssc, final AOKeyStoreManager keyStoreManager, final Component padre) throws AOException {
        // Recuperamos la clave del certificado
        PrivateKeyEntry privateKeyEntry = null;

        // Seleccionamos un certificado
        final String selectedcert =
            Utils.showCertSelectionDialog(keyStoreManager.getAliases(),
                                          keyStoreManager,
                                          padre,
                                          true,
                                          true,
                                          true,
                                          new Vector<CertificateFilter>(0),
                                          false);

        // Comprobamos si se ha cancelado la seleccion
        if (selectedcert == null)
        {
            throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$
        }

        final AOKeyStore store = kssc.getType();
        try {
            privateKeyEntry = keyStoreManager.getKeyEntry(selectedcert, Utils.getCertificatePC(store, padre));
        }
        catch (final KeyException e) {
            throw new AOException("Error obteniendo la clave privada del certificado", e); //$NON-NLS-1$
        }
        catch (final AOCancelledOperationException e) {
            // Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
            // Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
            // que las relanza en forma de AOException
            throw e;
        }
        catch (final Exception e) {
            logger.severe("No se ha podido obtener el certicado con el alias '" + selectedcert + "': " + e); //$NON-NLS-1$//$NON-NLS-2$
            throw new AOException("No se ha podido recuperar el certificado seleccionado", e); //$NON-NLS-1$
        }

        return privateKeyEntry;
    }
}
