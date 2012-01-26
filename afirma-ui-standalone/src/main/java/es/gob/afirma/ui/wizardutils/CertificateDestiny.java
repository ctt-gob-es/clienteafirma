/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardutils;

import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Vector;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.filters.CertificateFilter;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/** Certificado destinatario de un sobre digital. */
public class CertificateDestiny {

    static Logger logger = Logger.getLogger(CertificateDestiny.class.getName());

    /** Alias del certificado */
    private String alias = null;

    /** Certificado */
    private Certificate cert = null;

    public CertificateDestiny(final AOKeyStoreManager keyStoreManager, final JDialogWizard dialogo) {
        try {
            // Seleccionamos un certificado
            final String selectedcert =
                Utils.showCertSelectionDialog(keyStoreManager.getAliases(),
                                              keyStoreManager,
                                              dialogo,
                                              false,
                                              true,
                                              true,
                                              new Vector<CertificateFilter>(0),
                                              false);

            // Comprobamos si se ha cancelado la seleccion
            if (selectedcert == null) {
                throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$
            }

            Certificate cert1 = null;
            for (final KeyStore tmpKs : keyStoreManager.getKeyStores()) {
                try {
                    cert1 = tmpKs.getCertificate(selectedcert);
                    if (cert1 != null) {
                        break;
                    }
                    throw new AOException("El alias '" + selectedcert + "' no se corresponde con ning\u00FAn certificado accesible"); //$NON-NLS-1$ //$NON-NLS-2$
                }
                catch (final KeyStoreException e) {
                    throw new AOException("El almacen seleccionado no estaba inicializado: " + e); //$NON-NLS-1$
                }
            }

            if (!(cert1 instanceof X509Certificate)) {
                throw new AOException("El certificado recuperado no es un certificado v\u00E1lido para esta operaci\u00F3n"); //$NON-NLS-1$
            }

            this.alias = selectedcert;
            this.cert = cert1;
        }
        catch (final AOCancelledOperationException e) {
            logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
        }
        catch (final AOException e) {
            logger.severe(e.getMessage() + ": " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(dialogo, true, e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
        }
        catch (final Exception e) {
            logger.severe("No se ha podido recuperar el certificado seleccionado: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(dialogo, true, Messages.getString("Certificado.no.soportado"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
        }
    }

    public CertificateDestiny(final String alias, final Certificate cert) {
        this.alias = alias;
        this.cert = cert;
    }

    public String getAlias() {
        return this.alias;
    }

    public Certificate getCertificate() {
        return this.cert;
    }
}
