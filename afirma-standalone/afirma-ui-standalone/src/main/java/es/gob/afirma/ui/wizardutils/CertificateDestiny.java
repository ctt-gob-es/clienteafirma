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

import java.security.cert.Certificate;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.ui.utils.CertificateManagerDialog;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.Messages;

/** Certificado destinatario de un sobre digital. */
public class CertificateDestiny {

    private static Logger logger = Logger.getLogger(CertificateDestiny.class.getName());

    /** Alias del certificado */
    private String alias = null;

    /** Certificado */
    private Certificate cert = null;

    /** Constructor de la clase
     * @param keyStoreManager Almacen de certificados
     * @param dialogo Componente Java donde se mostrar&aacute;n los resultados */
    public CertificateDestiny(final AOKeyStoreManager keyStoreManager, final JDialogWizard dialogo) {
        try {
        	// Seleccionamos un certificado
        	CertificateManagerDialog certDialog = new CertificateManagerDialog();
            final Certificate[] selectedCertChain =
        			certDialog.showCerts(dialogo, keyStoreManager, false, true);
        	this.cert = selectedCertChain[0];
            this.alias = certDialog.getSelectedAlias();

        }
        catch (final AOCancelledOperationException e) {
            logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
        }
        catch (final java.security.ProviderException e) {
        	// Comprobacion especifica para el proveedor Java de DNIe
        	if (e.getCause() != null && e.getCause().getClass().getName().equals("es.gob.jmulticard.card.AuthenticationModeLockedException")) { //$NON-NLS-1$
        		CustomDialog.showMessageDialog(dialogo,
                        true,
                        Messages.getString("Firma.msg.error.dnie.AuthenticationModeLockedException"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        		return;
        	}
            CustomDialog.showMessageDialog(dialogo,
                                           true,
                                           Messages.getString("Firma.msg.error.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return;
        }
        catch (final AOCertificatesNotFoundException e) {
            logger.severe("No se han encontrado certificados validos en el almacen: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(dialogo, true, Messages.getString("No.certificates"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
        }
        catch (final Exception e) {
            logger.severe("No se ha podido recuperar el certificado seleccionado: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(dialogo, true, Messages.getString("Certificado.no.soportado"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
        }
    }

    /** Constructor de clase
     * @param alias Alias del certificado
     * @param cert Certificado */
    public CertificateDestiny(final String alias, final Certificate cert) {
        this.alias = alias;
        this.cert = cert;
    }

    /** Proporciona el alias del certificado
     * @return Una cadena de texto con el nombre del certificado */
    public String getAlias() {
        return this.alias;
    }

    /** Proporciona el certificado almacenado
     * @return Un objeto de tipo Certificate */
    public Certificate getCertificate() {
        return this.cert;
    }
}