package es.gob.afirma.standalone.ui.envelopes;

import java.awt.Component;
import java.security.cert.Certificate;
import java.util.logging.Logger;

import javax.swing.JDialog;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;

/** Certificado destinatario de un sobre digital. */
public class CertificateDestiny extends JDialog {

	private static final long serialVersionUID = 1L;
	private static Logger logger = Logger.getLogger(CertificateDestiny.class.getName());
    private String selectedAlias = null;
    private String alias = null;

    /** Certificado */
    private Certificate cert = null;

    /** Constructor de la clase
     * @param keyStoreManager Almacen de certificados
     * @param dialogo Componente Java donde se mostrar&aacute;n los resultados */
    public CertificateDestiny(final AOKeyStoreManager keyStoreManager, final JDialog dialogo) {
        try {
        	// Seleccionamos un certificado
            final Certificate[] selectedCertChain = showCerts(dialogo, keyStoreManager, false, true);
        	this.cert = selectedCertChain[0];
            this.alias = getSelectedAlias();
        }
        catch (final AOCancelledOperationException e) {
            logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
        }
        catch (final java.security.ProviderException e) {
        	// Comprobacion especifica para el proveedor Java de DNIe
        	if (e.getCause() != null && e.getCause().getClass().getName().equals("es.gob.jmulticard.card.AuthenticationModeLockedException")) { //$NON-NLS-1$
        		return;
        	}
            return;
        }
        catch (final AOCertificatesNotFoundException e) {
            logger.severe("No se han encontrado certificados validos en el almacen: " + e); //$NON-NLS-1$
        }
        catch (final Exception e) {
            logger.severe("No se ha podido recuperar el certificado seleccionado: " + e); //$NON-NLS-1$
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

	/**
	 * Muestra el dialogo de selecci&oacute;n.
	 * @param parentComponent Componente padre sobre el que mostrarse.
	 * @param ksm Almacen de certificados que debe mostrar.
	 * @param checkPrivateKeys
     *        Indica si se debe comprobar que el certificado tiene clave
     *        privada o no, para no mostrar aquellos que carezcan de ella
     * @param showExpiredCertificates
     *        Indica si se deben o no mostrar los certificados caducados o
     *        a&uacute;n no v&aacute;lidos
	 * @return Entrada con la clave y el certificado seleccionado.
	 * @throws AOCertificatesNotFoundException Si no se encuentran certificados.
	 * @throws AOCancelledOperationException Cuando no se selecciona ning&uacute;n certificado.
	 */
	public Certificate[] showCerts(final Component parentComponent, final AOKeyStoreManager ksm,
			final boolean checkPrivateKeys, final boolean showExpiredCertificates) throws AOCertificatesNotFoundException {

		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
				ksm,
				parentComponent,
				checkPrivateKeys,
				true,
				showExpiredCertificates);
		dialog.show();

		this.selectedAlias = dialog.getSelectedAlias();

		return ksm.getCertificateChain(this.selectedAlias);
	}

	/**
	 * Recupera el alias del certificado seleccionado.
	 * @return Alias del certificado seleccionado.
	 */
	public String getSelectedAlias() {
		return this.selectedAlias;
	}

}
