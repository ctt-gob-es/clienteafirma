package es.gob.afirma.keystores;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.NameCertificateBean;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.KeyStoreDialogManager;
import es.gob.afirma.keystores.filters.CertificateFilter;

/** Di&aacute;logo para la selecci&oacute;n de certificados.
 * @author Carlos Gamuci. */
public final class AOKeyStoreDialog implements KeyStoreDialogManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private KeyStoreManager ksm;
	private final Object parentComponent;
	private final boolean checkPrivateKeys;
	private final boolean checkValidity;
	private final boolean showExpiredCertificates;
	private final List<CertificateFilter> certFilters;
	private final boolean mandatoryCertificate;

	private String selectedAlias = null;

    /** Crea un di&aacute;logo para la selecci&oacute;n de un certificado.
     * @param ksm
     *        Gestor de los almac&eacute;nes de certificados a los que pertenecen los alias.
     *        Debe ser {@code null} si se quiere usar el m&eacute;todo para seleccionar
     *        otra cosa que no sean certificados X.509 (como claves de cifrado)
     * @param parentComponent
     *        Componente gr&aacute;fico sobre el que mostrar los di&aacute;logos.
     * @param checkPrivateKeys
     *        Indica si se debe comprobar que el certificado tiene clave
     *        privada o no, para no mostrar aquellos que carezcan de ella
     * @param checkValidity
     *        Indica si se debe comprobar la validez temporal de un
     *        certificado al ser seleccionado
     * @param showExpiredCertificates
     *        Indica si se deben o no mostrar los certificados caducados o
     *        a&uacute;n no v&aacute;lidos
     */
    public AOKeyStoreDialog(final AOKeyStoreManager ksm,
    		final Object parentComponent,
    		final boolean checkPrivateKeys,
    		final boolean showExpiredCertificates,
    		final boolean checkValidity) {

		if (ksm == null) {
    		throw new IllegalArgumentException("El almacen de claves no puede ser nulo"); //$NON-NLS-1$
    	}

		this.ksm = ksm;
		this.parentComponent = parentComponent;
		this.checkPrivateKeys = checkPrivateKeys;
		this.checkValidity = checkValidity;
		this.showExpiredCertificates = showExpiredCertificates;
		this.certFilters = null;
		this.mandatoryCertificate = false;
    }

    /**
     * Crea un di&aacute;logo para la selecci&oacute;n de un certificado.
     * @param ksm
     *        Gestor de los almac&eacute;nes de certificados entre los que se selecciona.
     * @param parentComponent
     *        Componente gr&aacute;fico sobre el que mostrar los di&aacute;logos.
     * @param checkPrivateKeys
     *        Indica si se debe comprobar que el certificado tiene clave
     *        privada o no, para no mostrar aquellos que carezcan de ella
     * @param checkValidity
     *        Indica si se debe comprobar la validez temporal de un
     *        certificado al ser seleccionado
     * @param showExpiredCertificates
     *        Indica si se deben o no mostrar los certificados caducados o
     *        aun no v&aacute;lidos
     * @param certFilters
     *        Filtros sobre los certificados a mostrar
     * @param mandatoryCertificate
     *        Indica si los certificados disponibles (tras aplicar el
     *        filtro) debe ser solo uno.
     */
	public AOKeyStoreDialog(final AOKeyStoreManager ksm,
			final Object parentComponent,
            final boolean checkPrivateKeys,
            final boolean checkValidity,
            final boolean showExpiredCertificates,
            final List<CertificateFilter> certFilters,
            final boolean mandatoryCertificate) {

		if (ksm == null) {
    		throw new IllegalArgumentException("El almacen de claves no puede ser nulo"); //$NON-NLS-1$
    	}

		this.ksm = ksm;
		this.parentComponent = parentComponent;
		this.checkPrivateKeys = checkPrivateKeys;
		this.checkValidity = checkValidity;
		this.showExpiredCertificates = showExpiredCertificates;
		this.certFilters = certFilters;
		this.mandatoryCertificate = mandatoryCertificate;
	}

	@Override
	public NameCertificateBean[] getNameCertificates() {

    	final Map<String, String> aliassesByFriendlyName =
        		KeyStoreUtilities.getAliasesByFriendlyName(
    				this.ksm.getAliases(),
    				this.ksm,
    				this.checkPrivateKeys,
    				this.showExpiredCertificates,
    				this.certFilters
    			);

    	int i = 0;
    	final NameCertificateBean[] namedCerts =
    			new NameCertificateBean[aliassesByFriendlyName.size()];
    	for (final String certAlias : aliassesByFriendlyName.keySet().toArray(new String[aliassesByFriendlyName.size()])) {
    		namedCerts[i++] = new NameCertificateBean(
    				certAlias,
    				aliassesByFriendlyName.get(certAlias),
    				this.ksm.getCertificateChain(certAlias));
    	}

		return namedCerts;
	}

	@Override
	public void setKeyStoreManager(final KeyStoreManager ksm) {
		if (this.ksm instanceof AggregatedKeyStoreManager && ksm instanceof AOKeyStoreManager) {
			((AggregatedKeyStoreManager) this.ksm).removeAll();
			((AggregatedKeyStoreManager) this.ksm).addKeyStoreManager((AOKeyStoreManager) ksm);
		}
		
		this.ksm = ksm;
	}

	@Override
	public Object getKeyEntry(final String alias) throws AOException {

		PrivateKeyEntry pke = null;
		if (this.checkPrivateKeys) {
			try {
				pke = this.ksm.getKeyEntry(
						alias,
						this.ksm instanceof AOKeyStoreManager ?
								((AOKeyStoreManager) this.ksm).getType().getCertificatePasswordCallback(this.parentComponent) :
									null);
			}
			catch (final Exception e) {
				LOGGER.severe("No se ha podido extraer la clave del almacen: " + e); //$NON-NLS-1$
				throw new AOException("No se ha podido extraer la clave del almacen", e); //$NON-NLS-1$
			}
		}

		this.selectedAlias = alias;

		if (this.checkValidity && this.ksm != null) {

    		String errorMessage = null;

			try {
				this.ksm.getCertificate(this.selectedAlias).checkValidity();
			}
			catch (final CertificateExpiredException e) {
				errorMessage = KeyStoreMessages.getString("AOKeyStoreDialog.2"); //$NON-NLS-1$
			}
			catch (final CertificateNotYetValidException e) {
				errorMessage = KeyStoreMessages.getString("AOKeyStoreDialog.3"); //$NON-NLS-1$
			}
			catch (final Exception e) {
				errorMessage = KeyStoreMessages.getString("AOKeyStoreDialog.4"); //$NON-NLS-1$
			}

    		boolean rejected = false;

			if (errorMessage != null) {
				LOGGER.warning("Error durante la validacion: " + errorMessage); //$NON-NLS-1$
				if (AOUIFactory.showConfirmDialog(
						this.parentComponent,
						errorMessage,
						KeyStoreMessages.getString("AOKeyStoreDialog.5"), //$NON-NLS-1$
						AOUIFactory.YES_NO_OPTION,
						AOUIFactory.WARNING_MESSAGE
				) != AOUIFactory.YES_OPTION) {
					rejected = true;
				}
			}

			if (rejected) {
				throw new AOCancelledOperationException("Se ha reusado un certificado probablemente no valido"); //$NON-NLS-1$
			}
    	}

		return this.checkPrivateKeys ? pke : this.ksm.getCertificateChain(alias);
	}

	@Override
	public String show() throws AOCertificatesNotFoundException {

		final NameCertificateBean[] namedCertificates = this.getNameCertificates();

		// No mostramos el dialogo de seleccion si se ha indicado que se autoseleccione
		// un certificado en caso de ser el unico
		if (this.mandatoryCertificate && namedCertificates != null && namedCertificates.length == 1) {
			this.selectedAlias = namedCertificates[0].getAlias();
			return this.selectedAlias;
		}

		//TODO: Esto es temporal. La siguiente version del dialogo permitira que no haya
		// certificados y mostrara el boton de actualizar por si el usuario quiere cargar
		// certificados en tarjeta
		if (namedCertificates == null || namedCertificates.length < 1) {
			throw new AOCertificatesNotFoundException("No se han encontrado certificados en el almacen acordes a los filtros establecidos"); //$NON-NLS-1$
		}

		this.selectedAlias = AOUIFactory.showCertificateSelectionDialog(this.parentComponent, this);
		if (this.selectedAlias == null) {
			throw new AOCancelledOperationException("No se ha seleccionado certificado"); //$NON-NLS-1$
		}

		return this.selectedAlias;
	}

	@Override
	public String getSelectedAlias() {
		return this.selectedAlias;
	}

	@Override
	public void refresh() throws IOException {
		this.ksm.refresh();
	}
}
