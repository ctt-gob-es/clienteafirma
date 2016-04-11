package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.Certificate;
import java.util.List;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.filters.CertificateFilter;

/**
 * Centraliza el manejo del di&aacute;logo de selecci&oacute;n de certificados.
 */
public class CertificateManagerDialog {

	private String selectedAlias = null;

	/**
	 * Muestra el dialogo de selecci&oacute;n.
	 * @param parentComponent Componente padre sobre el que mostrarse.
	 * @param ksm Almacen de certificados que debe mostrar.
	 * @return Entrada con la clave y el certificado seleccionado.
	 * @throws KeyStoreException Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
	 * @throws NoSuchAlgorithmException Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave
	 * @throws UnrecoverableEntryException Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada
	 * @throws AOCertificatesNotFoundException Si no se encuentran certificados.
	 * @throws AOCancelledOperationException Cuando no se selecciona ning&uacute;n certificado.
	 */
	public PrivateKeyEntry show(final Component parentComponent, final AOKeyStoreManager ksm) throws KeyStoreException, NoSuchAlgorithmException, UnrecoverableEntryException, AOCertificatesNotFoundException {
		return show(parentComponent, ksm, null, false);
	}

	/**
	 * Muestra el dialogo de selecci&oacute;n.
	 * @param parentComponent Componente padre sobre el que mostrarse.
	 * @param ksm Almacen de certificados que debe mostrar.
	 * @param filters Listado de filtros por los que deber&aacute;n pasar los certificados.
	 * @param mandatoryCertificate Indica si debe seleccionarse autom&aacute;ticamente el
	 * certificado cuando solo haya uno disponible que cumpla con los requisitos especificados.
	 * @return Entrada con la clave y el certificado seleccionado.
	 * @throws KeyStoreException Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
	 * @throws NoSuchAlgorithmException Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave
	 * @throws UnrecoverableEntryException Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada
	 * @throws AOCertificatesNotFoundException Si no se encuentran certificados.
	 * @throws AOCancelledOperationException Cuando no se selecciona ning&uacute;n certificado.
	 */
	public PrivateKeyEntry show(final Component parentComponent, final AOKeyStoreManager ksm, final List<CertificateFilter> filters, final boolean mandatoryCertificate) throws KeyStoreException, NoSuchAlgorithmException, UnrecoverableEntryException, AOCertificatesNotFoundException {

		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
			ksm,
			parentComponent,
			true,
			true, // showExpiredCertificates
			true, // checkValidity
			filters,
			mandatoryCertificate
		);
		dialog.show();

		this.selectedAlias = dialog.getSelectedAlias();

		return ksm.getKeyEntry(
			this.selectedAlias
		);
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
