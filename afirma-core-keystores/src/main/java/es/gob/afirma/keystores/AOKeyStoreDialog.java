package es.gob.afirma.keystores;

import java.util.List;

import es.gob.afirma.keystores.filters.CertificateFilter;

public class AOKeyStoreDialog {

	AOKeyStoreManager ksm;
	final Object parentComponent;
    final boolean checkPrivateKeys;
    final boolean checkValidity;
    final boolean showExpiredCertificates;
    final List<CertificateFilter> certFilters;
    final boolean mandatoryCertificate;
	
	public AOKeyStoreDialog(final AOKeyStoreManager ksm,
            final Object parentComponent,
            final boolean checkPrivateKeys,
            final boolean checkValidity,
            final boolean showExpiredCertificates,
            final List<CertificateFilter> certFilters,
            final boolean mandatoryCertificate) {

		if (ksm == null) {
    		throw new NullPointerException("No se ha indicado el almacen de claves"); //$NON-NLS-1$
    	}
		
		this.ksm = ksm;
		this.parentComponent = parentComponent;
		this.checkPrivateKeys = checkPrivateKeys;
		this.checkValidity = checkValidity;
		this.showExpiredCertificates = showExpiredCertificates;
		this.certFilters = certFilters;
		this.mandatoryCertificate = mandatoryCertificate;
	}
	
	public String showCertSelectionDialog() {
	
		return null;
	}
}
