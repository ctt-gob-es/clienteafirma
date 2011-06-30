package es.gob.afirma.cliente;

import java.security.cert.X509Certificate;

/** Filtro para certificados. Debe autocontener toda la l&oacute;gica que indique si un
 * certificado cumple o no las condiciones del filtro.
 * El establecimiento de los datos encesarios para las condiciones de filtrado queda fuera
 * del interfaz y debe ser espec&iacute;fico para cada implementaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface CertificateFilter {
    boolean matches(final X509Certificate cert);
}
