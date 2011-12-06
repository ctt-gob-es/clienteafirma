/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.filters.rfc;

import java.security.cert.X509Certificate;
import java.util.List;
import java.util.logging.Logger;

import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttributes;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import es.gob.afirma.keystores.main.filters.CertificateFilter;

/** Clase que representa un filtro de certificados para
 * el di&aacute;logo de selecci&oacute;n. */
public final class RFC2254CertificateFilter extends CertificateFilter {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    @Override
	public boolean matches(final X509Certificate cert) {
        return filterSubjectByRFC2254(this.rfc2254SubjectFilter, cert)
               && filterIssuerByRFC2254(this.rfc2254IssuerFilter, cert);
    }

    /** Filtro RFC2254 para el emisor del certificado. */
    private final String rfc2254IssuerFilter;

    /** Filtro RFC2254 para el emisor del certificado. */
    private final String rfc2254SubjectFilter;

    /** Construye un filtro para certificados.
     * @param subjectFilter Cadena seg&uacute;n la RFC2254 para filtro por el campo del titular (subject)
     * @param issuerFilter Cadena seg&uacute;n la RFC2254 para filtro por el campo del emisor (issuer) */
    public RFC2254CertificateFilter(final String subjectFilter, final String issuerFilter) {
        if (subjectFilter == null && issuerFilter == null) {
            throw new IllegalArgumentException("Al menos uno de los criterios de filtrado debe no ser nulo"); //$NON-NLS-1$
        }
        this.rfc2254IssuerFilter = issuerFilter;
        this.rfc2254SubjectFilter = subjectFilter;
    }

    private boolean filterSubjectByRFC2254(final String filter, final X509Certificate cert) {
        if (cert == null || filter == null) {
            return true;
        }
        return filterRFC2254(filter, cert.getSubjectDN().toString());
    }

    private boolean filterIssuerByRFC2254(final String filter, final X509Certificate cert) {
        if (cert == null || filter == null) {
            return true;
        }
        return filterRFC2254(filter, cert.getIssuerDN().toString());
    }

    /** Indica si un nombres LDAP se ajusta a los requisitos de un filtro.
     * @param f
     *        Filtro seg&uacute;n la RFC2254.
     * @param name
     *        Nombre LDAP al que se debe aplicar el filtro.
     * @return <code>true</code> si el nombre LDAP es nulo o se adec&uacute;a al
     *         filtro o este &uacute;ltimo es nulo, <code>false</code> en caso
     *         contrario */
    private boolean filterRFC2254(final String f, final String name) {
        try {
            return filterRFC2254(f, new LdapName(name));
        }
        catch (final Exception e) {
            LOGGER.warning("No ha sido posible filtrar el certificado (filtro: '" + f //$NON-NLS-1$
                                                      + "', nombre: '" //$NON-NLS-1$
                                                      + name
                                                      + "'), no se eliminara del listado: " //$NON-NLS-1$
                                                      + e);
            return true;
        }
    }

    /** Indica si un nombres LDAP se ajusta a los requisitos de un filtro.
     * @param f
     *        Filtro seg&uacute;n la RFC2254.
     * @param name
     *        Nombre LDAP al que se debe aplicar el filtro.
     * @return <code>true</code> si el nombre LDAP es nulo o se adec&uacute;a al
     *         filtro o este &uacute;ltimo es nulo, <code>false</code> en caso
     *         contrario */
    private boolean filterRFC2254(final String f, final LdapName name) {
        if (f == null || name == null) {
            return true;
        }
        try {
            final List<Rdn> rdns = name.getRdns();
            if (rdns == null || (rdns.isEmpty())) {
                LOGGER.warning("El nombre proporcionado para filtrar no contiene atributos, no se mostrara el certificado en el listado"); //$NON-NLS-1$
                return false;
            }
            final Attributes attrs = new BasicAttributes(true);
            for (final Rdn rdn : rdns) {
                attrs.put(rdn.getType(), rdn.getValue());
            }
            return new com.sun.jndi.toolkit.dir.SearchFilter(f).check(attrs);
        }
        catch (final Exception e) {
            LOGGER.warning("No ha sido posible filtrar el certificado (filtro: '" + f //$NON-NLS-1$
                                                      + "', nombre: '" //$NON-NLS-1$
                                                      + name
                                                      + "'), no se eliminara del listado: " //$NON-NLS-1$
                                                      + e);
            return true;
        }
    }

}
