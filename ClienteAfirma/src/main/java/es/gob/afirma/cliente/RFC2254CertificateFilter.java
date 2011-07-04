/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente;

import java.security.cert.X509Certificate;
import java.util.List;
import java.util.logging.Logger;

import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttributes;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import com.sun.jndi.toolkit.dir.SearchFilter;

/** Clase que representa un filtro de certificados para
 * el di&aacute;logo de selecci&oacute;n. */
public final class RFC2254CertificateFilter implements CertificateFilter {

    public boolean matches(final X509Certificate cert) {
        return matchesKeyUsageFilter(keyUsageFilter, cert) && filterSubjectByRFC2254(rfc2254SubjectFilter, cert)
               && filterIssuerByRFC2254(rfc2254IssuerFilter, cert);
    }

    /** El KeyUsage m&iacute;nimos que debe cumplir el certificado. */
    private final Boolean[] keyUsageFilter;

    /** Filtro RFC2254 para el emisor del certificado. */
    private final String rfc2254IssuerFilter;

    /** Filtro RFC2254 para el emisor del certificado. */
    private final String rfc2254SubjectFilter;

    /** Construye un filtro para certificados.
     * @param subjectFilter Cadena seg&uacute;n la RFC2254 para filtro por el campo del titular (subject)
     * @param issuerFilter Cadena seg&uacute;n la RFC2254 para filtro por el campo del emisor (issuer)
     * @param keyUsage M&aacute;scara de bits para filtro por <i>KeyUsage</i><br/>
     *        Cada certificado puede permitir simult&aacute;neamente cualquiera de
     *        estos 8 usos:<br/>
     *        <ol>
     *        <li><b>digitalSignature</b></li>
     *        <li><b>nonRepudiation</b></li>
     *        <li><b>keyEncipherment</b></li>
     *        <li><b>dataEncipherment</b></li>
     *        <li><b>keyAgreement</b></li>
     *        <li><b>keyCertSign</b></li>
     *        <li><b>cRLSign</b></li>
     *        <li><b>encipherOnly</b></li>
     *        <li><b>decipherOnly</b></li>
     *        </ol>
     *        Cada uno de los elementos del array designan en orden a uno de estos 8
     *        usos. El valor de cada elemento puede ser:
     *        <ul>
     *        <li>{@code null}: No establece un filtro para ese uso del certificado.</li>
     *        <li>{@code false}: El certificado no debe tener permitido ese uso.</li>
     *        <li>{@code true}: El certificado debe tener permitido ese uso.</li>
     *        </ul> */
    public RFC2254CertificateFilter(final String subjectFilter, final String issuerFilter, final Boolean[] keyUsage) {
        if (subjectFilter == null && issuerFilter == null && keyUsage == null) {
            throw new NullPointerException("Al menos uno de los criterios de filtrado debe no ser nulo");
        }
        keyUsageFilter = keyUsage.clone();
        rfc2254IssuerFilter = issuerFilter;
        rfc2254SubjectFilter = subjectFilter;
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
            Logger.getLogger("es.gob.afirma").warning("No ha sido posible filtrar el certificado (filtro: '" + f
                                                      + "', nombre: '"
                                                      + name
                                                      + "'), no se eliminara del listado: "
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
                Logger.getLogger("es.gob.afirma")
                      .warning("El nombre proporcionado para filtrar no contiene atributos, no se mostrara el certificado en el listado");
                return false;
            }
            final Attributes attrs = new BasicAttributes(true);
            for (final Rdn rdn : rdns) {
                attrs.put(rdn.getType(), rdn.getValue());
            }
            return new SearchFilter(f).check(attrs);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No ha sido posible filtrar el certificado (filtro: '" + f
                                                      + "', nombre: '"
                                                      + name
                                                      + "'), no se eliminara del listado: "
                                                      + e);
            return true;
        }
    }

    /** Comprueba si el uso de un certificado concuerda con un filtro dado.
     * @param cert
     *        Certificado X.509 que queremos comprobar
     * @param filter
     *        Filtro con los bits de uso (<i>KeyUsage</i>) a verificar
     * @return <code>true</code> si el certificado concuerda con el filtro, <code>false</code> en caso contrario */
    private boolean matchesKeyUsageFilter(final Boolean[] filter, final X509Certificate cert) {
        if (filter == null) {
            return true;
        }
        if (cert == null) {
            return false;
        }
        if (filter.length == 9) {
            boolean[] certUsage = cert.getKeyUsage();
            if (certUsage != null) {
                for (int j = 0; j < certUsage.length; j++) {
                    if (filter[j] != null && filter[j].booleanValue() != certUsage[j]) {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

}
