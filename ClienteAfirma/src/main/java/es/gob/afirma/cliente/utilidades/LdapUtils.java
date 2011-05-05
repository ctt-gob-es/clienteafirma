/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.utilidades;

import java.security.cert.CertStore;
import java.security.cert.Certificate;
import java.security.cert.LDAPCertStoreParameters;
import java.security.cert.X509CertSelector;
import java.security.cert.X509Certificate;

/**
 * Funciones para el manejo de certificados en servidores LDAP.
 */
public final class LdapUtils {
    
    /**
     * Recupera un certificado de un LDAP.
     * @param server Servidor LDAP.
     * @param port Puerto del servidor.
     * @param principal Principal del certificado a recuperar.
     * @return Certificado recuperado.
     * @throws Throwable Cuando ocurre alg&uacute;n error durante la recuperaci&oacute;n.
     */
    public static X509Certificate getCertificate(String server, int port, String principal) throws Throwable {
        if (server==null || "".equals(server))
            throw new NullPointerException("El servidor no puede ser nulo ni vacio");
        if (principal==null || "".equals(principal))
            throw new NullPointerException("El Principal del Certificado no puede ser nulo ni vacio");
        
        X509CertSelector certSelector = new X509CertSelector();
        certSelector.setSubject(principal);
        
        return (X509Certificate) CertStore.getInstance("LDAP", new LDAPCertStoreParameters(server, port))
                .getCertificates(certSelector).toArray(new Certificate[0])[0]; }
}
