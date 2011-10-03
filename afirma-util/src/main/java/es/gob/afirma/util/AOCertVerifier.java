/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.util;

import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.security.Security;
import java.security.cert.CertPath;
import java.security.cert.CertPathValidator;
import java.security.cert.CertPathValidatorException;
import java.security.cert.CertStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.LDAPCertStoreParameters;
import java.security.cert.PKIXParameters;
import java.security.cert.TrustAnchor;
import java.security.cert.X509CertSelector;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.naming.ldap.LdapName;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;

/** Clase para la verificaci&oacute;n de certificados X.509. Ejemplo de uso:<br>
 * <code><pre>
 *      // Instanciamos la clase verificadora
 *      AOCertVerifier v = new AOCertVerifier();
 *
 *      // Indicamos que verifique la validez temporal del certificado
 *      v.setCheckValidity(true);
 *
 *      // Anadimos un cerificado raiz desde LDAP (DNIe CA)
 *      v.addRootCertificatesFromLdap(
 *          "ldap.dnie.es",
 *          new LdapName("CN=AC RAIZ DNIE,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES")
 *      );
 *
 *      // Anadimos un par de certificados raiz desde disco duro (DNIe VA)
 *      v.addRootCertificate(new FileInputStream(new File("c:\\AVDNIEFNMTSHA1.cer")));
 *      v.addRootCertificate(new FileInputStream(new File("c:\\AVDNIEFNMTSHA2.cer")));
 *      // Habilitamos el OCSP (DNIe SHA-1)
 *      v.enableOCSP(
 *          new URL("http://ocsp.dnielectronico.es:80"),
 *          new LdapName("CN=AV DNIE FNMT,OU=FNMT,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES"),
 *          new LdapName("CN=AC DNIE 001,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES"),
 *          "34:23:43:b8:af:dd:e6:fd:4a:16:97:3f:bc:90:cc:b3"
 *      );
 *
 *      // Verificamos el certificado de alias 'myAlias' que esta en el KeyStore 'myKeyStore'
 *      v.checkCertificate(
 *              myKeyStore.getCertificateChain("myAlias")
 *      );
 * </pre></code>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @version 0.1 */

public final class AOCertVerifier {

    private final Set<TrustAnchor> tas = new HashSet<TrustAnchor>();
    
    private static final Logger LOGGER = Logger.getLogger("es.atosorigin"); //$NON-NLS-1$

    private boolean checkValidity = true;

    /** Mensaje de error devuelto por la &uacute;ltima operaci&oacute;n de
     * validaci&oacute;n. */
    private String errorMessage = null;

    /** A&ntilde;ade un certificado ra&iacute;z como parte de la cadena de
     * confianza.
     * @param c
     *        Certificado ra&iacute;z */
    public void addRootCertificate(final X509Certificate c) {
        if (c == null) {
            LOGGER.warning(
            "No se pueden anadir certificados nulos" //$NON-NLS-1$
            );
            return;
        }
        this.tas.add(new TrustAnchor(c, null));
    }

    /** A&ntilde;ade un certificado ra&iacute;z como parte de la cadena de
     * confianza.
     * @param url
     *        Ruta hacia el certificado ra&iacute;z */
    public void addRootCertificate(final URL url) {
        if (url == null) {
            LOGGER.warning(
                           "No se pueden anadir certificados desde una URL nula" //$NON-NLS-1$
            );
            return;
        }
        try {
            this.tas.add(new TrustAnchor((X509Certificate) CertificateFactory.getInstance("X509").generateCertificate( //$NON-NLS-1$
            AOUtil.loadFile(url.toURI())), null));
        }
        catch (final Exception e) {
            LOGGER.severe(
                          "No se pudo crear el certificado desde la URL '" + url.toString() + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
            );
            return;
        }
    }

    /** A&ntilde;ade un certificado ra&iacute;z como parte de la cadena de
     * confianza.
     * @param cert
     *        Flujo para la lectura del certificado ra&iacute;z */
    public void addRootCertificate(final InputStream cert) {
        if (cert == null) {
            LOGGER.warning(
                           "No se pueden anadir certificados nulos" //$NON-NLS-1$
            );
            return;
        }

        final X509Certificate ca;
        try {
            ca = (X509Certificate) CertificateFactory.getInstance("X509").generateCertificate(cert); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.severe(
                          "No se pudo crear el certificado: " + e //$NON-NLS-1$
            );
            return;
        }
        this.tas.add(new TrustAnchor(ca, null));

    }

    /** A&ntilde;ade certificados ra&iacute;z como parte de la cadena de
     * confianza.
     * @param svr
     *        Servidor LDAP donde se encuentran los certificados
     * @param location
     *        Ruta hacia los certificados dentro del servidor LDAP */
    public void addRootCertificatesFromLdap(final String svr, final LdapName location) {
        if (svr == null || "".equals(svr) || location == null) { //$NON-NLS-1$
            LOGGER.warning(
                           "No se pueden anadir certificados desde un servidor o una localizacion nula o vacia" //$NON-NLS-1$
            );
            return;
        }
        
        String server = svr;

        // Comprobamos que el nombre sea correcto
        if (server.startsWith("ldap://")) //$NON-NLS-1$
         {
            server = server.replace("ldap://", ""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        int port = 389;
        if (server.contains(":")) { //$NON-NLS-1$
            String tmpPort = server.substring(server.indexOf(":") + 1, server.length() //$NON-NLS-1$
            );
            server = server.substring(0, server.indexOf(":")); //$NON-NLS-1$
            String tmpRoot = null;
            if (tmpPort.contains("/")) { //$NON-NLS-1$
                if (tmpPort.indexOf("/") != tmpPort.length() - 1) { //$NON-NLS-1$
                    tmpRoot = tmpPort.substring(tmpPort.indexOf("/") + 1); //$NON-NLS-1$
                }
                tmpPort = tmpPort.substring(0, tmpPort.indexOf("/")); //$NON-NLS-1$
            }
            try {
                port = Integer.parseInt(tmpPort);
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
                "El puerto proporcionado (" + tmpPort + ") no es un numero: " + e //$NON-NLS-1$ //$NON-NLS-2$
                );
            }
            server = tmpRoot;
        }
        if (server.contains("/")) //$NON-NLS-1$
         {
            server = server.substring(0, server.indexOf("/")); //$NON-NLS-1$
        }

        final CertStore cs;
        try {
            cs = CertStore.getInstance("LDAP", //$NON-NLS-1$
                                       new LDAPCertStoreParameters(server, port));
        }
        catch (final Exception e) {
            LOGGER.warning(
                           "No se pudo anadir la configuracion del servidor LDAP, no se anadiran certificados: " + e //$NON-NLS-1$
            );
            return;
        }

        final X509CertSelector xcs = new X509CertSelector();
        try {
            xcs.setSubject(location.toString());
        }
        catch (final Exception e) {
            LOGGER.warning(
                           "No se pudo anadir la configuracion de la localizacion LDAP, no se anadiran certificados: " + e //$NON-NLS-1$
            );
            return;
        }

        try {
            for (final Certificate f : cs.getCertificates(xcs)) {
                this.tas.add(new TrustAnchor((X509Certificate) f, null));
            }
        }
        catch (final Exception e) {
            LOGGER.warning(
                           "No se pudieron anadir certificados desde el LDAP: " + e //$NON-NLS-1$
            );
        }

    }

    /** Establece si se debe comprobar o no si el certificado est&aacute; dentro
     * de su periodo de validez.
     * @param c
     *        <code>true</code> si se desea comprobar la validez temporal
     *        del certificado, <code>false</code> en caso contrario */
    public void setCheckValidity(final boolean c) {
        this.checkValidity = c;
    }

    /** Deshabilita las comprobaciones de revocaci&oacute;n por OCSP. */
    public void disableOCSP() {
        Security.setProperty("ocsp.enable", "false"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Habilita las comprobaciones de revocaci&oacute;n por OCSP.
     * @param responderURL
     *        URL del <i>responder</i> OCSP
     * @param responderCertSubjectName
     *        Nombre del titular del certificado del <i>responder</i> OCSP
     *        (puede ser <code>null</code>)
     * @param responderCertIssuerName
     *        Nombre del emisor del certificado del <i>responder</i> OCSP
     *        (puede ser <code>null</code>)
     * @param responderCertSerialNumber
     *        N&uacute;mero de serie del certificado del <i>responder</i>
     *        OCSP (puede ser <code>null</code>) */
    public void enableOCSP(final URL responderURL,
                           final LdapName responderCertSubjectName,
                           final LdapName responderCertIssuerName,
                           final String responderCertSerialNumber) {
        Security.setProperty("ocsp.enable", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        if (responderURL != null) {
            Security.setProperty("ocsp.responderURL", responderURL.toString()); //$NON-NLS-1$
        }
        if (responderCertSubjectName != null) {
            Security.setProperty("ocsp.responderCertSubjectName", responderCertSubjectName.toString()); //$NON-NLS-1$
        }
        if (responderCertIssuerName != null) {
            Security.setProperty("ocsp.responderCertIssuerName", responderCertIssuerName.toString()); //$NON-NLS-1$
        }
        if (responderCertSerialNumber != null) {
            Security.setProperty("ocsp.responderCertSerialNumber", responderCertSerialNumber); //$NON-NLS-1$
        }
    }

    /** Comprueba la validez de un certificado dentro de una cadena de confianza.
     * @param certChain
     *        Cadena de confianza a verificar
     * @param verifyRevocation
     *        <code>true</code> si deseamos que se compruebe la
     *        revocaci&oacute;n, <code>false</code> en caso contrario
     * @throws AOException
     *         Cuando falla la validaci&oacute;n, el mensaje de la
     *         excepci&oacute;n indica el motivo
     * @throws CertificateExpiredException
     *         Cuando el certificado est&aacute; caducado.
     * @throws CertificateNotYetValidException
     *         Cuando el certificado a&uacute;n no es v&aacute;lido.
     * @throws CertPathValidatorException
     *         Cuando no se ha podido validar la cadena de certificaci&oacute;n completa.
     * @throws AOCertificateRevokedException
     *         Cuando el certificado de usuario est&aacute; revocado. */
    public void checkCertificate(final Certificate[] certChain, final boolean verifyRevocation) throws AOException,
                                                                                               CertificateExpiredException,
                                                                                               CertificateNotYetValidException,
                                                                                               CertPathValidatorException,
                                                                                               AOCertificateRevokedException {

        this.errorMessage = null;

        if (this.checkValidity || !verifyRevocation) {
            for (final Certificate c : certChain) {
                try {
                    ((X509Certificate) c).checkValidity();
                }
                catch (final CertificateExpiredException e) {
                    this.errorMessage = UtilMessages.getString("AOCertVerifier.0"); //$NON-NLS-1$
                    throw e;
                }
                catch (final CertificateNotYetValidException e) {
                    this.errorMessage = UtilMessages.getString("AOCertVerifier.1"); //$NON-NLS-1$
                    throw e;
                }
                catch (final Exception e) {
                    this.errorMessage = UtilMessages.getString("AOCertVerifier.2"); //$NON-NLS-1$
                    throw new AOException(this.errorMessage, e);
                }
            }
        }

        if (!verifyRevocation) {
            return;
        }

        final List<X509Certificate> cplist = new ArrayList<X509Certificate>();
        for (final Certificate c : certChain) {
            cplist.add((X509Certificate) c);
        }
        final CertPath cp;
        try {
            cp = CertificateFactory.getInstance("X509").generateCertPath(cplist); //$NON-NLS-1$
        }
        catch (final Exception e) {
            this.errorMessage = UtilMessages.getString("AOCertVerifier.3"); //$NON-NLS-1$
            throw new AOException(this.errorMessage, e);
        }

        final PKIXParameters pp;
        try {
            pp = new PKIXParameters(this.tas);
        }
        catch (final Exception e) {
            this.errorMessage = UtilMessages.getString("AOCertVerifier.4"); //$NON-NLS-1$
            throw new AOException("Error creando los parametros PKIX", e); //$NON-NLS-1$
        }
        pp.setRevocationEnabled(true);

        final CertPathValidator cpv;
        try {
            cpv = CertPathValidator.getInstance("PKIX"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            this.errorMessage = UtilMessages.getString("AOCertVerifier.5"); //$NON-NLS-1$
            throw new AOException("Error obteniendo un validador PKIX", e); //$NON-NLS-1$
        }

        try {
            cpv.validate(cp, pp);
        }
        catch (final CertPathValidatorException e) {
            this.errorMessage = UtilMessages.getString("AOCertVerifier.7"); //$NON-NLS-1$

            // Este metodo permite detectar, tanto en Javad 6 como 7, si la causa de que
            // el certificado no sea valido es que este revocado. En Java 7, la clase
            // CertificateRevokedException es publica y permite recuperar la fecha y el motivo
            // de la revocacion
            final Throwable cause = e.getCause();
            final Class<? extends Throwable> exceptionClass = cause != null ? cause.getClass() : e.getClass();
            if (exceptionClass.getSimpleName().equals("CertificateRevokedException")) { //$NON-NLS-1$
                final AOCertificateRevokedException cre = new AOCertificateRevokedException(UtilMessages.getString("AOCertVerifier.8"), e); //$NON-NLS-1$
                try {
                    final Method getRevocationDateMethod = exceptionClass.getMethod("getRevocationDate", (Class[]) null); //$NON-NLS-1$
                    cre.setRevocationDate((Date) getRevocationDateMethod.invoke(cause, (Object[]) null));

                    final Method getRevocationReasonMethod = exceptionClass.getMethod("getRevocationReason", (Class[]) null); //$NON-NLS-1$
                    cre.setRevocationReason((String) getRevocationReasonMethod.invoke(cause, (Object[]) null));
                }
                catch (final Exception e2) {
                    // En java 6 estos metodos no existiran
                }
                throw cre;
            }
            throw e;
        }
        catch (final Exception e) {
            this.errorMessage = UtilMessages.getString("AOCertVerifier.6"); //$NON-NLS-1$
            throw new AOException("El certificado no ha sido validado", e); //$NON-NLS-1$
        }
    }

    /** Recupera el mensaje de error devuelta por la &uacute;ltima operacion de
     * verificaci&oacute;n. Si la &uacute;ltima operaci&oacute;n no produjo un
     * error, devolver&aacute; {@code null}.
     * @return Mensaje de error. */
    public String getErrorMessage() {
        return this.errorMessage;
    }
}
