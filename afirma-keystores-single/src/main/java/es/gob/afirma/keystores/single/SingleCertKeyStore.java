/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.single;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.security.Key;
import java.security.KeyStoreException;
import java.security.KeyStoreSpi;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/** <code>KeyStore</code> para el manejo de certificados en disco en formato
 * PKCS#7 o X.509 en Base64. */
public final class SingleCertKeyStore extends KeyStoreSpi {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final Hashtable<String, X509Certificate> certificates = new Hashtable<String, X509Certificate>();

    @Override
    public Enumeration<String> engineAliases() {
        return this.certificates.keys();
    }

    @Override
    public boolean engineContainsAlias(final String alias) {
        if (alias == null) {
            return false;
        }
        for (final Enumeration<String> e = engineAliases(); e.hasMoreElements();) {
            if (e.nextElement().equals(alias)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void engineDeleteEntry(final String alias) throws KeyStoreException {
        if (alias == null) {
            return;
        }
        this.certificates.remove(alias);
    }

    @Override
    public Certificate engineGetCertificate(final String alias) {
        if (alias == null) {
            return null;
        }
        return this.certificates.get(alias);
    }

    @Override
    public String engineGetCertificateAlias(final Certificate cert) {
        if (!(cert instanceof X509Certificate)) {
            return null;
        }
        String tmpAlias;
        for (final Enumeration<String> e = engineAliases(); e.hasMoreElements();) {
            tmpAlias = e.nextElement();
            if (this.certificates.get(tmpAlias).equals(cert)) {
                return tmpAlias;
            }
        }
        return null;
    }

    @Override
    public Certificate[] engineGetCertificateChain(final String alias) {
        if (!engineContainsAlias(alias)) {
            return new Certificate[0];
        }
        return new Certificate[] {
            this.certificates.get(alias)
        };
    }

    @Override
    public Date engineGetCreationDate(final String arg0) {
        return new Date();
    }

    @Override
    public Key engineGetKey(final String alias, final char[] arg1) throws NoSuchAlgorithmException, UnrecoverableKeyException {
        if (!engineContainsAlias(alias)) {
            throw new UnrecoverableKeyException("No hay ningun certificado con el alias '" + alias + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return engineGetCertificate(alias).getPublicKey();
    }

    @Override
    public boolean engineIsCertificateEntry(final String arg0) {
        return false;
    }

    @Override
    public boolean engineIsKeyEntry(final String arg0) {
        return false;
    }

    @Override
    public void engineLoad(final InputStream is, final char[] pwd) throws IOException, NoSuchAlgorithmException, CertificateException {
        if (is == null) {
            throw new IOException("Se necesitan certificados"); //$NON-NLS-1$
        }

        // Primero leemos todo el Stream en un ByteArray
        final byte[] certs = AOUtil.getDataFromInputStream(is);

        // Probamos con la factoría de Sun
        Collection<? extends Certificate> tmpColCerts = null;
        try {
            if (this.cf == null) {
                this.cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
            }
            tmpColCerts = this.cf.generateCertificates(new ByteArrayInputStream(certs));
        }
        catch (final Exception e) {
            LOGGER.warning("La factoria no ha podido generar los certificados directamente, se probara con un pretratamiento: " + e); //$NON-NLS-1$
            getCertificatesFromStream(new ByteArrayInputStream(certs));
            return;
        }
        if (tmpColCerts != null) {
            for (final Certificate c : tmpColCerts) {
                if (!(c instanceof X509Certificate)) {
                    LOGGER.warning("Se ha encontrado un certificado en un formato que no es X.509, se ignorara"); //$NON-NLS-1$
                    continue;
                }
                try {
                    this.certificates.put(AOUtil.getCN((X509Certificate) c), (X509Certificate) c);
                }
                catch (final Exception e) {
                    LOGGER.warning("Error anadiendo un certificado, se ignorara y se continuara con los siguientes: " + e); //$NON-NLS-1$
                }
            }
        }
        // Si la coleccion es nula queda la opcion de que haya que pretratar,
        // aun cuando no salto la
        // excepcion...
        else {
            getCertificatesFromStream(new ByteArrayInputStream(certs));
        }
    }

    @Override
    public void engineSetCertificateEntry(final String arg0, final Certificate arg1) throws KeyStoreException {
        // No soportado, se ignora la llamada
    }

    @Override
    public void engineSetKeyEntry(final String arg0, final byte[] arg1, final Certificate[] arg2) throws KeyStoreException {
     // No soportado, se ignora la llamada
    }

    @Override
    public void engineSetKeyEntry(final String arg0, final Key arg1, final char[] arg2, final Certificate[] arg3) throws KeyStoreException {
     // No soportado, se ignora la llamada
    }

    @Override
    public int engineSize() {
        return 1;
    }

    @Override
    public void engineStore(final OutputStream arg0, final char[] arg1) throws IOException, NoSuchAlgorithmException, CertificateException {
        // No soportado, se ignora la llamada
    }

    private void getCertificatesFromStream(final InputStream stream) {
        final BufferedReader br = new BufferedReader(new InputStreamReader(new DataInputStream(stream)));
        String strLine;
        String currentAlias = null;
        StringBuilder currentCertificate = null;
        try {
            while ((strLine = br.readLine()) != null) {
                // Certificado nuevo
                if (strLine.trim().equals("-----BEGIN CERTIFICATE-----")) { //$NON-NLS-1$
                    currentCertificate = new StringBuilder(strLine);
                    currentCertificate.append("\n"); //$NON-NLS-1$
                }
                else if (strLine.trim().equals("-----END CERTIFICATE-----")) { //$NON-NLS-1$
                    if (currentCertificate != null) {
                        currentCertificate.append(strLine);
                        addCertificate(currentCertificate.toString(), currentAlias);
                        currentCertificate = null;
                        currentAlias = null;
                    }
                }
                else if (strLine.trim().startsWith("friendlyName:")) { //$NON-NLS-1$
                    currentAlias = strLine.replace("friendlyName:", "").trim(); //$NON-NLS-1$ //$NON-NLS-2$
                }
                else if (currentCertificate != null) {
                    currentCertificate.append(strLine);
                    currentCertificate.append("\n"); //$NON-NLS-1$
                }
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error leyendo los certificados, puede que no se anadiesen todos: " + e); //$NON-NLS-1$
        }
    }

    private CertificateFactory cf = null;

    private void addCertificate(final String base64Cert, String alias) {
        if (base64Cert == null) {
            LOGGER.warning("El certificado es nulo, no se anadira al almacen"); //$NON-NLS-1$
            return;
        }
        final X509Certificate tmpCert;
        try {
            if (this.cf == null) {
                this.cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
            }
            tmpCert = (X509Certificate) this.cf.generateCertificate(new ByteArrayInputStream(base64Cert.getBytes()));
        }
        catch (final Exception e) {
            LOGGER.warning("Error generando el certificado, no se anadira al almacen: " + e); //$NON-NLS-1$
            return;
        }
        if (tmpCert == null) {
            LOGGER.warning("Error generando el certificado, no se anadira al almacen"); //$NON-NLS-1$
            return;
        }

        this.certificates.put((alias != null) ? alias : AOUtil.getCN(tmpCert), tmpCert);
    }

}
