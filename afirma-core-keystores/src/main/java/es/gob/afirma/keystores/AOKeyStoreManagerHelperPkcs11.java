package es.gob.afirma.keystores;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.security.auth.callback.PasswordCallback;

final class AOKeyStoreManagerHelperPkcs11 {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private AOKeyStoreManagerHelperPkcs11() {
		// No permitimos la instanciacion
	}

    /** Inicializa un almac&eacute;n PKCS#11.
     * @param pssCallBack Callback para la recuperaci&oacute;n de la
     *        contrase&ntilde;a del almac&eacute;n.
     * @param params Parametros adicionales para la configuraci&oacute;n del
     *        almac&eacute;n.
     * @return Almac&eacute;n configurado.
     * @throws AOKeyStoreManagerException Cuando ocurre un error durante la inicializaci&oacute;n.
     * @throws IOException Cuando se indique una contrase&ntilde;a incorrecta para la
     *         apertura del almac&eacute;n.
     * @throws es.gob.afirma.keystores.MissingSunPKCS11Exception Si no se encuentra la biblioteca SunPKCS11 */
    static KeyStore initPKCS11(final PasswordCallback pssCallBack,
    		                          final Object[] params) throws AOKeyStoreManagerException,
    		                                                        IOException {

        // En el "params" debemos traer los parametros:
        // [0] -p11lib: Biblioteca PKCS#11, debe estar en el Path (Windows) o en el LD_LIBRARY_PATH (UNIX, Linux, Mac OS X)
        // [1] -desc: Descripcion del token PKCS#11 (opcional)
        // [2] -slot: Numero de lector de tarjeta (Sistema Operativo) [OPCIONAL]

        // Anadimos el proveedor PKCS11 de Sun
        if (params == null || params.length < 2) {
            throw new IOException("No se puede acceder al KeyStore PKCS#11 si no se especifica la biblioteca"); //$NON-NLS-1$
        }
        final String p11lib;
        if (params[0] != null) {
            p11lib = params[0].toString();
        }
        else {
            throw new IllegalArgumentException("No se puede acceder al KeyStore PKCS#11 si se especifica una biblioteca nula"); //$NON-NLS-1$
        }

        // Numero de lector
        Integer slot = null;
        if (params.length >= 3 && params[2] instanceof Integer) {
            slot = (Integer) params[2];
        }

        // Agregamos un nombre a cada PKCS#11 para asegurarnos de no se
        // agregan mas de una vez como provider.
        // Si ya se cargo el PKCS#11 anteriormente, se volvera a instanciar.
        final String p11ProviderName = new File(p11lib).getName().replace('.', '_').replace(' ', '_');
        Provider p11Provider = Security.getProvider("SunPKCS11-" + p11ProviderName); //$NON-NLS-1$

        if (p11Provider == null) {

            Constructor<?> sunPKCS11Contructor;
            try {
                sunPKCS11Contructor = Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new MissingSunPKCS11Exception(e);
            }

            final byte[] config = KeyStoreUtilities.createPKCS11ConfigFile(p11lib, p11ProviderName, slot).getBytes();
            try {
                p11Provider = (Provider) sunPKCS11Contructor.newInstance(new ByteArrayInputStream(config));
            }
            catch (final Exception e) {
                // El PKCS#11 del DNIe a veces falla a la primera pero va
                // correctamente a la segunda asi que reintentamos una vez mas
                try {
                    p11Provider = (Provider) sunPKCS11Contructor.newInstance(new ByteArrayInputStream(config));
                }
                catch (final Exception ex) {
                    throw new AOKeyStoreManagerException("No se ha podido instanciar el proveedor SunPKCS11 para la la biblioteca " + p11lib, ex); //$NON-NLS-1$
                }
            }
            Security.addProvider(p11Provider);
        }
        else {
            LOGGER.info("El proveedor SunPKCS11 solicitado ya estaba instanciado, se reutilizara esa instancia: " + p11Provider.getName()); //$NON-NLS-1$
        }

        final KeyStore ks;
        try {
            ks = KeyStore.getInstance(AOKeyStore.PKCS11.getProviderName(), p11Provider);
        }
        catch (final Exception e) {
            Security.removeProvider(p11Provider.getName());
            p11Provider = null;
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen PKCS#11", e); //$NON-NLS-1$
        }

        try {
            ks.load(null, pssCallBack != null ? pssCallBack.getPassword() : null);
        }
        catch (final IOException e) {
            if (e.getCause() instanceof UnrecoverableKeyException ||
                    e.getCause() instanceof BadPaddingException) {
                throw new IOException("Contrasena invalida: " + e, e); //$NON-NLS-1$
            }
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen PKCS#11 solicitado", e); //$NON-NLS-1$
        }
        catch (final CertificateException e) {
            Security.removeProvider(p11Provider.getName());
            p11Provider = null;
            throw new AOKeyStoreManagerException("No se han podido cargar los certificados del almacen PKCS#11 solicitado", e); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
            Security.removeProvider(p11Provider.getName());
            p11Provider = null;
            throw new AOKeyStoreManagerException("No se ha podido verificar la integridad del almacen PKCS#11 solicitado", e); //$NON-NLS-1$
		}
        return ks;
    }


}
