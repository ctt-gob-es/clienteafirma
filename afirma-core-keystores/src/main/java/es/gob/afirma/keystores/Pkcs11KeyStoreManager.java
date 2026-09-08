package es.gob.afirma.keystores;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

import javax.security.auth.callback.PasswordCallback;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.cert.CertificateException;
import java.util.logging.Level;

public class Pkcs11KeyStoreManager extends AOKeyStoreManager {

    private String libraryPath = null;

    Pkcs11KeyStoreManager() {
        setType(AOKeyStore.PKCS11);
    }

    @Override
    public void init(AOKeyStore type, InputStream store, PasswordCallback pssCallBack, Object[] params,
                     boolean forceReset) throws IOException, AOKeyStoreManagerException {

        // Comprobamos los parametros:
        // [0] -Biblioteca PKCS#11, debe estar en el Path (Windows) o en el LD_LIBRARY_PATH (UNIX, Linux, Mac OS X)
        // [1] -Descripcion del token PKCS#11 (opcional)
        // [2] -Numero de lector de tarjeta (Sistema Operativo) [OPCIONAL]

        if (params == null || params.length < 1) {
            throw new IOException(
                    "No se puede acceder al KeyStore PKCS#11 si no se especifica la biblioteca" //$NON-NLS-1$
            );
        }
        if (params[0] == null) {
            throw new IllegalArgumentException(
                    "No se puede acceder al KeyStore PKCS#11 si se especifica una biblioteca nula" //$NON-NLS-1$
            );

        }
        this.libraryPath = params[0].toString();

        // Descripcion del token PKCS#11
        String description = null;
        if (params.length >= 2 && params[1] instanceof String) {
            description = (String) params[1];
        }

        // Numero de lector
        Integer slot = null;
        if (params.length >= 3 && params[2] instanceof Integer) {
            slot = (Integer) params[2];
        }

        // Agregamos un nombre a cada PKCS#11 para asegurarnos de no se agregan mas de una vez como Provider.
        // Si ya se cargo el PKCS#11 anteriormente, se volvera a instanciar.
        final String p11ProviderName = new File(this.libraryPath).getName().replace('.', '_').replace(' ', '_');
        Provider p11Provider = Security.getProvider("SunPKCS11-" + p11ProviderName); //$NON-NLS-1$

        if (p11Provider != null && (forceReset || Boolean.getBoolean("es.gob.afirma.keystores.DoNotReusePkcs11Provider"))) { //$NON-NLS-1$
            LOGGER.info("Se retira el proveedor " + p11Provider); //$NON-NLS-1$
            Security.removeProvider(p11Provider.getName());
            p11Provider = null;
        }

        if (p11Provider == null) {

            final byte[] config = KeyStoreUtilities.createPKCS11ConfigFile(this.libraryPath, p11ProviderName, slot, description).getBytes();
            try {
                p11Provider = getP11Provider(config);
            }
            catch (final Exception e) {
                LOGGER.warning(
                        "Ha fallado el primer intento de inicializacion del PKCS#11 para la la biblioteca '" + this.libraryPath + "', se reintentara: " + e //$NON-NLS-1$ //$NON-NLS-2$
                );
                // El PKCS#11 del DNIe a veces falla a la primera pero va
                // correctamente a la segunda asi que reintentamos una vez mas
                try {
                    p11Provider = getP11Provider(config);
                }
                catch (final Exception ex) {
                    LOGGER.log(Level.WARNING,
                            "Ha fallado el segundo intento de inicializacion del PKCS#11 para la la biblioteca " + this.libraryPath, e //$NON-NLS-1$
                    );
                    throw new AOKeyStoreManagerException(
                            "No se ha podido instanciar el proveedor SunPKCS11 para la la biblioteca '" + this.libraryPath + "': " + ex, ex, KeyStoreErrorCode.Internal.LOADING_PKCS11_KEYSTORE_ERROR  //$NON-NLS-1$//$NON-NLS-2$
                    );
                }
            }
        } else {
            LOGGER.info(
                    "El proveedor SunPKCS11 solicitado ya estaba instanciado, se reutilizara esa instancia: " + p11Provider.getName() //$NON-NLS-1$
            );
        }

        KeyStore keyStore;
        if (pssCallBack == null) {
            keyStore = getKeyStoreWithNullPassword(p11Provider);
        } else {
            if (pssCallBack instanceof UIPasswordCallback) {
                String moduleDescription = description != null ? description : AOKeyStore.PKCS11.getName();
                final String promptText = KeyStoreMessages.getString("AOKeyStore.15", moduleDescription); //$NON-NLS-1$
                ((UIPasswordCallback) pssCallBack).setPrompt(promptText);
            }
            try {
                keyStore = KeyStoreUtilities.getKeyStoreWithPasswordCallbackHandler(
                        AOKeyStore.PKCS11,
                        pssCallBack,
                        p11Provider,
                        this.getParentComponent()
                );
            }
            catch (final AOCancelledOperationException e) {
                // Se retira el proveedor si el usuario cancela el uso de la tarjeta
                Security.removeProvider("SunPKCS11-" + p11ProviderName); //$NON-NLS-1$
                throw e;
            }
            catch (final Exception e) {
                // En caso de no poder instanciar la tarjeta en cuestion, se retira el proveedor
                Security.removeProvider("SunPKCS11-" + p11ProviderName); //$NON-NLS-1$
                throw new AOKeyStoreManagerException(
                        "Error construyendo el KeyStore PKCS#11 para la biblioteca '" + this.libraryPath + "': " + e, e, KeyStoreErrorCode.Internal.LOADING_PKCS11_KEYSTORE_ERROR //$NON-NLS-1$ //$NON-NLS-2$
                );
            }
        }

        setKeyStore(keyStore);
    }

    private static KeyStore getKeyStoreWithNullPassword(final Provider p11Provider) throws AOKeyStoreManagerException {
        final KeyStore ks;
        try {
            ks = KeyStore.getInstance(AOKeyStore.PKCS11.getProviderName(), p11Provider);
        }
        catch (final Exception e) {
            Security.removeProvider(p11Provider.getName());
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen PKCS#11: " + e, e, KeyStoreErrorCode.Internal.LOADING_PKCS11_KEYSTORE_ERROR); //$NON-NLS-1$
        }

        try {
            ks.load(null, null);
        }
        catch (final IOException e) {
            throw new AOKeyStoreManagerException(
                    "No se ha podido obtener el almacen PKCS#11 solicitado: " + e, e, KeyStoreErrorCode.Internal.LOADING_PKCS11_KEYSTORE_ERROR //$NON-NLS-1$
            );
        }
        catch (final CertificateException e) {
            Security.removeProvider(p11Provider.getName());
            throw new AOKeyStoreManagerException(
                    "No se han podido cargar los certificados del almacen PKCS#11 solicitado: " + e, e, KeyStoreErrorCode.Internal.LOADING_PKCS11_KEYSTORE_ERROR //$NON-NLS-1$
            );
        }
        catch (final NoSuchAlgorithmException e) {
            Security.removeProvider(p11Provider.getName());
            throw new AOKeyStoreManagerException(
                    "No se ha podido verificar la integridad del almacen PKCS#11 solicitado: " + e, e, KeyStoreErrorCode.Internal.LOADING_PKCS11_KEYSTORE_ERROR //$NON-NLS-1$
            );
        }
        return ks;
    }

    private static Provider getP11Provider(final byte[] p11NSSConfigFileContents) throws NoSuchMethodException,
            SecurityException,
            IllegalAccessException,
            IllegalArgumentException,
            InvocationTargetException,
            InstantiationException,
            ClassNotFoundException,
            IOException {
        return AOUtil.isJava9orNewer() ?
                getP11ProviderJava9(p11NSSConfigFileContents) :
                getP11ProviderJava8(p11NSSConfigFileContents);
    }

    private static Provider getP11ProviderJava9(final byte[] p11NSSConfigFileContents) throws IOException,
            NoSuchMethodException,
            SecurityException,
            IllegalAccessException,
            IllegalArgumentException,
            InvocationTargetException {
        final Provider p = Security.getProvider("SunPKCS11"); //$NON-NLS-1$
        final File f = File.createTempFile("pkcs11_", ".cfg");  //$NON-NLS-1$//$NON-NLS-2$
        try (
                final OutputStream fos = new FileOutputStream(f);
        ) {
            fos.write(p11NSSConfigFileContents);
        }
        final Method configureMethod = Provider.class.getMethod("configure", String.class); //$NON-NLS-1$
        final Provider configuredProvider = (Provider) configureMethod.invoke(p, f.getAbsolutePath());
        f.deleteOnExit();
        Security.addProvider(configuredProvider);
        return configuredProvider;
    }

    private static Provider getP11ProviderJava8(final byte[] p11NSSConfigFileContents) throws InstantiationException,
            IllegalAccessException,
            IllegalArgumentException,
            InvocationTargetException,
            NoSuchMethodException,
            SecurityException,
            ClassNotFoundException {
        final Provider p = (Provider) Class.forName("sun.security.pkcs11.SunPKCS11") //$NON-NLS-1$
                .getConstructor(InputStream.class)
                .newInstance(new ByteArrayInputStream(p11NSSConfigFileContents));
        Security.addProvider(p);
        return p;
    }

    @Override
    public String getReference() {
        return this.getType().name() + ":" + this.libraryPath; //$NON-NLS-1$
    }

    @Override
    public void refresh() throws IOException {
        // El PKCS#11 no se puede refrescar, asi que unicamente reseteamos la cache de alias para asegurarnos que
        // futuras consultas volveran a listar los certificados del almacen
        resetCachedAliases();
    }
}
