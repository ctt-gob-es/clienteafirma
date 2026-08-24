package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.Arrays;

/**
 * Perfil de Mozilla Firefox.
 */
public class MozillaProfile {
    /** Nombre del usuario al que pertenece el perfil. */
    private String username;
    /** Nombre del perfil. */
    private String name;
    /** Indica si el perfil esta activo. */
    private boolean active = false;
    /** Indica si la ruta del perfil es relativa. */
    private boolean relative = true;
    /** Ruta del perfil. */
    private String path;
    /** Directorio del perfil. */
    private File profileDir;
    /** Indica si el perfil est&aacute; bloqueado. */
    private boolean locked = false;

    /** Indica si se ha inicializado el perfil con la informacion necesaria para su uso. */
    private boolean prepared = false;
    /** Indica si el perfil tiene contrase&ntilde;a maestra asociada. */
    private boolean hasMasterPassword = false;
    /** Fichero con la contrase&ntilde;a maestra del perfil. */
    private File passwordFile = null;

    /**
     * Ficheros que denotan un almac&eacute;n NSS ya inicializado, en sus dos formatos:
     * <i>sql</i> (<code>cert9.db</code>, <code>pkcs11.txt</code>) y el antiguo
     * <i>dbm</i> (<code>cert8.db</code>).
     */
    // Se reutiliza el codigo aportado por el usuario jmorenobl de GitHub.
    private static final String[] NSS_KEYSTORE_FILES = new String[] {
            "cert9.db", //$NON-NLS-1$
            "pkcs11.txt", //$NON-NLS-1$
            "cert8.db" //$NON-NLS-1$
    };

    /**
     * Recupera el nombre de usuario propietario del perfil.
     * @return Nombre de usuario.
     */
    public String getUsername() {
        return this.username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * Recupera el nombre del perfil.
     * @return Nombre de perfil.
     */
    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    /**
     * Recupera el directorio del perfil.
     * @return Directorio del perfil.
     */
    public File getProfileDir() {
        return this.profileDir;
    }

    /**
     * Establece el directorio del perfil.
     * @param profileDir Directorio del perfil.
     */
    public void setProfileDir(File profileDir) {
        this.profileDir = profileDir;
    }

    /**
     * Indica si se considera que el perfil est&aacute; activo.
     * @return {@code true} si se considera que el perfil est&aacute; activo,
     * {@code false} en caso contrario.
     */
    public boolean isActive() {
        return this.active;
    }

    /**
     * Establece si el perfil est&aacute; activo o no.
     * @param active Se debe indicar {@code true} si se considera que el perfil est&aacute; activo,
     * {@code false} en caso contrario.
     */
    public void setActive(boolean active) {
        this.active = active;
    }

    boolean isRelative() {
        return this.relative;
    }

    void setRelative(final boolean r) {
        this.relative = r;
    }

    public String getPath() {
        return this.path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    boolean isLocked() {
        return this.locked;
    }

    void setLocked(final boolean lock) {
        this.locked = lock;
    }

    /**
     * Indica si se ha ejecutado el m&eacute;todo {@link #prepare(File)} para para preparar la informaci&oacute;n
     * necesaria para ejecutar comandos de certutil con el perfil.
     * @return Indica si se ha preparado el perfil para su uso con certutil.
     */
    public boolean isPrepared() {
        return this.prepared;
    }

    /**
     * Establece la informaci&oacute;n necesaria para ejecutar comandos de certutil con el perfil.
     * @param hasMasterPassword Indica si el el perfil tiene contrase&ntilde;a maestra asociada.
     * @param passwordFile Fichero con la contrase&ntilde;a si se indic&oacute;.
     */
    public void prepare(boolean hasMasterPassword, File passwordFile) {
        this.hasMasterPassword = hasMasterPassword;
        if (passwordFile != null) {
            this.passwordFile = passwordFile;
        }
        this.prepared = true;
    }

    public boolean hasMasterPassword() {
        return this.hasMasterPassword;
    }

    public File getPasswordFile() {
        return this.passwordFile;
    }

    /**
     * Marca el perfil como no preparado y elimina la informaci&oacute;n sensible
     * asociada al perfil.
     */
    public void reset() {
        if (this.passwordFile != null && this.passwordFile.isFile()) {
            secureDelete(this.passwordFile);
        }
        this.passwordFile = null;
        this.hasMasterPassword = false;
        this.prepared = false;
    }

    private void secureDelete(File file) {
        try {
            final RandomAccessFile raf = new RandomAccessFile(file, "rws"); //$NON-NLS-1$
            final long length = raf.length();
            final byte[] data = new byte[64];
            Arrays.fill(data, (byte) 0);
            for (long i = 0; i < length; i += data.length) {
                raf.write(data);
            }
            raf.close();
        } catch (final Exception e) {
            // No se pudo sobreescribir el fichero. No dejamos constancia en el log por seguridad
        }
        if (!file.delete()) {
            // No se pudo eliminar el fichero. No dejamos constancia en el log por seguridad
        }
    }

    /**
     * Indica si el perfil contiene ya un almac&eacute;n NSS.
     * @return {@code true} si el directorio ya tiene un almac&eacute;n NSS,
     *         {@code false} en caso contrario.
     */
    public boolean hasNssKeyStore() {
        for (final String keystoreFile : NSS_KEYSTORE_FILES) {
            if (new File(this.profileDir, keystoreFile).isFile()) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        return "Perfil de Firefox" + //$NON-NLS-1$
                (this.locked ? " bloqueado" : " no bloqueado") + //$NON-NLS-1$ //$NON-NLS-2$
                (this.active ? " y por defecto " : "") + //$NON-NLS-1$ //$NON-NLS-2$
                (this.profileDir != null ? " situado en: " + this.profileDir.getAbsolutePath() : ""); //$NON-NLS-1$ //$NON-NLS-2$
    }
}