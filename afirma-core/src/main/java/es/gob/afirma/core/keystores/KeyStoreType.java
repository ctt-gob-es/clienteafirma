package es.gob.afirma.core.keystores;

/**
 * Clase que representa un tipo de almac&eacute;n de certificados
 * para su uso en el di&oacute;logo.
 */
public class KeyStoreType
{

    public static final int SYSTEM = 1; //$NON-NLS-1$

    public static final int MOZILLA = 2; //$NON-NLS-1$

    public static final int PKCS12 = 3; //$NON-NLS-1$

    public static final int DNIE = 4; //$NON-NLS-1$

    public static final int PKCS11 = 5; //$NON-NLS-1$

    public static final int BROWSER = 6; //$NON-NLS-1$

    private String id;

    private int type;

    /**
     * Construye un tipo de almac&eacute;n de certificados.
     * @param name Identificador del almac&eacute;n.
     * @param type Tipo de almac&eacute;n.
     */
    public KeyStoreType(String name, int type) {
        this.id = name;
        this.type = type;
    }

    /**
     * Obtiene el identificador del almac&eacute;n.
     * @return Identificador del almac&eacute;n.
     */
    public String getId() {
        return id;
    }

    /**
     * Obtiene el tipo del almac&eacute;n.
     * @return Tipo del almac&eacute;n.
     */
    public int getType() {
        return type;
    }
}
