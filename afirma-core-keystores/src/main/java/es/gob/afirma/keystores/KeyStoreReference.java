package es.gob.afirma.keystores;

/**
 * Referencia a un almac&eacute;n de claves mediante su tipo y, si es necesario, el nombre de la biblioteca, fichero o
 * ruta de acceso al mismo.
 */
public class KeyStoreReference {

    private final AOKeyStore type;
    private final String libraryName;

    /**
     * Construye una referencia a un almac&eacute;n de claves.
     * @param type Tipo de almac&eacute;n de claves.
     * @param libraryName Nombre de la biblioteca, fichero o ruta de acceso al mismo. Puede ser {@code null} si no es necesario.
     */
    public KeyStoreReference(final AOKeyStore type, final String libraryName) {
        this.type = type;
        this.libraryName = libraryName;
    }

    /**
     * Obtiene el tipo de almac&eacute;n de claves.
     * @return Tipo de almac&eacute;n de claves.
     */
    public AOKeyStore getType() {
        return this.type;
    }

    /**
     * Obtiene el nombre de la biblioteca, fichero o ruta de acceso al mismo.
     * @return Nombre de la biblioteca, fichero o ruta de acceso al mismo. Puede ser {@code null} si no es necesario.
     */
    public String getLibraryName() {
        return this.libraryName;
    }
}