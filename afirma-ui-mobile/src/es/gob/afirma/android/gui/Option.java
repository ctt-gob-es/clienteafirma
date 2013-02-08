package es.gob.afirma.android.gui;

/** Clase que almac&eacute;n los datos b&aacute;sicos de un directorio o fichero del sistema operativo
 * @author Alberto Mart&iacute;nez */
public class Option implements Comparable<Option> {
    private final String name;
    private final String data;
    private final String path;

    /** @param n Nombre del fichero
     * @param d Cadena de texto personalizable para mostrar informaci&oacute;n del fichero
     * @param p Indica la ruta absoluta al fichero */
    public Option(final String n, final String d, final String p) {
        this.name = n;
        this.data = d;
        this.path = p;
    }

    /** Proporciona el nombre del elemento
     * @return Devuelve el nombre del fichero */
    public String getName() {
        return this.name;
    }

    /** Proporciona una cadena de texto personalizable con informaci&oacute;n del fichero
     * @return Devuelve informaci&oacute;n b&aacute;sica del fichero */
    public String getData() {
        return this.data;
    }

    /** Proporciona la ruta absoluta del fichero
     * @return Devuelve la ruta del fichero */
    public String getPath() {
        return this.path;
    }

    @Override
    public int compareTo(final Option o) {
        if (this.name != null) {
            return this.name.toLowerCase().compareTo(o.getName().toLowerCase());
        }
        throw new IllegalArgumentException();
    }
}
