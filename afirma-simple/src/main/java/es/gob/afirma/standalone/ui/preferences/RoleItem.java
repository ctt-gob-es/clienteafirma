package es.gob.afirma.standalone.ui.preferences;

/** Rol utilizado para FacturaE con clave y valor */
public class RoleItem {
	
    private final String key;   
    private final String value;

    public RoleItem(final String key, final String value) {
        this.key = key;
        this.value = value;
    }

    public String getKey() {
        return this.key;
    }

    public String getValue() {
        return this.value;
    }

    @Override
    public String toString() {
        return this.key;
    }

}
