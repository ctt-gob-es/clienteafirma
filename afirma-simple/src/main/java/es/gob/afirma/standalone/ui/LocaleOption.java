package es.gob.afirma.standalone.ui;

import java.util.Locale;

/** Clase que contiene el nombre y locale de un idioma del men&uacute;*/
public class LocaleOption {
	
    private final String name;
    private final Locale locale;

    LocaleOption(final String name, final Locale locale) {
        this.name = name;
        this.locale = locale;
    }

    public Locale getLocale() {
		return this.locale;
	}

	@Override
    public String toString() {
        return this.name; // Esto se mostrará en el menú
    }
}
