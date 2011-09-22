package com.lowagie.text.pdf;

import java.util.Calendar;
import java.util.GregorianCalendar;

/** Proporciona una fecha (<code>Calendar</code>) inmutable para garantizar que se usa la misma
 * en todas las operaciones relacionadas con la firma de PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class PdfDateProvider {
    
    private PdfDateProvider() {
        // No permitimos la instanciacion
    }
    
    private static Calendar cal = null;
    
    /** Devuelve siempre el mismo calendario.
     * @return Calendario fijo, iniciaizado al momento de la primera llamada */
    public static Calendar getCalendar() {
        if (cal == null) {
            cal = new GregorianCalendar();
        }
        return cal;
    }

}
