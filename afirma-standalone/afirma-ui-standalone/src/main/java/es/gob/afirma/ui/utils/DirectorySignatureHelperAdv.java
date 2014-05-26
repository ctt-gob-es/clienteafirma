/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.massive.DirectorySignatureHelper;

/** M&oacute;dulo para la ejecuci&oacute;n de firmas y multifirmas de ficheros. Durante el proceso
 * se muestra una barra de progreso que informa de la situaci&oacute;n. */
public class DirectorySignatureHelperAdv extends DirectorySignatureHelper {

    /** Crea un instancia de la clase con una configuraci&oacute;n y un componente padre asignado.
     * @param algorithm Algoritmo de firma electr&oacute;nica.
     * @param format Formato de firma por defecto.
     * @param mode Modo de firma.
     * @throws AOUnsupportedSignFormatException Cuando el formato de firma no esta soportado. */
    public DirectorySignatureHelperAdv(final String algorithm, final String format, final String mode) throws AOUnsupportedSignFormatException {
        super(algorithm, format, mode);
    }
}
