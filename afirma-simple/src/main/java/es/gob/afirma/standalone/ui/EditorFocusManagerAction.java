/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import javax.swing.event.HyperlinkEvent;

/** Define los requerimientos de las clases con las tareas a hacer en caso de acci&oacute;n sobre
 * hiperv&iacute;nculos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public interface EditorFocusManagerAction {

    /** Acci&oacute;n a realizar en la apertura de un hiperv&iacute;nculo.
     * @param he Evento de hiperv&iacute;nculo.
     * @param linkIndex &Iacute;ndice del enlace pulsado. */
    void openHyperLink(HyperlinkEvent he, int linkIndex);

}
