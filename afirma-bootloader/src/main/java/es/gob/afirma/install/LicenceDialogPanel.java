/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.install;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Label;
import java.io.InputStream;
import java.util.Locale;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;


/** Panel con el acuerdo de licencia del cliente de firma @firma versi&oacute;n 3. */
final class LicenceDialogPanel {

    /** Componente sobre el que se debe mostrar el acuerdo de licencia. */
    private final Component parentComponent;

    /** Crea el di&aacute;logo y establece un componente padre sobre el que debe mostrarse.
     * @param parentComponent Componente sobre el que se mostrar&aacute; el acuerdo de licencia. */
    LicenceDialogPanel(final Component parentComponent) {
        this.parentComponent = parentComponent;
    }

    /** Muestra el texto del acuerdo de licencia.
     * @return Devuelve <code>true</code> si se acepta el acuerdo de licencia, <code>false</code> en caso contrario. */
    boolean showDisclaimer() {

        // Texto del dialogo
        final Label textLabel = new Label(BootLoaderMessages.getString("LicenceDialogPanel.0")); //$NON-NLS-1$

        // Leemos el acuerdo de licencia desde un fichero
        String licenseText;
        InputStream licenseIs = AOBootUtil.getCleanClassLoader().getResourceAsStream("resources/licenses_" + Locale.getDefault() + ".txt"); //$NON-NLS-1$ //$NON-NLS-2$
        if (licenseIs == null) {
            licenseIs = AOBootUtil.getCleanClassLoader().getResourceAsStream("resources/licenses_" + Locale.getDefault().getLanguage() + ".txt"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        if (licenseIs == null) {
            licenseIs = AOBootUtil.getCleanClassLoader().getResourceAsStream("resources/licenses.txt"); //$NON-NLS-1$
        }

        try {
            licenseText = new String(AOBootUtil.getDataFromInputStream(licenseIs), "UTF-8"); //$NON-NLS-1$
        }
        catch (final Exception e2) {
            licenseText = BootLoaderMessages.getString("LicenceDialogPanel.2"); //$NON-NLS-1$
            Logger.getLogger("es.gob.afirma").warning("Error al acceder a las condiciones de la licencia: " + e2); //$NON-NLS-1$ //$NON-NLS-2$
        }

        try {
            licenseIs.close();
        }
        catch (final Exception e) {
            // Ignoramos los errores en el cierre
        }

        // Texto del acuerdo de licencia
        final JTextArea textArea = new JTextArea(licenseText);
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
        textArea.setEditable(false);

        // Cuadro con el scroll
        final JScrollPane scrollPane = new JScrollPane(textArea);
        scrollPane.setPreferredSize(new Dimension(250, 400));
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

        // Restricciones del layout
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;

        // Panel con el contenido del dialogo modal
        final Container licencePanel = new Container();
        licencePanel.setLayout(new GridBagLayout());

        c.gridx = 0;
        c.gridy = 0;
        c.weightx = 1.0;
        c.weighty = 1.0;
        licencePanel.add(textLabel, c);

        c.gridy = 1;
        c.weighty = 0.0;
        licencePanel.add(scrollPane, c);

        // Mostramos el dialogo e indicamos si se acepto
        return JOptionPane.OK_OPTION == JOptionPane.showConfirmDialog(this.parentComponent, licencePanel, BootLoaderMessages.getString("LicenceDialogPanel.1"), //$NON-NLS-1$
                                                                      JOptionPane.OK_CANCEL_OPTION,
                                                                      JOptionPane.PLAIN_MESSAGE);
    }

}
