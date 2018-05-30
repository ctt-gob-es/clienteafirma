/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.plugins;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;

/**
 * Panel gr&aacute;fico con las opciones de gesti&oacute;n de plugins.
 */
public final class PluginsManagementPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 8384222173067817402L;

	private final Window window;

	private JList<AfirmaPlugin> pluginsList;
	private JButton addButton;
	private JButton removeButton;

	private JLabel pluginInfoPane;
	private JButton configButton;

	private JButton closeButton;

	final PluginsManagementHandler eventsHandler;

	/** Constructor con la ventana padre.
	 * @param w Ventana sobre la que mostrar la clase.
	 */
	PluginsManagementPanel(final Window w) {
		this.window = w;
		this.eventsHandler = new PluginsManagementHandler(this);
		createUI();
		this.eventsHandler.registerComponents();

        new Thread(new Runnable() {

			@Override
			public void run() {
				PluginsManagementPanel.this.eventsHandler.loadViewData();
			}
		}).start();
	}

	/**
	 * Dibuja la ventana con las opciones de restauraci&oacute;n
	 */
	private void createUI() {

		setLayout(new GridBagLayout());

        // Creamos un panel con el listado con los plugins instalados
		final JPanel pluginsListPanel = createPluginsListPanel();
		pluginsListPanel.setMinimumSize(new Dimension(250, 500));
		pluginsListPanel.setPreferredSize(new Dimension(250, 500));

		// Creamos un panel en el que mostrar la informacion de los plugins
		final JPanel pluginInfoPanel = createPluginInfoPanel();

        // Creamos un panel para el boton de restauracion
        final JPanel buttonsPanel = createButtonsPanel();

        // Mostramos los paneles
        final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.VERTICAL;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(11, 11,  0,  0);
		add(pluginsListPanel, c);

		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridx++;
		c.insets = new Insets(11, 11,  0,  11);
		add(pluginInfoPanel, c);

        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.weighty = 0;
        c.gridwidth = 2;
        c.gridx = 0;
        c.gridy++;
		c.ipady = 11;
		c.insets = new Insets(0, 0, 0, 0);
        add(buttonsPanel, c);
	}

	/** Construye el panel en donde se muestra el listado de plugins cargados y sus
	 * opciones de configuracion.
	 * @return Panel con el listado de plugins. */
	private JPanel createPluginsListPanel() {
		final JPanel pluginListPanel = new JPanel(new GridBagLayout());
		pluginListPanel.setBorder(BorderFactory.createTitledBorder("Plugins instalados"));

		final DefaultListModel<AfirmaPlugin> listModel = new DefaultListModel<>();
		this.pluginsList = new JList<>(listModel);
		final JScrollPane pluginsListScrollPane = new JScrollPane(this.pluginsList);

		if (Platform.getOS() == Platform.OS.MACOSX) {
			pluginsListScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
			pluginsListScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		}
		else {
			pluginsListScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		}

		this.addButton = new JButton("Agregar");
		this.removeButton = new JButton("Eliminar");

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridwidth = 2;
		c.gridx = 0;
		c.gridy = 0;
		pluginListPanel.add(pluginsListScrollPane, c);
		c.weightx = 0.5;
		c.weighty = 0;
		c.gridwidth = 1;
		c.gridx = 0;
		c.gridy++;
		c.insets = new Insets(6, 0, 0, 0);
		pluginListPanel.add(this.addButton, c);
		c.gridx++;
		c.insets = new Insets(6, 6, 0, 0);
		pluginListPanel.add(this.removeButton, c);

		return pluginListPanel;
	}

	/** Construye el objeto gr&aacute;fico que representa el panel
	 * donde se ubican los botones de la ventana de restauraci&oacute;n.
	 * @return Panel donde se ubican los botones de la ventana de restauraci&oacute;n. */
	private JPanel createPluginInfoPanel() {
		final JPanel infoPanel = new JPanel(new GridBagLayout());
		infoPanel.setBorder(BorderFactory.createTitledBorder("Informaci\u00F3n del plugin"));

		// Panel con la informacion del plugin
		this.pluginInfoPane = new JLabel();
		this.pluginInfoPane.setFocusable(false);

		// Panel con el boton para la configuracion del plugin. El boton perman
		final JPanel configPanel = new JPanel(new FlowLayout(FlowLayout.TRAILING));
		this.configButton = new JButton("Configurar");
		this.configButton.setVisible(false);
		configPanel.add(this.configButton);

		// Agregamos los paneles
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridy = 0;
		infoPanel.add(this.pluginInfoPane, c);
		c.weighty = 1.0;
		c.gridy++;
		infoPanel.add(new JPanel(), c); // Panel vacio para rellenar la vertical
		c.weightx = 0;
		c.weighty = 0;
		c.gridy++;
		infoPanel.add(configPanel, c);

		return infoPanel;
	}

	/** Construye el objeto gr&aacute;fico que representa el panel
	 * donde se ubican los botones de la ventana de restauraci&oacute;n.
	 * @return Panel donde se ubican los botones de la ventana de restauraci&oacute;n. */
	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.TRAILING));

		this.closeButton = new JButton(SimpleAfirmaMessages.getString("RestoreConfigPanel.5")); //$NON-NLS-1$
		this.closeButton.setMnemonic('C');
		this.closeButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("RestoreConfigPanel.6") //$NON-NLS-1$
		);
		panel.add(this.closeButton);

		return panel;
	}

	/** Devuelve la ventana desde donde se abri&oacute; la ventana actual.
	 * @return Ventana desde donde se abri&oacute; la ventana actual. */
	Window getParentWindow() {
		return this.window;
	}

	/**
	 * Devuelve el listado en el que se visualizan los plugins instalados.
	 * @return Listado con los plugins.
	 */
	JList<AfirmaPlugin> getPluginsList() {
		return this.pluginsList;
	}

	/**
	 * Devuelve el bot&oacute;n para la instalaci&oacute;n de un nuevo plugin.
	 * @return Bot&oacute;n de instalaci&oacute;n.
	 */
	JButton getAddButton() {
		return this.addButton;
	}

	/**
	 * Devuelve el bot&oacute;n para la desinstalaci&oacute;n de un plugin.
	 * @return Bot&oacute;n de desinstalaci&oacute;n.
	 */
	JButton getRemoveButton() {
		return this.removeButton;
	}

	/**
	 * Devuelve el componente de texto en el que se debe mostrar la informaci&oacute;n
	 * del plugin seleccionado.
	 * @return Panel de texto.
	 */
	JLabel getPluginInfoPane() {
		return this.pluginInfoPane;
	}

	/**
	 * Devuelve el bot&oacute;n para la configutaci&oacute;n de un plugin.
	 * @return Bot&oacute;n de configuraci&oacute;n.
	 */
	JButton getConfigButton() {
		return this.configButton;
	}

	/**
	 * Devuelve el bot&oacute;n de cierre del di&aacute;logo.
	 * @return Bot&oacute;n de cierre.
	 */
	JButton getCloseButton() {
		return this.closeButton;
	}
}
