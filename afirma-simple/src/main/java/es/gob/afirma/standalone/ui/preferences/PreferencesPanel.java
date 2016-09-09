/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class PreferencesPanel extends JPanel implements KeyListener, DisposableInterface {

	private static final long serialVersionUID = -3168095095548385291L;


	private final JButton applyButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.0")); //$NON-NLS-1$

	private final ModificationListener modificationListener;

    private final Window window;
    Window getParentWindow() {
        return this.window;
    }

	private PreferencesPanelGeneral preferencesPanelGeneral;
	private PreferencesPanelCades preferencesPanelCades;
	private PreferencesPanelPades preferencesPanelPades;
	private PreferencesPanelKeyStores preferencesPanelKeyStores;
	private PreferencesPanelFacturaE preferencesPanelFacturaE;
	private PreferencesPanelXades preferencesPanelXades;

	private final JTabbedPane tabbedPane = new JTabbedPane();

	void createUI(final int selectedTabIndex) {


		this.preferencesPanelGeneral = new PreferencesPanelGeneral(this, this.modificationListener, this, this);
		this.preferencesPanelCades = new PreferencesPanelCades(this, this.modificationListener);
		this.preferencesPanelPades = new PreferencesPanelPades(this, this.modificationListener);
		this.preferencesPanelKeyStores = new PreferencesPanelKeyStores(this, this.modificationListener);
		this.preferencesPanelFacturaE = new PreferencesPanelFacturaE(this, this.modificationListener);
		this.preferencesPanelXades = new PreferencesPanelXades(this, this.modificationListener);

		this.tabbedPane.addKeyListener(this);

		int count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.2"), //$NON-NLS-1$
			null,
			this.preferencesPanelGeneral,
			SimpleAfirmaMessages.getString("PreferencesPanel.40") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_G);

		count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.3"), //$NON-NLS-1$
			null,
			this.preferencesPanelPades,
			SimpleAfirmaMessages.getString("PreferencesPanel.41") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_F);

		count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.4"), //$NON-NLS-1$
			null,
			this.preferencesPanelCades,
			SimpleAfirmaMessages.getString("PreferencesPanel.42") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_E);

		count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.5"), //$NON-NLS-1$
			null,
			this.preferencesPanelXades,
			SimpleAfirmaMessages.getString("PreferencesPanel.43") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_X);

		count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.100"), //$NON-NLS-1$
			null,
			this.preferencesPanelFacturaE,
			SimpleAfirmaMessages.getString("PreferencesPanel.101") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_L);

		//TODO: Descomentar una vez se entregue
//		count = this.tabbedPane.getTabCount();
//		this.tabbedPane.addTab(
//			SimpleAfirmaMessages.getString("PreferencesPanel.84"), //$NON-NLS-1$
//			null,
//			this.preferencesPanelKeyStores,
//			SimpleAfirmaMessages.getString("PreferencesPanel.85") //$NON-NLS-1$
//		);
//		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_V);

		this.tabbedPane.setSelectedIndex(selectedTabIndex);

		setLayout(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridy = 0;

		add(this.tabbedPane, c);
		c.gridy++;
		c.weighty = 0.0;
		c.ipady = 11;
		add(createButtonsPanel(), c);
	}

    boolean savePreferences() {

		if (!checkPreferences()) {
			return false;
		}

		//****************************************************************************
		//**** PREFERENCIAS ALMACENES ************************************************
		//****************************************************************************
		this.preferencesPanelKeyStores.savePreferences();

		//****************************************************************************
		//**** PREFERENCIAS FACTURAE ************************************************
		//****************************************************************************
		this.preferencesPanelFacturaE.savePreferences();

		//****************************************************************************
		//**** PREFERENCIAS GENERALES ************************************************
		//****************************************************************************
		this.preferencesPanelGeneral.savePreferences();

		//****************************************************************************
		//**** PREFERENCIAS CADES ****************************************************
		//****************************************************************************
		this.preferencesPanelCades.savePreferences();

		//****************************************************************************
		//**** PREFERENCIAS PADES ****************************************************
		//****************************************************************************
		this.preferencesPanelPades.savePreferences();

		//****************************************************************************
		//**** PREFERENCIAS XADES ****************************************************
		//****************************************************************************
		this.preferencesPanelXades.savePreferences();

		try {
			PreferencesManager.flush();
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al guardar las preferencias de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

	    return true;

	}

    void loadPreferences() {
    	//****************************************************************************
    			//**** PREFERENCIAS ALMACENES ************************************************
    			//****************************************************************************
    			this.preferencesPanelKeyStores.loadPreferences();

    			//****************************************************************************
    			//**** PREFERENCIAS FACTURAE ************************************************
    			//****************************************************************************
    			this.preferencesPanelFacturaE.loadPreferences();

    			//****************************************************************************
    			//**** PREFERENCIAS GENERALES ************************************************
    			//****************************************************************************
    			this.preferencesPanelGeneral.loadPreferences();

    			//****************************************************************************
    			//**** PREFERENCIAS CADES ****************************************************
    			//****************************************************************************
    			this.preferencesPanelCades.loadPreferences();

    			//****************************************************************************
    			//**** PREFERENCIAS PADES ****************************************************
    			//****************************************************************************
    			this.preferencesPanelPades.loadPreferences();

    			//****************************************************************************
    			//**** PREFERENCIAS XADES ****************************************************
    			//****************************************************************************
    			this.preferencesPanelXades.loadPreferences();
    }

	/** Comprueba que los datos configurados sean v&aacute;lidos.
	 * @return {@code true} cuando los datos son v&aacute;lidos, {@code false} en caso
	 * contrario. */
	private boolean checkPreferences() {
		try {
			this.preferencesPanelXades.checkPreferences();
		}
		catch(final Exception e) {
			AOUIFactory.showErrorMessage(
				this,
				"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.6") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			this.tabbedPane.setSelectedIndex(3);
			return false;
		}

		try {
			this.preferencesPanelPades.checkPreferences();
		}
		catch(final Exception e) {
			AOUIFactory.showErrorMessage(
				this,
				"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.7") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			this.tabbedPane.setSelectedIndex(1);
			return false;
		}

		try {
			this.preferencesPanelCades.checkPreferences();
		}
		catch(final Exception e) {
			AOUIFactory.showErrorMessage(
				this,
				"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.38") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			this.tabbedPane.setSelectedIndex(2);
			return false;
		}

		return true;
	}

	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.31")); //$NON-NLS-1$
		cancelButton.setMnemonic('C');
		cancelButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.32") //$NON-NLS-1$
		);
		cancelButton.addKeyListener(this);
		cancelButton.addActionListener(
			new ActionListener() {
			    /** {@inheritDoc} */
	            @Override
	            public void actionPerformed(final ActionEvent ae) {
	            	disposeInterface();
	            }
	        }
		);

		final JButton acceptButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.33")); //$NON-NLS-1$
		acceptButton.setMnemonic('A');
		acceptButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.34") //$NON-NLS-1$
		);
		acceptButton.addKeyListener(this);
		acceptButton.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (savePreferences()) {
					disposeInterface();
				}
			}
		});

		this.applyButton.setMnemonic('p');
		this.applyButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.35") //$NON-NLS-1$
		);
		this.applyButton.addKeyListener(this);
		this.applyButton.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (savePreferences()) {
				    setModified(false);
				}
			}
		});
		this.applyButton.setEnabled(false);

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(cancelButton);
			panel.add(this.applyButton);
			panel.add(acceptButton);
		}
		else {
			panel.add(this.applyButton);
			panel.add(acceptButton);
			panel.add(cancelButton);
		}
		return panel;
	}

	PreferencesPanel(final Window w) {
		this(w, 0);
	}

	PreferencesPanel(final Window w, final int selectedTabIndex) {

	    if (w==null) {
	        throw new IllegalArgumentException(
        		"Es necesario proporcionar una referencia a la ventana contenedora" //$NON-NLS-1$
    		);
	    }
	    this.window = w;
	    this.modificationListener = new ModificationListener(this);
	    createUI(selectedTabIndex);
	}

	/** Indica si se ha modificado alg&uacute;n dato desde el &uacute;ltimo guardado,
	 * y por lo tanto si hay algo nuevo para guardar.
	 * @param mod <code>true</code> si se ha modificado alg&uacute;n dato, <code>false</code>
	 *            en caso contrario. */
	void setModified(final boolean mod) {
	    this.applyButton.setEnabled(mod);
	}

	/** {@inheritDoc} */
	@Override
	public void keyPressed(final KeyEvent e) { /* Vacio */ }

	/** {@inheritDoc} */
	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE && !Platform.OS.MACOSX.equals(Platform.getOS())) {
			disposeInterface();
		}
	}

	/** {@inheritDoc} */
	@Override
	public void keyTyped(final KeyEvent e) { /* Vacio */ }

	/** Par de cadenas para su uso en ComboBox. Una cadena es el valor del elemento seleccionado y
	 * la otra el texto que se debe mostrar. */
	static final class ValueTextPair {

		private final String value;
		private final String text;

		public ValueTextPair(final String valueText) {
			this.value = valueText;
			this.text = valueText;
		}

		public ValueTextPair(final String value, final String text) {
			this.value = value;
			this.text = text;
		}

		public String getValue() {
			return this.value;
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == null) {
				return false;
			}
			if (obj instanceof ValueTextPair) {
				return this.value.equals(((ValueTextPair) obj).value);
			}
			return this.value.equals(obj.toString());
		}

		@Override
		public String toString() {
			return this.text;
		}

		@Override
		public int hashCode() {
			// Funciona aleatoria para calcular el hashcode
			return 5 * this.text.length() + 7 * this.value.length();
		}
	}

	@Override
	public void disposeInterface() {
		PreferencesPanel.this.getParentWindow().dispose();
	}
}