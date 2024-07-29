/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.keystores.NameCertificateBean;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;

/** Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica Windows 7. */
final class CertificateSelectionPanel extends JPanel implements ListSelectionListener {

	private static final long serialVersionUID = 6288294705582545804L;

	private static final String VERDANA_FONT_NAME = "Verdana"; //$NON-NLS-1$

	private static final int TITLE_FONT_SIZE = 14;
	private static final int TEXT_FONT_SIZE = 12;

	private static final Font TITLE_FONT = new Font(VERDANA_FONT_NAME, Font.BOLD, TITLE_FONT_SIZE);
	private static final Font TEXT_FONT = new Font(VERDANA_FONT_NAME, Font.PLAIN, TEXT_FONT_SIZE);

	/** Altura de un elemento de la lista de certificados. */
	private static final int CERT_LIST_ELEMENT_HEIGHT = 86;

	/** Nombre de la preferencia que almacena el nombre de &uacute;ltima la vista de certificado seleccionada. */
	private static final String PREFERENCE_CERT_VIEW = "certView"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private JList<CertificateLine> certList;

	JScrollPane sPane;

	private JPanel certListPanel;

	private JPanel textMessagePanel;

	private int selectedIndex = -1;

	private NameCertificateBean[] certificateBeans;

	private final String dialogSubHeadline;

	private CertificateLineView certLineView;

    private static boolean highContrast;

	CertificateSelectionPanel(final NameCertificateBean[] el,
			                  final CertificateSelectionDialog selectionDialog,
			                  final String dialogHeadline,
			                  final String dialogSubHeadline,
				              final boolean showControlButons,
				              final boolean allowExternalStores,
				              final int[] availablesKeyStoreTypes) {

		this.certificateBeans = el == null ? new NameCertificateBean[0] : el.clone();
		this.dialogSubHeadline = dialogSubHeadline;

		this.certLineView = loadPreferredCertificateView();

		createUI(
			selectionDialog,
			dialogHeadline,
			showControlButons,
			allowExternalStores,
			availablesKeyStoreTypes
		);
	}

	private void createUI(final CertificateSelectionDialog selectionDialog,
			              final String dialogHeadline,
			              final boolean showControlButons,
			              final boolean allowExternalStores,
			              final int[] availablesKeyStoreTypes) {

		setLayout(new GridBagLayout());

		Color windowColor = Color.BLACK;
		setBackground(windowColor);

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(13, 15, 8, 15);
		c.weightx = 1.0;
		c.weighty = 0.0;
		c.gridx = 0;
		c.gridy = 0;

		final JLabel mainMessage = new JLabel(
			dialogHeadline != null ?
				dialogHeadline :
					CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.0") //$NON-NLS-1$
		);
		mainMessage.setFont(TITLE_FONT);

        // Establecemos la configuracion de color
        final Object highContrastProp = Toolkit.getDefaultToolkit().getDesktopProperty("win.highContrast.on"); //$NON-NLS-1$
        if (highContrastProp instanceof Boolean) {
        	CertificateSelectionPanel.highContrast = ((Boolean) highContrastProp).booleanValue();
        }

        if (!CertificateSelectionPanel.highContrast) {
        	try {
        		mainMessage.setForeground(Color.decode("0x0033BC")); //$NON-NLS-1$
        		windowColor = UIManager.getColor("window") != null ? //$NON-NLS-1$
    	    			 new Color(UIManager.getColor("window").getRGB()) : //$NON-NLS-1$
    	    			Color.WHITE;
        	}
        	catch (final Throwable e) {
        		windowColor = Color.WHITE;
    		}
            setBackground(windowColor);
        } else {
    		mainMessage.setForeground(Color.WHITE);
        }

		setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));

		this.add(mainMessage, c);

		c.insets = new Insets(13, 0, 8, 5);
		c.weightx = 0.0;
		c.gridx++;

		if (showControlButons) {

			URL refreshImgResource;

			if (!CertificateSelectionPanel.highContrast) {
				refreshImgResource = CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_autorenew_black_18dp.png"); //$NON-NLS-1$
			} else {
				refreshImgResource = CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_autorenew_white_18dp.png"); //$NON-NLS-1$
			}

			// Boton de refresco del almacen
			final JButton refresh = new JButton(
				new ImageIcon(
					refreshImgResource,
					CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
				)
			);
			refresh.setBorder(BorderFactory.createEmptyBorder());
			refresh.setRolloverEnabled(false);
			refresh.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			refresh.getAccessibleContext().setAccessibleDescription(
				CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
			);
			refresh.setToolTipText(CertificateSelectionDialogMessages.getString("UtilToolBar.1")); //$NON-NLS-1$
			refresh.addActionListener(
				new ActionListener() {
					@Override
					public void actionPerformed(final ActionEvent e) {
						UtilActions.doRefresh(selectionDialog, CertificateSelectionPanel.this);
					}
				}
			);
			refresh.addFocusListener(
					new FocusListener() {
						  @Override
						  public void focusGained(final FocusEvent e) {
							  if (isHighContrast()) {
								 refresh.setIcon(new ImageIcon(
										 CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_autorenew_black_18dp.png"), //$NON-NLS-1$
										 CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
								));
							 }
						  }
						  @Override
						  public void focusLost(final FocusEvent e) {
							  if (isHighContrast()) {
								 refresh.setIcon(new ImageIcon(
										 CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_autorenew_white_18dp.png"), //$NON-NLS-1$
										 CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
								));
							 }
						  }
					}
			);
			this.add(refresh, c);

			c.gridx++;

			// Boton de apertura de almacen externo
			if (allowExternalStores && availablesKeyStoreTypes != null && availablesKeyStoreTypes.length > 0) {

				final JPopupMenu keystoresMenu = new JPopupMenu();

				// Opcion del almacen del sistema
				if (contains(availablesKeyStoreTypes, 1)) {
					final JMenuItem menuItem = new JMenuItem(CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.35")); //$NON-NLS-1$
					menuItem.addActionListener(new ChangeKeyStoreActionListener(this, selectionDialog, 1, menuItem.getText(), null));
					keystoresMenu.add(menuItem);
				}

				// Opcion del almacen de Firefox
				if (contains(availablesKeyStoreTypes, 2)) {
					final JMenuItem menuItem = new JMenuItem(CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.36")); //$NON-NLS-1$
					menuItem.addActionListener(new ChangeKeyStoreActionListener(this, selectionDialog, 2, menuItem.getText(), null));
					keystoresMenu.add(menuItem);
				}

				// Opcion de almacen PKCS#12
				if (contains(availablesKeyStoreTypes, 3)) {
					final JMenuItem menuItem = new JMenuItem(CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.37")); //$NON-NLS-1$
					menuItem.addActionListener(new ChangeKeyStoreActionListener(this, selectionDialog, 3, menuItem.getText(), null));
					keystoresMenu.add(menuItem);
				}

				// Opcion del DNIe
				if (contains(availablesKeyStoreTypes, 4)) {
					final JMenuItem menuItem = new JMenuItem(CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.38")); //$NON-NLS-1$
					menuItem.addActionListener(new ChangeKeyStoreActionListener(this, selectionDialog, 4, menuItem.getText(), null));
					keystoresMenu.add(menuItem);
				}

				// Opciones de los almacenes en tarjeta definidos por el usuario
				final Map<String, String> userRegResult = KeyStorePreferencesManager.getUserSmartCardsRegistered();
				for (final String key : userRegResult.keySet()) {
				    final String value = userRegResult.get(key);
					final JMenuItem menuItem = new JMenuItem(key);
					menuItem.addActionListener(new ChangeKeyStoreActionListener(this, selectionDialog, 5, key, value));
					keystoresMenu.add(menuItem);
				}

				// Opciones de los almacenes en tarjeta definidos por el sistema
				final Map<String, String> systemRegResult = KeyStorePreferencesManager.getSystemSmartCardsRegistered();
				for (final String key : systemRegResult.keySet()) {
				    final String value = systemRegResult.get(key);
					final JMenuItem menuItem = new JMenuItem(key);
					menuItem.addActionListener(new ChangeKeyStoreActionListener(this, selectionDialog, 5, key, value));
					keystoresMenu.add(menuItem);
				}

				URL openInBrowserImgResource;

				if (!CertificateSelectionPanel.highContrast) {
					openInBrowserImgResource = CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_open_in_browser_black_18dp.png"); //$NON-NLS-1$
				} else {
					openInBrowserImgResource = CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_open_in_browser_white_18dp.png"); //$NON-NLS-1$
				}

				final JDropDownButton openButton = new JDropDownButton(
					new ImageIcon(
						openInBrowserImgResource,
						CertificateSelectionDialogMessages.getString("UtilToolBar.2") //$NON-NLS-1$
					)
				);
				openButton.setComponentPopupMenu(keystoresMenu);
				openButton.setRolloverEnabled(false);
				openButton.setBorder(BorderFactory.createEmptyBorder());
				openButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
				openButton.getAccessibleContext().setAccessibleDescription(
					CertificateSelectionDialogMessages.getString("UtilToolBar.2") //$NON-NLS-1$
				);
				openButton.setToolTipText(CertificateSelectionDialogMessages.getString("UtilToolBar.2")); //$NON-NLS-1$
				openButton.addFocusListener(
						new FocusListener() {
							  @Override
							  public void focusGained(final FocusEvent e) {
								  if (isHighContrast()) {
									  openButton.setIcon(new ImageIcon(
											 CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_open_in_browser_black_18dp.png"), //$NON-NLS-1$
											 CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
									));
								 }
							  }
							  @Override
							  public void focusLost(final FocusEvent e) {
								  if (isHighContrast()) {
									  openButton.setIcon(new ImageIcon(
											 CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_open_in_browser_white_18dp.png"), //$NON-NLS-1$
											 CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
									));
								 }
							  }
						}
				);
				this.add(openButton, c);
						}




			// Boton de ayuda
			c.insets = new Insets(13, 0, 8, 15);
			c.gridx++;

			URL helpImgResource;

			if (!CertificateSelectionPanel.highContrast) {
				helpImgResource = CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_help_black_18dp.png"); //$NON-NLS-1$
			} else {
				helpImgResource = CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_help_white_18dp.png"); //$NON-NLS-1$
			}

			final JButton help = new JButton(
				new ImageIcon(
					helpImgResource,
					CertificateSelectionDialogMessages.getString("UtilToolBar.3") //$NON-NLS-1$
				)
			);
			help.setBorder(BorderFactory.createEmptyBorder());
			help.setRolloverEnabled(false);
			help.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			help.getAccessibleContext().setAccessibleDescription(
				CertificateSelectionDialogMessages.getString("UtilToolBar.3") //$NON-NLS-1$
			);
			help.setToolTipText(CertificateSelectionDialogMessages.getString("UtilToolBar.3")); //$NON-NLS-1$
			help.addActionListener(
				new ActionListener() {
					@Override
					public void actionPerformed(final ActionEvent e) {
						UtilActions.doHelp();
					}
				}
			);
			help.addFocusListener(
					new FocusListener() {
						  @Override
						  public void focusGained(final FocusEvent e) {
							  if (isHighContrast()) {
								 help.setIcon(new ImageIcon(
										 CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_help_black_18dp.png"), //$NON-NLS-1$
										 CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
								));
							 }
						  }
						  @Override
						  public void focusLost(final FocusEvent e) {
							  if (isHighContrast()) {
								 help.setIcon(new ImageIcon(
										 CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_help_white_18dp.png"), //$NON-NLS-1$
										 CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
								));
							 }
						  }
					}
			);
			this.add(help, c);
		}

		// Boton para el cambio de vista de certificados
		c.gridx++;

		final JPopupMenu popupMenu = new JPopupMenu();

		final ButtonGroup certViewsGroup = new ButtonGroup();
		final JRadioButtonMenuItem menuItemPersonalView = new JRadioButtonMenuItem(
				CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.25")); //$NON-NLS-1$
		menuItemPersonalView
				.addActionListener(new ChangeViewActionListener(this, selectionDialog, CertificateLineView.PERSONAL));
		menuItemPersonalView.setSelected(this.certLineView == CertificateLineView.PERSONAL);
		certViewsGroup.add(menuItemPersonalView);
		popupMenu.add(menuItemPersonalView);

		final JRadioButtonMenuItem menuItemRepresentativeView = new JRadioButtonMenuItem(
				CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.26")); //$NON-NLS-1$
		menuItemRepresentativeView.addActionListener(
				new ChangeViewActionListener(this, selectionDialog, CertificateLineView.REPRESENTATIVE));
		menuItemRepresentativeView.setSelected(this.certLineView == CertificateLineView.REPRESENTATIVE);
		certViewsGroup.add(menuItemRepresentativeView);
		popupMenu.add(menuItemRepresentativeView);

		final JRadioButtonMenuItem menuItemPseudonymView = new JRadioButtonMenuItem(
				CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.27")); //$NON-NLS-1$
		menuItemPseudonymView
				.addActionListener(new ChangeViewActionListener(this, selectionDialog, CertificateLineView.PSEUDONYM));
		menuItemPseudonymView.setSelected(this.certLineView == CertificateLineView.PSEUDONYM);
		certViewsGroup.add(menuItemPseudonymView);
		popupMenu.add(menuItemPseudonymView);

		URL viewArrowImgResource;

		if (!CertificateSelectionPanel.highContrast) {
			viewArrowImgResource = CertificateSelectionPanel.class
					.getResource("/resources/toolbar/ic_view_arrow_black.png"); //$NON-NLS-1$
		} else {
			viewArrowImgResource = CertificateSelectionPanel.class
					.getResource("/resources/toolbar/ic_view_arrow_white.png"); //$NON-NLS-1$
		}

		final JDropDownButton dropDownButton = new JDropDownButton(
				new ImageIcon(viewArrowImgResource, CertificateSelectionDialogMessages.getString("UtilToolBar.4") //$NON-NLS-1$
				));
		dropDownButton.setComponentPopupMenu(popupMenu);
		dropDownButton.setRolloverEnabled(false);
		dropDownButton.setBorder(BorderFactory.createEmptyBorder());
		dropDownButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		dropDownButton.getAccessibleContext()
				.setAccessibleDescription(CertificateSelectionDialogMessages.getString("UtilToolBar.4") //$NON-NLS-1$
				);
		dropDownButton.setToolTipText(CertificateSelectionDialogMessages.getString("UtilToolBar.4")); //$NON-NLS-1$
		dropDownButton.addFocusListener(new FocusListener() {
			@Override
			public void focusGained(final FocusEvent e) {
				if (isHighContrast()) {
					dropDownButton.setIcon(new ImageIcon(
							CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_view_arrow_black.png"), //$NON-NLS-1$
							CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
					));
				}
			}

			@Override
			public void focusLost(final FocusEvent e) {
				if (isHighContrast()) {
					dropDownButton.setIcon(new ImageIcon(
							CertificateSelectionPanel.class.getResource("/resources/toolbar/ic_view_arrow_white.png"), //$NON-NLS-1$
							CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
					));
				}
			}
		});
		this.add(dropDownButton, c);

		c.gridwidth = 5;
		c.insets = new Insets(0, 15, 4, 15);
		c.gridx = 0;
		c.gridy++;

		this.textMessagePanel = new JPanel();
		this.textMessagePanel.setLayout(new GridBagLayout());
		this.textMessagePanel.setOpaque(false);
		this.textMessagePanel.setBorder(null);
		this.add(this.textMessagePanel, c);

		c.insets = new Insets(4, 15, 8, 15);
		c.gridy++;

		this.add(new JSeparator(), c);

		c.insets = new Insets(8, 18, 13, 18);
		c.weighty = 1.0;
		c.gridy++;

		this.certListPanel = new JPanel();
		this.certListPanel.setLayout(new GridBagLayout());
		this.certListPanel.setBorder(null);

		this.certList = new JList<>();
		this.certList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		this.certList.setCellRenderer(new CertListCellRendered(windowColor, CertificateSelectionPanel.highContrast));

		updateCertListInfo(this.certificateBeans);

		this.certList.addListSelectionListener(this);
		final CertLinkMouseListener mouseListener = new CertLinkMouseListener();
		this.certList.addMouseMotionListener(mouseListener);
		this.certList.addMouseListener(mouseListener);

		this.add(this.certListPanel, c);
	}

	/** Indica si un valor entero se encuentra dentro de un array.
	 * @param elements Array con los valores entre los que buscar.
	 * @param value Valor a buscar.
	 * @return {@code true} si se encuentra el valor, {@code false}
	 * en caso contrario.
	 */
	private static boolean contains(final int[] elements, final int value) {
		for (final int e : elements) {
			if (e == value) {
				return true;
			}
		}
		return false;
	}

	void setCertLineView(final CertificateLineView certLineView) {
		this.certLineView = certLineView;
	}

	/** Recarga el di&aacute;logo para mostrar un grupo distinto de certificados.
	 * @param certs Conjunto de datos de los certificados a mostrar. */
	void refresh(final NameCertificateBean[] certs) {

		this.certificateBeans = certs.clone();

		// Devolvemos el scroll al inicio antes de refrescar la lista
		if (this.sPane.getVerticalScrollBar() != null) {
			this.sPane.getVerticalScrollBar().setValue(0);
		}

		// Actualizamos el listado
		updateCertListInfo(certs);

		// Seleccionamos el primer elemento
		if (certs.length > 0) {
			this.certList.setSelectedIndex(0);
		}
	}

	private static List<CertificateLine> createCertLines(
			final NameCertificateBean[] certBeans,
			final CertificateLineView view) {

		final CertificateLineFactory certLineFactory = CertificateLineFactory.newInstance(view);
		final List<CertificateLine> certLines = new ArrayList<>();
		for (final NameCertificateBean nameCert : certBeans) {
			CertificateLine certLine;
		    try {
		    	certLine = certLineFactory.buildCertificateLine(nameCert.getName(), nameCert.getCertificate(), isHighContrast());
		    }
		    catch(final Exception e) {
		        continue;
		    }
			certLine.setPreferredSize(new Dimension(0, CERT_LIST_ELEMENT_HEIGHT));
			certLines.add(certLine);
		}
		return certLines;
	}

	/**
	 * Reconstruye el di&aacute;logo para recargar los datos de los certificados ya cargados.
	 * S&oacute;lo ser&aacute; perceptible visualmente si con anterioridad se ha cambiado la
	 * vista con la que deben mostrarse los certificados.
	 */
	void updateCertListInfo() {
		updateCertListInfo(this.certificateBeans);

		// Mostramos y seleccionamos el primer elemento
		if (this.certificateBeans.length > 0) {
			this.certList.setSelectedIndex(0);
			this.sPane.getVerticalScrollBar().setValue(0);
		}
	}

	/**
	 * Reconstruye el di&aacute;logo para mostrar los datos de los certificados indicados.
	 * Actualiza la dimensi&oacute;n del di&aacute;logo, su texto y el listado de certificados.
	 * @param certs Certificados que se deben mostrar.
	 */
	void updateCertListInfo(final NameCertificateBean[] certs) {

		final List<CertificateLine> certLines = createCertLines(certs, this.certLineView);

		// Actualizamos el mensaje del dialogo en base al numero de certificados
		// Mostramos un texto de cabecera si corresponde
		this.textMessagePanel.removeAll();
		if (certLines.size() <= 1) {
			String msg;
			if (certLines.size() == 1) {
				msg = this.dialogSubHeadline != null ?
						this.dialogSubHeadline :
							CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.1"); //$NON-NLS-1$
			}
			else {
				msg = CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.8"); //$NON-NLS-1$
			}

			final JTextPane textMessage = new JTextPane();
			textMessage.setOpaque(false);
			textMessage.setText(msg);
			textMessage.setFont(TEXT_FONT);
			textMessage.setBorder(null);
			textMessage.setPreferredSize(new Dimension(370, 40));

			final GridBagConstraints tmC = new GridBagConstraints();
			tmC.fill = GridBagConstraints.BOTH;
			tmC.weightx = 1.0;
			tmC.weighty = 1.0;

			this.textMessagePanel.add(textMessage, tmC);
		}

		// Actualizamos el listado de certificados
		this.certList.setListData(certLines.toArray(new CertificateLine[certLines.size()]));
		this.certList.setVisibleRowCount(Math.max(Math.min(4, certLines.size()), 1));

		if (certLines.size() > 0) {
			this.certList.setSelectedIndex(0);
			this.selectedIndex = 0;
		}
		else {
			this.selectedIndex = -1;
		}

		// Actualizamos las dimensiones del dialogo
		final JScrollPane jScrollPane = new JScrollPane(
				this.certList,
				ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER
			);
		jScrollPane.setBorder(null);

		final int panelHeight = CERT_LIST_ELEMENT_HEIGHT * this.certList.getVisibleRowCount() + 3;
		jScrollPane.setPreferredSize(new Dimension(500, panelHeight));

		final GridBagConstraints ic = new GridBagConstraints();
		ic.fill = GridBagConstraints.BOTH;
		ic.weightx = 1.0;
		ic.weighty = 1.0;

		if (this.sPane != null) {
			this.certListPanel.remove(this.sPane);
		}
		this.sPane = jScrollPane;
		this.certListPanel.add(this.sPane, ic);
	}

	/** Selecciona la lista de certificados. */
    void selectCertificateList() {
		this.certList.requestFocusInWindow();
	}

	/** Recupera el alias del certificado seleccionado.
	 * @return Alias del certificado seleccionado o {@code null} si no se seleccion&oacute; ninguno. */
	String getSelectedCertificateAlias() {
		return this.selectedIndex == -1 ? null : this.certificateBeans[this.selectedIndex].getAlias();
	}

	/**
	 * Obtiene el n&uacute;mero de certificados que se muestran al usuario.
	 * @return N&uacute;mero de certificados mostrados.
	 */
	int getShowedCertsCount() {
		return this.certificateBeans == null ? 0 : this.certificateBeans.length;
	}

	/**
	 * Obtiene cual es la vista de certificado que se est&aacute; aplicando en el di&aacute;logo.
	 * @return Vista de certificado.
	 */
	public CertificateLineView getCertLineView() {
		return this.certLineView;
	}

	NameCertificateBean[] getCertificateBeans() {
		return this.certificateBeans;
	}

	/**
	 * Indica si se est&aacute; usando el modo de alto contraste.
	 * @return Devuelve true si est&aacute; activado el alto contraste.
	 */
	public static boolean isHighContrast() {
		return highContrast;
	}

	/** Agrega un gestor de eventos de rat&oacute;n a la lista de certificados para poder
	 * gestionar a trav&eacute;s de &eacute;l eventos especiales sobre la lista.
	 * @param listener Manejador de eventos de rat&oacute;n. */
	void addCertificateListMouseListener(final MouseListener listener) {
		this.certList.addMouseListener(listener);
	}

	/** {@inheritDoc} */
	@Override
	public void valueChanged(final ListSelectionEvent e) {
		this.selectedIndex = this.certList.getSelectedIndex();
	}

	/** Renderer para mostrar la informaci&oacute;n de un certificado. */
	private static final class CertListCellRendered implements ListCellRenderer<CertificateLine> {

		private final Color bgColor;
		private final boolean highContrast;

		CertListCellRendered(final Color bgColor, final boolean highContrast) {
			this.bgColor = bgColor;
			this.highContrast = highContrast;
		}

		/** {@inheritDoc} */
		@Override
		public Component getListCellRendererComponent(final JList<? extends CertificateLine> list, final CertificateLine value,
				final int index, final boolean isSelected, final boolean cellHasFocus) {

			final CertificateLine line = value;
			if (isSelected) {
				line.setBackground(Color.decode("0xD9EAFF")); //$NON-NLS-1$
				if (isHighContrast()) {
					if (line instanceof DefaultCertificateLine) {
						((DefaultCertificateLine)line).getAlias().setForeground(Color.BLACK);
						((DefaultCertificateLine)line).getIssuer().setForeground(Color.BLACK);
						((DefaultCertificateLine)line).getDates().setForeground(Color.BLACK);
						((DefaultCertificateLine)line).getPropertiesLink().setForeground(Color.BLUE);
					} else if (line instanceof PseudonymCertificateLine) {
						((PseudonymCertificateLine)line).getPositionLabel().setForeground(Color.BLACK);
						((PseudonymCertificateLine)line).getOrganizationLabel().setForeground(Color.BLACK);
						((PseudonymCertificateLine)line).getDates().setForeground(Color.BLACK);
						((PseudonymCertificateLine)line).getPropertiesLink().setForeground(Color.BLUE);
					} else if (line instanceof RepresentativeCertificateLine) {
						((RepresentativeCertificateLine)line).getOrganizationLabel().setForeground(Color.BLACK);
						((RepresentativeCertificateLine)line).getAgentLabel().setForeground(Color.BLACK);
						((RepresentativeCertificateLine)line).getDates().setForeground(Color.BLACK);
						((RepresentativeCertificateLine)line).getPropertiesLink().setForeground(Color.BLUE);
					}
				}
				line.setBorder(BorderFactory.createCompoundBorder(
						BorderFactory.createLineBorder(Color.WHITE, 1),
						BorderFactory.createLineBorder(Color.decode("0x84ACDD"), 1))); //$NON-NLS-1$
			} else {
				line.setBackground(this.bgColor);
				if (isHighContrast()) {
					if (line instanceof DefaultCertificateLine) {
						((DefaultCertificateLine)line).getAlias().setForeground(Color.WHITE);
						((DefaultCertificateLine)line).getIssuer().setForeground(Color.WHITE);
						((DefaultCertificateLine)line).getDates().setForeground(Color.WHITE);
						((DefaultCertificateLine)line).getPropertiesLink().setForeground(Color.YELLOW);
					} else if (line instanceof PseudonymCertificateLine) {
						((PseudonymCertificateLine)line).getPositionLabel().setForeground(Color.WHITE);
						((PseudonymCertificateLine)line).getOrganizationLabel().setForeground(Color.WHITE);
						((PseudonymCertificateLine)line).getDates().setForeground(Color.WHITE);
						((PseudonymCertificateLine)line).getPropertiesLink().setForeground(Color.YELLOW);
					} else if (line instanceof RepresentativeCertificateLine) {
						((RepresentativeCertificateLine)line).getOrganizationLabel().setForeground(Color.WHITE);
						((RepresentativeCertificateLine)line).getAgentLabel().setForeground(Color.WHITE);
						((RepresentativeCertificateLine)line).getDates().setForeground(Color.WHITE);
						((RepresentativeCertificateLine)line).getPropertiesLink().setForeground(Color.YELLOW);
					}
				}
				line.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 4));
			}

			return line;
		}

	}

	/** Manejador de eventos de raton para la lista de certificados. */
	private final class CertLinkMouseListener extends MouseAdapter {

		private boolean entered = false;

		CertLinkMouseListener() {
			// Vacio
		}

		/** {@inheritDoc} */
		@Override
		public void mouseClicked(final MouseEvent me) {
			final JList<?> tmpList = (JList<?>) me.getSource();
			final CertificateLine tmpLine = (CertificateLine) tmpList.getSelectedValue();
			if (tmpLine != null && tmpLine.getCertificateLinkBounds() != null &&
				me.getClickCount() == 1 &&
						me.getY() < CERT_LIST_ELEMENT_HEIGHT * tmpList.getModel().getSize() &&
					tmpLine.getCertificateLinkBounds().contains(me.getX(), me.getY() % CERT_LIST_ELEMENT_HEIGHT)) {
						try {
							CertificateUtils.openCert(
									CertificateSelectionPanel.this,
									tmpLine.getCertificate());
						}
						catch (final AOCancelledOperationException e) {
							/* No hacemos nada */
						}
			}
		}

		/** {@inheritDoc} */
		@Override
		public void mouseMoved(final MouseEvent me) {
			final JList<?> tmpList = (JList<?>) me.getSource();
			final CertificateLine tmpLine = (CertificateLine) tmpList.getSelectedValue();
			if (tmpLine != null) {
				if (me.getY() < CERT_LIST_ELEMENT_HEIGHT * tmpList.getModel().getSize() &&
						tmpLine.getCertificateLinkBounds().contains(me.getX(), me.getY() % CERT_LIST_ELEMENT_HEIGHT)) {
					if (!this.entered) {
						tmpList.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
						this.entered = true;
					}
				}
				else if (this.entered) {
					tmpList.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					this.entered = false;
				}
			}
		}

	}

	/**
	 * Carga de las preferencias del usuario cu&aacute;l ha sido la &uacute;ltima vista de
	 * certificados seleccionada.
	 * @return Vista de certificados.
	 */
	private static CertificateLineView loadPreferredCertificateView() {
		CertificateLineView view = null;
		try {
			final Preferences pref = Preferences.userNodeForPackage(CertificateSelectionPanel.class);
			final String certView = pref.get(PREFERENCE_CERT_VIEW, null);
			if (certView != null) {
				view = CertificateLineView.valueOf(certView);
			}
		}
		catch (final Exception e) {
			view = null;
			LOGGER.log(Level.WARNING,
					"No se pudo cargar la vista de certificado de las preferencias del usuario. Se usara la por defecto", e); //$NON-NLS-1$
		}

		// Si no se pudo cargar cual es la vista configurada, se establece la por defecto
		if (view == null) {
			view = CertificateLineView.PERSONAL;
		}
		return view;
	}

	/**
	 * Guarda en las preferencias del usuario la &uacute;ltima vista de certificados seleccionada.
	 */
	void savePreferredCertificateView() {
		try {
			final Preferences pref = Preferences.userNodeForPackage(CertificateSelectionPanel.class);
			if (this.certLineView != null) {
				pref.put(PREFERENCE_CERT_VIEW, this.certLineView.name());
			}
			else {
				pref.remove(PREFERENCE_CERT_VIEW);
			}
			pref.sync();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo guardar la vista utilizada en las preferencias del usuario", e); //$NON-NLS-1$
		}
	}

	private static class ChangeViewActionListener implements ActionListener {

		private final CertificateSelectionPanel panel;
		private final CertificateSelectionDialog dialog;
		private final CertificateLineView view;

		public ChangeViewActionListener(final CertificateSelectionPanel panel, final CertificateSelectionDialog dialog, final CertificateLineView view) {
			this.panel = panel;
			this.dialog = dialog;
			this.view = view;
		}

		@Override
		public void actionPerformed(final ActionEvent e) {

			UtilActions.doChangeView(
					this.panel.getCertificateBeans(),
					this.view,
					this.dialog,
					this.panel);
		}
	}

	private static class ChangeKeyStoreActionListener implements ActionListener {

		private final CertificateSelectionPanel panel;
		private final CertificateSelectionDialog dialog;
		private final int keyStoreType;
		private final String keyStoreName;
		private final String ksLibPath;

		public ChangeKeyStoreActionListener(final CertificateSelectionPanel panel, final CertificateSelectionDialog dialog, final int keyStoreType) {
			this.panel = panel;
			this.dialog = dialog;
			this.keyStoreType = keyStoreType;
			this.keyStoreName = null;
			this.ksLibPath = null;
		}

		public ChangeKeyStoreActionListener(final CertificateSelectionPanel panel, final CertificateSelectionDialog dialog,
											final int keyStoreType, final String ksName, final String ksLibPath) {
			this.panel = panel;
			this.dialog = dialog;
			this.keyStoreType = keyStoreType;
			this.keyStoreName = ksName;
			this.ksLibPath = ksLibPath;
		}

		@Override
		public void actionPerformed(final ActionEvent e) {
			UtilActions.doChangeKeyStore(this.keyStoreType, this.dialog, this.panel, this.keyStoreName, this.ksLibPath);
		}
	}
}
