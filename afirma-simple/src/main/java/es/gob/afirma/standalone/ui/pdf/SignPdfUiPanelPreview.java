/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.AWTKeyStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.KeyboardFocusManager;
import java.awt.RenderingHints;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Base64;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.ListCellRenderer;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.basic.BasicComboBoxRenderer;
import javax.swing.text.DefaultFormatter;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signers.pades.PdfExtraParams;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.EditorFocusManager;
import es.gob.afirma.standalone.ui.pdf.SignPdfUiPanel.SignPdfUiPanelListener;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

final class SignPdfUiPanelPreview extends JPanel implements KeyListener {

	private static final long serialVersionUID = 1848879900511003335L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int PREFERRED_WIDTH = 470;
	private static final int PREFERRED_HEIGHT = 620;
	private static final int VIEWLABEL_PREFERRED_WIDTH = 475;
	private static final int VIEWLABEL_PREFERRED_HEIGHT = 140;
	private static final int MAX_TEXT_SIZE = 50;
	private static final int MIN_TEXT_SIZE = 1;
	private static final int STEP_TEXT_SIZE = 1;
	private static final int INIT_TEXT_SIZE = 12;
	private static final char NEWLINE = '\n';
	private static final String SPACE_SEPARATOR = " "; //$NON-NLS-1$
	private static final String SPLIT_REGEXP= "\\s+"; //$NON-NLS-1$
	static final String IMAGE_EXT[] = {"jpg", "jpeg", "png", "gif"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

	private JDialog dialogParent;

	private float scale = 1;
	private BufferedImage image;
	private BufferedImage signImage;
	BufferedImage getSignImage() {
		return this.signImage;
	}

	private final SignPdfUiPanelListener listener;
	SignPdfUiPanelListener getListener() {
		return this.listener;
	}

	private DropTarget dropTarget;

	private final Properties prop;
	Properties getProp() {
		return this.prop;
	}

	JLabel viewLabel;
	JLabel getViewLabel() {
		return this.viewLabel;
	}

	JTextField signatureImagePath;

	private final JButton browseImageButton = new JButton(SignPdfUiMessages.getString("SignPdfUiPreview.38")); //$NON-NLS-1$
	JButton getBrowseImageButton() {
		return this.browseImageButton;
	}

	final JButton clearImageButton = new JButton(SignPdfUiMessages.getString("SignPdfUiPreview.40")); //$NON-NLS-1$
	JButton getClearImageButton() {
		return this.clearImageButton;
	}

	private Font font;
	Font getViewFont() {
		return this.font;
	}

	void setViewFont(final Font f) {
		this.font = f;
	}

	private int style;
	int getStyle() {
		return this.style;
	}
	void setStyle(final int style) {
		this.style = style;
	}

	private final JToggleButton boldButton = new JToggleButton("<html><b>N</b></html>"); //$NON-NLS-1$
	JToggleButton getBoldButton() {
		return this.boldButton;
	}

	private final JToggleButton italicButton = new JToggleButton("<html><i>K</i></html>"); //$NON-NLS-1$
	JToggleButton getItalicButton() {
		return this.italicButton;
	}

	private final JToggleButton underlineButton = new JToggleButton("<html><u>S</u></html>"); //$NON-NLS-1$
	JToggleButton getUnderlineButton() {
		return this.underlineButton;
	}

	private final JToggleButton strikethroughButton = new JToggleButton("<html><s>T</s></html>"); //$NON-NLS-1$
	JToggleButton getStrikethroughButton() {
		return this.strikethroughButton;
	}

	private final JCheckBox saveConfig = new JCheckBox(SignPdfUiMessages.getString("SignPdfUiPreview.33"), true); //$NON-NLS-1$
	JCheckBox getSaveConfig() {
		return this.saveConfig;
	}

	private final JButton okButton = new JButton(SignPdfUiMessages.getString("SignPdfUiPreview.5")); //$NON-NLS-1$
	JButton getOkButton() {
		return this.okButton;
	}

	private JComboBox<FontResource> fontFamilyCombo;
	JComboBox<FontResource> getLetterType() {
		return this.fontFamilyCombo;
	}


	private JComboBox<RotationAngles> rotateSignature;
	JComboBox<RotationAngles> getRotateSignature() {
		return this.rotateSignature;
	}

	private CustomComboBox colorCombobox;
	CustomComboBox getColorCombobox() {
		return this.colorCombobox;
	}

	SpinnerNumberModel sizeSpinnerModel = new SpinnerNumberModel(
		INIT_TEXT_SIZE,
		MIN_TEXT_SIZE,
		MAX_TEXT_SIZE,
		STEP_TEXT_SIZE
	);
	JSpinner fontSizeSpinner = new JSpinner(this.sizeSpinnerModel);
	JSpinner getSizeSpinner() {
		return this.fontSizeSpinner;
	}
	int getSelectedSize() {
		return ((Integer)this.fontSizeSpinner.getValue()).intValue();
	}
	void setSelectedSize(final int size) {
		this.fontSizeSpinner.setValue(Integer.valueOf(size));
	}

	private final JTextArea signatureText = new JTextArea(4, 60);
	JTextArea getTextArea() {
		return this.signatureText;
	}

	private JPanel previewPanel;
	JPanel getPreviewPanel() {
		return this.previewPanel;
	}

	SignPdfUiPanelPreview (final SignPdfUiPanelListener spul,
						   final Properties p,
						   final BufferedImage im) {

		if (spul == null) {
			throw new IllegalArgumentException(
				"La clase a la que notificar la seleccion no puede ser nula" //$NON-NLS-1$
			);
		}
		this.listener = spul;
		this.prop = p;
		this.style = 0;

		this.image = im;
		this.signImage = null;
		this.previewPanel = new JPanel();

		createUI();

		loadProperties();

	}

	void createUI() {

		addKeyListener(this);

		getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPreview.3") //$NON-NLS-1$
		);

		// Establecemos un tamano preferido cualquiera para que se redimensione
		// correctamente el panel de scroll en el que se mostrara este panel
		setPreferredSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));
		setLayout(new GridBagLayout());

		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(10, 5, 0, 5);
		gbc.weightx = 1.0;
		gbc.gridy = 0;
		add(createPreviewPanel(), gbc);
		gbc.gridy++;
		add(createConfigurationPanel(), gbc);
		gbc.gridy++;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weighty = 0.0;
		add(this.saveConfig, gbc);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.gridy++;
		add(new JPanel(), gbc);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weighty = 0.0;
		gbc.gridy++;
		add(createButtonsPanel(), gbc);
		showPreview();
		this.fontFamilyCombo.requestFocusInWindow();
	}

	private JPanel createPreviewPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.setBorder(BorderFactory.createTitledBorder(
			SignPdfUiMessages.getString("SignPdfUiPreview.0")) //$NON-NLS-1$
		);

		this.dropTarget = new DropTarget(panel, DnDConstants.ACTION_COPY, new DropTargetListener() {

            @Override
            public void dropActionChanged(final DropTargetDragEvent dtde) { /* No implementado */}

            @Override
            public void dragOver(final DropTargetDragEvent dtde) { /* No implementado */ }

            @Override
            public void dragExit(final DropTargetEvent dte) { /* No implementado */ }

            @Override
            public void drop(final DropTargetDropEvent dtde) {

                final Transferable tr = dtde.getTransferable();
                if (getRotateSignature().getSelectedItem() == RotationAngles.DEGREES_0 &&
                		tr.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
                    dtde.acceptDrop(DnDConstants.ACTION_COPY);
                    final Object transData;
                    try {
                        transData = tr.getTransferData(DataFlavor.javaFileListFlavor);
                    }
                    catch (final Exception e) {
                    	LOGGER.warning(
                            "Ha fallado la operacion de arrastrar y soltar: " + e //$NON-NLS-1$
                        );
                        dtde.dropComplete(false);
                        return;
                    }
                    if (transData instanceof List) {
                        dtde.getDropTargetContext().dropComplete(true);
                        final List<?> fileList = (List<?>) transData;
                        if (fileList.isEmpty()) {
                            dtde.dropComplete(false);
                            return;
                        }
                        if (fileList.size() > 1) {
                        	AOUIFactory.showErrorMessage(
                        			SignPdfUiPanelPreview.this.getDialogParent(),
                        			SimpleAfirmaMessages.getString("SignPanel.18"), //$NON-NLS-1$
                        			SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
                        			JOptionPane.WARNING_MESSAGE,
                        			null);
                        }
                        File file = null;
                        final String filename = fileList.get(0).toString();
                        if (filename.startsWith("http://") || //$NON-NLS-1$
                        	filename.startsWith("https://") || //$NON-NLS-1$
                        	filename.startsWith("ftp://") //$NON-NLS-1$
                        ) {
                        	AOUIFactory.showErrorMessage(
                        			SignPdfUiPanelPreview.this.getDialogParent(),
                        			SimpleAfirmaMessages.getString("SignPanel.24"), //$NON-NLS-1$
                        			SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                        			JOptionPane.ERROR_MESSAGE,
                        			null);
                            dtde.dropComplete(false);
                            return;
                        }
                        else if (filename.startsWith("file://")) { //$NON-NLS-1$
                            try {
                            	file = new File(new URI(filename));
                            }
                            catch (final Exception e) {
                            	LOGGER.warning(
                            		"Ha fallado la operacion de arrastrar y soltar al obtener la ruta del fichero arrastrado: " + e //$NON-NLS-1$
                                );
                                dtde.dropComplete(false);
                                return;
                            }
                        }
                        try {
                            loadFile(file != null ? file : new File(filename));
                        }
                        catch (final Exception e) {
                        	LOGGER.warning(
                                "Ha fallado la operacion de arrastrar y soltar al cargar el fichero arrastrado: " + e //$NON-NLS-1$
                            );
                            dtde.dropComplete(false);
                        }
                    }
                }
                else {
                    dtde.rejectDrop();
                    dtde.dropComplete(false);
                }
            }

            @Override
            public void dragEnter(final DropTargetDragEvent dtde) {
                if (getRotateSignature().getSelectedItem() != RotationAngles.DEGREES_0 ||
                		!dtde.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
                    dtde.rejectDrag();
                }
            }
        }, true);

		panel.setDropTarget(this.dropTarget);

		if (getProp().getProperty(PdfExtraParams.SIGNATURE_PAGE) != null &&
				getProp().getProperty(PdfExtraParams.SIGNATURE_PAGE).equals("append")) { //$NON-NLS-1$
			createImage();
		}

		this.viewLabel = new JLabel();
		this.viewLabel.setPreferredSize(new Dimension(VIEWLABEL_PREFERRED_WIDTH, VIEWLABEL_PREFERRED_HEIGHT));

		if (this.image.getHeight() > VIEWLABEL_PREFERRED_HEIGHT || this.image.getWidth() > VIEWLABEL_PREFERRED_WIDTH) {
			resizeImage(this.viewLabel);
		}

		this.viewLabel.setIcon(new ImageIcon(this.image));

		this.viewLabel.setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.26")); //$NON-NLS-1$
		this.viewLabel.addMouseListener(
			new MouseAdapter() {
	            @Override
				public void mouseClicked(final MouseEvent evt) {
	            	if (SwingUtilities.isRightMouseButton(evt)) {
	            		if (getSignImage() != null) {
		            		final RemoveImPopUpMenu menu = new RemoveImPopUpMenu();
		                    menu.show(evt.getComponent(), evt.getX(), evt.getY());
	            		}
	            		else {
	            			final AddImPopUpMenu menu = new AddImPopUpMenu();
		                    menu.show(evt.getComponent(), evt.getX(), evt.getY());
	            		}
	            	}
	            	else {
	            		selectSignatureImage();
	                }
		        }
			}
		);
		this.viewLabel.setCursor(new Cursor(Cursor.HAND_CURSOR));

		panel.add(this.viewLabel);
		panel.add(createPreviewHintLabel());

		this.previewPanel = panel;

		return panel;

	}

	private JPanel createConfigurationPanel() {

		final JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(BorderFactory.createTitledBorder(
			SignPdfUiMessages.getString("SignPdfUiPreview.1")) //$NON-NLS-1$
		);

		final JPanel signatureImagePanel = createBrowseImagePanel();

		final JLabel signatureTextLabel = new JLabel(SignPdfUiMessages.getString("SignPdfUiPreview.36")); //$NON-NLS-1$
		signatureTextLabel.setLabelFor(this.signatureText);
		this.signatureText.setEditable(true);
		final Set<AWTKeyStroke> forwardKeys = new HashSet<>(this.signatureText.getFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS));
		forwardKeys.add(AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_TAB, 0));
		this.signatureText.setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, forwardKeys);
		final Set<AWTKeyStroke> backwardKeys = new HashSet<>(this.signatureText.getFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS));
		backwardKeys.add(AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_TAB, InputEvent.SHIFT_DOWN_MASK));
		this.signatureText.setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, backwardKeys);
		this.signatureText.setBackground(Color.WHITE);
		this.signatureText.setText(SignPdfUiMessages.getString("SignPdfUiPreview.25")); //$NON-NLS-1$
		this.signatureText.setLineWrap(true);
		this.signatureText.setWrapStyleWord(true);
		this.signatureText.addKeyListener(this);

		final JScrollPane scrollPane = new JScrollPane();
		scrollPane.setMinimumSize(new Dimension(400, 90));
		scrollPane.setViewportView(this.signatureText);
		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

		this.signatureText.getDocument().addDocumentListener(
			new DocumentListener() {
		        @Override
		        public void removeUpdate(final DocumentEvent e) {
		        	showPreview();
		        }
		        @Override
		        public void insertUpdate(final DocumentEvent e) {
		        	showPreview();
		        }
		        @Override
		        public void changedUpdate(final DocumentEvent e) {
		        	showPreview();
		        }
		    }
		);

		final JPanel fontPanel = new JPanel(new GridBagLayout());

		this.fontFamilyCombo = new JComboBox<>(FontResource.getAllFontresources());
		setViewFont(
			new Font(
				((FontResource)this.fontFamilyCombo.getSelectedItem()).getFont().getFontName(),
				getStyle(),
				getSelectedSize()
			)
		);

		this.fontFamilyCombo.setSelectedItem(FontResource.COURIER);
		this.fontFamilyCombo.setRenderer(new ComboRenderer());
		this.fontFamilyCombo.addKeyListener(this);
		this.fontFamilyCombo.setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.20")); //$NON-NLS-1$
		this.fontFamilyCombo.getAccessibleContext().setAccessibleName(SignPdfUiMessages.getString("SignPdfUiPreview.20")); //$NON-NLS-1$

		final JComponent comp = this.fontSizeSpinner.getEditor();
	    final JFormattedTextField field = (JFormattedTextField) comp.getComponent(0);
	    final DefaultFormatter formatter = (DefaultFormatter) field.getFormatter();
	    formatter.setAllowsInvalid(false);
		this.fontSizeSpinner.addChangeListener(
			e -> {
				setViewFont(getViewFont().deriveFont(getStyle(), getSelectedSize()));
				showPreview();
			}
		);
		this.fontSizeSpinner.getEditor().getComponent(0).addKeyListener(this);
		this.fontSizeSpinner.getAccessibleContext().setAccessibleName("SignPdfUiPreview.18"); //$NON-NLS-1$
		this.fontSizeSpinner.setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.18")); //$NON-NLS-1$

		this.rotateSignature = new JComboBox<>(RotationAngles.values());
		this.rotateSignature.addKeyListener(this);
		this.rotateSignature.addItemListener(
			e -> {
				showPreview();
			}
		);
		this.rotateSignature.getAccessibleContext().setAccessibleName("SignPdfUiPreview.42"); //$NON-NLS-1$
		this.rotateSignature.setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.42")); //$NON-NLS-1$


		final GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.weightx = 1.0;
		cons.gridx = 0;
		cons.insets = new Insets(0, 0, 0, 0);
		fontPanel.add(this.fontFamilyCombo, cons);
		cons.weightx = 0.0;
		cons.gridx++;
		fontPanel.add(this.fontSizeSpinner, cons);
		cons.gridx++;
		fontPanel.add(this.rotateSignature, cons);

		final JPanel fontStylePanel = new JPanel(new GridBagLayout());

		this.boldButton.setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.14")); //$NON-NLS-1$
		this.boldButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPreview.10") //$NON-NLS-1$
		);
		this.boldButton.addActionListener(
			e -> {
				if (getBoldButton().isSelected()) {
					if(getUnderlineButton().isSelected()) {
						getUnderlineButton().doClick();
					}
					if(getStrikethroughButton().isSelected()) {
						getStrikethroughButton().doClick();
					}
					addStyle(Font.BOLD);
					setViewFont(getViewFont().deriveFont(getStyle()));
					showPreview();
				}
				else {
					deleteStyle(Font.BOLD);
					setViewFont(getViewFont().deriveFont(getStyle()));
					showPreview();
				}
			}
		);
		this.boldButton.addKeyListener(this);

		this.italicButton.setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.15")); //$NON-NLS-1$
		this.italicButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPreview.11") //$NON-NLS-1$
		);
		this.italicButton.addActionListener(
			e -> {
				if (getItalicButton().isSelected()) {
					if(getUnderlineButton().isSelected()) {
						getUnderlineButton().doClick();
					}
					if(getStrikethroughButton().isSelected()) {
						getStrikethroughButton().doClick();
					}
					addStyle(Font.ITALIC);
					setViewFont(getViewFont().deriveFont(getStyle()));
					showPreview();
				}
				else {
					deleteStyle(Font.ITALIC);
					setViewFont(getViewFont().deriveFont(getStyle()));
					showPreview();
				}
			}
		);
		this.italicButton.addKeyListener(this);

		this.underlineButton.setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.16")); //$NON-NLS-1$
		this.underlineButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPreview.12") //$NON-NLS-1$
		);
		this.underlineButton.addActionListener(
			e -> {
				if (getUnderlineButton().isSelected()) {
					if(getBoldButton().isSelected()) {
						getBoldButton().doClick();
					}
					if(getItalicButton().isSelected()) {
						getItalicButton().doClick();
					}
					if(getStrikethroughButton().isSelected()) {
						getStrikethroughButton().doClick();
					}
					final Map<TextAttribute, Integer> atr = new HashMap<>();
					atr.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
					setViewFont(getViewFont().deriveFont(atr));
					showPreview();
				}
				else {
					setViewFont(new Font(getViewFont().getFontName(), getStyle(), getSelectedSize()));
					showPreview();
				}
			}
		);
		this.underlineButton.addKeyListener(this);

		this.strikethroughButton.setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.17")); //$NON-NLS-1$
		this.strikethroughButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPreview.13") //$NON-NLS-1$
		);
		this.strikethroughButton.addActionListener(
			e -> {
				if (getStrikethroughButton().isSelected()) {
					if(getBoldButton().isSelected()) {
						getBoldButton().doClick();
					}
					if(getItalicButton().isSelected()) {
						getItalicButton().doClick();
					}
					if(getUnderlineButton().isSelected()) {
						getUnderlineButton().doClick();
					}
					final Map<TextAttribute, Boolean> atr = new HashMap<>();
					atr.put(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON);
					setViewFont(getViewFont().deriveFont(atr));
					showPreview();
				}
				else {
					setViewFont(new Font(getViewFont().getFontName(), getStyle(), getSelectedSize()));
					showPreview();
				}
			}
		);
		this.strikethroughButton.addKeyListener(this);

		this.colorCombobox = new CustomComboBox(ColorResource.getAllColorResources());
		this.colorCombobox.getColorList().addKeyListener(this);
		this.colorCombobox.getColorList().setToolTipText(SignPdfUiMessages.getString("SignPdfUiPreview.19")); //$NON-NLS-1$

		cons.gridx = 0;
		cons.gridy = 0;
		cons.weightx = 1.0;
		fontStylePanel.add(this.boldButton, cons);
		cons.gridx++;
		fontStylePanel.add(this.italicButton, cons);
		cons.gridx++;
		fontStylePanel.add(this.underlineButton, cons);
		cons.gridx++;
		fontStylePanel.add(this.strikethroughButton, cons);
		cons.gridx++;
		fontStylePanel.add(this.colorCombobox, cons);

		final GridBagConstraints c = new GridBagConstraints();
		c.weightx = 1.0;
		c.insets = new Insets(4,  0,  0,  0);
		c.gridy = 0;
		c.anchor = GridBagConstraints.WEST;
		c.fill = GridBagConstraints.HORIZONTAL;
		panel.add(signatureImagePanel, c);
		c.gridy++;
		panel.add(signatureTextLabel, c);
		c.insets = new Insets(0,  0,  0,  0);
		c.gridy++;
		panel.add(scrollPane, c);
		c.insets = new Insets(4,  0,  0,  0);
		c.gridy++;
		panel.add(fontPanel, c);
		c.gridy++;
		panel.add(fontStylePanel, c);

		return panel;
	}

	private JPanel createBrowseImagePanel() {
		final JPanel signatureImagePanel = new JPanel(new GridBagLayout());
		final JLabel signatureImageLabel = new JLabel(SignPdfUiMessages.getString("SignPdfUiPreview.37")); //$NON-NLS-1$
		this.signatureImagePath = new JTextField();
		this.signatureImagePath.setEditable(false);
		this.browseImageButton.getAccessibleContext().setAccessibleName(SignPdfUiMessages.getString("SignPdfUiPreview.39")); //$NON-NLS-1$
		this.browseImageButton.addActionListener(
				evt -> selectSignatureImage());
		this.clearImageButton.getAccessibleContext().setAccessibleName(SignPdfUiMessages.getString("SignPdfUiPreview.41")); //$NON-NLS-1$
		this.clearImageButton.addActionListener(evt -> {
			clearSignImage();
			showPreview();
		});

		final GridBagConstraints imageCons = new GridBagConstraints();
		imageCons.fill = GridBagConstraints.HORIZONTAL;
		imageCons.weightx = 1.0;
		imageCons.gridx = 0;
		imageCons.gridy = 0;
		imageCons.gridwidth = 3;
		signatureImagePanel.add(signatureImageLabel, imageCons);
		imageCons.gridy = 1;
		imageCons.gridwidth = 1;
		signatureImagePanel.add(this.signatureImagePath, imageCons);
		imageCons.weightx = 0.0;
		imageCons.gridx = 1;
		signatureImagePanel.add(this.browseImageButton, imageCons);
		imageCons.gridx = 2;
		signatureImagePanel.add(this.clearImageButton, imageCons);

		return signatureImagePanel;
	}

	/** Crea el panel con los botones de aceptar y cancelar.
	 * @return Panel de botones. */
	private Component createPreviewHintLabel() {

		final JEditorPane helpLabel = new JEditorPane();
		helpLabel.setContentType("text/html"); //$NON-NLS-1$
		helpLabel.setText(SignPdfUiMessages.getString("SignPdfUiPreview.4")); //$NON-NLS-1$

        final EditorFocusManager editorFocusManager = new EditorFocusManager (
    		helpLabel,
    		(he, linkIndex) -> {
			    try {
			        if (he.getURL() != null) {
			            Desktop.getDesktop().browse(he.getURL().toURI());
			        }
			    }
			    catch (final Exception e) {
			    	LOGGER.severe(
		    			"No ha podido abrirse el navegador de sistema: " + e //$NON-NLS-1$
	    			);
			    	AOUIFactory.showErrorMessage(
			    			SignPdfUiPanelPreview.this.getDialogParent(),
			    			SimpleAfirmaMessages.getString("SignResultPanel.0") + he.getURL(), //$NON-NLS-1$
			    			SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
			    			JOptionPane.ERROR_MESSAGE,
			    			e);
			    }
			}
		);

        helpLabel.addHyperlinkListener(editorFocusManager);
        helpLabel.addFocusListener(editorFocusManager);
        helpLabel.addKeyListener(editorFocusManager);
        helpLabel.setEditable(false);
        helpLabel.setBackground(new Color(0,0,0,0));

        return helpLabel;
	}

	/** Crea el panel con los botones de aceptar y cancelar.
	 * @return Panel de botones. */
	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		final JButton defaultButton = new JButton(SignPdfUiMessages.getString("SignPdfUiPreview.31")); //$NON-NLS-1$
		defaultButton.setMnemonic('R');
		defaultButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPreview.7") //$NON-NLS-1$
		);
		defaultButton.addActionListener(
			e -> {
				loadDefaultProperties();
			}
		);
		defaultButton.addKeyListener(this);

		this.okButton.setMnemonic('A');
		this.okButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPanel.2") //$NON-NLS-1$
		);
		this.okButton.addActionListener(
			e -> {
				if (!getTextArea().getText().trim().isEmpty()) {
					getProp().put(PdfExtraParams.LAYER2_TEXT, getTextArea().getText().toString());
					getProp().put(PdfExtraParams.LAYER2_FONTFAMILY, ((FontResource)getLetterType().getSelectedItem()).getPdfFontIndex());
					getProp().put(PdfExtraParams.LAYER2_FONTSIZE, Integer.toString(getSelectedSize()));
					getProp().put(PdfExtraParams.LAYER2_FONTSTYLE, Integer.toString(getStyleIndex()));
					getProp().put(PdfExtraParams.LAYER2_FONTCOLOR, getColorCombobox().getSelectedItem().getPdfColorKey());
					getProp().put(PdfExtraParams.SIGNATURE_ROTATION, Integer.toString(((RotationAngles) getRotateSignature().getSelectedItem()).getDegrees()));
				}

				if (!this.signatureImagePath.getText().isEmpty()) {

					// Obtenemos la imagen directamente de la ruta para no perder calidad al reescalarla mas tarde
					BufferedImage signImageFromPath;
					final File signImageFile = new File(this.signatureImagePath.getText());
					try {
						signImageFromPath = ImageIO.read(signImageFile);
					} catch (final IOException ex) {
						LOGGER.log(Level.WARNING, "No se ha podido cargar la imagen de rubrica desde la ruta proporcionada", ex); //$NON-NLS-1$
						signImageFromPath = null;
					}

					if (signImageFromPath != null) {
						getProp().put(PdfExtraParams.SIGNATURE_RUBRIC_IMAGE, getInsertImageBase64(signImageFromPath));
					}
				}

				if (this.saveConfig.isSelected()) {
					saveProperties(getProp());
				}
				getListener().nextPanel(getProp(), null);
			}
		);

		this.okButton.addKeyListener(this);
		panel.add(this.okButton);

		final JButton cancelButton = new JButton(SignPdfUiMessages.getString("SignPdfUiPreview.6")); //$NON-NLS-1$
		cancelButton.setMnemonic('C');
		cancelButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPreview.7") //$NON-NLS-1$
		);
		cancelButton.addActionListener(
			e -> getListener().positionCancelled()
		);
		cancelButton.addKeyListener(this);

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(defaultButton);
			panel.add(cancelButton);
			panel.add(this.okButton);
		}
		else {
			panel.add(defaultButton);
			panel.add(this.okButton);
			panel.add(cancelButton);
		}

		return panel;
	}

	/**
     * Permite al usuario seleccionar una imagen de firma. Toma como ruta por defecto
     * la de la imagen actualmente establecida.
     */
	void selectSignatureImage() {
		String defaultPath = null;
		String defaultFilename = null;

		try {
			defaultPath = SignPdfUiPanelPreview.this.signatureImagePath.getText();
			if (defaultPath != null && !defaultPath.isEmpty()) {
				final File imgFile = new File(defaultPath);
				if (imgFile.isFile()) {
					defaultPath = imgFile.getParent();
					defaultFilename = imgFile.getName();
				}
			}
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se tiene acceso a la ruta definida para la imagen de firma", e); //$NON-NLS-1$
		}
    	try {
			final String imPath = AOUIFactory.getLoadFiles(
				SignPdfUiMessages.getString("SignPdfUiPreview.21"), //$NON-NLS-1$,
				defaultPath,
				defaultFilename,
				IMAGE_EXT,
				SignPdfUiMessages.getString("SignPdfUiPreview.22"), //$NON-NLS-1$,,
				false,
				false,
				null,
				SignPdfUiPanelPreview.this
			)[0].getAbsolutePath();
			loadSignImage(imPath);
			showPreview();

			if (checkTransparency()) {
	        	AOUIFactory.showMessageDialog(
	        			this,
						SignPdfUiMessages.getString("SignPdfDialog.9"),  //$NON-NLS-1$
						SignPdfUiMessages.getString("SignPdfDialog.8"),  //$NON-NLS-1$
	                    JOptionPane.WARNING_MESSAGE,
	                    null
	                );
			}
		}
		catch(final AOCancelledOperationException ex) {
			// Operacion cancelada por el usuario
		}
    	catch (final Exception e) {
			LOGGER.severe(
				"No ha sido posible cargar la imagen: " + e //$NON-NLS-1$
			);
			AOUIFactory.showErrorMessage(
					this.dialogParent,
					SignPdfUiMessages.getString("SignPdfUiPreview.24"),  //$NON-NLS-1$
					SignPdfUiMessages.getString("SignPdfUiPreview.23"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e);
		}
	}

	/** Recupera las propiedades de la firma. */
	private void loadProperties() {
		getTextArea().setText(PreferencesManager.get(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2TEXT));

		final String pdfFontIndex = PreferencesManager.get(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTFAMILY);
		int comboFontIndex = 0;
		final FontResource[] fonts = FontResource.getAllFontresources();
		for (int i = 0; i < fonts.length; ++i) {
			if (fonts[i].getPdfFontIndex().equals(pdfFontIndex)) {
				comboFontIndex = i;
			}
		}

		getLetterType().setSelectedIndex(comboFontIndex);

		setSelectedSize(
				Integer.parseInt(PreferencesManager.get(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTSIZE)));

		final int fontStyle = Integer.parseInt(PreferencesManager.get(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTSTYLE));
		if (fontStyle == 8) {
			getStrikethroughButton().doClick();
		} else if (fontStyle == 4) {
			getUnderlineButton().doClick();
		} else if (fontStyle == 3) {
			getItalicButton().doClick();
			getBoldButton().doClick();
		} else if (fontStyle == 2) {
			getItalicButton().doClick();
		} else if (fontStyle == 1) {
			getBoldButton().doClick();
		}

		final String pdfColorIndex = PreferencesManager.get(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTCOLOR);
		int comboColorIndex = 0;
		final ColorResource[] colors = ColorResource.getAllColorResources();
		for (int i = 0; i < colors.length; ++i) {
			if (colors[i].getPdfColorKey().equals(pdfColorIndex)) {
				comboColorIndex = i;
			}
		}
		getColorCombobox().setSelectedItem(colors[comboColorIndex]);
		getColorCombobox().getColorList().setSelectedIndex(comboColorIndex);

		final Font fon = fonts[comboFontIndex].getFont();
		final Map<TextAttribute, Object> atr = (Map<TextAttribute, Object>) getViewFont().getAttributes();
		atr.put(TextAttribute.FAMILY, fon.getFontName());
		setViewFont(getViewFont().deriveFont(atr));

		final String rotateSign = PreferencesManager.get(PreferencesManager.PREFERENCE_PDF_SIGN_SIGNATUREROTATION);
		getRotateSignature().setSelectedItem(RotationAngles.parse(Integer.parseInt(rotateSign)));

		final String imagePath = PreferencesManager.get(PreferencesManager.PREFERENCE_PDF_SIGN_IMAGE);
		if (imagePath != null && !imagePath.isEmpty()) {
			try {
				loadSignImage(imagePath);
			} catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se ha podido cargar la imagen almacenada en las preferencias", e); //$NON-NLS-1$
			}
		}

		showPreview();

	}

	/** Restaura las propiedades por defecto de la firma. */
	private void loadDefaultProperties() {
		getTextArea().setText(
			PreferencesManager.getDefaultPreference(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2TEXT)
		);

		final String pdfFontIndex =
			PreferencesManager.getDefaultPreference(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTFAMILY);
		int comboFontIndex = 0;
		final FontResource[] fonts = FontResource.getAllFontresources();
		for (int i = 0; i < fonts.length; ++i) {
			if (fonts[i].getPdfFontIndex().equals(pdfFontIndex)) {
				comboFontIndex = i;
			}
		}

		getLetterType().setSelectedIndex(comboFontIndex);

		setSelectedSize(Integer.parseInt(
			PreferencesManager.getDefaultPreference(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTSIZE)
		));

		final int fontStyle = Integer.parseInt(
			PreferencesManager.getDefaultPreference(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTSTYLE)
		);
		switch(fontStyle) {
			case 8:
				getStrikethroughButton().doClick();
				break;
			case 4:
				getUnderlineButton().doClick();
				break;
			case 3:
				getItalicButton().doClick();
				getBoldButton().doClick();
				break;
			case 2:
				getItalicButton().doClick();
				break;
			case 1:
				getBoldButton().doClick();
				break;
			default:
				LOGGER.warning("Estilo de texto desconocido: " + fontStyle); //$NON-NLS-1$
		}

		final String pdfColorIndex = PreferencesManager.getDefaultPreference(
			PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTCOLOR
		);
		int comboColorIndex = 0;
		final ColorResource[] colors = ColorResource.getAllColorResources();
		for (int i = 0; i < colors.length; ++i) {
			if (colors[i].getPdfColorKey().equals(pdfColorIndex)) {
				comboColorIndex = i;
			}
		}
		getColorCombobox().setSelectedItem(colors[comboColorIndex]);
		getColorCombobox().getColorList().setSelectedIndex(comboColorIndex);

		final Font fon = fonts[comboFontIndex].getFont();
		final Map<TextAttribute, Object> atr = (Map<TextAttribute, Object>) getViewFont().getAttributes();
		atr.put(TextAttribute.FAMILY, fon.getFontName());
		setViewFont(getViewFont().deriveFont(atr));

		final String rotateSign = PreferencesManager.getDefaultPreference(
			PreferencesManager.PREFERENCE_PDF_SIGN_SIGNATUREROTATION
		);
		getRotateSignature().setSelectedItem(RotationAngles.parse(Integer.parseInt(rotateSign)));

		final String imagePath = PreferencesManager.getDefaultPreference(
			PreferencesManager.PREFERENCE_PDF_SIGN_IMAGE
		);
		if (imagePath != null && !imagePath.isEmpty()) {
			try {
				loadSignImage(imagePath);
			} catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "No se ha podido cargar la imagen almacenada en las preferencias", e); //$NON-NLS-1$
			}
		}

		showPreview();

	}

	/** Almacena las propiedades de la firma.
	 * @param params Colecci&oacute;n de propiedades de la firma. */
	private void saveProperties(final Properties params) {

		if (params.getProperty(PdfExtraParams.LAYER2_TEXT) != null) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2TEXT, params.getProperty(PdfExtraParams.LAYER2_TEXT)
			);
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTFAMILY, params.getProperty(PdfExtraParams.LAYER2_FONTFAMILY)
			);
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTSIZE, params.getProperty(PdfExtraParams.LAYER2_FONTSIZE)
			);
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTSTYLE, params.getProperty(PdfExtraParams.LAYER2_FONTSTYLE)
			);
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2FONTCOLOR, params.getProperty(PdfExtraParams.LAYER2_FONTCOLOR)
			);
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PDF_SIGN_SIGNATUREROTATION, params.getProperty(PdfExtraParams.SIGNATURE_ROTATION)
			);
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_PDF_SIGN_LAYER2TEXT);
		}

		if (params.getProperty(PdfExtraParams.SIGNATURE_RUBRIC_IMAGE) != null &&
				this.signatureImagePath.getText() != null) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PDF_SIGN_IMAGE, this.signatureImagePath.getText()
			);
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_PDF_SIGN_IMAGE);
		}
		try {
			PreferencesManager.flush();
		} catch (final Exception e) {
			LOGGER.severe("Error al guardar las preferencias de firma visible PDF: " + e); //$NON-NLS-1$
		}
	}

	 /** Carga la imagen para a&ntilde;adir a la firma.
     * @param file Imagen para a&ntilde;adir.
	 * @throws IOException Si hay problemas en el pintado de la imagen. */
	void loadFile(final File file) throws IOException {

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        String errorMessage = null;
        if (!file.exists()) {
            errorMessage = SimpleAfirmaMessages.getString("SignPanel.3"); //$NON-NLS-1$
        }
        else if (file.isDirectory()) {
        	errorMessage = SimpleAfirmaMessages.getString("SignPanel.21"); //$NON-NLS-1$
        }
        else if (!file.canRead()) {
            errorMessage = SimpleAfirmaMessages.getString("SignPanel.7"); //$NON-NLS-1$
        }
        else if (file.length() < 1) {
            errorMessage = SimpleAfirmaMessages.getString("SignPanel.5"); //$NON-NLS-1$
        }
        else if (!isValidImage(file.getName())) {
        	errorMessage = SimpleAfirmaMessages.getString("SignPanel.101"); //$NON-NLS-1$
        }
        if (errorMessage != null) {
        	AOUIFactory.showErrorMessage(
        			this.dialogParent,
        			errorMessage,
        			SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
        			JOptionPane.ERROR_MESSAGE,
        			null);
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            return;
        }

        loadSignImage(file.getAbsolutePath());
        showPreview();
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

	private static boolean isValidImage(final String file) {
		boolean valid = false;
		for (final String ext : IMAGE_EXT) {
			if (file.endsWith(ext)) {
				valid = true;
			}
		}
		return valid;
	}

	static String getInsertImageBase64(final BufferedImage bi) {
		try (final ByteArrayOutputStream osImage = new ByteArrayOutputStream()) {
			ImageIO.write(bi, "jpg", osImage); //$NON-NLS-1$
			return Base64.getEncoder().encodeToString(osImage.toByteArray());
		}
        catch (final Exception e1) {
        	LOGGER.severe(
				"No ha sido posible pasar la imagen a JPG: " + e1 //$NON-NLS-1$
			);
		}
		return null;
	}

	void setInsertImageBase64(final String base64Image) {
		try (final ByteArrayInputStream isImage = new ByteArrayInputStream(Base64.getDecoder().decode(base64Image))) {
			final BufferedImage bi = ImageIO.read(isImage);

			if(bi == null) {
				this.signImage = null;
				return;
			}

			final BufferedImage newImage = new BufferedImage(
			    this.image.getWidth(), this.image.getHeight(), this.image.getType()
			);

			final Graphics2D g = newImage.createGraphics();
			g.drawImage(this.image, 0, 0, null);
			g.drawImage(
				bi.getScaledInstance(this.image.getWidth(), this.image.getHeight(), Image.SCALE_SMOOTH), 0, 0, null
			);
			g.dispose();
			this.signImage = newImage;
		}
		catch (final Exception e1) {
			LOGGER.severe(
					"No ha sido posible recuperar la imagen guardada: " + e1 //$NON-NLS-1$
			);
		}
	}

	private void createImage() {
		final BufferedImage bi = new BufferedImage(
			this.image.getWidth(),
			this.image.getHeight(),
            BufferedImage.TYPE_INT_RGB
        );
		final Graphics2D ig2 = bi.createGraphics();
		ig2.setPaint (Color.WHITE);
		ig2.fillRect (0, 0, bi.getWidth(), bi.getHeight());
		ig2.dispose();
		this.image = bi;
	}

	void loadSignImage(final String path) throws IOException, IllegalArgumentException {

		// Cargamos la imagen
		final BufferedImage bi = ImageIO.read(new File(path));

		if (bi == null) {
			throw new IllegalArgumentException("La ruta proporcionada no se corresponde con la de un fichero de imagen soportada");  //$NON-NLS-1$
		}

		final BufferedImage newImage = new BufferedImage(
		    this.image.getWidth(), this.image.getHeight(), this.image.getType()
		);

		final Graphics2D g = newImage.createGraphics();
		g.drawImage(bi.getScaledInstance(this.image.getWidth(), this.image.getHeight(), Image.SCALE_SMOOTH), 0, 0, null);
		g.dispose();
		this.signImage = newImage;

		// Mostramos la ruta en el campo de carga
		this.signatureImagePath.setText(path);
	}

	void clearSignImage() {
		this.signImage = null;

		// Eliminamos la ruta del campo de carga
		this.signatureImagePath.setText(null);
	}

	public static String breakLines(final String input, final double maxLineLength, final FontMetrics fm ) {
	    final String[] tokens = input.split(SPLIT_REGEXP);
	    final StringBuilder output = new StringBuilder(input.length());
	    int lineLen = 0;
	    for (int i = 0; i < tokens.length; i++) {
	        String word = tokens[i];

	        if (lineLen + fm.stringWidth(SPACE_SEPARATOR + word) > maxLineLength) {
	            if (i > 0) {
	                output.append(NEWLINE);
	            }
	            lineLen = 0;
	        }
	        if (i < tokens.length - 1 && lineLen + fm.stringWidth(word + SPACE_SEPARATOR + tokens[i + 1]) <= maxLineLength) {
	            word += SPACE_SEPARATOR;
	        }
	        output.append(word);
	        lineLen += fm.stringWidth(word);
	    }
	    return output.toString();
	}

	void showPreview() {

		final BufferedImage bi = new BufferedImage(
	    		this.image.getWidth(),
	    		this.image.getHeight(),
	    		this.image.getType()
	    		);

		final Graphics2D g = bi.createGraphics();
	 	g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
		g.drawImage(this.image, 0, 0, null);

	    g.setColor(this.colorCombobox.getSelectedItem().getColor());
	    final int scaledSize = Math.max(1,Math.round(getViewFont().getSize() / this.scale) - 3);
	    g.setFont(getViewFont().deriveFont(getStyle(), scaledSize));

	    final RotationAngles rotationGrades = (RotationAngles) this.rotateSignature.getSelectedItem();

	    if (this.signImage != null) {
	    	rotateImgWithText(g, this.signImage, this.signImage.getWidth(), this.signImage.getHeight(),
	    			 		getTextArea().getText(), rotationGrades, true);
	    }
	    else {
	    	rotateImgWithText(g, this.image, this.image.getWidth(), this.image.getHeight(),
	    			 		getTextArea().getText(), rotationGrades, false);
	    }

	    g.dispose();

	    getViewLabel().setIcon(new ImageIcon(bi));
	}

	private void rotateImgWithText(final Graphics2D g, final BufferedImage loadedImage, final int imageWidth, final int imageHeight,
			final String text, final RotationAngles rotation, final boolean isSignImage) {

		final AffineTransform originalTransform = g.getTransform();

		final FontMetrics fontMetrics = g.getFontMetrics();
		final int fontHeight = fontMetrics.getHeight();
		//final int fontHeight = g.getFont().getSize();
		final int MARGIN = 4;

		int lineWidth;
		int posX;
		int posY;
		BufferedImage res = loadedImage;

		// Realizamos la rotacion correspondiente y calculamos las referencias
		// para el posicionamiento del texto. La imagen solo se rotara en caso de que sea
		// la rubrica, no la imagen de fondo que pertenece al PDF
		switch (rotation) {
		case DEGREES_90:
			lineWidth = imageHeight;
			posX = -imageHeight + MARGIN;
			posY = 0;
			g.rotate(-Math.PI/2);
			if (isSignImage) {
				res = resizeSignImage(loadedImage);
				g.drawImage(res, posX - MARGIN, posY, null);
			}
			break;
		case DEGREES_180:
			lineWidth = imageWidth;
			posX = -imageWidth + MARGIN;
			posY = -imageHeight;
			g.rotate(Math.PI);
			if (isSignImage) {
				g.drawImage(res, posX - MARGIN, posY, null);
			}
			break;
		case DEGREES_270:
			lineWidth = imageHeight;
			posX = MARGIN;
			posY = -imageWidth;
			g.rotate(Math.PI/2);
			if (isSignImage) {
				res = resizeSignImage(loadedImage);
				g.drawImage(res, 0, posY, null);
			}
			break;
		default:  //case 0:
			lineWidth = imageWidth;
			posX = MARGIN;
			posY = 0;
			if (isSignImage) {
				g.drawImage(res, 0, posY, null);
			}
			break;
		}

		// Imprimimos el texto en lineas en base a las referencias determinadas
		// por la rotacion

		int textLength;
		for (final String line : text.split("\n")) { //$NON-NLS-1$
			textLength = fontMetrics.stringWidth(line);
			if (textLength > lineWidth) {
				final String lineWrapped = breakLines(line, lineWidth, fontMetrics);
				for (final String s : lineWrapped.split("\n")) { //$NON-NLS-1$
					g.drawString(s, posX, posY += fontHeight);
				}
			}
			else {
				g.drawString(line, posX, posY += fontHeight);
			}
		}

		g.setTransform(originalTransform);
	}

	private void resizeImage(final JComponent panel) {
		int newWidth = this.image.getWidth();
		int newHeight = this.image.getHeight();

		final Dimension screen = panel.getPreferredSize();
		while (newWidth > screen.width || newHeight > screen.height) {
			newWidth = (int) Math.round(newWidth * 0.9);
			newHeight = (int) Math.round(newHeight * 0.9);
		}
		final float aspectRatio = (float) newWidth / this.image.getWidth();

		newWidth = Math.round(this.image.getWidth() * aspectRatio);
		newHeight = Math.round(this.image.getHeight() * aspectRatio);

		this.scale = this.image.getWidth() / newWidth;
		final Image newIm = this.image.getScaledInstance(newWidth, newHeight, Image.SCALE_SMOOTH);
	    final BufferedImage newBi = new BufferedImage(
    		newWidth,
    		newHeight,
	        this.image.getType()
        );
	    final Graphics g = newBi.createGraphics();
	    g.drawImage(newIm, 0, 0, null);

	    g.dispose();
		this.image = newBi;
	}

	/**
	 * M&eacute;todo que permite escalar la imagen al tamaño necesario para que se
	 * adapte al panel con la vista previa de la firma visible
	 * @param bi Imagen a escalar
	 * @return Imagen con el nuevo tama&ntilde;o
	 */
	private BufferedImage resizeSignImage(final BufferedImage bi) {

		final int newWidth = this.signImage.getWidth();
		final int newHeight = this.signImage.getHeight();

		final Image tmp = bi.getScaledInstance(newHeight, newWidth, Image.SCALE_SMOOTH);
		final BufferedImage dimg = new BufferedImage(newHeight, newWidth, Image.SCALE_SMOOTH);

		final Graphics2D g2d = dimg.createGraphics();
	    g2d.drawImage(tmp, 0, 0, null);
	    g2d.dispose();

		return dimg;
	}

	/**
	 * Comprueba si la imag&eacute;n tiene alg&uacute;n canal alfa o alg&eacute;n
	 * p&iacute; transparente
	 * @return devuelve true en caso de que detecte transparencias
	 */
	public boolean checkTransparency(){

		boolean hasTransparency = false;

		if (!this.signatureImagePath.getText().isEmpty()) {

			final File input = new File(this.signatureImagePath.getText());
			BufferedImage image = null;
			try {
				image = ImageIO.read(input);
			} catch (final IOException e) {
				LOGGER.warning("Error al leer la imagen de firma visible"); //$NON-NLS-1$
			}

			if (image != null) {
				if (containsAlphaChannel(image) || containsTransparency(image)){
					hasTransparency = true;
				}
			}

		}

		return hasTransparency;
	}

	/**
	 * M&eacute;todo que comprueba si la imagen tiene alg&uacute;n canal alfa
	 * @param image imagen a comprobar
	 * @return devuelve true en caso de que disponga de un canal alfa
	 */
    private static boolean containsAlphaChannel(final BufferedImage image){
        return image.getColorModel().hasAlpha();
    }

    /**
     * Comprueba pixel a pixel de la imagen si alguno es transparente
     * @param image imagen a comprobar
     * @return devuelve true si detecta alg&uacute;n p&iacute;xel transparente
     */
    private static boolean containsTransparency(final BufferedImage image){
        for (int i = 0; i < image.getHeight(); i++) {
            for (int j = 0; j < image.getWidth(); j++) {
                if (isTransparent(image, j, i)){
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Comprueba la transparencia de un p&iacute;xel concreto
     * @param image imagen a comprobar
     * @param x posici&oacute;n x
     * @param y posici&oacute;n y
     * @return devuelve true si el p&iacute;xel es transparente
     */
    public static boolean isTransparent(final BufferedImage image, final int x, final int y ) {
        final int pixel = image.getRGB(x,y);
        return pixel>>24 == 0x00;
    }

	int getStyleIndex() {
		if (getStrikethroughButton().isSelected()) {
			return 8;
		}
		else if (getUnderlineButton().isSelected()) {
			return 4;
		}
		else if (getBoldButton().isSelected() && getItalicButton().isSelected()) {
			return 3;
		}
		else if (getItalicButton().isSelected()) {
			return 2;
		}
		else if (getBoldButton().isSelected()) {
			return 1;
		}
		else {
			return 0;
		}
	}

	void addStyle(final int s) {
		setStyle(getStyle() + s);
	}

	void deleteStyle(final int s) {
		setStyle(getStyle() - s);
	}

	@Override public void keyTyped(final KeyEvent e) { /* vacio */ }
	@Override public void keyReleased(final KeyEvent e) { /* vacio */ }

	@Override
	public void keyPressed(final KeyEvent ke) {
		if (ke != null) {
			if (ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
				getListener().positionCancelled();
			}
			else if (ke.getKeyCode() == KeyEvent.VK_N && ke.isControlDown()) {
				this.boldButton.doClick();
			}
			else if (ke.getKeyCode() == KeyEvent.VK_K && ke.isControlDown()) {
				this.italicButton.doClick();
			}
			else if (ke.getKeyCode() == KeyEvent.VK_S && ke.isControlDown()) {
				this.underlineButton.doClick();
			}
			else if (ke.getKeyCode() == KeyEvent.VK_T && ke.isControlDown()) {
				this.strikethroughButton.doClick();
			}
			else if (ke.getKeyCode() == KeyEvent.VK_P && ke.isControlDown()) {
				SwingUtilities.invokeLater(() ->  {
		                getViewLabel().dispatchEvent(
	                		new MouseEvent(
		                		 SignPdfUiPanelPreview.this,
			                     MouseEvent.MOUSE_CLICKED,
			                     1,
			                     MouseEvent.BUTTON1,
			                     0, 0,
			                     1,
			                     false
			                )
                		);
		            }
				);
			}
		}
	}

	private final class CustomComboBox extends JPanel {

		private static final long serialVersionUID = 6905237438341447834L;
		private ColorResource color = ColorResource.BLACK;
		final ColorResource[] colorStrings;
		private final JComboBox<Integer> colorList ;
		JComboBox<Integer> getColorList() {
			return this.colorList;
		}
		ImageIcon[] images;


	    ColorResource getSelectedItem() {
	    	return this.color;
	    }

	    void setSelectedItem(final ColorResource c) {
	    	this.color = c;
	    }

	    public CustomComboBox(final ColorResource[] colorResources) {
	        super(new BorderLayout());

	        this.colorStrings = colorResources;
	        this.images = new ImageIcon[this.colorStrings.length];
	        final Integer[] intArray = new Integer[this.colorStrings.length];
	        for (int i = 0; i < this.colorStrings.length; i++) {
	            intArray[i] = Integer.valueOf(i);
	            this.images[i] = new ImageIcon(this.colorStrings[i].getImage());
	            if (this.images[i] != null) {
	                this.images[i].setDescription(this.colorStrings[i].toString());
	            }
	        }

	        this.colorList = new JComboBox<>(intArray);
	        final ComboBoxRenderer renderer = new ComboBoxRenderer();
	        renderer.setPreferredSize(new Dimension(60, 20));
	        this.colorList.setRenderer(renderer);
	        this.colorList.setMaximumRowCount(3);

	        add(this.colorList, BorderLayout.PAGE_START);
	    }

	    private final class ComboBoxRenderer extends JLabel implements ListCellRenderer<Object> {

			private static final long serialVersionUID = -9197195477672319113L;
			private Font uhOhFont;

	        public ComboBoxRenderer() {
	            setOpaque(true);
	            setHorizontalAlignment(LEFT);
	            setVerticalAlignment(CENTER);
	        }


	        @Override
			public Component getListCellRendererComponent(final JList<?> list,
	                                                      final Object value,
	                                                      final int index,
	                                                      final boolean isSelected,
	                                                      final boolean cellHasFocus) {

	            final int selectedIndex = ((Integer)value).intValue();
	            final ImageIcon icon = CustomComboBox.this.images[selectedIndex];
	            final String col = CustomComboBox.this.colorStrings[selectedIndex].toString();
	            if (isSelected) {
	            	setSelectedItem(CustomComboBox.this.colorStrings[selectedIndex]);
	            	showPreview();
	                setBackground(list.getSelectionBackground());
	                setForeground(list.getSelectionForeground());
	            }
	            else {
	                setBackground(list.getBackground());
	                setForeground(list.getForeground());
	            }

	            setBorder(new EmptyBorder(1, 3, 1, 1));
	            setIcon(icon);
	            if (icon != null) {
	                setText(col);
	                setFont(list.getFont());
	            }
	            else {
	                setUhOhText(col + " (no image available)", list.getFont()); //$NON-NLS-1$
	            }

	            return this;
	        }

	        //Fuente cuando no encuentra la imagen
	        protected void setUhOhText(final String uhOhText, final Font normalFont) {
	            if (this.uhOhFont == null) {
	                this.uhOhFont = normalFont.deriveFont(Font.PLAIN);
	            }
	            setFont(this.uhOhFont);
	            setText(uhOhText);
	        }
	    }
	}

	private final class ComboRenderer extends BasicComboBoxRenderer {

		private static final long serialVersionUID = 3706919693882358747L;
		private final DefaultListCellRenderer defaultRenderer = new DefaultListCellRenderer();
        private int row;

        ComboRenderer() {
			// vacio
		}

		@Override
        public Component getListCellRendererComponent(final JList list,
        		                                      final Object value,
        		                                      final int index,
        		                                      final boolean isSelected,
        		                                      final boolean cellHasFocus) {

        	super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

            final JLabel renderer =
            	(JLabel) this.defaultRenderer.getListCellRendererComponent(list, value, this.row, isSelected, cellHasFocus);
            final Object fntObj = value;
            final Font fon = ((FontResource) fntObj).getFont();
            final Font itemFont = new Font(fon.getFontName(), Font.PLAIN, 14);
            final Map<TextAttribute, Object> atr = (Map<TextAttribute, Object>) getViewFont().getAttributes();
			atr.put(TextAttribute.FAMILY, fon.getFontName());
			atr.put(TextAttribute.SIZE, Integer.valueOf(getSelectedSize()));
            final Font newFont = new Font(atr);
			setViewFont(getViewFont().deriveFont(atr));
            getLetterType().setFont(itemFont);
            renderer.setFont(itemFont);
            renderer.setText(fntObj.toString());
            if (isSelected) {
	            setViewFont(newFont);
	            showPreview();
            }
            return renderer;
        }
    }

	class RemoveImPopUpMenu extends JPopupMenu {

		private static final long serialVersionUID = 8491839704433747625L;
		JMenuItem removeImageItem;

		public RemoveImPopUpMenu(){
	        this.removeImageItem = new JMenuItem(SignPdfUiMessages.getString("SignPdfUiPreview.27")); //$NON-NLS-1$
	        this.removeImageItem.addActionListener(
    			e -> {
					clearSignImage();
					showPreview();
				}
    		);
	        add(this.removeImageItem);
	    }
	}

	class AddImPopUpMenu extends JPopupMenu {

		private static final long serialVersionUID = 8491839704433747625L;
		JMenuItem addImageItem;

		public AddImPopUpMenu(){
	        this.addImageItem = new JMenuItem(SignPdfUiMessages.getString("SignPdfUiPreview.28")); //$NON-NLS-1$
	        this.addImageItem.addActionListener(
    			e -> SwingUtilities.invokeLater(() ->  {
				        getViewLabel().dispatchEvent(
				    		new MouseEvent(
				        		 SignPdfUiPanelPreview.this,
				                 MouseEvent.MOUSE_CLICKED,
				                 1,
				                 MouseEvent.BUTTON1,
				                 0, 0,
				                 1,
				                 false
				            )
						);
				    }
				)
    		);
	        add(this.addImageItem);
	    }
	}

	/**
	 * Niveles de rotaci&oacute;n permitidos para el texto.
	 */
	private enum RotationAngles {
		DEGREES_0 (0, "RotationModel.0"), //$NON-NLS-1$
		DEGREES_90(90, "RotationModel.1"), //$NON-NLS-1$
		DEGREES_180(180, "RotationModel.2"), //$NON-NLS-1$
		DEGREES_270(270, "RotationModel.3"); //$NON-NLS-1$

		private int degrees;
		private String text;

		RotationAngles(final int degrees, final String text) {
			this.degrees = degrees;
			this.text = text;
		}

		public int getDegrees() {
			return this.degrees;
		}

		/**
		 * Recupera el texto que identifica al &aacute;ngulo de rotaci&oacute;n.
		 * @return Texto descriptivo del &aacute;gulo de rotaci&oacute;n.
		 */
		public String getText() {
			return SignPdfUiMessages.getString(this.text);
		}

		@Override
		public String toString() {
			return getText();
		}

		/**
		 * Identifica qu&eacute; elemento del enumerado se corresponde con los grados indicados. obtener un elemento del numerado
		 * @param degrees Grados de rotaci&oacute;n deseados.
		 * @return &Aacute;ngulo de rotaci&oacute;n o nulo si no es un &aacute;ngulo permitido.
		 */
		public static RotationAngles parse(final int degrees) {
			for (final RotationAngles angle : values()) {
				if (angle.getDegrees() == degrees) {
					return angle;
				}
			}
			return null;
		}
	}

	public void setDialogParent(final JDialog dialogParent) {
		this.dialogParent = dialogParent;
	}

	JDialog getDialogParent() {
		return this.dialogParent;
	}
}