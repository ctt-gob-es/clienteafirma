/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.accessibility.Accessible;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIManager;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.core.ui.KeyStoreDialogManager;
import es.gob.afirma.ui.core.jse.errors.ErrorManagementDialog;

/** Gestor de componentes de interfaz gr&aacute;fico (tanto para Applet como para
 * aplicaci&oacute;n de escritorio) de la aplicaci&oacute;n.
 * @version 0.4 */
public class JSEUIManager implements AOUIManager {

	/** <code>Logger</code> de la clase y sus hijas. */
    protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final int ASCII_LOWER_INDEX = 32;
    private static final int ASCII_HIGHER_INDEX = 126;

    /** Objecto general de preferencias donde se guarda la configuraci&oacute;n de la
	 * aplicaci&oacute;n. */
	private static final Preferences PREFERENCES;
	static {
		PREFERENCES = Preferences.userNodeForPackage(JSEUIManager.class);
	}

	/** Recupera el valor de una cadena de texto almacenada entre las preferencias de la
	 * aplicaci&oacute;n.
	 * @param key Clave del valor que queremos recuperar.
	 * @param def Valor que se devolver&aacute;a si la preferencia no se encontraba almacenada.
	 * @return La preferencia almacenada o {@code def} si no se encontr&oacute;. */
	public static String get(final String key, final String def) {
		return PREFERENCES.get(key, def);
	}

	/** Establece una cadena de texto en la configuraci&oacute;n de la aplicaci&oacute;n
	 * identific&aacute;ndola con una clave. Para realizar el guardado completo, es
	 * necesario ejecutar el m&eacute;todo {@code flush()}.
	 * @param key Clave con la que identificaremos el valor.
	 * @param value Valor que se desea almacenar. */
	public static void put(final String key, final String value) {
		PREFERENCES.put(key, value);
	}

	/** Guarda el directorio actual. */
	public static final String PREFERENCE_DIRECTORY = "currentDir"; //$NON-NLS-1$

	/** Pregunta al usuario por una contrase&ntilde;a.
     * @param text Texto que se muestra en el di&aacute;logo para pedir la contrase&ntilde;a.
     * @param c Componente padre (para la modalidad).
     * @return Contrase&ntilde;a introducida por el usuario.
     * @throws AOCancelledOperationException Cuando el usuario cancela el proceso de solicitud de
     *         contrase&ntilde;a. */
    @Override
	public final char[] getPassword(final String text, final Object c) {
        return getPassword(text, null, null, false, c);
    }

    /** Muestra un di&aacute;logo para pedir una contrase&ntilde;a al usuario.
     * @param text Texto con el que se solicitar&aacute; la entrada de texto al
     *             usuario (<i>prompt</i>).
     * @param imageIcon Objeto de tipo {@code javax.swing.Icon} con el icono del di&aacute;logo o
     * 			   {@code null} para no mostrar icono.
     * @param charSet Juego de caracteres aceptados para la contrase&ntilde;a.
     * @param beep <code>true</code> si se desea un sonido de advertencia al
     *             introducir un caracter no v&aacute;lido, <code>false</code> en
     *             caso contrario.
     * @param c Componente padre (para la modalidad).
     * @return Array de caracteres del texto introducido como contrase&ntilde;a.
     * @throws AOCancelledOperationException Cuando el usuario cancela o cierra el di&aacute;logo. */
    @Override
	public final char[] getPassword(final String text,
			                        final Object imageIcon,
			                        final String charSet,
			                        final boolean beep,
			                        final Object c) {

        final JPasswordField pwd = new JPasswordField(10);
        if (charSet != null) {
            pwd.setDocument(new JTextFieldFilter(charSet, beep));
        }
        final JLabel lbText = new JLabel(text != null ? text : JSEUIMessages.getString("JSEUIManager.24")); //$NON-NLS-1$
        lbText.setMinimumSize(
    		new Dimension(lbText.getFontMetrics(lbText.getFont()).stringWidth(text), lbText.getSize().height)
		);
        lbText.setLabelFor(pwd);
        final JPanel panel = new JPanel();

        final GridBagConstraints constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.weightx = 1.0;
        constraints.anchor = GridBagConstraints.CENTER;

        panel.setLayout(new GridBagLayout());
        panel.add(lbText, constraints);
        constraints.gridy = 1;
        panel.add(pwd, constraints);

        final Icon icon = imageIcon instanceof Icon ? (Icon) imageIcon : null;

        final JOptionPane pane = new JOptionPane(panel, JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_CANCEL_OPTION, icon) {
            private static final long serialVersionUID = -3012522768561175760L;

            /** {@inheritDoc} */
            @Override
            public void selectInitialValue() {
                pwd.requestFocusInWindow();
            }
        };

        final Component parent = c instanceof Component ? (Component) c : null;

        final JDialog dialog = pane.createDialog(parent, JSEUIMessages.getString("JSEUIManager.24")); //$NON-NLS-1$
        dialog.setVisible(true);

        final Object selectedValue = pane.getValue();
        if (selectedValue == null) {
        	throw new AOCancelledOperationException(
        		"La insercion de contrasena ha sido cancelada por el usuario" //$NON-NLS-1$
            );
        }
        if (((Integer) selectedValue).intValue() == JOptionPane.OK_OPTION) {
            return pwd.getPassword();
        }
        throw new AOCancelledOperationException(
    		"La insercion de contrasena ha sido cancelada por el usuario" //$NON-NLS-1$
        );
    }

    /** Muestra un di&aacute;logo para pedir dos veces una contrase&ntilde;a al usuario (ambas deben coincidir).
     * Es el procedimiento normal cuando se pide el establecimiento de una nueva contrase&ntilde;a, para evitar errores.
     * @param text Texto con el que se solicitar&aacute; la entrada de texto al
     *             usuario (<i>prompt</i>).
     * @param text2 Texto con el que se solicitar&aacute; al usuario que repita la contrase&ntilde;a.
     * @param imageIcon Objeto de tipo {@code javax.swing.Icon} con el icono del di&aacute;logo o
     * 			   {@code null} para no mostrar icono.
     * @param charSet Juego de caracteres aceptados para la contrase&ntilde;a.
     * @param beep <code>true</code> si se desea un sonido de advertencia al
     *             introducir un caracter no v&aacute;lido, <code>false</code> en
     *             caso contrario.
     * @param c Componente padre (para la modalidad).
     * @return Array de caracteres del texto introducido como contrase&ntilde;a.
     * @throws AOCancelledOperationException Cuando el usuario cancela o cierra el di&aacute;logo. */
    @Override
	public final char[] getDoublePassword(final String text,
										  final String text2,
			                              final Object imageIcon,
			                              final String charSet,
			                              final boolean beep,
			                              final Object c) {

        final JPasswordField pwd1 = new JPasswordField(10);
        if (charSet != null) {
            pwd1.setDocument(new JTextFieldFilter(charSet, beep));
        }
        final JLabel lbText1 = new JLabel(text != null ? text : JSEUIMessages.getString("JSEUIManager.24")); //$NON-NLS-1$
        lbText1.setMinimumSize(
    		new Dimension(
				lbText1.getFontMetrics(lbText1.getFont()).stringWidth(text != null ? text : JSEUIMessages.getString("JSEUIManager.24")), //$NON-NLS-1$
				lbText1.getSize().height
			)
		);
        lbText1.setLabelFor(pwd1);

        final JPasswordField pwd2 = new JPasswordField(10);
        if (charSet != null) {
            pwd2.setDocument(new JTextFieldFilter(charSet, beep));
        }
        final JLabel lbText2 = new JLabel(text2 != null ? text2 : JSEUIMessages.getString("JSEUIManager.2")); //$NON-NLS-1$
        lbText2.setMinimumSize(
    		new Dimension(
				lbText2.getFontMetrics(lbText2.getFont()).stringWidth(text2 != null ? text2 : JSEUIMessages.getString("JSEUIManager.2")), //$NON-NLS-1$
				lbText2.getSize().height
			)
		);
        lbText2.setLabelFor(pwd2);

        final JPanel panel = new JPanel();

        final GridBagConstraints constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.weightx = 1.0;
        constraints.anchor = GridBagConstraints.CENTER;
        panel.setLayout(new GridBagLayout());

        constraints.gridy = 0;
        panel.add(lbText1, constraints);

        constraints.gridy = 1;
        panel.add(pwd1, constraints);

        constraints.gridy = 2;
        panel.add(lbText2, constraints);

        constraints.gridy = 3;
        panel.add(pwd2, constraints);

        final Icon icon = imageIcon instanceof javax.swing.Icon ? (javax.swing.Icon) imageIcon : null;

        final JButton okBtn = new JButton(JSEUIMessages.getString("JSEUIManager.3")); //$NON-NLS-1$
        okBtn.getAccessibleContext().setAccessibleDescription(
    		JSEUIMessages.getString("JSEUIManager.4") //$NON-NLS-1$
		);
        okBtn.setEnabled(false);

        final JButton cancelBtn = new JButton(JSEUIMessages.getString("JSEUIManager.5")); //$NON-NLS-1$
        cancelBtn.getAccessibleContext().setAccessibleDescription(
        		JSEUIMessages.getString("JSEUIManager.6") //$NON-NLS-1$
		);

        final DocumentListener dl = new DocumentListener() {

        	private void something() {
        		if (pwd1.getPassword() != null && pwd1.getPassword().length > 1 && Arrays.equals(pwd1.getPassword(), pwd2.getPassword())) {
    				okBtn.setEnabled(true);
    			}
    			else {
    				okBtn.setEnabled(false);
    			}
        	}

			@Override public void removeUpdate(final DocumentEvent e) { something(); }
			@Override public void insertUpdate(final DocumentEvent e) { something(); }
			@Override public void changedUpdate(final DocumentEvent e) { something(); }
		};

		pwd1.getDocument().addDocumentListener(dl);
		pwd2.getDocument().addDocumentListener(dl);

        final JOptionPane pane = new JOptionPane(
    		panel,                             // Cuerpo del dialogo
    		JOptionPane.QUESTION_MESSAGE,      // Tipo de dialogo
    		JOptionPane.OK_CANCEL_OPTION,      // Opciones del dialogo
    		icon,                              // Icono
    		new Object[] { okBtn, cancelBtn }, // Opciones
    		okBtn                              // Opcion por defecto
		) {
            private static final long serialVersionUID = -3012522768561175760L;

            /** {@inheritDoc} */
            @Override
            public void selectInitialValue() {
                pwd1.requestFocusInWindow();
            }
        };

        okBtn.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				pane.setValue(Integer.valueOf(JOptionPane.OK_OPTION));
			}
		});

        cancelBtn.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				pane.setValue(Integer.valueOf(JOptionPane.CANCEL_OPTION));
			}
		});

        final Component parent = c instanceof Component ? (Component) c : null;

        final JDialog dialog = pane.createDialog(parent, JSEUIMessages.getString("JSEUIManager.24")); //$NON-NLS-1$
        dialog.setVisible(true);

        final Object selectedValue = pane.getValue();
        if (selectedValue == null) {
            return new char[0];
        }
        if (((Integer) selectedValue).intValue() == JOptionPane.OK_OPTION) {
            return pwd1.getPassword();
        }
        throw new AOCancelledOperationException(
    		"La insercion de contrasena ha sido cancelada por el usuario" //$NON-NLS-1$
        );

    }

    /** {@inheritDoc} */
    @Override
	public final Object showInputDialog(final Object parentComponent,
                                        final Object message,
                                        final String title,
                                        final int messageType,
                                        final Object icon,
                                        final Object[] selectionValues,
                                        final Object initialSelectionValue) {

    	final Component parent = parentComponent instanceof Component ? (Component) parentComponent : null;
        final Icon dialogIcon = icon instanceof Icon ? (Icon) icon : null;
        if (selectionValues == null) {
        	return JOptionPane.showInputDialog(
    			parent,
    			message,
    			title,
    			messageType
			);
        }
        return JOptionPane.showInputDialog(
    		parent,
    		message,
    		title,
    		messageType,
    		dialogIcon,
    		selectionValues,
    		initialSelectionValue
		);
    }

    /** {@inheritDoc} */
    @Override
	public String showCertificateSelectionDialog(final Object parentComponent,
    												   final KeyStoreDialogManager ksdm) {

    	final Component parent = parentComponent instanceof Component ? (Component) parentComponent : null;

    	try {
    		final Class<?> csdClass = Class.forName(
				"es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog" //$NON-NLS-1$
    		);
    		final Constructor<?> csdConstructor = csdClass.getConstructor(
				Component.class,
				KeyStoreDialogManager.class
			);
    		final Object csd =  csdConstructor.newInstance(parent, ksdm);
    		final Method showDialogMethod = csdClass.getMethod("showDialog"); //$NON-NLS-1$

    		final Object result = showDialogMethod.invoke(csd);

    		return result instanceof String ? (String) result : null;
    	}
    	catch (final InvocationTargetException e) {
    		LOGGER.severe("Se genero un error en el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
    		if (e.getCause() instanceof RuntimeException) {
    			throw (RuntimeException) e.getCause();
    		}
    		throw new IllegalStateException(
				"No se encontraron certificados en el almacen o se produjo un error durante la extraccion del certificado seleccionado: " + e, e.getCause() //$NON-NLS-1$
			);
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se encuentra disponible el proyecto del interfaz grafico del dialogo de seleccion: " + e); //$NON-NLS-1$
    		throw new IllegalStateException(
				"No se encuentra disponible el proyecto del interfaz grafico del dialogo de seleccion u ocurrio un error durante su ejecucion: " + e, e //$NON-NLS-1$
			);
    	}
    }

    /** Original code: <a href="http://tactika.com/realhome/realhome.html">http://tactika.com/realhome/realhome.html</a>
     * @author Real Gagnon */
    private static final class JTextFieldFilter extends PlainDocument {

        private static final long serialVersionUID = -5746396042117084830L;

        private String acceptedChars = null;

        /** Crea un nuevo filtro para campo de entrada de texto.
         * @param acceptedchars Cadena que debe contener todos los caracteres aceptados.
         *                      Cualquier caracter no incluido en esta cadena ser&aacute;
         *                      considerado inv&aacute;lido.
         * @param beepOnError <code>true</code> si desea que se reproduzca un sonido
         *                    cuando el usuario introduce un caracter no v&aacute;lido,
         *        			  false en caso contrario. */
        JTextFieldFilter(final String acceptedchars, final boolean beepOnError) {
            this.beep = beepOnError;
            this.acceptedChars = acceptedchars;
        }

        /** Indica si se debe reproducir o no un pitido cuando el usuario introduce un caracter no v&aacute;lido. */
        private boolean beep = false;

        /** {@inheritDoc} */
        @Override
        public void insertString(final int offset, final String str, final AttributeSet attr) throws BadLocationException {
            if (str == null) {
                return;
            }
            for (int i = 0; i < str.length(); i++) {
                if (this.acceptedChars.indexOf(String.valueOf(str.charAt(i))) == -1) {
                    if (this.beep) {
                        Toolkit.getDefaultToolkit().beep();
                    }
                    return;
                }
            }
            super.insertString(offset, str, attr);
        }

    }

    /** Filtro de caracteres ASCII imprimibles. */
    public static final class JTextFieldASCIIFilter extends PlainDocument {

        private static final long serialVersionUID = 1979726487852842735L;

        /** Indica si se debe reproducir o no un pitido cuando el usuario introduce un caracter no v&aacute;lido. */
        private boolean beep = false;

        /** Crea un nuevo filtro para campo de entrada de texto.
         * @param beepOnError <code>true</code> si desea que se reproduzca un sonido
         *                    cuando el usuario introduce un caracter no v&aacute;lido,
         *                    false en caso contrario. */
        public JTextFieldASCIIFilter(final boolean beepOnError) {
            this.beep = beepOnError;
        }

        /** {@inheritDoc} */
        @Override
        public void insertString(final int offset,
        		                 final String str,
        		                 final AttributeSet attr) throws BadLocationException {
            if (str == null) {
                return;
            }

            for (int i = 0; i < str.length(); i++) {
                if (str.charAt(i) < ASCII_LOWER_INDEX || str.charAt(i) > ASCII_HIGHER_INDEX) {
                    if (this.beep) {
                        Toolkit.getDefaultToolkit().beep();
                    }
                    return;
                }
            }
            super.insertString(offset, str, attr);
        }

    }

    /** {@inheritDoc} */
    @Override
	public final int showConfirmDialog(final Object parentComponent,
			                           final Object message,
			                           final String title,
			                           final int optionType,
			                           final int messageType) {

    	final JOptionPane option = new JOptionPane(
    			message,
    			messageType,
				optionType
				);
		final JDialog dialog = option.createDialog(
							parentComponent instanceof Component ? (Component) parentComponent : null,
							title
							);

		if (message instanceof Accessible) {
			dialog.getAccessibleContext().setAccessibleDescription(
					((Accessible) message).getAccessibleContext().getAccessibleDescription());
		} else if (message instanceof String) {
			dialog.getAccessibleContext().setAccessibleDescription((String) message);
		}

		dialog.setVisible(true);

        if (option.getValue() == null) {
        	return JOptionPane.CANCEL_OPTION;
        }
		return (int) option.getValue();
    }

    @Override
   	public void showMessageDialog(final Object parentComponent,
   								  final Object message,
   								  final String title,
   								  final int messageType) {

    	final JOptionPane option = new JOptionPane(
    			message,
    			messageType
				);
		final JDialog dialog = option.createDialog(
							parentComponent instanceof Component ? (Component) parentComponent : null,
							title
							);

		if (message instanceof Accessible) {
			dialog.getAccessibleContext().setAccessibleDescription(
					((Accessible) message).getAccessibleContext().getAccessibleDescription());
		} else if (message instanceof String) {
			dialog.getAccessibleContext().setAccessibleDescription((String) message);
		}

		dialog.setVisible(true);
    }

    @Override
	public void showMessageDialog(final Object parentComponent,
								  final Object message,
								  final String title,
								  final int messageType,
								  final Object icon) {

    	final JOptionPane option = new JOptionPane(
    			message,
    			messageType,
    			JOptionPane.DEFAULT_OPTION,
    			icon instanceof Icon ? (Icon) icon : null
				);
		final JDialog dialog = option.createDialog(
							parentComponent instanceof Component ? (Component) parentComponent : null,
							title
							);

		if (message instanceof JPanel) {
			dialog.getAccessibleContext().setAccessibleDescription(
					((JPanel)message).getAccessibleContext().getAccessibleDescription());
		} else {
			dialog.getAccessibleContext().setAccessibleDescription((String) message);
		}

		dialog.setVisible(true);
    }

    /** {@inheritDoc} */
    @Override
	public final int getPlainMessageCode() {
        return JOptionPane.PLAIN_MESSAGE;
    }

    /** {@inheritDoc} */
    @Override
	public final int getYesNoOptionCode() {
        return JOptionPane.YES_NO_OPTION;
    }

    /** {@inheritDoc} */
    @Override
	public final int getWarningMessageCode() {
        return JOptionPane.WARNING_MESSAGE;
    }

    /** {@inheritDoc} */
    @Override
	public final int getYesOptionCode() {
        return JOptionPane.YES_OPTION;
    }

    /** {@inheritDoc} */
    @Override
	public final int getNoOptionCode() {
        return JOptionPane.NO_OPTION;
    }

    /** {@inheritDoc} */
    @Override
	public final int getOkCancelOptionCode() {
        return JOptionPane.OK_CANCEL_OPTION;
    }

    /** {@inheritDoc} */
    @Override
	public final int getOkOptionCode() {
        return JOptionPane.OK_OPTION;
    }

    /** {@inheritDoc} */
    @Override
	public final int getInformationMessageCode() {
        return JOptionPane.INFORMATION_MESSAGE;
    }

    /** {@inheritDoc} */
    @Override
	public final int getErrorMessageCode() {
        return JOptionPane.ERROR_MESSAGE;
    }

    /** {@inheritDoc} */
    @Override
	public final int getQuestionMessageCode() {
        return JOptionPane.QUESTION_MESSAGE;
    }

    /** {@inheritDoc} */
    @Override
	public File[] getLoadFiles(final String dialogTitle,
							   final String currentDir,
							   final String filename,
                               final String[] extensions,
                               final String description,
                               final boolean selectDirectory,
                               final boolean multiSelect,
                               final Object icon,
                               final Object parent) {

        final Component parentComponent = parent instanceof Component ? (Component) parent : null;

        final JFileChooser jfc;
        if (icon instanceof Image) {
        	jfc = new JFileChooser() {
			private static final long serialVersionUID = 5631612687512882773L;
			   @Override
			    protected JDialog createDialog(final Component p) {
			        final JDialog dialog = super.createDialog(p);
			        dialog.setIconImage((Image) icon);
			        return dialog;
			    }
        	};
        }
        else {
        	if (icon != null) {
        		LOGGER.warning(
    				"Se ha proporcionado un icono del tipo " + icon.getClass().getName() + ", pero solo se admite Image" //$NON-NLS-1$ //$NON-NLS-2$
				);
        	}
        	jfc = new JFileChooser();
        }
        if (selectDirectory) {
        	jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        }

        // Configuramos el directorio y fichero por defecto
        configureDefaultDir(jfc, currentDir, filename);

        jfc.setMultiSelectionEnabled(multiSelect);
        if (dialogTitle != null && dialogTitle.length() > 0) {
            jfc.setDialogTitle(dialogTitle);
        }
        if (extensions != null && extensions.length > 0) {
            jfc.setFileFilter(
        		new FileNameExtensionFilter(
    				description,
    				extensions
				)
    		);
        }
        final int ret = jfc.showOpenDialog(parentComponent);
        if (ret == JFileChooser.APPROVE_OPTION) {
        	final File[] files;
        	if (multiSelect) {
        		files = jfc.getSelectedFiles();
        	}
        	else {
				files = new File[] { jfc.getSelectedFile() };
			}
        	put(PREFERENCE_DIRECTORY, jfc.getCurrentDirectory().getPath());
            return files;
        }
        throw new AOCancelledOperationException();
    }

    /** {@inheritDoc} */
    @Override
	public File saveDataToFile(final byte[] data,
							   final String dialogTitle,
							   final String currentDir,
			                   final String selectedFile,
			                   final List<GenericFileFilter> filters,
			                   final Object parent) throws IOException {

        return saveDataToFile(data, dialogTitle, currentDir, selectedFile, filters, null, parent);
    }

    /** {@inheritDoc} */
    @Override
	public File saveDataToFile(final byte[] data,
							   final String dialogTitle,
							   final String currentDir,
			                   final String selectedFile,
			                   final List<GenericFileFilter> filters,
			                   final GenericFileFilter defaultFilter,
			                   final Object parent) throws IOException {

        final Component parentComponent = parent instanceof Component ? (Component) parent : null;

        boolean tryAgain = true;
        File file = null;
        while (tryAgain) {

            tryAgain = false;
            final JFileChooser fileChooser = new CustomFileChooserForSave();

            // Accesibilidad con textos fijos
            fileChooser.getAccessibleContext().setAccessibleName(JSEUIMessages.getString("JSEUIManager.81")); //$NON-NLS-1$
            fileChooser.getAccessibleContext().setAccessibleDescription(JSEUIMessages.getString("JSEUIManager.82")); //$NON-NLS-1$
            fileChooser.setToolTipText(JSEUIMessages.getString("JSEUIManager.81")); //$NON-NLS-1$

            if (dialogTitle != null) {
            	fileChooser.setDialogTitle(dialogTitle);
            }

            // Solo aplicamos filtros cuando esten definidos, para evitar que el
            // desplegable de la ventana de guardado nos aparecezca vacio
            if (filters != null) {
            	fileChooser.setAcceptAllFileFilterUsed(true);
            	for (final GenericFileFilter gff: filters) {
            		if (gff.getExtensions() != null && gff.getExtensions().length != 0) {
            			// Insertamos el nuevo filtro
            			final FileFilter extFilter = new FileNameExtensionFilter(
    							gff.getDescription(),
    							gff.getExtensions());
            			fileChooser.addChoosableFileFilter(extFilter);
            			// Si es el por defecto, lo seleccionamos
            			if (defaultFilter != null && defaultFilter.equals(gff)) {
            				fileChooser.setFileFilter(extFilter);
            			}
            		}
            	}
            }

            // Configuramos el directorio y fichero por defecto
            configureDefaultDir(fileChooser, currentDir, selectedFile);

            int selectedOption = JOptionPane.YES_OPTION;
            final int returnCode = fileChooser.showSaveDialog(parentComponent);
            switch(returnCode) {

            	case JFileChooser.CANCEL_OPTION:
            		throw new AOCancelledOperationException();

            	case JFileChooser.APPROVE_OPTION:

            		file = fileChooser.getSelectedFile().getCanonicalFile();

            		// Si se han definido filtros y se ha seleccionado uno distinto al filtro
            		// global, si el nombre de fichero no tiene una extension compatible con ese
            		// filtro, le agregamos la primera extension compatible
            		if (filters != null) {
            			final FileFilter ff = fileChooser.getFileFilter();
            			if (fileChooser.getAcceptAllFileFilter() != ff) {
            				if (ff instanceof FileNameExtensionFilter && !ff.accept(file)) {
            					final String exts[] = ((FileNameExtensionFilter)ff).getExtensions();
            					if (exts != null && exts.length > 0 && exts[0] != null) {
            						file = new File(file.getParent(), file.getName() + '.' + exts[0].toLowerCase());
        	                	}
            				}
            			}
            		}

            		// Si el fichero existe, se ofrece el sobreescribirlo
	                if (file.exists()) {
	                	selectedOption = showConfirmDialog(parentComponent,
	                						JSEUIMessages.getString("JSEUIManager.77", file.getAbsolutePath()),  //$NON-NLS-1$
	                						JSEUIMessages.getString("JSEUIManager.85"),  //$NON-NLS-1$
	                						JOptionPane.YES_NO_CANCEL_OPTION,
	                						JOptionPane.INFORMATION_MESSAGE);

	                    if (selectedOption == JOptionPane.CANCEL_OPTION) {
	                        LOGGER.info("Se ha cancelado la operacion de guardado."); //$NON-NLS-1$
	                        throw new AOCancelledOperationException();
	                    }
	                    // Si se ha seleccionado la opcion YES (se desea
	                    // sobreescribir) continuamos
	                    // normalmente con el guardado del fichero
	                }

	                // Si no se desea sobreescribir, se vuelve a mostrar el dialogo
	                // para que se seleccione un nuevo nombre
	                if (selectedOption == JOptionPane.NO_OPTION) {
	                    tryAgain = true;
	                    break;
	                }

	                // Si se proporcionan datos, se guardan y se devuelve el fichero donde se ha hecho.
	                // Si no se proporcionan datos, se devuelve el fichero seleccionado, permitiendo que
	                // el guardado se haga externamente.
	                if (data != null) {
		                try (
	                		final OutputStream fos = new FileOutputStream(file)
	            		) {
	                        fos.write(data);
	                        fos.flush();
	                    }
	                    catch (final Exception ex) {
	                        LOGGER.warning("No se pudo guardar la informacion en el fichero indicado: " + ex); //$NON-NLS-1$
	                        showErrorMessage(
	                            JSEUIMessages.getString("JSEUIManager.88"), //$NON-NLS-1$
	                            JSEUIMessages.getString("JSEUIManager.89"), //$NON-NLS-1$
	                            JOptionPane.ERROR_MESSAGE,
	                            ex
	                        );
	                        // Volvemos a intentar guardar
	                        tryAgain = true;
	                        continue;
	                    }
	                }
                    put(PREFERENCE_DIRECTORY, fileChooser.getCurrentDirectory().getPath());
                    return file;

            	default:
            		throw new IOException("Error al seleccionar el fichero: " + returnCode); //$NON-NLS-1$
            }
        }

        // Devolvemos el path del fichero en el que se han guardado los datos
        return file;
    }

    /** Configura un {@code JFileChooser} para que muestre por defecto un directorio y nombre de fichero.
     * Si no se proporciona el directorio, se leer&aacute; de las preferencias de la aplicaci&oacute;n.
     * En caso de no haber directorio por defecto (es nulo y no est&aacute; en preferencias, se mostrar&aacute;
     * el por defecto de Java (directorio del usuario) y, en caso de no haber nombre de fichero, se dejar&aacute;
     * sin configurar.
     * @param jfc Selector de fichero.
     * @param defaultDir Directorio por defecto.
     * @param filename Nombre de fichero. */
    private static void configureDefaultDir(final JFileChooser jfc,
    		                                final String defaultDir,
    		                                final String filename) {

    	// El metodo setSelectedFile determina el directorio actual y el nombre de fichero
    	try {
    		if (filename != null && defaultDir != null) {
    			jfc.setSelectedFile(new File(defaultDir, filename));
    		}
    		else {
    			final String newDir = defaultDir != null ? defaultDir : get(PREFERENCE_DIRECTORY, null);
    			if (filename != null) {
    				if (newDir != null) {
    					jfc.setSelectedFile(new File(newDir, filename));
    				}
    				else {
    					jfc.setSelectedFile(new File(filename));
    				}
    			}
    			else if (newDir != null) {
    				jfc.setCurrentDirectory(new File(newDir));
    			}
    		}
    	}
    	// Hay extranos casos en los que el mostrar el dialogo de guardado falla con
    	// un IndexOutOfBoundsException al intentar prefijar el directorio y
    	// nombre por defecto del fichero. En esos casos, ignoramos estos parametros
    	catch (final Exception e) {
    		LOGGER.warning("No se pudo seleccionar el directorio por defecto para el guardado: " + e); //$NON-NLS-1$
    		if (filename != null) {
    			try {
    				jfc.setSelectedFile(new File(filename));
    			} catch (final Exception ex) {
    					LOGGER.warning("No se pudo seleccionar nombre de fichero a secas en el dialogo de guardado: " + ex); //$NON-NLS-1$
    			}
    		}
    	}
    }

	@Override
    public void showErrorMessage(final Object message, final String title, final int messageType, final Throwable t) {

		final JFrame errorFrame = new JFrame("Error"); //$NON-NLS-1$
		errorFrame.setBounds(100, 20, 900, 900);

		ErrorManagementDialog.show(errorFrame, true, message, title, messageType, t);
    }


    /**
     * Muestra un di&aacute;logo de error de forma modal. Difiere del normal mostrado con
     * <code>JOptionPane</code> en que, siguiendo la gu&iacute;a de estilo de interfaces de
     * Microsoft, el bot&oacute;n no es "OK", sino cerrar. El comportamiento por lo dem&aacute;s es
     * igual, incluyendo los par&aacute;metros, a <code>JOptionPane</code>
	 * @param parentComponent Componente padre sobre el que mostrar el di&aacute;logo. Puede ser de
	 * tipo Frame o JDialog. Si no, se ignorar&aacute;.
     * @param message Mensaje de error.
     * @param title Titulo de la ventana de error.
     * @param messageType Tipo de mensaje.
     * @param t Informaci&oacute;n sobre el error.
     */
	@Override
    public void showErrorMessage(final Object parentComponent, final Object message, final String title, final int messageType, final Throwable t) {

		if (parentComponent instanceof JDialog) {
			ErrorManagementDialog.show((JDialog) parentComponent, true, message, title, messageType, t);
		} else if (parentComponent instanceof Frame) {
			ErrorManagementDialog.show((Frame) parentComponent, true, message, title, messageType, t);
		} else {
			ErrorManagementDialog.show((Frame) null, true, message, title, messageType, t);
		}
	}

	/** Di&aacute;logo a medida que permite el control de las extensiones
	 * de fichero al cambiar de filtro.
	 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
	private static final class CustomFileChooserForSave extends JFileChooser {

		private static final long serialVersionUID = -2107199679670180110L;

		private File file = null;
	    File getFile() {
	    	return this.file;
	    }

	    CustomFileChooserForSave() {
	    	addPropertyChangeListener(
	    			JFileChooser.FILE_FILTER_CHANGED_PROPERTY,
	    			new PropertyChangeListener() {

						@Override
						public void propertyChange(final PropertyChangeEvent e) {
							if (!(e.getOldValue() instanceof FileNameExtensionFilter) || !(e.getNewValue() instanceof FileNameExtensionFilter)) {
								return;
							}

							final FileNameExtensionFilter oldValue = (FileNameExtensionFilter) e.getOldValue();
							final FileNameExtensionFilter newValue = (FileNameExtensionFilter) e.getNewValue();
							if (
									oldValue.getExtensions() == null || oldValue.getExtensions().length < 1 ||
									newValue.getExtensions() == null || newValue.getExtensions().length < 1
									) {
								return;
							}
							final String extold = oldValue.getExtensions()[0];
							final String extnew = newValue.getExtensions()[0];

							String filename = getFile().getName();
							if (filename.endsWith(extold)) {
								filename = filename.replace(extold, extnew);
							} else if (!filename.endsWith(extnew)) {
								filename += extnew;
							}
							setSelectedFile(new File(filename));
						}
					});
	    }

	    @Override
	    public void setSelectedFile(final File file) {
	        super.setSelectedFile(file);
            this.file = file;
	    }

	}
}
