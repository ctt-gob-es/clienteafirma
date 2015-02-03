/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.ui.core.jse;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.logging.Logger;

import javax.swing.Icon;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIManager;
import es.gob.afirma.core.ui.KeyStoreDialogManager;

/** Gestor de componentes de interfaz gr&aacute;fico (tanto para Applet como para
 * aplicaci&oacute;n de escritorio) de la aplicaci&oacute;n.
 * @version 0.4 */
public class JSEUIManager implements AOUIManager {

    protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final int ASCII_LOWER_INDEX = 32;
    private static final int ASCII_HIGHER_INDEX = 126;

    /** Pregunta al usuario por una contrase&ntilde;a.
     * @param text Texto que se muestra en el di&aacute;logo para pedir la contrase&ntilde;a
     * @param c Componente padre (para la modalidad)
     * @return Contrase&ntilde;a introducida por el usuario
     * @throws AOCancelledOperationException Cuando el usuario cancela el proceso de solicitud de
     *         contrase&ntilde;a */
    @Override
	public final char[] getPassword(final String text, final Object c) {
        return getPassword(text, null, null, false, c);
    }

    /** Muestra un di&aacute;logo para pedir una contrase&ntilde;a al usuario.
     * @param text Texto con el que se solicitar&aacute; la entrada de texto al
     *             usuario (<i>prompt</i>)
     * @param imageIcon Objeto de tipo {@code javax.swing.Icon} con el icono del di&aacute;logo o
     * 			   {@code null} para no mostrar icono.
     * @param charSet Juego de caracteres aceptados para la contrase&ntilde;a
     * @param beep <code>true</code> si se desea un sonido de advertencia al
     *             introducir un caracter no v&aacute;lido, <code>false</code> en
     *             caso contrario
     * @param c Componente padre (para la modalidad)
     * @return Array de caracteres del texto introducido como contrase&ntilde;a
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancela o cierra el di&aacute;logo */
    @Override
	public final char[] getPassword(final String text, final Object imageIcon, final String charSet, final boolean beep, final Object c) {
        final JPasswordField pwd = new JPasswordField(10);
        if (charSet != null) {
            pwd.setDocument(new JTextFieldFilter(charSet, beep));
        }
        final JLabel lbText = new JLabel(text != null ? text : JSEUIMessages.getString("JSEUIManager.24")); //$NON-NLS-1$
        lbText.setMinimumSize(new Dimension(lbText.getFontMetrics(lbText.getFont()).stringWidth(text), lbText.getSize().height));
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

        Icon icon = null;
        if (imageIcon instanceof javax.swing.Icon) {
        	icon = (javax.swing.Icon) imageIcon;
        }

        final JOptionPane pane = new JOptionPane(panel, JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_CANCEL_OPTION, icon) {
            private static final long serialVersionUID = -3012522768561175760L;

            /** {@inheritDoc} */
            @Override
            public void selectInitialValue() {
                pwd.requestFocusInWindow();
            }
        };

        Component parent = null;
        if (c instanceof Component) {
            parent = (Component) c;
        }

        pane.createDialog(parent, JSEUIMessages.getString("JSEUIManager.24")).setVisible(true); //$NON-NLS-1$

        final Object selectedValue = pane.getValue();
        if (selectedValue == null) {
            return new char[0];
        }
        if (((Integer) selectedValue).intValue() == JOptionPane.OK_OPTION) {
            return pwd.getPassword();
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
        Component parent = null;
        if (parentComponent instanceof Component) {
            parent = (Component) parentComponent;
        }
        Icon dialogIcon = null;
        if (icon instanceof Icon) {
            dialogIcon = (Icon) icon;
        }
        if (selectionValues == null) {
        	return JOptionPane.showInputDialog(parent, message, title, messageType);
        }
        return JOptionPane.showInputDialog(parent, message, title, messageType, dialogIcon, selectionValues, initialSelectionValue);
    }

    /** {@inheritDoc} */
    @Override
	public final String showCertificateSelectionDialog(final Object parentComponent,
    												   final KeyStoreDialogManager ksdm) {

    	Component parent = null;
    	if (parentComponent instanceof Component) {
    		parent = (Component) parentComponent;
    	}

    	try {
    		final Class<?> csdClass = Class.forName(
    				"es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog" //$NON-NLS-1$
    		);
    		final Constructor<?> csdConstructor =
    				csdClass.getConstructor(
    						Component.class,
    						KeyStoreDialogManager.class
    						);
    		final Object csd =  csdConstructor.newInstance(parent, ksdm);
    		final Method showDialogMethod = csdClass.getMethod("showDialog"); //$NON-NLS-1$

    		final Object result = showDialogMethod.invoke(csd);

    		return result instanceof String ? (String) result : null;
    	}
    	catch (final InvocationTargetException e) {
    		LOGGER.severe("Ocurrio un error al extraer el certificado seleccionado: " + e); //$NON-NLS-1$
    		if (e.getCause() instanceof RuntimeException) {
    			throw (RuntimeException) e.getCause();
    		}
    		throw new IllegalStateException(
				"Ocurrio un error durante al extraer el certificado seleccionado: " + e, e.getCause() //$NON-NLS-1$
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
         * @param acceptedchars
         *        Cadena que debe contener todos los caracteres aceptados.
         *        Cualquier caracter no incluido en esta cadena ser&aacute;
         *        considerado inv&aacute;lido
         * @param beepOnError
         *        <code>true</code> si desea que se reproduzca un sonido
         *        cuando el usuario introduce un caracter no v&aacute;lido,
         *        false en caso contrario */
        JTextFieldFilter(final String acceptedchars, final boolean beepOnError) {
            this.beep = beepOnError;
            this.acceptedChars = acceptedchars;
        }

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

    /** Filtro de caracteres ASCCI imprimibles. */
    public static final class JTextFieldASCIIFilter extends PlainDocument {

        private static final long serialVersionUID = 1979726487852842735L;

        private boolean beep = false;

        /** Crea un nuevo filtro para campo de entrada de texto.
         * @param beepOnError
         *        <code>true</code> si desea que se reproduzca un sonido
         *        cuando el usuario introduce un caracter no v&aacute;lido,
         *        false en caso contrario */
        public JTextFieldASCIIFilter(final boolean beepOnError) {
            this.beep = beepOnError;
        }

        /** {@inheritDoc} */
        @Override
        public void insertString(final int offset, final String str, final AttributeSet attr) throws BadLocationException {
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
        Component parent = null;
        if (parentComponent instanceof Component) {
            parent = (Component) parentComponent;
        }
        return JOptionPane.showConfirmDialog(parent, message, title, optionType, messageType);
    }

    @Override
	public void showMessageDialog(final Object parentComponent, final Object message, final String title, final int messageType) {
        Component parent = null;
        if (parentComponent instanceof Component) {
            parent = (Component) parentComponent;
        }
        JOptionPane.showMessageDialog(parent, message, title, messageType);
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
        Component parentComponent = null;
        if (parent instanceof Component) {
            parentComponent = (Component) parent;
        }

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
        	jfc = new JFileChooser();
        }
        if (selectDirectory) {
        	jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        }
        if (currentDir != null) {
        	jfc.setCurrentDirectory(new File(currentDir));
        }
        if (filename != null) {
        	if (currentDir != null) {
        		jfc.setSelectedFile(new File(currentDir, filename));
        	}
        	else {
        		jfc.setSelectedFile(new File(filename));
        	}
        }

        jfc.setMultiSelectionEnabled(multiSelect);
        if (dialogTitle != null && dialogTitle.length() > 0) {
            jfc.setDialogTitle(dialogTitle);
        }
        if (extensions != null && extensions.length > 0) {
            jfc.setFileFilter(new ExtFilter(extensions, description));
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
			                   final String[] exts,
			                   final String description,
			                   final Object parent) throws IOException {

        if (data == null) {
            throw new IllegalArgumentException("No se introdujeron datos que almacenar"); //$NON-NLS-1$
        }

        Component parentComponent = null;
        if (parent instanceof Component) {
            parentComponent = (Component) parent;
        }

        final File resultFile = null;
        boolean tryAgain = true;
        File file;
        while (tryAgain) {

            tryAgain = false;
            final JFileChooser fileChooser = new JFileChooser();

            // Accesibilidad con textos fijos
            fileChooser.getAccessibleContext().setAccessibleName(JSEUIMessages.getString("JSEUIManager.81")); //$NON-NLS-1$
            fileChooser.getAccessibleContext().setAccessibleDescription(JSEUIMessages.getString("JSEUIManager.82")); //$NON-NLS-1$
            fileChooser.setToolTipText(JSEUIMessages.getString("JSEUIManager.81")); //$NON-NLS-1$

            if (dialogTitle != null) {
            	fileChooser.setDialogTitle(dialogTitle);
            }

            // El metodo setSelectedFile determina tambien el directorio actual, asi que lo usamos cuando
            // se indica el nombre de fichero
            if (selectedFile != null && currentDir != null) {
            	fileChooser.setSelectedFile(new File(currentDir, selectedFile));
            } else if (selectedFile != null) {
            	fileChooser.setSelectedFile(new File(selectedFile));
            } else if (currentDir != null) {
            	fileChooser.setCurrentDirectory(new File(currentDir));
            }

            // Solo aplicamos el filtro cuando este definido para evitar que el
            // desplegable de la ventana de guardado nos aparecezca vacio
            if (exts != null && exts.length > 0) {
                fileChooser.setFileFilter(new FileExtensionFilter(exts, description));
            }

            int selectedOption = JOptionPane.YES_OPTION;
            final int returnCode = fileChooser.showSaveDialog(parentComponent);
            switch(returnCode) {
            	case JFileChooser.CANCEL_OPTION:
            		throw new AOCancelledOperationException();
            	case JFileChooser.APPROVE_OPTION:
	                file = fileChooser.getSelectedFile();
	                if (file.exists()) {
	                    selectedOption = JOptionPane.showConfirmDialog(
                    		parentComponent,
	                        JSEUIMessages.getString("JSEUIManager.77", file.getAbsolutePath()), //$NON-NLS-1$
	                        JSEUIMessages.getString("JSEUIManager.85"), //$NON-NLS-1$
	                        JOptionPane.YES_NO_CANCEL_OPTION
                        );
	                    if (selectedOption == JOptionPane.CANCEL_OPTION) {
	                        LOGGER.info("Se ha cancelado la operacion de guardado."); //$NON-NLS-1$
	                        return null;
	                    }
	                    // Si se ha seleccionado la opcion YES (se desea
	                    // sobreescribir) continuamos
	                    // normalmente con el guardado del fichero
	                }

	                if (selectedOption == JOptionPane.NO_OPTION) {
	                    tryAgain = true;
	                    break;
	                }
	                // Hemos seleccionado la opcion de sobreescribir
                    try {
                    	final FileOutputStream fos = new FileOutputStream(file);
                        fos.write(data);
                        fos.flush();
                        fos.close();
                    }
                    catch (final Exception ex) {
                        LOGGER.warning("No se pudo guardar la informacion en el fichero indicado: " + ex); //$NON-NLS-1$
                        JOptionPane.showMessageDialog(
                    		parentComponent,
                            JSEUIMessages.getString("JSEUIManager.88"), //$NON-NLS-1$
                            JSEUIMessages.getString("JSEUIManager.89"), //$NON-NLS-1$
                            JOptionPane.ERROR_MESSAGE
                        );
                        // Volvemos a intentar guardar
                        tryAgain = true;
                    }
                    return file;
			default:
				throw new IOException("Error al seleccionar el fichero: " + returnCode); //$NON-NLS-1$



            }
        }

        // Devolvemos el path del fichero en el que se han guardado los datos
        return resultFile;
    }

    /** Filtra los ficheros por extensi&oacute;n para los di&aacute;logos de
     * carga y guardado. Se declara como p&uacute;blico para que pueda ser usado
     * tambi&eacute;n por el interfaz de aplicaci&oacute;n de escritorio. No
     * usamos <code>FileNameExtensionFilter</code> directamente para
     * compatibilizar con Java 1.4
     * @version 0.3 */
    private static final class ExtFilter extends FileFilter implements java.io.FileFilter {

        private final String[] extensions;
        private final String description;

        /** Construye un filtro para la selecci&oacute;n de ficheros en un <code>JFileChooser</code>.
         * @param exts
         *        Extensiones de fichero permitidas
         * @param desc
         *        Descripci&oacute;n del tipo de fichero correspondiente a
         *        las extensiones */
        public ExtFilter(final String[] exts, final String desc) {
            if (exts == null || exts.length < 1) {
                throw new IllegalArgumentException("No se puede crear un filtro vacio"); //$NON-NLS-1$
            }
            this.extensions = exts.clone();
            this.description = desc != null ? desc : JSEUIMessages.getString("JSEUIManager.0"); //$NON-NLS-1$
        }

        /** {@inheritDoc} */
        @Override
        public boolean accept(final File f) {
            if (f.isDirectory()) {
                return true;
            }
            // getExtension() pasa la extension a minusculas, no hace falta
            // el "ignoreCase"
            final String extension = getExtension(f);
            for (final String extension2 : this.extensions) {
                if (extension2.equalsIgnoreCase(extension)) {
                    return true;
                }
            }
            return false;
        }

        /** {@inheritDoc} */
        @Override
        public String getDescription() {
            return this.description;
        }

        /** Devuelve la extensi&oacute;n de un fichero.
         * @param f
         *        Fichero del cual queremos conocer la extensi&oacute;n
         * @return Extensi&oacute;n del fichero o cadena vac&iacute;a si este no
         *         tiene extensi&oacute;n */
        private static String getExtension(final File f) {
            final String s = f.getName();
            final int i = s.lastIndexOf('.');
            if (i > 0 && i < s.length() - 1) {
                return s.substring(i + 1).toLowerCase();
            }
            return ""; //$NON-NLS-1$
        }

    }

	@Override
    public void showErrorMessage(final Object parent, final Object message, final String title, final int messageType) {
        final String buttonTxt = JSEUIMessages.getString("JSEUIManager.1"); //$NON-NLS-1$
        JOptionPane.showOptionDialog(
                parent instanceof Component ? (Component) parent : null,
                message,
                title,
                JOptionPane.OK_OPTION,
                messageType,
                null,
                new String[] {
            		buttonTxt
                },
                buttonTxt
        );
    }
}
