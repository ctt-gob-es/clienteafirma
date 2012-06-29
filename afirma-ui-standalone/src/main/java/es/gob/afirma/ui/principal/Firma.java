/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.text.Caret;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.KeyStoreConfiguration;
import es.gob.afirma.keystores.main.common.KeyStoreUtilities;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xmldsig.AOXMLDSigSigner;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.ExtFilter;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.SignedFileManager;
import es.gob.afirma.ui.utils.UIPasswordCallbackAccessibility;
import es.gob.afirma.ui.utils.Utils;

/** Clase que muestra los elementos necesarios para realizar una firma. */
final class Firma extends JPanel {

    private static Logger logger = Logger.getLogger(Firma.class.getName());

    private static final long serialVersionUID = 1L;

    // Nombres de los diferentes formatos de firmado
    private final List<String> formatosL = new ArrayList<String>(Arrays.asList("Firma est\u00E1ndar (XAdES Detached)", //$NON-NLS-1$
                                                                               // "XAdES Enveloping",
                                                                               // "XAdES Enveloped",
                                                                               "CAdES", //$NON-NLS-1$
                                                                               "PAdES" //$NON-NLS-1$
    ));

    // Constantes de los diferentes formatos de firmado
    private final List<String> formatosV = new ArrayList<String>(Arrays.asList(AOSignConstants.SIGN_FORMAT_XADES_DETACHED,
    // AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
    // AOConstants.SIGN_FORMAT_XADES_ENVELOPED,
                                                                               AOSignConstants.SIGN_FORMAT_CADES,
                                                                               AOSignConstants.SIGN_FORMAT_PDF));

    Firma() {
        initComponents();
    }

    /** Carga el combo de almacen y repositorios
     * @param comboAlmacen Combo donde se guarda la lista */
    private static void cargarComboAlmacen(final JComboBox comboAlmacen) {
        comboAlmacen.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
    }

    /** Pulsar boton examinar: Muestra una ventana para seleccinar un archivo.
     * Modifica el valor de la caja con el nombre del archivo seleccionado
     * @param campoFichero Campo en el que se escribe el nombre del fichero seleccionado */
    void examinarActionPerformed(final JTextField campoFichero) {
        final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("PrincipalGUI.chooser.title")); //$NON-NLS-1$
        if (selectedFile != null) {
            campoFichero.setText(selectedFile.getAbsolutePath());
        }
    }

    /** Firma el fichero seleccionado
     * @param comboAlmacen Combo con el almacen / repositorio de certificados
     * @param comboFormato Combo con los formatos de firmado
     * @param campoFichero Campo con el nombre del archivo a firmar */
    void firmarActionPerformed(final JComboBox comboAlmacen, final JComboBox comboFormato, final JTextField campoFichero) {// GEN-FIRST:event_firmarActionPerformed
        // Obtenemos la constante del formato a utilizar
        final String formato = this.formatosV.get(comboFormato.getSelectedIndex());

        // Obtenemos la ruta del fichero a firmar
        if (campoFichero.getText() == null || campoFichero.getText().equals("")) { //$NON-NLS-1$
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            campoFichero.requestFocusInWindow();
            return;
        }

        // Mensaje que indica que se va a realizar el proceso de firma y que puede llevar un tiempo
        CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                       true,
                                       Messages.getString("Firma.msg.info"), Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$

        try {
            PasswordCallback pssCallback;
            final KeyStoreConfiguration kssc = (KeyStoreConfiguration) comboAlmacen.getSelectedItem();
            final AOKeyStore store = kssc.getType();
            String lib = kssc.getLib();
            if (store == AOKeyStore.WINDOWS || store == AOKeyStore.WINROOT || store == AOKeyStore.SINGLE) {
                pssCallback = new NullPasswordCallback();
            }
            else if (store == AOKeyStore.DNIEJAVA) {
                pssCallback = null;
            }
            else if (store == AOKeyStore.PKCS12) {
                pssCallback =
                        new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription() + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                                            SwingUtilities.getRoot(this),
                                                            Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title") //$NON-NLS-1$ //$NON-NLS-2$
                        );
                final File selectedFile =
                        SelectionDialog.showFileOpenDialog(this, Messages.getString("Open.repository"), (ExtFilter) Utils.getRepositoryFileFilter()); //$NON-NLS-1$
                if (selectedFile != null) {
                    lib = selectedFile.getAbsolutePath();
                }
                else {
                    return;
                }
            }
            else {
                pssCallback =
                        new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription() + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                                            SwingUtilities.getRoot(this),
                                                            Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title") //$NON-NLS-1$ //$NON-NLS-2$
                        );
            }

            // Keystore
            final AOKeyStoreManager keyStoreManager;

            try {
                keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(store, lib, kssc.toString(), pssCallback, this);
            }
            catch (final IOException e) {
            	// Condiciones especificas para el proveedor Java de DNIe
            	final String msg;
            	if (e.getClass().getName().equals("es.gob.jmulticard.card.dnie.BurnedDnieCardException")) { //$NON-NLS-1$
            		msg = Messages.getString("Firma.msg.error.dnie.BurnedDnieCardException"); //$NON-NLS-1$
            	}
            	else if (e.getClass().getName().equals("es.gob.jmulticard.card.InvalidCardException")) { //$NON-NLS-1$
            		msg = Messages.getString("Firma.msg.error.dnie.InvalidCardException"); //$NON-NLS-1$
            	}
            	else if (e.getClass().getName().equals("es.gob.jmulticard.apdu.connection.CardNotPresentException")) { //$NON-NLS-1$
            		msg = Messages.getString("Firma.msg.error.dnie.CardNotPresentException"); //$NON-NLS-1$
            	}
            	else if (e.getClass().getName().equals("es.gob.jmulticard.apdu.connection.NoReadersFoundException")) { //$NON-NLS-1$
            		msg = Messages.getString("Firma.msg.error.dnie.NoReadersFoundException"); //$NON-NLS-1$
            	}
            	else if ("es.gob.jmulticard.apdu.connection.ApduConnectionException".equals(e.getClass().getName())) { //$NON-NLS-1$
            	    msg = Messages.getString("Firma.msg.error.dnie.ApduConnectionException"); //$NON-NLS-1$
            	}
            	else {
            	    msg = Messages.getString("Firma.msg.error.contrasenia"); //$NON-NLS-1$
            	}

                // Control de la excepcion generada al introducir mal la contrasena para el almacen
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, msg,
                								Messages.getString("error"), //$NON-NLS-1$
                								JOptionPane.ERROR_MESSAGE);
                return;
            }
            catch (final Exception e) {
            	e.printStackTrace();
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.almacen"), //$NON-NLS-1$
                                               Messages.getString("error"), //$NON-NLS-1$
                                               JOptionPane.ERROR_MESSAGE);
                return;
            }


            // Recuperamos la clave del certificado
            final PrivateKeyEntry privateKeyEntry;
            try {
            	// Seleccionamos un certificado
            	final String selectedcert =
            		Utils.showCertSelectionDialog(keyStoreManager.getAliases(),
            				keyStoreManager,
            				SwingUtilities.getRoot(this),
            				true,
            				true,
            				true,
            				null,
            				false);

            	// Comprobamos si se ha cancelado la seleccion
            	if (selectedcert == null) {
            		throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$
            	}

            	privateKeyEntry = keyStoreManager.getKeyEntry(selectedcert, KeyStoreUtilities.getCertificatePC(store, SwingUtilities.getRoot(this)));
            }
            catch (final java.security.ProviderException e) {
            	// Comprobacion especifica para el proveedor Java de DNIe
            	if (e.getCause() != null && e.getCause().getClass().getName().equals("es.gob.jmulticard.card.AuthenticationModeLockedException")) { //$NON-NLS-1$
            		CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                            true,
                            Messages.getString("Firma.msg.error.dnie.AuthenticationModeLockedException"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            		return;
            	}
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                               true,
                                               Messages.getString("Firma.msg.error.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
                return;
            }
            catch (final java.security.UnrecoverableEntryException e) {
                // Control de la excepcion generada al introducir mal la contrasena para el certificado
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                               true,
                                               Messages.getString("Firma.msg.error.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
                return;
            }
            catch (final AOCancelledOperationException e) {
                // Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
                // Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
                // que las relanza en forma de AOException
                throw e;
            }
            catch (final Exception e) {
            	e.printStackTrace();
                throw new AOException("No se ha podido recuperar el certificado seleccionado", e.getCause()); //$NON-NLS-1$
            }

            if (privateKeyEntry == null) {
                throw new KeyException("No se pudo obtener la informacion del certificado, no se firmara el fichero"); //$NON-NLS-1$
            }

            // Firmamos los datos
            final AOSigner signer;
            try {
                signer = AOSignerFactory.getSigner(formato);
            }
            catch (final Exception e) {
                logger.warning("Formato de firma no soportado: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                               true,
                                               Messages.getString("Firma.msg.error.formato"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$//$NON-NLS-2$
                setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return;
            }

            final URI uri;
            try {
                uri = AOUtil.createURI(campoFichero.getText());
            }
            catch (final Exception e) {
                logger.severe("La ruta del fichero de datos no es valida: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                               true,
                                               Messages.getString("Firma.msg.error.ruta"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
                setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                campoFichero.requestFocusInWindow();
                return;
            }

            byte[] fileData;
            InputStream fileIn = null;
            try {
                fileIn = AOUtil.loadFile(uri);
                fileData = AOUtil.getDataFromInputStream(fileIn);
            }
            catch (final FileNotFoundException e) {
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero.noencontrado"), //$NON-NLS-1$
                                               Messages.getString("error"), //$NON-NLS-1$
                                               JOptionPane.ERROR_MESSAGE);
                return;
            }
            catch (final IOException e) {
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero.leer"), //$NON-NLS-1$
                                               Messages.getString("error"), //$NON-NLS-1$
                                               JOptionPane.ERROR_MESSAGE);
                return;
            }
            finally {
                setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                if (fileIn != null) {
                    try {
                        fileIn.close();
                    }
                    catch (final Exception e) {
                        /* Ignorada */
                    }
                }
            }

            // Se introduce la logica necesaria para que no se pueda firmar en formato XAdES o XMLdSign
            // una factura electronica ya firmada
            if (signer instanceof AOXAdESSigner || signer instanceof AOXMLDSigSigner) {
            	if (new AOFacturaESigner().isSign(fileData)) {
            		CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
            				true, Messages.getString("Firma.dialog.msg"), //$NON-NLS-1$
            				Messages.getString("Firma.dialog.title"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$
            		return;
            	}
            }

            // En el caso de firma CAdES, preguntamos al usuario si desea incluir el documento que
            // se firma en la propia firma. El documento se incluira en la firma, salvo que se indique
            // los contrario
            String modoFirma = AOSignConstants.SIGN_MODE_IMPLICIT;
            if (formato.equals(AOSignConstants.SIGN_FORMAT_CADES)) {
            	final int incluir = CustomDialog.showConfirmDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.incluir.original"), //$NON-NLS-1$
            			Messages.getString("Firma.dialog.title"), //$NON-NLS-1$
            			JOptionPane.YES_NO_OPTION,
            			JOptionPane.INFORMATION_MESSAGE);
                modoFirma = (incluir == JOptionPane.NO_OPTION ? AOSignConstants.SIGN_MODE_EXPLICIT : AOSignConstants.SIGN_MODE_IMPLICIT);
            }

            final Properties prop = GeneralConfig.getSignConfig();

            prop.setProperty("format", formato); //$NON-NLS-1$
            prop.setProperty("mode", modoFirma); //$NON-NLS-1$
            prop.setProperty("uri", uri.toASCIIString()); //$NON-NLS-1$

            // En el caso de firmas XAdES no incluimos la cadena de certificacion
            if (signer instanceof AOXAdESSigner) {
            	prop.setProperty("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            }

            final byte[] signedData;
            try {
                signedData = signer.sign(fileData, GeneralConfig.getSignAlgorithm(), privateKeyEntry, prop);
            }
            catch (final AOFormatFileException e) {
                logger.severe("Error al generar la firma electronica: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                               true,
                                               Messages.getString("Firma.msg.error.generar.formato"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
                return;
            }
            catch (final AOException e) {
                logger.severe("Error al generar la firma electronica: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                               true,
                                               Messages.getString("Firma.msg.error.generar.firma"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
                return;
            }
            catch (final Exception e) {
                logger.severe("Error al generar la firma electronica: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                               true,
                                               Messages.getString("Firma.msg.error.generar.firma"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
                return;
            }

            // Si el proceso de firma devuelve una firma nula o vacia, lanzamos una excepcion
            if (signedData == null || signedData.length == 0) {
                throw new AOException("La firma generada esta vacia"); //$NON-NLS-1$
            }

            // Guardamos la firma en fichero
            final File savedFile =
                    SelectionDialog.saveDataToFile(Messages.getString("Firma.filechooser.save.title"), //$NON-NLS-1$
                                                   signedData,
                                                   SignedFileManager.getOutFileName(Firma.getFilename(campoFichero.getText()), formato),
                                                   SignedFileManager.getOutFileFilter(formato),
                                                   this);

            if (savedFile != null) {
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.ok"), //$NON-NLS-1$
                                               Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma"), //$NON-NLS-1$
                                               JOptionPane.INFORMATION_MESSAGE);
            }
        }
        catch (final AOCancelledOperationException e) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
        }
        catch (final AOException e) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            logger.severe("Error: " + e.getMessage()); //$NON-NLS-1$
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, e.getMessage(), "Firma", JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
        }
        catch (final Exception e) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            logger.severe("Error al generar la firma electronica: " + e); //$NON-NLS-1$
        }
    }

    /** Obtiene el nombre del archivo desde el path
     * @param path Path del archivo
     * @return Nombre del archivo */
    private static String getFilename(final String path) {
        final int i = path.lastIndexOf(System.getProperty("file.separator")); //$NON-NLS-1$
        if (i > 0 && i < path.length() - 1) {
            return path.substring(i + 1);
        }
        return path;
    }

    /** Inicializacion de los componentes */
    private void initComponents() {
        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.gridx = 0;

        // Etiqueta fichero a firmar digitalmente
        final JLabel etiquetaFichero = new JLabel();
        etiquetaFichero.setText(Messages.getString("Firma.buscar")); // NOI18N //$NON-NLS-1$
        etiquetaFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Firma.buscar.description")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFichero);
        Utils.setFontBold(etiquetaFichero);
        add(etiquetaFichero, c);

        c.insets = new Insets(0, 13, 0, 0);
        c.gridwidth = 1;
        c.weightx = 1.0;
        c.gridy = 1;

        // Campo donde se guarda el nombre del fichero a firmar
        final JTextField campoFichero = new JTextField();
        campoFichero.setToolTipText(Messages.getString("Firma.buscar.caja.description")); // NOI18N //$NON-NLS-1$
        campoFichero.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.buscar.caja.description.status"))); //$NON-NLS-1$
        campoFichero.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.buscar.caja.description.status"))); //$NON-NLS-1$
        campoFichero.getAccessibleContext().setAccessibleName(etiquetaFichero.getText() + " " + "ALT + G."); //$NON-NLS-1$ //$NON-NLS-2$
        campoFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Firma.buscar.caja.description")); // NOI18N //$NON-NLS-1$
        campoFichero.addAncestorListener(new RequestFocusListener(false));
        Utils.remarcar(campoFichero);
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            campoFichero.setCaret(caret);
        }
        Utils.setFontBold(campoFichero);
        add(campoFichero, c);

        // Relacion entre etiqueta y campo de texto
        etiquetaFichero.setLabelFor(campoFichero);
        // Asignacion de mnemonico
        etiquetaFichero.setDisplayedMnemonic(KeyEvent.VK_G);

        c.insets = new Insets(0, 10, 0, 13);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton examinar
        final JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setText(Messages.getString("PrincipalGUI.Examinar")); // NOI18N //$NON-NLS-1$
        examinar.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N //$NON-NLS-1$
        examinar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                      Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        examinar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                      Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        // examinar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.Examinar") + " " +
        // Messages.getString("PrincipalGUI.Examinar.description.status"));
        examinar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarActionPerformed(campoFichero);
            }
        });
        Utils.remarcar(examinar);
        Utils.setContrastColor(examinar);
        Utils.setFontBold(examinar);

        panelExaminar.add(examinar);
        add(panelExaminar, c);

        // Espacio en blanco
        final JPanel emptyPanel01 = new JPanel();
        emptyPanel01.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 2;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel01, c);

        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridwidth = 2;
        c.gridx = 0;
        c.gridy = 3;

        // Etiqueta para el Almacen / repositorio
        final JLabel etiquetaAlmacen = new JLabel();
        etiquetaAlmacen.setText(Messages.getString("Firma.almacen.certificados")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAlmacen);
        Utils.setFontBold(etiquetaAlmacen);
        add(etiquetaAlmacen, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.weightx = 1.0;
        c.gridy = 4;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con las opciones del almacen o repositorio
        final JComboBox comboAlmacen = new JComboBox();
        comboAlmacen.setToolTipText(Messages.getString("Firma.almacen.certificados.description")); // NOI18N //$NON-NLS-1$
        comboAlmacen.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.almacen.certificados.description"))); //$NON-NLS-1$
        comboAlmacen.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.almacen.certificados.description"))); //$NON-NLS-1$
        // comboAlmacen.getAccessibleContext().setAccessibleName(etiquetaAlmacen.getText()+" "+Messages.getString("Firma.almacen.certificados.description")
        // + " ALT + A."); // NOI18N

        Utils.remarcar(comboAlmacen);
        cargarComboAlmacen(comboAlmacen);
        Utils.setContrastColor(comboAlmacen);
        Utils.setFontBold(comboAlmacen);
        add(comboAlmacen, c);

        // Relacion entre etiqueta y combo
        etiquetaAlmacen.setLabelFor(comboAlmacen);
        // Asignacion de mnemonico
        etiquetaAlmacen.setDisplayedMnemonic(KeyEvent.VK_A);

        // Espacio en blanco
        final JPanel emptyPanel02 = new JPanel();
        emptyPanel02.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 5;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel02, c);

        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridy = 6;
        c.weighty = 0.0;
        c.fill = GridBagConstraints.HORIZONTAL;

        // Etiqueta formato / formato
        final JLabel etiquetaFormato = new JLabel();
        etiquetaFormato.setText(Messages.getString("Firma.formato")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFormato);
        Utils.setFontBold(etiquetaFormato);
        add(etiquetaFormato, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.weightx = 1.0;
        c.gridy = 7;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con los diferentes formatos de firma
        final JComboBox comboFormato = new JComboBox();
        comboFormato.setToolTipText(Messages.getString("Firma.formato.description")); // NOI18N //$NON-NLS-1$
        comboFormato.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.formato.description.status"))); //$NON-NLS-1$
        comboFormato.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.formato.description.status"))); //$NON-NLS-1$
        // comboFormato.getAccessibleContext().setAccessibleName(etiquetaFormato.getText()+" " + Messages.getString("Firma.formato.description") +
        // " ALT + O."); // NOI18N

        if (GeneralConfig.isAvanzados()) {
            // XAdES Enveloping (Solo en la vista avanzada)
            this.formatosL.add("XAdES Enveloping"); //$NON-NLS-1$
            this.formatosV.add(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);
            // XAdES Enveloped (Solo en la vista avanzada)
            this.formatosL.add("XAdES Enveloped"); //$NON-NLS-1$
            this.formatosV.add(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
            // OOXML (Solo en la vista avanzada)
            this.formatosL.add("OOXML"); //$NON-NLS-1$
            this.formatosV.add(AOSignConstants.SIGN_FORMAT_OOXML);
            // ODF (Solo en la vista avanzada)
            this.formatosL.add("ODF"); //$NON-NLS-1$
            this.formatosV.add(AOSignConstants.SIGN_FORMAT_ODF);
        }
        comboFormato.setModel(new DefaultComboBoxModel(this.formatosL.toArray()));
        Utils.remarcar(comboFormato);
        Utils.setContrastColor(comboFormato);
        Utils.setFontBold(comboFormato);
        add(comboFormato, c);

        // Relacion entre etiqueta y combo
        etiquetaFormato.setLabelFor(comboFormato);
        // Asignacion de mnemonico
        etiquetaFormato.setDisplayedMnemonic(KeyEvent.VK_O);

        c.weighty = 1.0;
        c.gridy = 8;
        c.gridheight = 4;
        c.fill = GridBagConstraints.HORIZONTAL;

        // Panel vacio para alinear el boton de aceptar en la parte de abajo de la pantalla
        final JPanel emptyPanel = new JPanel();
        add(emptyPanel, c);

        // Panel con los botones
        final JPanel panelBotones = new JPanel(new GridBagLayout());

        final GridBagConstraints cons = new GridBagConstraints();
        cons.anchor = GridBagConstraints.FIRST_LINE_START; // control de la orientacion de componentes al redimensionar
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.ipadx = 15;
        cons.gridx = 0;

        // Etiqueta para rellenar a la izquierda
        final JLabel label = new JLabel();
        panelBotones.add(label, cons);

        final JPanel panelFirmar = new JPanel(new GridLayout(1, 1));
        // Boton firmar
        final JButton firmar = new JButton();
        firmar.setMnemonic(KeyEvent.VK_R);
        firmar.setText(Messages.getString("PrincipalGUI.firmar")); // NOI18N //$NON-NLS-1$
        firmar.setToolTipText(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N //$NON-NLS-1$
        // firmar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.firmar") + " " +
        // Messages.getString("PrincipalGUI.firmar.description.status"));
        firmar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                    Messages.getString("PrincipalGUI.firmar.description.status"))); //$NON-NLS-1$
        firmar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                    Messages.getString("PrincipalGUI.firmar.description.status"))); //$NON-NLS-1$
        firmar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                firmarActionPerformed(comboAlmacen, comboFormato, campoFichero);
            }
        });
        firmar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(firmar);
        Utils.setContrastColor(firmar);
        Utils.setFontBold(firmar);

        cons.ipadx = 0;
        cons.gridx = 1;
        cons.weightx = 1.0;

        panelFirmar.add(firmar);
        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelFirmar, BorderLayout.CENTER);
        panelBotones.add(buttonPanel, cons);

        cons.ipadx = 15;
        cons.weightx = 0.0;
        cons.gridx = 2;

        final JPanel panelAyuda = new JPanel();
        // Boton ayuda
        final JButton botonAyuda = HelpUtils.helpButton("firma"); //$NON-NLS-1$
        botonAyuda.setName("helpButton"); //$NON-NLS-1$
        panelAyuda.add(botonAyuda);
        panelBotones.add(panelAyuda, cons);

        c.gridwidth = 2;
        c.insets = new Insets(13, 13, 13, 13);
        c.weighty = 0.0;
        c.gridy = 12;

        add(panelBotones, c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoFichero, "firma.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(examinar, "firma.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(comboAlmacen, "firma.almacen"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(comboFormato, "firma.formato"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(firmar, "firma"); //$NON-NLS-1$
    }
}