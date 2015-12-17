/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardsobres;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.UnrecoverableKeyException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.crypto.Cipher;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.envelopers.cms.AOCMSEnveloper;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.ExtFilter;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.CertificateDestiny;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Clase que contiene los elementos necesarios para crear un grupo de Remitenres
 * a partir de una seleccion de certificados de remitentes. */
final class PanelRemitentes extends JAccessibilityDialogWizard {

	private static final Integer AES_MAX_KEY_SIZE = Integer.valueOf(256);

    /** Botonera con funciones para la pagina panel de multifirma - cofirma */
    private class Botonera extends BotoneraInferior {

        private static final long serialVersionUID = 1L;

        public Botonera(final List<JDialogWizard> ventanas, final int posicion) {
            super(ventanas, posicion);
        }

        @Override
        protected void siguienteActionPerformed(final JButton anterior, final JButton siguiente, final JButton finalizar) {

            boolean continuar = true;
            continuar = ensobrar();

            if (continuar) {
                super.siguienteActionPerformed(anterior, siguiente, finalizar);
            }
        }
    }

    /** Log. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** UID. */
    private static final long serialVersionUID = 1L;
    /** Constante sobre autenticado. */
    public static final int SOBRE_AUTENTICADO = 0;
    /** Constante sobre firmado. */
    public static final int SOBRE_FIRMADO = 1;

    /** Constante sobre simple. */
    public static final int SOBRE_SIMPLE = 2;

    /** Etiqueta con el texto "Anadir remitente desde...". */
    private JLabel etiquetaAnadir = new JLabel();

    /** Lista con los certificados a utilizar */
    private List<CertificateDestiny> listaCertificados;

    /** Lista con los remitentes */
    private final List<CertificateDestiny> listaCertificadosRe = new ArrayList<>();

    /** Lista de remitentes. */
    private final JList<String> listaRemitentes = new JList<>();

    /** Clave del certificado */
    private PrivateKeyEntry privateKeyEntry;

    /** Ruta donde se encuentra el fichero a ensobrar */
    private final String rutafichero;

    /** Tipo de ensobrado */
    private final int tipo;

    /** Constructor.
     * @param rutafichero Ruta del fichero a ensobrar.
     * @param tipo Tipo de sobre a generar. */
    public PanelRemitentes(final String rutafichero, final int tipo) {
        this.rutafichero = rutafichero;
        this.tipo = tipo;
        initComponents();
    }

    /** A&ntilde;ade un nuevo remitente desde el repositorio indicado
     * @param comboRepositorios combo con el listado de repositorios / almacenes
     * @param eliminar Boton para eliminar un remitente del listado de repositorios
     * @param anadir Boton para anadir un remitente al listado de repositorios */
    void anadirActionPerformed(final JComboBox comboRepositorios, final JButton eliminar, final JButton anadir) {

    	final KeyStoreConfiguration kconf = (KeyStoreConfiguration) comboRepositorios.getSelectedItem();

        final AOKeyStoreManager keyStoreManager;
        try {
            final AOKeyStore ao = kconf.getType();
            String lib = null;
            if (ao == AOKeyStore.PKCS12 || ao == AOKeyStore.SINGLE) {
                ExtFilter filter;
                if (ao == AOKeyStore.PKCS12) {
                    filter = new ExtFilter(new String[] {
                            "p12", "pfx"}, //$NON-NLS-1$ //$NON-NLS-2$
                                           Messages.getString("Filtro.fichero.pkcs12.descripcion")); //$NON-NLS-1$
                }
                else {
                    filter = new ExtFilter(new String[] {
                            "cer", "p7b", "p7s"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                           Messages.getString("Filtro.fichero.certificado.descripcion")); //$NON-NLS-1$
                }
                final File keystorePath = SelectionDialog.showFileOpenDialog(this, Messages.getString("Ensobrado.dialogo.almacen.titulo"), Main.getPreferences().get("dialog.load.repository.pkcs12", null), filter); //$NON-NLS-1$ //$NON-NLS-2$
                if (keystorePath == null) {
                    throw new AOCancelledOperationException();
                }
                lib = keystorePath.getAbsolutePath();
                Main.getPreferences().put("dialog.load.repository.pkcs12", lib); //$NON-NLS-1$
            }
            keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
        		ao,
        		lib,
        		null,
        		ao.getStorePasswordCallback(this),
        		this
    		);
        }
        catch (final AOCancelledOperationException e) {
            LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
            return;
        }
        catch (final IOException e) {
            // Control de la excepcion generada al introducir mal la contrasena para el almacen
            CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.error.almacen.contrasenia"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return;
        }
        catch (final AOKeystoreAlternativeException e) {
            // Control de la excepcion generada al introducir una contraseoa vacia para el almacen
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.sobres.error.almacen.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return;
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido abrir el almacen de certificados: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.error.certificados.almacen"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return;
        }

        // Obtenemos el certificado
        final CertificateDestiny certDest = new CertificateDestiny(keyStoreManager, this);

        // Comprobamos que el certificado es correcto
        if (certDest.getAlias() != null && !certDest.equals("")) { //$NON-NLS-1$
            boolean copiar = true;
            final DefaultListModel listModel = (DefaultListModel) this.listaRemitentes.getModel();
            for (int i = 0; i < listModel.getSize(); i++) {
                final CertificateDestiny c = (CertificateDestiny) listModel.getElementAt(i);
                if (certDest.getAlias().equals(c.getAlias())) {
                    copiar = false;
                }
            }
            if (copiar) {
                listModel.addElement(certDest.getAlias());
                this.listaCertificadosRe.add(certDest);
                anadir.setEnabled(false);
                anadir.setMnemonic(0); // Se asigna un atajo vacio puesto que se ha deshabilitado el boton
                comboRepositorios.setEnabled(false);
                this.etiquetaAnadir.setDisplayedMnemonic(0); // Se asigna un atajo vacio puesto que se ha deshabilitado el combo asociado
                this.etiquetaAnadir.getAccessibleContext().setAccessibleName(this.etiquetaAnadir.getText() + " " //$NON-NLS-1$
                                                                             + Messages.getString("Wizard.sobres.etiquetaAnadir")); //$NON-NLS-1$
                this.etiquetaAnadir.setFocusable(true);
                eliminar.setEnabled(true);
                eliminar.setMnemonic(KeyEvent.VK_E); // Se asigna un atajo al boton ya que ha sido habilitado
            }
            else {
                CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.error.usuario"), //$NON-NLS-1$
                                               Messages.getString("error"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$
            }
        }

        // Preguntamos por la contrasena del certificado
        if (!this.listaCertificadosRe.isEmpty()) {
            try {
                this.privateKeyEntry = getPrivateKeyEntry(keyStoreManager, certDest.getAlias(), kconf);
            }
            catch (final UnrecoverableKeyException e) {
                // Control de la excepcion generada al introducir mal la contrasena para el certificado
                CustomDialog.showMessageDialog(
            		this,
                    true,
                    Messages.getString("Wizard.sobres.error.certificados.contrasenia"),  //$NON-NLS-1$
                    Messages.getString("error"),  //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
                return;
            }
            catch (final AOException e) {
                LOGGER.warning("Error al obtener la clave del certificado: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(
            		this,
            		true,
            		Messages.getString("Ensobrado.msg.error.clave"), //$NON-NLS-1$
                    Messages.getString("error"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );

                clearWizard(comboRepositorios, eliminar, anadir);

                return;
            }
            catch (final AOCancelledOperationException e) {
                clearWizard(comboRepositorios, eliminar, anadir);
            }
        }
    }

    /** Carga un combo con los repositorios/almacenes disponibles
     * @param comboRepositorios Combo donde se deben cargar los repositorios */
    private static void cargarCombo(final JComboBox comboRepositorios) {
        comboRepositorios.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
    }

    /** Resetea todo el wizard de esta pagina dejandolo como al inicio
     * @param comboRepositorios Combo con los repositorios
     * @param eliminar Boton eliminar
     * @param anadir Boton anadir */
    private void clearWizard(final JComboBox comboRepositorios, final JButton eliminar, final JButton anadir) {
        this.listaCertificadosRe.remove(this.listaCertificadosRe.get(0));
        ((DefaultListModel) this.listaRemitentes.getModel()).remove(0);

        if (this.listaCertificadosRe.isEmpty()) {
            anadir.setEnabled(true);
            anadir.setMnemonic(KeyEvent.VK_R); // Se asigna un atajo al boton puesto que se ha habilitado
            comboRepositorios.setEnabled(true);
            this.etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D); // Se asigna un atajo puesto que se ha habilitado el combo asociado
            this.etiquetaAnadir.setFocusable(false);
            eliminar.setEnabled(false);
            eliminar.setMnemonic(0); // Se asigna un atajo vacio al boton ya que ha sido deshabilitado
        }
    }

    /** Elimina un remitente de la lista de remitentes
     * @param comboRepositorios Combo con el repositorio / almacen
     * @param eliminar Boton para eliminar un remitente de la lista
     * @param anadir Boton para anadir un remitente a la lista */
    void eliminarActionPerformed(final JComboBox comboRepositorios, final JButton eliminar, final JButton anadir) {
        for (int i = 0; i < this.listaCertificadosRe.size(); i++) {
            if (this.listaCertificadosRe.get(i).getAlias().equals(this.listaRemitentes.getSelectedValue())) {
                this.listaCertificadosRe.remove(this.listaCertificadosRe.get(i));
                ((DefaultListModel) this.listaRemitentes.getModel()).remove(this.listaRemitentes.getSelectedIndex());
                break;
            }
        }

        if (this.listaCertificadosRe.isEmpty()) {
            anadir.setEnabled(true);
            anadir.setMnemonic(KeyEvent.VK_R); // Se asigna un atajo al boton puesto que se ha habilitado
            comboRepositorios.setEnabled(true);
            this.etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D); // Se asigna un atajo puesto que se ha habilitado el combo asociado
            this.etiquetaAnadir.setFocusable(false);
            eliminar.setEnabled(false);
            eliminar.setMnemonic(0); // Se asigna un atajo vacio al boton ya que ha sido deshabilitado
        }

        // Borramos las posibles claves del certificado
        this.privateKeyEntry = null;
    }

    /** Ensobra el fichero dado
     * @return True o false si la operacion ha sido satisfactoria */
    public boolean ensobrar() {
        if (this.tipo == SOBRE_AUTENTICADO || this.tipo == SOBRE_FIRMADO) {
            final DefaultListModel listModel = (DefaultListModel) this.listaRemitentes.getModel();
            if (listModel.isEmpty()) {
                CustomDialog.showMessageDialog(
                		this,
                        true,
                        Messages.getString("WizardCifrado.error.remitente"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
                return false;
            }
        }

        try {
            final X509Certificate[] certs = new X509Certificate[this.listaCertificados.size()];
            for (int i = 0; i < this.listaCertificados.size(); i++) {
                certs[i] = (X509Certificate) this.listaCertificados.get(i).getCertificate();
            }

            final AOCMSEnveloper enveloper = new AOCMSEnveloper();
            enveloper.setSignatureAlgorithm(GeneralConfig.getSignAlgorithm());

            final AOCipherConfig cipherConfig = new AOCipherConfig(AOCipherAlgorithm.AES, null, null);

            // por defecto se realizara un sobre envelopedData
            byte[] envelopedData = null;

            byte[] contentData = null;
            try {
            	contentData = readFile(this.rutafichero);
            }
            catch(final OutOfMemoryError e) {
            	CustomDialog.showMessageDialog(
        			SwingUtilities.getRoot(this),
        			true,
        			Messages.getString("WizardCifrado.error.tamanodatos"), //$NON-NLS-1$
                    Messages.getString("error"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
            	this.getBotonera().getCancelar().doClick();
            }
            Integer keySize = null;
            final int aesMaxKeySize = Cipher.getMaxAllowedKeyLength(AOCipherAlgorithm.AES.getName());
            if (aesMaxKeySize == Integer.MAX_VALUE && CustomDialog.showConfirmDialog(
    			SwingUtilities.getRoot(this),
    			true,
    			Messages.getString("Wizard.sobres.unlimitedencryption"), //$NON-NLS-1$
    			Messages.getString("Wizard.sobres.unlimitedencryption.title"), //$NON-NLS-1$
    			JOptionPane.YES_NO_OPTION,
    			JOptionPane.WARNING_MESSAGE
			) == 0) {
        		LOGGER.info("Se ha establecido la clave AES a " + Integer.toString(aesMaxKeySize) + " bits"); //$NON-NLS-1$ //$NON-NLS-2$
        		keySize = AES_MAX_KEY_SIZE;
        	}
            try {
                if (this.tipo == SOBRE_AUTENTICADO) {
                    envelopedData = enveloper.createCMSAuthenticatedEnvelopedData(contentData, this.privateKeyEntry, cipherConfig, certs, keySize);
                }
                else if (this.tipo == SOBRE_FIRMADO) {
                    envelopedData = enveloper.createCMSSignedAndEnvelopedData(contentData, this.privateKeyEntry, cipherConfig, certs, keySize);
                }
                else if (this.tipo == SOBRE_SIMPLE) {
                    envelopedData = enveloper.createCMSEnvelopedData(contentData, this.privateKeyEntry, cipherConfig, certs, keySize);
                }
            }
            catch (final Exception e) {
            	LOGGER.severe("No se ha posido crear el sobre: " + e); //$NON-NLS-1$
                return false;
            }

            // Guardamos el sobre generado
            final File savedFile = SelectionDialog.saveDataToFile(
        		Messages.getString("Wizard.sobres.filechooser.save.title"), //$NON-NLS-1$
                envelopedData,
                new File(this.rutafichero).getName() + ".enveloped", //$NON-NLS-1$
                null,
                this
            );
            // Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
            if (savedFile == null) {
                return false;
            }

        }
        catch (final AOCancelledOperationException e) {
            LOGGER.info("La operacion ha sido cancelada por el usuario: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Ensobrado.msg.error.generacion"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }
        catch (final FileNotFoundException e) {
            LOGGER.warning("No se puede encontrar el fichero seleccionado: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.error.encontrar.fichero"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }
        catch (final IOException e) {
            LOGGER.warning("No ha sido posible leer el fichero indicado: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.lectura.generico"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }
        catch (final Exception e) {
            LOGGER.warning("Error durante la desenvoltura: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.error.desenvoltura"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }

    /** Relacion minima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 8;
    }

    /** Obtiene la clave privada
     * @param keyStoreManager Manager del KeyStore
     * @param seleccionado Certificado seleccionado
     * @param kconf1 Configuracion del KeyStore
     * @return
     * @throws AOException
     * @throws UnrecoverableKeyException */
    private static PrivateKeyEntry getPrivateKeyEntry(final AOKeyStoreManager keyStoreManager,
    		                                   final String seleccionado,
    		                                   final KeyStoreConfiguration kconf1) throws AOException,
                                                                                                                                                      UnrecoverableKeyException {

        // Comprobamos si se ha cancelado la seleccion
        if (seleccionado == null) {
            throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$
        }

        // Recuperamos la clave del certificado
        try {
            return keyStoreManager.getKeyEntry(
        		seleccionado
    		);
        }
        catch (final KeyStoreException e) {
        	LOGGER.severe("No se ha podido obtener el certicado con el alias '" + seleccionado + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
        	throw new AOException(e.getMessage(), e);
		}
        catch (final NoSuchAlgorithmException e) {
            LOGGER.severe("No se ha podido obtener el certicado con el alias '" + seleccionado + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            throw new AOException(e.getMessage(), e);
		}
        catch (final UnrecoverableEntryException e) {
	          LOGGER.severe("No se ha podido obtener el certicado con el alias '" + seleccionado + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
	          throw new AOException(e.getMessage(), e);
		}

    }

    /** Inicializacion de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("Wizard.sobres.titulo")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior =
                new CabeceraAsistente("Wizard.sobres.pagina2.titulo", "Wizard.sobres.pagina2.titulo.explicacion1", "Wizard.sobres.pagina2.titulo.explicacion2", null); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);

        // Panel central
        final JPanel panelCentral = new JPanel();
        panelCentral.setMinimumSize(new Dimension(603, 289));
        panelCentral.setLayout(new GridBagLayout());

        // Configuramos el layout
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(10, 20, 0, 20);
        c.gridwidth = 2;
        c.weightx = 1.0;
        c.gridx = 0;

        // Etiqueta con el texto "Puede anadir uno o..."
        final InfoLabel labelText = new InfoLabel(Messages.getString("Wizard.sobres.pagina2.contenido.explicacion1"), false); //$NON-NLS-1$
        panelCentral.add(labelText, c);

        c.gridy = 1;
        c.gridwidth = 1;

        final JPanel panelEtiquetaAnadir = new JPanel(new GridLayout(1, 1));
        // Etiqueta con el texto "Anadir remitente desde..."
        this.etiquetaAnadir = new JLabel();
        this.etiquetaAnadir.setText(Messages.getString("Wizard.sobres.aniadir.originante")); //$NON-NLS-1$
        Utils.setContrastColor(this.etiquetaAnadir);
        Utils.setFontBold(this.etiquetaAnadir);
        panelEtiquetaAnadir.add(this.etiquetaAnadir);
        panelCentral.add(panelEtiquetaAnadir, c);

        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth = 1;
        c.gridy = 2;
        c.weightx = 1.0;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con los repositorios / almacenes
        final JComboBox comboRepositorios = new JComboBox();
        comboRepositorios.setToolTipText(Messages.getString("Wizard.comboRepositorios.description")); //$NON-NLS-1$
        comboRepositorios.getAccessibleContext().setAccessibleName(this.etiquetaAnadir.getText() + " " //$NON-NLS-1$
                                                                   + comboRepositorios.getToolTipText()
                                                                   + " ALT + D."); //$NON-NLS-1$
        comboRepositorios.getAccessibleContext().setAccessibleDescription(comboRepositorios.getToolTipText());
        Utils.remarcar(comboRepositorios);
        Utils.setContrastColor(comboRepositorios);
        Utils.setFontBold(comboRepositorios);
        cargarCombo(comboRepositorios);
        panelCentral.add(comboRepositorios, c);

        // Relacion entre etiqueta y combo
        this.etiquetaAnadir.setLabelFor(comboRepositorios);
        // Asignacion de mnemonico
        this.etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D);

        c.insets = new Insets(0, 10, 0, 20);
        c.gridx = 1;
        c.weightx = 0.0;
        c.weighty = 0.0;
        c.fill = GridBagConstraints.HORIZONTAL;

        final JPanel panelAnadir = new JPanel(new GridLayout(1, 1));
        // Boton Anadir
        final JButton anadir = new JButton();
        final JButton eliminar = new JButton();
        anadir.setToolTipText(Messages.getString("Wizard.sobres.aniadir.originante.description")); //$NON-NLS-1$
        anadir.setText(Messages.getString("wizard.aniadir")); //$NON-NLS-1$
        anadir.setAutoscrolls(true);
        anadir.setMnemonic(KeyEvent.VK_I); // Se asigna un atajo al boton
        anadir.getAccessibleContext().setAccessibleName(anadir.getText() + " " + anadir.getToolTipText()); //$NON-NLS-1$
        anadir.getAccessibleContext().setAccessibleDescription(anadir.getToolTipText());
        anadir.addActionListener(new ActionListener() {
            /** Accion del boton aniadir.
             * @param evt evento. */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                anadirActionPerformed(comboRepositorios, eliminar, anadir);
            }
        });
        Utils.remarcar(anadir);
        Utils.setContrastColor(anadir);
        Utils.setFontBold(anadir);
        panelAnadir.add(anadir);
        panelCentral.add(panelAnadir, c);

        c.insets = new Insets(10, 20, 0, 20);
        c.gridx = 0;
        c.gridy = 3;
        c.weightx = 0.0;

        // Etiqueta con el texto "Remitentes"
        final JLabel senderLabel = new JLabel();
        senderLabel.setText(Messages.getString("Wizard.sobres.listaRemitentes")); //$NON-NLS-1$
        Utils.setContrastColor(senderLabel);
        Utils.setFontBold(senderLabel);
        panelCentral.add(senderLabel, c);

        c.insets = new Insets(0, 20, 0, 20);
        c.ipady = 80;
        c.gridwidth = 2;
        c.gridy = 4;
        c.gridx = 0;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.fill = GridBagConstraints.BOTH;

        // Panel que contiene la lista de remitentes
        final JScrollPane panelLista = new JScrollPane();
        panelCentral.add(panelLista, c);

        // Listado de remitentes
        this.listaRemitentes.setToolTipText(Messages.getString("Wizard.sobres.listaRemitentes.description")); //$NON-NLS-1$
        this.listaRemitentes.setModel(new DefaultListModel());
        this.listaRemitentes.getAccessibleContext().setAccessibleName(senderLabel.getText() + " " + this.listaRemitentes.getToolTipText()); //$NON-NLS-1$
        this.listaRemitentes.getAccessibleContext().setAccessibleDescription(this.listaRemitentes.getToolTipText());
        Utils.remarcar(this.listaRemitentes);
        Utils.setContrastColor(this.listaRemitentes);
        Utils.setFontBold(this.listaRemitentes);
        panelLista.setViewportView(this.listaRemitentes);

        // Relacion entre etiqueta y lista
        senderLabel.setLabelFor(this.listaRemitentes);
        // Asignacion de mnemonico
        senderLabel.setDisplayedMnemonic(KeyEvent.VK_T);

        c.ipady = 0;
        c.gridwidth = 1;
        c.gridy = 5;
        c.weightx = 1.0;
        c.fill = GridBagConstraints.HORIZONTAL;

        // Espacio izdo del boton
        final Panel panelVacio = new Panel();
        panelCentral.add(panelVacio, c);

        c.insets = new Insets(5, 0, 5, 20);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelEliminar = new JPanel(new GridLayout(1, 1));
        // Boton eliminar
        eliminar.setEnabled(false);
        eliminar.setToolTipText(Messages.getString("Wizard.sobres.eliminar.remitente.description")); //$NON-NLS-1$
        eliminar.setText(Messages.getString("Wizard.sobres.eliminar.remitente")); //$NON-NLS-1$
        eliminar.getAccessibleContext().setAccessibleName(eliminar.getText() + " " + eliminar.getToolTipText()); //$NON-NLS-1$
        eliminar.getAccessibleContext().setAccessibleDescription(eliminar.getToolTipText());
        eliminar.addActionListener(new ActionListener() {
            /** Accion del boton eliminar. */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                eliminarActionPerformed(comboRepositorios, eliminar, anadir);
            }
        });

        Utils.remarcar(eliminar);
        Utils.setContrastColor(eliminar);
        Utils.setFontBold(eliminar);
        panelEliminar.add(eliminar);
        panelCentral.add(panelEliminar, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(comboRepositorios, "ensobrado.wizard.repositorio.remitente"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(labelText, "ensobrado.wizard. remitentes"); //$NON-NLS-1$
    }

    /** Lectura de fichero.
     * @param filepath
     * @return
     * @throws java.io.FileNotFoundException
     * @throws IOException */
    private static byte[] readFile(final String filepath) throws IOException {
        try ( final InputStream fileIn = AOUtil.loadFile(AOUtil.createURI(filepath)); ) {
            return AOUtil.getDataFromInputStream(fileIn);
        }
        catch (final URISyntaxException e) {
        	throw new IOException("Ruta hacia el fichero de datos invalida: " + filepath, e); //$NON-NLS-1$
		}
    }

    /** Asignaci&oacute;n de la lista de certificados.
     * @param listaCertificados Lista de certificados de los remitentes.*/
    public void setListaCertificados(final List<CertificateDestiny> listaCertificados) {
        this.listaCertificados = listaCertificados;
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    public void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 2));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
}
