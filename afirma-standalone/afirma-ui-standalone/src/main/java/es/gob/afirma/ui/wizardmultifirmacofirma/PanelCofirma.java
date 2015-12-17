/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmacofirma;

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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.text.Caret;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.MultisignUtils;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/**
 * Clase que muestra el contenido principal de multifirmas - cofirma.
 */
final class PanelCofirma extends JAccessibilityDialogWizard {
	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Log.
	 */
	private static Logger logger = Logger.getLogger(PanelCofirma.class.getName());
	/**
	 * Relacion minima para el redimensionado de componentes.
	 */
	@Override
	public int getMinimumRelation(){
		return 9;
	}

	/**
	 * Configuracion del KeyStore
	 */
	private KeyStoreConfiguration kssc = null;

	/**
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	public void setVentanas(final List<JDialogWizard> ventanas) {
		this.setBotonera(new Botonera(ventanas, 1));
    	getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
	}
	/**
	 * Constructor.
	 * @param kssc Configuraci&oacute;n del almac&eacute;n de certificados.
	 */
	public PanelCofirma(final KeyStoreConfiguration kssc) {
		this.kssc = kssc;
		initComponents();
	}

	/**
	 * Campo donde se guarda el nombre del fichero de datos.
	 */
	private final JTextField campoDatos = new JTextField();
	/**
	 * Campo donde se guarda el nombre del fichero de firma.
	 */
	private final JTextField campoFirma = new JTextField();

	/**
	 * Inicializacion de componentes
	 */
	private void initComponents() {
		// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.multifirma.simple.cofirma.titulo")); //$NON-NLS-1$

		// Panel con la cabecera
        final CabeceraAsistente panelSuperior =
                new CabeceraAsistente("Wizard.multifirma.simple.ventana1.titulo", "Wizard.multifirma.simple.ventana1.titulo.descripcion", null); //$NON-NLS-1$ //$NON-NLS-2$
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
        c.insets = new Insets(20, 20, 0, 20);
        c.gridwidth = 2;
		c.weightx = 1.0;
		c.gridx = 0;

	    // Etiqueta "Fichero de firma:"
        final JLabel etiquetaFirma = new JLabel();
        etiquetaFirma.setText(Messages.getString("Wizard.multifirma.simple.ventana1.fichero.firma")); //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFirma);
        Utils.setFontBold(etiquetaFirma);
        panelCentral.add(etiquetaFirma, c);


		c.insets = new Insets(0, 20, 0, 0);
		c.gridwidth = 1;
		c.gridy	= 1;

	      // Caja de texto donde se guarda el nombre del archivo de la firma
        this.campoFirma.setToolTipText(Messages.getString("Wizard.multifirma.simple.ventana1.fichero.firma.description")); // NOI18N //$NON-NLS-1$
        this.campoFirma.getAccessibleContext().setAccessibleName(etiquetaFirma.getText() + " " + this.campoFirma.getToolTipText() + " " + "ALT + I."); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        this.campoFirma.getAccessibleContext().setAccessibleDescription(this.campoFirma.getToolTipText());
         if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoFirma.setCaret(caret);
        }
        Utils.remarcar(this.campoFirma);
        Utils.setContrastColor(this.campoFirma);
        Utils.setFontBold(this.campoFirma);
        panelCentral.add(this.campoFirma, c);

        //Relacion entre etiqueta y campo de texto
        etiquetaFirma.setLabelFor(this.campoFirma);
        //Asignacion de mnemonico
        etiquetaFirma.setDisplayedMnemonic(KeyEvent.VK_I);

		c.insets = new Insets(0, 10, 0, 20);
		c.weightx = 0.0;
		c.gridx = 1;

		final JPanel panelExaminarFirma = new JPanel(new GridLayout(1, 1));
		// Boton examinar (fichero firma)
        final JButton examinarFirma = new JButton();
        examinarFirma.setMnemonic(KeyEvent.VK_X); //mnemonico diferente al boton "Examinar" anterior
        examinarFirma.setText(Messages.getString("PrincipalGUI.Examinar")); //$NON-NLS-1$
        examinarFirma.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); //$NON-NLS-1$
        examinarFirma.getAccessibleContext().setAccessibleName(examinarFirma.getText() + " " + examinarFirma.getToolTipText()); //$NON-NLS-1$
        examinarFirma.getAccessibleContext().setAccessibleDescription(examinarFirma.getToolTipText());
        examinarFirma.addActionListener(new ActionListener() {
        	/**
        	 * Accion boton examinar.
        	 */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarFirmaActionPerformed();
            }
        });
        Utils.remarcar(examinarFirma);
        Utils.setContrastColor(examinarFirma);
        Utils.setFontBold(examinarFirma);
        panelExaminarFirma.add(examinarFirma);
        panelCentral.add(panelExaminarFirma, c);

		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth = 2;
		c.gridy	= 2;
		c.gridx = 0;

        // Etiqueta "Fichero de datos:"
        final JLabel etiquetaDatos = new JLabel();
        etiquetaDatos.setText(Messages.getString("Wizard.multifirma.simple.ventana1.fichero.datos")); //$NON-NLS-1$
        Utils.setContrastColor(etiquetaDatos);
        Utils.setFontBold(etiquetaDatos);
        panelCentral.add(etiquetaDatos, c);

		c.insets = new Insets(0, 20, 0, 0);
		c.gridwidth = 1;
		c.gridy	= 3;

		// Caja de texto donde se guarda el nombre del archivo de datos
        this.campoDatos.setToolTipText(Messages.getString("Wizard.multifirma.simple.ventana1.fichero.datos.description")); //$NON-NLS-1$
        this.campoDatos.getAccessibleContext().setAccessibleName(etiquetaDatos.getText() + " " + this.campoDatos.getToolTipText() + " " + "ALT + F."); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        this.campoDatos.getAccessibleContext().setAccessibleDescription(this.campoDatos.getToolTipText());

         if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoDatos.setCaret(caret);
        }
        Utils.remarcar(this.campoDatos);
        Utils.setContrastColor(this.campoDatos);
        Utils.setFontBold(this.campoDatos);
        panelCentral.add(this.campoDatos, c);

        //Relacion entre etiqueta y campo de texto
        etiquetaDatos.setLabelFor(this.campoDatos);
        //Asignacion de mnemonico
        etiquetaDatos.setDisplayedMnemonic(KeyEvent.VK_F);

		c.insets = new Insets(0, 10, 0, 20);
		c.gridx = 1;

		final JPanel panelExaminarDatos = new JPanel(new GridLayout(1, 1));
		// Boton examinar (fichero datos)
        final JButton examinarDatos = new JButton();
        examinarDatos.setMnemonic(KeyEvent.VK_E);
        examinarDatos.setText(Messages.getString("PrincipalGUI.Examinar")); //$NON-NLS-1$
        examinarDatos.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); //$NON-NLS-1$
        examinarDatos.getAccessibleContext().setAccessibleName(examinarDatos.getText() + " " + examinarDatos.getToolTipText()); //$NON-NLS-1$
        examinarDatos.getAccessibleContext().setAccessibleDescription(examinarDatos.getToolTipText());
        examinarDatos.addActionListener(new ActionListener() {
        	/**
        	 * Accion examinar datos.
        	 */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarDatosActionPerformed();
            }
        });
        Utils.remarcar(examinarDatos);
        Utils.setContrastColor(examinarDatos);
        Utils.setFontBold(examinarDatos);
        panelExaminarDatos.add(examinarDatos);
        panelCentral.add(panelExaminarDatos, c);

		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth = 2;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 4;

		// Panel introducido para poder mantener la linea superior correcta
		final Panel panelVacio = new Panel();
		panelCentral.add(panelVacio, c);

		getContentPane().add(panelCentral, BorderLayout.CENTER);

		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(this.campoDatos, "multifirma.wizard.ficherodatos"); //$NON-NLS-1$
		HelpUtils.enableHelpKey(this.campoFirma, "multifirma.wizard.ficherofirma"); //$NON-NLS-1$
	}


    /**
     * Examina si el archivo seleccionado es un archivo de firma y guarda el nombre en su caja
     */
    void examinarFirmaActionPerformed() {
        final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("Wizard.multifirma.simple.chooserFirm.tittle"), Main.getPreferences().get("dialog.load.dir", null)); //$NON-NLS-1$ //$NON-NLS-2$
        if (selectedFile != null) {
            this.campoFirma.setText(selectedFile.getAbsolutePath());
        }
    }

	/**
	 * Examina si se ha seleccionado un archivo correcto y guarda el nombre en su caja
	 */
	void examinarDatosActionPerformed() {
		final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("PrincipalGUI.chooser.title"), Main.getPreferences().get("dialog.load.dir", null)); //$NON-NLS-1$ //$NON-NLS-2$
		if (selectedFile != null) {
			this.campoDatos.setText(selectedFile.getAbsolutePath());
		}
	}

	/**
	 * Botonera con funciones para la pagina panel de multifirma - cofirma
	 */
	private class Botonera extends BotoneraInferior {
		/**
		 * UID.
		 */
		private static final long serialVersionUID = 1L;
		/**
		 * Constructor.
		 * @param ventanas Lista de ventanas que componen el wizard.
		 * @param posicion posicion de la ventana donde se inserta esta botonera.
		 */
		public Botonera(final List<JDialogWizard> ventanas, final int posicion) {
			super(ventanas, posicion);
		}
		/**
		 * Accion para el boton siguiente.
		 */
		@Override
		protected void siguienteActionPerformed(final JButton anterior,
				final JButton siguiente, final JButton finalizar) {

			boolean continuar = true;
			try {
				continuar = cofirmaFichero();
			} catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").warning("Ocurrio un error durante la lectura de los datos: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				return;
			}

			if (continuar) {
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
			}
		}
	}

	/**
	 * Cofirma un fichero dado
	 * @return	true o false indicando si se ha cofirmado correctamente
	 * @throws IOException Cuando ocurre alg&uacute;n error durante la lectura de los datos.
	 */
	public boolean cofirmaFichero() throws IOException {
		//comprobacion de la ruta de fichero de entrada.
		final String ficheroDatos = this.campoDatos.getText();
		final String ficheroFirma = this.campoFirma.getText();

		if (ficheroFirma == null || ficheroFirma.equals("")){ //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.firma.vacio"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        else if (!new File(ficheroFirma).exists() && !new File(ficheroFirma).isFile()){
        	CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.firma"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }

		final byte[] sign;
		byte[] data;

		try ( final InputStream signIs = new FileInputStream(ficheroFirma); ) {
		    sign = AOUtil.getDataFromInputStream(signIs);
		}
		catch (final Exception e) {
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.firma"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
		catch(final OutOfMemoryError e) {
        	CustomDialog.showMessageDialog(
    			SwingUtilities.getRoot(this), true, Messages.getString("Wizard.multifirma.simple.error.firma.tamano"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return false;
        }

		final AOSigner signer = AOSignerFactory.getSigner(sign);
        if (signer == null) {
        	CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.manejador"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
		try {
		    data = signer.getData(sign);
		} catch (final Exception e) {
		    Logger.getLogger("es.gob.afirma").warning("No se ha podido leer los datos contenidos en la firma, se tomaran los del fichero indicado: " + e);  //$NON-NLS-1$//$NON-NLS-2$
		    data = null;
        }

		if (data == null) {
		    if (ficheroDatos == null || ficheroDatos.equals("")){ //$NON-NLS-1$
		    	CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.datos.vacio"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
		        return false;
		    }
		    else if (!new File(ficheroDatos).exists() && !new File(ficheroDatos).isFile()) {
		    	CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.datos"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
		        return false;
		    }
		}

		try {
		    final String intText = ".cosign"; //$NON-NLS-1$
		    byte[] coSignedData = null;

		    PrivateKeyEntry keyEntry;

		    // Recuperamos la clave del certificado
		    try {
		        final AOKeyStoreManager keyStoreManager = MultisignUtils.getAOKeyStoreManager(this.kssc,this);
		        keyEntry = MultisignUtils.getPrivateKeyEntry(this.kssc, keyStoreManager, this);
		    }
		    catch (final AOException e) {
		    	CustomDialog.showMessageDialog(this, true, Messages.getString("Desensobrado.msg.error.certificado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
		        return false;
		    }

		    if (data == null) {
		    	try ( final InputStream dataIs = new FileInputStream(ficheroDatos); ) {
		    		data = AOUtil.getDataFromInputStream(dataIs);
		    	}
		    }

		    if (!signer.isValidDataFile(data)) {
		    	CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.fichero"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
		        return false;
		    }
		    if (!signer.isSign(sign)) {
		    	CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.fichero.soportado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
		        return false;
		    }
		    try {
		        coSignedData = cosignOperation(signer, data, sign, keyEntry, ficheroDatos);
		    } catch (final AOException e) {
		        logger.severe("Error en la cofirma: " + e.toString()); //$NON-NLS-1$
		        e.printStackTrace();
		        CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.cofirma.error"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
		        return false;
		    }

		    final File savedFile = SelectionDialog.saveDataToFile(Messages.getString("Wizard.multifirma.simple.filechooser.save.title"), coSignedData, //$NON-NLS-1$
		            signer.getSignedName(ficheroFirma, intText), null, this);
		    // Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
		    if (savedFile == null) {
		        return false;
		    }

		}
		catch (final AOCancelledOperationException e) {
            logger.severe(e.toString());
            return false;
        }
        catch (final java.security.ProviderException e) {
        	// Comprobacion especifica para el proveedor Java de DNIe
        	if (e.getCause() != null && e.getCause().getClass().getName().equals("es.gob.jmulticard.card.AuthenticationModeLockedException")) { //$NON-NLS-1$
        		CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                        true,
                        Messages.getString("Firma.msg.error.dnie.AuthenticationModeLockedException"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        		return false;
        	}
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                           true,
                                           Messages.getString("Firma.msg.error.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
		catch (final Exception e) {
		    logger.severe(e.toString());
		    CustomDialog.showMessageDialog(this, true, e.getMessage() != null ? e.getMessage() : e.toString(), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
		    return false;
		}

		return true;
	}

	/**
	 * Cofirma de un fichero de datos.
	 * @param dataFile Fichero de datos a firma.
	 * @param keyEntry Clave de firma.
	 * @param filepath Ruta del fichero firmado.
	 * @return Contenido de la firma.
	 * @throws AOException Ocurrio un error durante el proceso de firma.
	 * @throws IOException Cuando ocurre alg&uacute;n error durante la lectura de los datos.
	 */
	private static byte[] cosignOperation(final AOSigner signer, final byte[] data, final byte[] sign, final PrivateKeyEntry keyEntry, final String filepath) throws AOException, IOException {

		final Properties prop = GeneralConfig.getSignConfig();
		prop.setProperty("uri", filepath); //$NON-NLS-1$

		// Respetaremos si la firma original contenia o no los datos firmados
		prop.setProperty("mode", signer.getData(sign) == null ? AOSignConstants.SIGN_MODE_EXPLICIT : AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

        // En el caso de firmas XAdES no incluimos la cadena de certificacion
        if (signer instanceof AOXAdESSigner) {
        	prop.setProperty("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        }

		// Realizamos la cofirma
		return signer.cosign(
			data,
			sign,
			GeneralConfig.getSignAlgorithm(),
			keyEntry.getPrivateKey(),
			keyEntry.getCertificateChain(),
			prop
		);
	}
}