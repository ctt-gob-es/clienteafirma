/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmacontrafirma;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.AbstractListModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.KeyStoreConfiguration;
import es.gob.afirma.ui.core.jse.JSEUtils;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.MultisignUtils;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 * Panel para el wizard de multifirma.
 * @author inteco
 *
 */
public class PanelMultifirma extends JAccessibilityDialogWizard {

	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * log.
	 */
	static Logger logger = Logger.getLogger(PanelMultifirma.class.getName());

	/**
	 * Relacion minima para el redimensionado de componentes.
	 */
	@Override
	public int getMinimumRelation(){
		return 9;
	}
	
	/**
	 * Modelo de la lista
	 */
	private AbstractListModel modeloLista;

	/**
	 * Ruta del fichero a multifirmar
	 */
	private String rutaFichero;
	
	/** Firma que se desea multifirmar. */
	private byte[] signData;

	/**
	 * Configuracion del KeyStore
	 */
	private KeyStoreConfiguration kssc = null;

	/**
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	public void setVentanas(List<JDialogWizard> ventanas) {
		this.setBotonera(new Botonera(ventanas, 2));
    	getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
	}

	/**
	 * Carga el modelo del arbol y del listado con el archivo de firma seleccionado
	 * @param signPath	Ruta del fichero de firma
	 * @param sign Firma
	 * @return	{@code true} si se ha cargado correctamente.
	 */
	public boolean cargarDatos(String signPath, byte[] sign) {
		this.rutaFichero = signPath;
		this.signData = sign.clone();

		// Generamos el modelo del arbol a partir del fichero
		FileInputStream fis = null;
		javax.swing.tree.DefaultTreeModel modeloArbolSwing;
		try {
			AOTreeModel modeloArbol = AOSignerFactory.getSigner(this.signData).getSignersStructure(this.signData, false);
			modeloArbolSwing = JSEUtils.convertToSwingModel(modeloArbol);

			this.arbolFirmas.setModel(modeloArbolSwing);

			for (int i=0; i<this.arbolFirmas.getRowCount(); i++) {
				this.arbolFirmas.expandRow(i);
			}

			this.arbolFirmas.setSelectionRow(0);
		} catch (Exception e) {
			logger.severe("No se pudo cargar el arbol de firmas del fichero '" + this.rutaFichero + "': "+e);
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.arbol"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		} finally {
			if (fis != null) {
				try { fis.close(); } catch (Exception e) {}
			}
		}

		// Generamos el modelo de la lista a partir del modelo del arbol
		HashSet<String> signersSet = new HashSet<String>();

		// Recorremos todos los nodos menos el root que no contiene informacion de firmante
		DefaultMutableTreeNode root = (DefaultMutableTreeNode) modeloArbolSwing.getRoot();
		try {
			for (int i=0; i<root.getChildCount(); i++)
				getSigners((DefaultMutableTreeNode)root.getChildAt(i), signersSet);
		} catch(Exception e) {
			logger.warning("El arbol introducido contiene elementos no validos: " + e);
			return false;
		}

		// Recogemos los firmantes de los nodos
		final String[] signers = new String[signersSet.size()];
		signersSet.toArray(signers);

		this.modeloLista = new AbstractListModel() {

			private static final long serialVersionUID = 1L;
			
			String[] strings = signers;
			/**
			 * Devuelve el tamaño.
			 */
			@Override
            public int getSize() { 
				return this.strings.length; 
			}
			/**
			 * Devuelve el elemento contenido en la posicion indicada.
			 */
			@Override
            public Object getElementAt(int i) { 
				return this.strings[i]; 
			}
		};

		// Asignamos el modelo
		this.listaFirmantes.setModel(this.modeloLista);

		return true;
	}

	/**
	 * Introduce los alias de todos los certificados de firma del nodo indicado y todos sus subnodos.
	 * @param node Nodo de firma.
	 * @param signersSet Conjunto con los alias de los certificados de firma.
	 */
	private final void getSigners(DefaultMutableTreeNode node, HashSet<String> signersSet) {
		signersSet.add((String)node.getUserObject());
		for(int i=0; i<node.getChildCount(); i++) {
			getSigners((DefaultMutableTreeNode)node.getChildAt(i), signersSet);
		}
	}

	/**
	 * Constructor.
	 * @param kssc configuracion.
	 */
	public PanelMultifirma(KeyStoreConfiguration kssc) {
		this.kssc = kssc;
		initComponents();
	}

	// Arbol de firmas
	private JTree arbolFirmas = new JTree();
	// Listado de firmantes
	private JList listaFirmantes = new JList();
	// Combo con las opciones de firma
	private JComboBox comboFirmas = new JComboBox();

	/**
	 * Inicializacion de componentes
	 */
	private void initComponents() {
		// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.multifirma.simple.contrafirma.titulo")); //$NON-NLS-1$
		
		// Panel con la cabecera
		CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.multifirma.simple.contrafirma.ventana2.titulo",  //$NON-NLS-1$
				"Wizard.multifirma.simple.contrafirma.ventana2.titulo.description", null, true); //$NON-NLS-1$
		Utils.setContrastColor(panelSuperior);
		Utils.setFontBold(panelSuperior);
		getContentPane().add(panelSuperior, BorderLayout.NORTH);

		// Panel central
		JPanel panelCentral = new JPanel();
		panelCentral.setMinimumSize(new Dimension(603, 289));
		panelCentral.setLayout(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		
		// Etiqueta con el texto "Firmas que desea..."
		JLabel etiquetaFirmas = new JLabel();
		etiquetaFirmas.setText(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas"));
		Utils.setContrastColor(etiquetaFirmas);
		Utils.setFontBold(etiquetaFirmas);
		panelCentral.add(etiquetaFirmas, c);

		// Combo con tipos de firmas
		final JLabel etiqueta = new JLabel();
		final JScrollPane panelArbol = new JScrollPane();
		final JScrollPane panelLista = new JScrollPane();
		panelLista.setVisible(false);

        c.insets = new Insets(0, 20, 0, 20);
		c.weightx = 1.0;
		c.gridy	= 1;
		c.weighty = 0.1;
		c.fill = GridBagConstraints.BOTH;
		
		this.comboFirmas.addItemListener(new ItemListener() {
			/**
			 * Evento que se lanza cuando el componente cambia de estado.
			 */
			@Override
            public void itemStateChanged(ItemEvent evt) {
				comboOpcionesItemStateChanged(etiqueta, panelArbol, panelLista);
			}
		});
		this.comboFirmas.setModel(new DefaultComboBoxModel(new String[]{
				Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion1"), //$NON-NLS-1$
				Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion2"), //$NON-NLS-1$
				Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion3"), //$NON-NLS-1$
				Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion4")} //$NON-NLS-1$
		));
		
		this.comboFirmas.setToolTipText(Messages.getString("Wizard.multifirma.simple.contrafirma.comboFirmas.description")); // NOI18N //$NON-NLS-1$
		this.comboFirmas.getAccessibleContext().setAccessibleName(etiquetaFirmas.getText() + " " +this.comboFirmas.getToolTipText() + "ALT + F.");
		this.comboFirmas.getAccessibleContext().setAccessibleDescription(this.comboFirmas.getToolTipText());
		Utils.remarcar(this.comboFirmas);
        Utils.setContrastColor(this.comboFirmas);
		Utils.setFontBold(this.comboFirmas);
		panelCentral.add(this.comboFirmas, c);
		
		 //Relación entre etiqueta y combo
		etiquetaFirmas.setLabelFor(this.comboFirmas);
  		//Asignación de mnemónico
		etiquetaFirmas.setDisplayedMnemonic(KeyEvent.VK_F);

		c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		c.gridy	= 2;

		// Etiqueta con el texto "Arbol de firmas"
		etiqueta.setText(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.arbol"));
		Utils.setContrastColor(etiqueta);
		Utils.setFontBold(etiqueta);
		panelCentral.add(etiqueta, c);

		/*
		 * Para las opciones del combo:
		 * Firmas seleccionadas, todo el arbol y firmas de ultimo nivel hacemos uso del arbol
		 */

		// Arbol de firmas

		// Icono para el arbol
		ImageIcon leafIcon = null;
		try {
			leafIcon = new ImageIcon(getClass().getResource("/resources/images/firma_mini_ico.png"));
		} catch (Exception e) {
			logger.warning("No se ha podido cargar la imagen para los nodos del arbol de firmantes: "+e);
		}
		
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(0, 20, 20, 20);
		c.weightx = 1.0;
		c.gridy	= 3;
		c.weighty = 1.0;
		
		DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
		renderer.setLeafIcon(leafIcon);
		renderer.setOpenIcon(leafIcon);
		renderer.setClosedIcon(leafIcon);
		
		this.arbolFirmas.setRowHeight(25);
		this.arbolFirmas.setCellRenderer(renderer);
		this.arbolFirmas.addTreeExpansionListener(new TreeExpansionListener() {
			/**
			 * Evento que se lanza cuando la rama se contrae.
			 */
			@Override
            public void treeCollapsed(TreeExpansionEvent event) {
				((JTree)event.getSource()).expandPath(event.getPath());
			}
			/**
			 * Evento que se lanza cuando la rama se expande.
			 */
			@Override
            public void treeExpanded(TreeExpansionEvent event) {}
		}); 
		this.arbolFirmas.setToolTipText(Messages.getString("Wizard.multifirma.simple.contrafirma.arbolFirmas.description")); // NOI18N
		this.arbolFirmas.getAccessibleContext().setAccessibleName(etiqueta.getText() + " " + this.arbolFirmas.getToolTipText() + "ALT + R.");
		this.arbolFirmas.setSelectionRow(0);
		this.arbolFirmas.setRootVisible(false);
		Utils.remarcar(this.arbolFirmas);
		Utils.setContrastColor(this.arbolFirmas);
		Utils.setFontBold(this.arbolFirmas);
		// Panel del arbol (firmantes seleccionados)
		panelArbol.setViewportView(this.arbolFirmas);
		panelCentral.add(panelArbol, c);
		
		//Relación entre etiqueta y arbol
		etiqueta.setLabelFor(this.arbolFirmas);
  		//Asignación de mnemónico
		etiqueta.setDisplayedMnemonic(KeyEvent.VK_R);

		/*
		 * Para las opciones del combo:
		 * Firmantes seleccionados hacemos uso del listado
		 */

		// Lista de firmantes ()
		Utils.remarcar(this.listaFirmantes);
		Utils.setFontBold(this.listaFirmantes);
		this.listaFirmantes.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.listaFirmantes")); // NOI18N
		this.listaFirmantes.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.listaFirmantes.description")); // NOI18N

		// Panel de la lista
		panelLista.setViewportView(this.listaFirmantes);
		panelCentral.add(panelLista, c);
		
		//Relación entre etiqueta y lista
		etiqueta.setLabelFor(this.listaFirmantes);
  		//Asignación de mnemónico
		etiqueta.setDisplayedMnemonic(KeyEvent.VK_R);

		getContentPane().add(panelCentral, BorderLayout.CENTER);

		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(panelLista, "multifirma.wizard.lista");
		HelpUtils.enableHelpKey(this.comboFirmas, "multifirma.wizard.lista");
		HelpUtils.enableHelpKey(panelArbol, "multifirma.wizard.arbolfirmantes"); 
	}

	/**
	 * Modifica el interfaz cambiando el arbol por un listado dependiendo
	 * de la opci&oacute;n seleccionada
	 * @param etiqueta 		Etiqueta superior al arbol/listado
	 * @param panelArbol 	Panel del arbol
	 * @param panelLista	Panel de la lista	
	 */
	void comboOpcionesItemStateChanged(JLabel etiqueta, 
			JScrollPane panelArbol, JScrollPane panelLista) {
		// Mostramos el listado para Firmantes seleccionados
		if (this.comboFirmas.getSelectedIndex() == 1){
			panelArbol.setVisible(false);
			panelLista.setVisible(true);
			etiqueta.setText(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.listaFirmantes"));
		} 
		// Mostramos el arbol para todos los demas casos
		else {
			panelArbol.setVisible(true);
			panelLista.setVisible(false);
			etiqueta.setText(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.arbol"));
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
		public Botonera(List<JDialogWizard> ventanas, int posicion) {
			super(ventanas, posicion);
		}
		/**
		 * Accion para el boton siguiente.
		 */
		@Override
		protected void siguienteActionPerformed(JButton anterior,
				JButton siguiente, JButton finalizar) {

			// Si solo hay un elemento, se selecciona automaticamente
			if (PanelMultifirma.this.arbolFirmas.getRowCount() == 1) {
				PanelMultifirma.this.arbolFirmas.setSelectionRow(0);
			}
			
			// Comprobamos si se ha seleccionado algun elemento
			if (PanelMultifirma.this.arbolFirmas.isVisible() && PanelMultifirma.this.arbolFirmas.getSelectionCount() == 0) {
				CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.lista"), 
						Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			} else {
				// Salvo que la firma finalice correctamente, permaneceremos en la ventana actual
				if (multifirmarFichero()) {
					super.siguienteActionPerformed(anterior, siguiente, finalizar);
				}
			}
		}
	}

	/**
	 * Multifirma un fichero dado
	 * @return	true o false indicando si se ha multifirmado correctamente
	 */
	public boolean multifirmarFichero() {
		try {
			String intText = ".countersign";
			byte[] signedData = null;
			MultisignUtils msUtils = new MultisignUtils();
			AOKeyStoreManager keyStoreManager = msUtils.getAOKeyStoreManager(this.kssc,this);
			
			byte[] signData1 = readFile(this.rutaFichero);
			
			// Recuperamos la clave del certificado
			PrivateKeyEntry keyEntry = msUtils.getPrivateKeyEntry(this.kssc, keyStoreManager, this);
			AOSigner signer = AOSignerFactory.getSigner(signData1);

			if (signer == null) {
				throw new AOFormatFileException("El cliente no dispone de ningun manejador que soporte\r\nel fichero de firma indicado"); //$NON-NLS-1$
			}
			if (!signer.isSign(signData1)) {
				throw new AOInvalidFormatException("El fichero '" + this.rutaFichero //$NON-NLS-1$
						+ "' no es un fichero de firma soportado."); //$NON-NLS-1$
			}
			
			// Firmamos
			signedData = counterSignOperation(signer, signData1, keyEntry);

			if (signedData == null)
				return false;

			// Salvamos el fichero de datos
			final File savedFile = SelectionDialog.saveDataToFile(Messages.getString("Wizard.multifirma.simple.contrafirma.filechooser.save.title"), signedData,
                    signer.getSignedName(this.rutaFichero, intText), null, this);
			// Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
			if (savedFile == null) {
				return false;
			}
		} catch (AOCancelledOperationException e){
		    logger.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
		    return false;
		} 
		catch (final Exception e){
			logger.severe(e.toString());
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error"),  //$NON-NLS-1$
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
			return false;
		}
		
		return true;
	}
	
	/**
	 * Cofirma de un fichero de datos.
	 * @param signer Manejador de firma.
	 * @param signData1 Firma que queremos contrafirmar.
	 * @param keyEntry Clave de firma.
	 * @return Contenido de la firma.
	 * @throws FileNotFoundException No se encuentra el fichero de datos.
	 * @throws AOException Error durante el proceso de firma.
	 */
	private byte[] counterSignOperation(AOSigner signer, byte[] signData1, PrivateKeyEntry keyEntry) throws FileNotFoundException, AOException {

		// Realizamos la cofirma
		byte[] signedData = null;
		try {
			// Configuramos la operacion
			Properties prop = GeneralConfig.getSignConfig();
			
			switch (this.comboFirmas.getSelectedIndex()){
				// NODOS SELECCIONADOS
				case 0: {
					int[] nodosSeleccionados = getSelectedSignNodes();
					if (nodosSeleccionados == null) {
						CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.lista"),  //$NON-NLS-1$
								Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
						return null;
					}
				    final Integer[] nodos = new Integer[nodosSeleccionados.length];
				    int i = 0;
				    for (int value : nodosSeleccionados) {
				        nodos[i++] = Integer.valueOf(value);
				    }
					signedData = signer.countersign(
                        signData1, 
                        GeneralConfig.getSignAlgorithm(), 
                        CounterSignTarget.NODES,
						nodos, 
						keyEntry, 
						prop
					);
					break;
				}
				// FIRMANTES SELECCIONADOS
				case 1:{
					String[] nodosSeleccionados = getSelectedSignNodesS();
					if (nodosSeleccionados == null) {
						CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.firmante"), 
								Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
						return null;
					}
					else {
						signedData = signer.countersign(signData1, GeneralConfig.getSignAlgorithm(), CounterSignTarget.SIGNERS,
								nodosSeleccionados, keyEntry, prop);
					}
					break;
				}
				// EL ARBOL DE FIRMAS
				case 2:{
					signedData = signer.countersign(signData1, GeneralConfig.getSignAlgorithm(), CounterSignTarget.TREE,
							null, keyEntry, prop);
					break;
				}
				// FIRMA DE HOJAS
				case 3:{
					signedData = signer.countersign(signData1, GeneralConfig.getSignAlgorithm(), CounterSignTarget.LEAFS,
							null, keyEntry, prop);
					break;
				}
			}
		} catch (AOException e) {
			logger.warning(e.getMessage()+": "+e);
			CustomDialog.showMessageDialog(this, true, e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
			return null;
		} catch (UnsupportedOperationException e) {
			logger.warning("La firma seleccionada no soporta la operacion de contrafirma: "+e); //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.firma.soporte"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
			return null;
		} catch (Exception e){
			logger.warning("Error al contrafirmar el fichero de firma: "+e);
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.contrafirmar"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return null;
		}

		return signedData;
	}
	
	/**
	 * Recuperamos el listado de nodos de firma seleccionados.
	 * @return &Iacute;ndices de los nodos de firma seleccionados.
	 */
	private int[] getSelectedSignNodes() {
		if (this.arbolFirmas.getSelectionRows() != null) {
			int[] nodesIndexes = this.arbolFirmas.getSelectionRows();
			// El array de nodos debe ir ordenado para asegurar que se firman los nodos correctos
			Arrays.sort(nodesIndexes);
			int[] nodesIndexesI = new int[nodesIndexes.length];
			for (int i=0; i<nodesIndexes.length; i++) {
				nodesIndexesI[i] = nodesIndexes[i];
			}
			return nodesIndexesI;
		}
		return null;
	}
	
	/**
	 * Recuperamos el listado de nodos de firma seleccionados.
	 * @return &Iacute;ndices de los nodos de firma seleccionados.
	 */
	private String[] getSelectedSignNodesS() {
		if (this.listaFirmantes.getSelectedValues() != null) {
			// Devolvemos los firmantes seleccionados
			Object[] selectedValues = this.listaFirmantes.getSelectedValues();
	
			String[] signers = new String[selectedValues.length];
			for (int i=0; i<selectedValues.length; i++) 
				signers[i] = (String)selectedValues[i];
			
			return signers;
		}
		return null;
	}
	
	private byte[] readFile(String filepath) {
		byte[] data = null;
		InputStream fileIn = null;
		try {
			fileIn = AOUtil.loadFile(AOUtil.createURI(filepath));
			data = AOUtil.getDataFromInputStream(fileIn);
		} 
		catch (FileNotFoundException e) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.fichero.encontrar"),  //$NON-NLS-1$
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
			return null;
		} 
		catch (IOException e) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.fichero.leer"),  //$NON-NLS-1$
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
			return null;
		}
		catch (AOException e) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		} 
		finally {
			if (fileIn != null) {
				try { fileIn.close(); } catch (Exception e) {}
			}
		}
		
		return data;
	}
}
