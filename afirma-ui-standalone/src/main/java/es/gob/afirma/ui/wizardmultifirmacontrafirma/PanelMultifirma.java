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
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.ui.jse.JSEUtils;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.keystores.common.AOKeyStoreManager;
import es.gob.afirma.keystores.common.KeyStoreConfiguration;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.JAccessibilityOptionPane;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.MultisignUtils;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.util.signers.AOSignerFactory;


public class PanelMultifirma extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelMultifirma.class.getName());

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
		Botonera botonera = new Botonera(ventanas, 2);
		getContentPane().add(botonera, BorderLayout.PAGE_END);
	}

	/**
	 * Carga el modelo del arbol y del listado con el archivo de firma seleccionado
	 * @param signPath	Ruta del fichero de firma
	 * @param sign Firma
	 * @return	{@code true} si se ha cargado correctamente.
	 */
	public Boolean cargarDatos(String signPath, byte[] sign) {
		this.rutaFichero = signPath;
		this.signData = sign;

		// Generamos el modelo del arbol a partir del fichero
		FileInputStream fis = null;
		javax.swing.tree.DefaultTreeModel modeloArbolSwing;
		try {
			AOTreeModel modeloArbol = (AOTreeModel) AOSignerFactory.getSigner(signData).getSignersStructure(signData, false);
			modeloArbolSwing = JSEUtils.convertToSwingModel(modeloArbol);

			arbolFirmas.setModel(modeloArbolSwing);

			for (int i=0; i<arbolFirmas.getRowCount(); i++) {
				arbolFirmas.expandRow(i);
			}

			arbolFirmas.setSelectionRow(0);
		} catch (Exception e) {
			logger.severe("No se pudo cargar el arbol de firmas del fichero '" + rutaFichero + "': "+e);
			JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error.arbol"), 
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

		modeloLista = new AbstractListModel() {

			private static final long serialVersionUID = 1L;
			
			String[] strings = signers;

			public int getSize() { 
				return strings.length; 
			}

			public Object getElementAt(int i) { 
				return strings[i]; 
			}
		};

		// Asignamos el modelo
		listaFirmantes.setModel(modeloLista);

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
    	setTitulo(Messages.getString("Wizard.multifirma.simple.contrafirma.titulo"));
		
		// Panel con la cabecera
		CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.multifirma.simple.contrafirma.ventana2.titulo", 
				"Wizard.multifirma.simple.contrafirma.ventana2.titulo.description", null, true);
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
		
		comboFirmas.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent evt) {
				comboOpcionesItemStateChanged(etiqueta, panelArbol, panelLista);
			}
		});
		comboFirmas.setModel(new DefaultComboBoxModel(new String[]{
				Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion1"),
				Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion2"),
				Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion3"),
				Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion4")}
		));
		
		comboFirmas.setToolTipText(Messages.getString("Wizard.multifirma.simple.contrafirma.comboFirmas.description")); // NOI18N
		comboFirmas.getAccessibleContext().setAccessibleName(etiquetaFirmas.getText() + " " +comboFirmas.getToolTipText() + "ALT + F.");
		comboFirmas.getAccessibleContext().setAccessibleDescription(comboFirmas.getToolTipText());
		Utils.remarcar(comboFirmas);
        Utils.setContrastColor(comboFirmas);
		Utils.setFontBold(comboFirmas);
		panelCentral.add(comboFirmas, c);
		
		 //Relación entre etiqueta y combo
		etiquetaFirmas.setLabelFor(comboFirmas);
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
		
		arbolFirmas.setRowHeight(25);
		arbolFirmas.setCellRenderer(renderer);
		arbolFirmas.addTreeExpansionListener(new TreeExpansionListener() {
			public void treeCollapsed(TreeExpansionEvent event) {
				((JTree)event.getSource()).expandPath(event.getPath());
			}
			public void treeExpanded(TreeExpansionEvent event) {}
		}); 
		arbolFirmas.setToolTipText(Messages.getString("Wizard.multifirma.simple.contrafirma.arbolFirmas.description")); // NOI18N
		arbolFirmas.getAccessibleContext().setAccessibleName(etiqueta.getText() + " " + arbolFirmas.getToolTipText() + "ALT + R.");
		arbolFirmas.setSelectionRow(0);
		arbolFirmas.setRootVisible(false);
		Utils.remarcar(arbolFirmas);
		Utils.setContrastColor(arbolFirmas);
		Utils.setFontBold(arbolFirmas);
		// Panel del arbol (firmantes seleccionados)
		panelArbol.setViewportView(arbolFirmas);
		panelCentral.add(panelArbol, c);
		
		//Relación entre etiqueta y arbol
		etiqueta.setLabelFor(arbolFirmas);
  		//Asignación de mnemónico
		etiqueta.setDisplayedMnemonic(KeyEvent.VK_R);

		/*
		 * Para las opciones del combo:
		 * Firmantes seleccionados hacemos uso del listado
		 */

		// Lista de firmantes ()
		Utils.remarcar(listaFirmantes);
		Utils.setFontBold(listaFirmantes);
		listaFirmantes.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.listaFirmantes")); // NOI18N
		listaFirmantes.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.listaFirmantes.description")); // NOI18N

		// Panel de la lista
		panelLista.setViewportView(listaFirmantes);
		panelCentral.add(panelLista, c);
		
		//Relación entre etiqueta y lista
		etiqueta.setLabelFor(listaFirmantes);
  		//Asignación de mnemónico
		etiqueta.setDisplayedMnemonic(KeyEvent.VK_R);

		getContentPane().add(panelCentral, BorderLayout.CENTER);

		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(panelLista, "multifirma.wizard.lista");
		HelpUtils.enableHelpKey(comboFirmas, "multifirma.wizard.lista");
		HelpUtils.enableHelpKey(panelArbol, "multifirma.wizard.arbolfirmantes"); 
	}

	/**
	 * Modifica el interfaz cambiando el arbol por un listado dependiendo
	 * de la opciï¿½n seleccionada
	 * @param etiqueta 		Etiqueta superior al arbol/listado
	 * @param panelArbol 	Panel del arbol
	 * @param panelLista	Panel de la lista	
	 */
	private void comboOpcionesItemStateChanged(JLabel etiqueta, 
			JScrollPane panelArbol, JScrollPane panelLista) {
		// Mostramos el listado para Firmantes seleccionados
		if (comboFirmas.getSelectedIndex() == 1){
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

		private static final long serialVersionUID = 1L;

		public Botonera(List<JDialogWizard> ventanas, Integer posicion) {
			super(ventanas, posicion);
		}

		@Override
		protected void siguienteActionPerformed(JButton anterior,
				JButton siguiente, JButton finalizar) {

			// Si solo hay un elemento, se selecciona automaticamente
			if (arbolFirmas.getRowCount() == 1) {
				arbolFirmas.setSelectionRow(0);
			}
			
			// Comprobamos si se ha seleccionado algun elemento
			if (arbolFirmas.isVisible() && arbolFirmas.getSelectionCount() == 0) {
				JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error.lista"), 
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
			AOKeyStoreManager keyStoreManager = msUtils.getAOKeyStoreManager(kssc,this);
			
			byte[] signData = readFile(rutaFichero);
			
			// Recuperamos la clave del certificado
			PrivateKeyEntry keyEntry = msUtils.getPrivateKeyEntry(kssc, keyStoreManager, this);
			AOSigner signer = AOSignerFactory.getSigner(signData);

			if (signer == null) {
				throw new AOFormatFileException("El cliente no dispone de ning\u00FAn manejador que soporte\r\nel fichero de firma indicado");
			}
			if (!signer.isSign(signData)) {
				throw new AOInvalidFormatException("El fichero '" + rutaFichero
						+ "' no es un fichero de firma soportado.");
			}
			
			// Firmamos
			signedData = counterSignOperation(signer, signData, keyEntry);

			if (signedData == null)
				return false;

			// Salvamos el fichero de datos
			final File savedFile = SelectionDialog.saveDataToFile(Messages.getString("Wizard.multifirma.simple.contrafirma.filechooser.save.title"), signedData,
                    signer.getSignedName(rutaFichero, intText), null, this);
			// Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
			if (savedFile == null) {
				return false;
			}
		} catch (AOCancelledOperationException e){
		    logger.warning("Operacion cancelada por el usuario: " + e);
		    return false;
		} catch (Exception e){
		    e.printStackTrace();
			logger.severe(e.toString());
			JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		}
		
		return true;
	}
	
	/**
	 * Cofirma de un fichero de datos.
	 * @param signer Manejador de firma.
	 * @param signData Firma que queremos contrafirmar.
	 * @param keyEntry Clave de firma.
	 * @return Contenido de la firma.
	 * @throws FileNotFoundException No se encuentra el fichero de datos.
	 * @throws AOException Ocurrio un error durante el proceso de firma.
	 */
	private byte[] counterSignOperation(AOSigner signer, byte[] signData, PrivateKeyEntry keyEntry) throws FileNotFoundException, AOException {

		// Realizamos la cofirma
		byte[] signedData = null;
		try {
			// Configuramos la operacion
			Properties prop = GeneralConfig.getSignConfig();
			
			switch (comboFirmas.getSelectedIndex()){
				// NODOS SELECCIONADOS
				case 0: {
					Integer[] nodosSeleccionados = getSelectedSignNodes();
					if (nodosSeleccionados == null) {
						JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error.lista"), 
								Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
						return null;
					}
					else {
						signedData = signer.countersign(signData, GeneralConfig.getSignAlgorithm(), CounterSignTarget.Nodes,
								nodosSeleccionados, keyEntry, prop);
					}
					break;
				}
				// FIRMANTES SELECCIONADOS
				case 1:{
					String[] nodosSeleccionados = getSelectedSignNodesS();
					if (nodosSeleccionados == null) {
						JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error.firmante"), 
								Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
						return null;
					}
					else {
						signedData = signer.countersign(signData, GeneralConfig.getSignAlgorithm(), CounterSignTarget.Signers,
								nodosSeleccionados, keyEntry, prop);
					}
					break;
				}
				// EL ARBOL DE FIRMAS
				case 2:{
					signedData = signer.countersign(signData, GeneralConfig.getSignAlgorithm(), CounterSignTarget.Tree,
							null, keyEntry, prop);
					break;
				}
				// FIRMA DE HOJAS
				case 3:{
					signedData = signer.countersign(signData, GeneralConfig.getSignAlgorithm(), CounterSignTarget.Leafs,
							null, keyEntry, prop);
					break;
				}
			}
		} catch (AOException e) {
			logger.warning(e.getMessage()+": "+e);
			JAccessibilityOptionPane.showMessageDialog(this, e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
			return null;
		} catch (UnsupportedOperationException e) {
			logger.warning("La firma seleccionada no soporta la operacion de contrafirma: "+e);
			JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error.firma.soporte"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return null;
		} catch (Exception e){
			logger.warning("Ocurrio un error al contrafirmar el fichero de firma: "+e);
			JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error.contrafirmar"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return null;
		}

		return signedData;
	}
	
	/**
	 * Recuperamos el listado de nodos de firma seleccionados.
	 * @return &Iacute;ndices de los nodos de firma seleccionados.
	 */
	private Integer[] getSelectedSignNodes() {
		if (arbolFirmas.getSelectionRows() != null) {
			int[] nodesIndexes = arbolFirmas.getSelectionRows();
			// El array de nodos debe ir ordenado para asegurar que se firman los nodos correctos
			Arrays.sort(nodesIndexes);
			Integer[] nodesIndexesI = new Integer[nodesIndexes.length];
			for (int i=0; i<nodesIndexes.length; i++) {
				nodesIndexesI[i] = Integer.valueOf(nodesIndexes[i]);
			}
			return nodesIndexesI;
		}
		else
			return null;
	}
	
	/**
	 * Recuperamos el listado de nodos de firma seleccionados.
	 * @return &Iacute;ndices de los nodos de firma seleccionados.
	 */
	private String[] getSelectedSignNodesS() {
		if (listaFirmantes.getSelectedValues() != null) {
			// Devolvemos los firmantes seleccionados
			Object[] selectedValues = listaFirmantes.getSelectedValues();
	
			String[] signers = new String[selectedValues.length];
			for (int i=0; i<selectedValues.length; i++) 
				signers[i] = (String)selectedValues[i];
			
			return signers;
		}
		else
			return null;
	}
	
	private byte[] readFile(String filepath) {
		byte[] data = null;
		InputStream fileIn = null;
		try {
			fileIn = AOUtil.loadFile(AOUtil.createURI(filepath));
			data = AOUtil.getDataFromInputStream(fileIn);
		} catch (FileNotFoundException e) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error.fichero.encontrar"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return null;
		} catch (IOException e) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			JAccessibilityOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.simple.error.fichero.leer"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return null;
		}
		catch (AOException e) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		} finally {
			if (fileIn != null) {
				try { fileIn.close(); } catch (Exception e) {}
			}
		}
		
		return data;
	}
}
