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
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
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
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.ui.core.jse.JSEUtils;
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

/** Panel para el wizard de multifirma.
 * @author inteco */
final class PanelMultifirma extends JAccessibilityDialogWizard {

    /** Botonera con funciones para la pagina panel de multifirma - cofirma */
    private class Botonera extends BotoneraInferior {

        /** UID. */
        private static final long serialVersionUID = 1L;

        /** Constructor.
         * @param ventanas Lista de ventanas que componen el wizard.
         * @param posicion posicion de la ventana donde se inserta esta botonera. */
        public Botonera(final List<JDialogWizard> ventanas, final int posicion) {
            super(ventanas, posicion);
        }

        /** Accion para el boton siguiente. */
        @Override
        protected void siguienteActionPerformed(final JButton anterior, final JButton siguiente, final JButton finalizar) {

            // Si solo hay un elemento, se selecciona automaticamente
            if (PanelMultifirma.this.getArbolFirmas().getRowCount() == 1) {
                PanelMultifirma.this.getArbolFirmas().setSelectionRow(0);
            }

            // Comprobamos si se ha seleccionado algun elemento
            if (PanelMultifirma.this.getArbolFirmas().isVisible() && PanelMultifirma.this.getArbolFirmas().getSelectionCount() == 0) {
                CustomDialog.showMessageDialog(this,
                                               true,
                                               Messages.getString("Wizard.multifirma.simple.error.lista"), //$NON-NLS-1$
                                               Messages.getString("error"), //$NON-NLS-1$
                                               JOptionPane.ERROR_MESSAGE);
            }
            else {
                // Salvo que la firma finalice correctamente, permaneceremos en la ventana actual
                if (multifirmarFichero()) {
                    super.siguienteActionPerformed(anterior, siguiente, finalizar);
                }
            }
        }
    }

    /** log. */
    private static final Logger LOGGER = Logger.getLogger(PanelMultifirma.class.getName());

    /** UID. */
    private static final long serialVersionUID = 1L;

    // Arbol de firmas
    private final JTree arbolFirmas = new JTree();
    JTree getArbolFirmas() {
    	return this.arbolFirmas;
    }

    // Combo con las opciones de firma
    private final JComboBox<String> comboFirmas = new JComboBox<>();

    /** Configuracion del KeyStore */
    private KeyStoreConfiguration kssc = null;

    // Listado de firmantes
    private final JList listaFirmantes = new JList();

    /** Ruta del fichero a multifirmar */
    private String rutaFichero;

    /** Constructor.
     * @param kssc configuracion. */
    public PanelMultifirma(final KeyStoreConfiguration kssc) {
        this.kssc = kssc;
        initComponents();
    }

    /** Carga el modelo del arbol y del listado con el archivo de firma seleccionado
     * @param signPath Ruta del fichero de firma
     * @param sign Firma
     * @return {@code true} si se ha cargado correctamente. */
    public boolean cargarDatos(final String signPath, final byte[] sign) {
        this.rutaFichero = signPath;
        /** Firma que se desea multifirmar. */

        final byte[] signData = sign.clone();

        // Generamos el modelo del arbol a partir del fichero
        javax.swing.tree.DefaultTreeModel modeloArbolSwing;
        try {
            final AOTreeModel modeloArbol = AOSignerFactory.getSigner(signData).getSignersStructure(signData, false);
            modeloArbolSwing = JSEUtils.convertToSwingModel(modeloArbol);

            this.arbolFirmas.setModel(modeloArbolSwing);

            for (int i = 0; i < this.arbolFirmas.getRowCount(); i++) {
                this.arbolFirmas.expandRow(i);
            }

            this.arbolFirmas.setSelectionRow(0);
        }
        catch (final Exception e) {
            LOGGER.severe("No se pudo cargar el arbol de firmas del fichero '" + this.rutaFichero + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.multifirma.simple.error.arbol"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // Generamos el modelo de la lista a partir del modelo del arbol
        final HashSet<String> signersSet = new HashSet<>();

        // Recorremos todos los nodos menos el root que no contiene informacion de firmante
        final DefaultMutableTreeNode root = (DefaultMutableTreeNode) modeloArbolSwing.getRoot();
        try {
            for (int i = 0; i < root.getChildCount(); i++) {
                getSigners((DefaultMutableTreeNode) root.getChildAt(i), signersSet);
            }
        }
        catch (final Exception e) {
            LOGGER.warning("El arbol introducido contiene elementos no validos: " + e); //$NON-NLS-1$
            return false;
        }

        // Recogemos los firmantes de los nodos
        final String[] signers = new String[signersSet.size()];
        signersSet.toArray(signers);


        /** Modelo de la lista */
        final AbstractListModel modeloLista = new AbstractListModel() {

            private static final long serialVersionUID = 1L;

            private final String[] strings = signers;

            /** Devuelve el elemento contenido en la posicion indicada. */
            @Override
            public Object getElementAt(final int i) {
                return this.strings[i];
            }

            /** Devuelve el tamano. */
            @Override
            public int getSize() {
                return this.strings.length;
            }
        };

        // Asignamos el modelo
        this.listaFirmantes.setModel(modeloLista);

        return true;
    }
    /** Modifica el interfaz cambiando el arbol por un listado dependiendo
     * de la opci&oacute;n seleccionada
     * @param etiqueta Etiqueta superior al arbol/listado
     * @param panelArbol Panel del arbol
     * @param panelLista Panel de la lista */
    void comboOpcionesItemStateChanged(final JLabel etiqueta, final JScrollPane panelArbol, final JScrollPane panelLista) {
        // Mostramos el listado para Firmantes seleccionados
        if (this.comboFirmas.getSelectedIndex() == 1) {
            panelArbol.setVisible(false);
            panelLista.setVisible(true);
            etiqueta.setText(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.listaFirmantes")); //$NON-NLS-1$
        }
        // Mostramos el arbol para todos los demas casos
        else {
            panelArbol.setVisible(true);
            panelLista.setVisible(false);
            etiqueta.setText(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.arbol")); //$NON-NLS-1$
        }
    }
    /** Cofirma de un fichero de datos.
     * @param signer Manejador de firma.
     * @param signData1 Firma que queremos contrafirmar.
     * @param keyEntry Clave de firma.
     * @return Contenido de la firma.
     * @throws FileNotFoundException No se encuentra el fichero de datos.
     * @throws AOException Error durante el proceso de firma. */
    private byte[] counterSignOperation(final AOSigner signer, final byte[] signData1, final PrivateKeyEntry keyEntry) throws FileNotFoundException, AOException {

        // Realizamos la cofirma
        byte[] signedData = null;
        try {
            // Configuramos la operacion
            final Properties prop = GeneralConfig.getSignConfig();

            // En el caso de firmas XAdES no incluimos la cadena de certificacion
            if (signer instanceof AOXAdESSigner) {
            	prop.setProperty("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            }

            switch (this.comboFirmas.getSelectedIndex()) {
                // NODOS SELECCIONADOS
                case 0: {
                    final int[] nodosSeleccionados = getSelectedSignNodes();
                    if (nodosSeleccionados == null) {
                        CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error.lista"), //$NON-NLS-1$
                                                       Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                        return null;
                    }
                    final Integer[] nodos = new Integer[nodosSeleccionados.length];
                    int i = 0;
                    for (final int value : nodosSeleccionados) {
                        nodos[i++] = Integer.valueOf(value);
                    }
                    signedData = signer.countersign(
                		signData1,
                		GeneralConfig.getSignAlgorithm(),
                		CounterSignTarget.NODES,
                		nodos,
                		keyEntry.getPrivateKey(),
                		keyEntry.getCertificateChain(),
                		prop
            		);
                    break;
                }
                // FIRMANTES SELECCIONADOS
                case 1: {
                    final String[] nodosSeleccionados = getSelectedSignNodesS();
                    if (nodosSeleccionados == null) {
                        CustomDialog.showMessageDialog(this,
                                                       true,
                                                       Messages.getString("Wizard.multifirma.simple.error.firmante"), //$NON-NLS-1$
                                                       Messages.getString("error"), //$NON-NLS-1$
                                                       JOptionPane.ERROR_MESSAGE);
                        return null;
                    }
                    signedData =
                        signer.countersign(
                    		signData1,
                            GeneralConfig.getSignAlgorithm(),
                            CounterSignTarget.SIGNERS,
                            nodosSeleccionados,
                            keyEntry.getPrivateKey(),
                            keyEntry.getCertificateChain(),
                            prop
                        );
                    break;
                }
                // EL ARBOL DE FIRMAS
                case 2: {
                    signedData = signer.countersign(
                		signData1,
                		GeneralConfig.getSignAlgorithm(),
                		CounterSignTarget.TREE,
                		null,
                		keyEntry.getPrivateKey(),
                		keyEntry.getCertificateChain(),
                		prop
            		);
                    break;
                }
                // FIRMA DE HOJAS
                case 3: {
                    signedData = signer.countersign(
                		signData1,
                		GeneralConfig.getSignAlgorithm(),
                		CounterSignTarget.LEAFS,
                		null,
                		keyEntry.getPrivateKey(),
                		keyEntry.getCertificateChain(),
                		prop
            		);
                    break;
                }
            }
        }
        catch (final AOException e) {
            LOGGER.warning(e.getMessage() + ": " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, e.getMessage(), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return null;
        }
        catch (final UnsupportedOperationException e) {
            LOGGER.warning("La firma seleccionada no soporta la operacion de contrafirma: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.multifirma.simple.error.firma.soporte"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.warning("Error al contrafirmar el fichero de firma: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.multifirma.simple.error.contrafirmar"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return null;
        }

        return signedData;
    }

    /** Relacion minima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 9;
    }

    /** Recuperamos el listado de nodos de firma seleccionados.
     * @return &Iacute;ndices de los nodos de firma seleccionados. */
    private int[] getSelectedSignNodes() {
        if (this.arbolFirmas.getSelectionRows() != null) {
            final int[] nodesIndexes = this.arbolFirmas.getSelectionRows();
            // El array de nodos debe ir ordenado para asegurar que se firman los nodos correctos
            Arrays.sort(nodesIndexes);
            final int[] nodesIndexesI = new int[nodesIndexes.length];
            System.arraycopy(nodesIndexes, 0, nodesIndexesI, 0, nodesIndexes.length);
            return nodesIndexesI;
        }
        return null;
    }

    /** Recuperamos el listado de nodos de firma seleccionados.
     * @return &Iacute;ndices de los nodos de firma seleccionados. */
    private String[] getSelectedSignNodesS() {
        if (this.listaFirmantes.getSelectedValues() != null) {
            // Devolvemos los firmantes seleccionados
            final Object[] selectedValues = this.listaFirmantes.getSelectedValues();
            final String[] signers = new String[selectedValues.length];
            for (int i = 0; i < selectedValues.length; i++) {
                signers[i] = (String) selectedValues[i];
            }
            return signers;
        }
        return null;
    }

    /** Introduce los alias de todos los certificados de firma del nodo indicado y todos sus subnodos.
     * @param node Nodo de firma.
     * @param signersSet Conjunto con los alias de los certificados de firma. */
    private void getSigners(final DefaultMutableTreeNode node, final Set<String> signersSet) {
        signersSet.add((String) node.getUserObject());
        for (int i = 0; i < node.getChildCount(); i++) {
            getSigners((DefaultMutableTreeNode) node.getChildAt(i), signersSet);
        }
    }

    /** Inicializaci&oacute;n de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("Wizard.multifirma.simple.contrafirma.titulo")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.multifirma.simple.contrafirma.ventana2.titulo", //$NON-NLS-1$
                                                                      "Wizard.multifirma.simple.contrafirma.ventana2.titulo.description", null); //$NON-NLS-1$
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);

        // Panel central
        final JPanel panelCentral = new JPanel();
        panelCentral.setMinimumSize(new Dimension(603, 289));
        panelCentral.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 0, 20);
        c.weightx = 1.0;

        // Etiqueta con el texto "Firmas que desea..."
        final JLabel etiquetaFirmas = new JLabel();
        etiquetaFirmas.setText(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas")); //$NON-NLS-1$
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
        c.gridy = 1;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        this.comboFirmas.addItemListener(new ItemListener() {
            /** Evento que se lanza cuando el componente cambia de estado. */
            @Override
            public void itemStateChanged(final ItemEvent evt) {
                comboOpcionesItemStateChanged(etiqueta, panelArbol, panelLista);
            }
        });
        this.comboFirmas.setModel(new DefaultComboBoxModel<>(new String[] {
             Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion1"), //$NON-NLS-1$
             Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion2"), //$NON-NLS-1$
             Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion3"), //$NON-NLS-1$
             Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.combo.firmas.opcion4")} //$NON-NLS-1$
        ));

        this.comboFirmas.setToolTipText(Messages.getString("Wizard.multifirma.simple.contrafirma.comboFirmas.description")); // NOI18N //$NON-NLS-1$
        this.comboFirmas.getAccessibleContext().setAccessibleName(etiquetaFirmas.getText() + " " + this.comboFirmas.getToolTipText() + "ALT + F."); //$NON-NLS-1$ //$NON-NLS-2$
        this.comboFirmas.getAccessibleContext().setAccessibleDescription(this.comboFirmas.getToolTipText());
        Utils.remarcar(this.comboFirmas);
        Utils.setContrastColor(this.comboFirmas);
        Utils.setFontBold(this.comboFirmas);
        panelCentral.add(this.comboFirmas, c);

        // Relacion entre etiqueta y combo
        etiquetaFirmas.setLabelFor(this.comboFirmas);
        // Asignacion de mnemonico
        etiquetaFirmas.setDisplayedMnemonic(KeyEvent.VK_F);

        c.insets = new Insets(20, 20, 0, 20);
        c.weightx = 1.0;
        c.gridy = 2;

        // Etiqueta con el texto "Arbol de firmas"
        etiqueta.setText(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.arbol")); //$NON-NLS-1$
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
            leafIcon = new ImageIcon(getClass().getResource("/resources/images/firma_mini_ico.png")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido cargar la imagen para los nodos del arbol de firmantes: " + e); //$NON-NLS-1$
        }

        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(0, 20, 20, 20);
        c.weightx = 1.0;
        c.gridy = 3;
        c.weighty = 1.0;

        final DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
        renderer.setLeafIcon(leafIcon);
        renderer.setOpenIcon(leafIcon);
        renderer.setClosedIcon(leafIcon);

        this.arbolFirmas.setRowHeight(25);
        this.arbolFirmas.setCellRenderer(renderer);
        this.arbolFirmas.addTreeExpansionListener(new TreeExpansionListener() {
            /** Evento que se lanza cuando la rama se contrae. */
            @Override
            public void treeCollapsed(final TreeExpansionEvent event) {
                ((JTree) event.getSource()).expandPath(event.getPath());
            }

            /** Evento que se lanza cuando la rama se expande. */
            @Override
            public void treeExpanded(final TreeExpansionEvent event) { /* se ignora */ }
        });
        this.arbolFirmas.setToolTipText(Messages.getString("Wizard.multifirma.simple.contrafirma.arbolFirmas.description")); // NOI18N //$NON-NLS-1$
        this.arbolFirmas.getAccessibleContext().setAccessibleName(etiqueta.getText() + " " + this.arbolFirmas.getToolTipText() + "ALT + R.");  //$NON-NLS-1$//$NON-NLS-2$
        this.arbolFirmas.setSelectionRow(0);
        this.arbolFirmas.setRootVisible(false);
        Utils.remarcar(this.arbolFirmas);
        Utils.setContrastColor(this.arbolFirmas);
        Utils.setFontBold(this.arbolFirmas);
        // Panel del arbol (firmantes seleccionados)
        panelArbol.setViewportView(this.arbolFirmas);
        panelCentral.add(panelArbol, c);

        /*
         * Para las opciones del combo:
         * Firmantes seleccionados hacemos uso del listado
         */

        // Lista de firmantes ()
        Utils.remarcar(this.listaFirmantes);
        Utils.setFontBold(this.listaFirmantes);
        this.listaFirmantes.getAccessibleContext()
        .setAccessibleName(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.listaFirmantes")); // NOI18N //$NON-NLS-1$
        this.listaFirmantes.getAccessibleContext()
        .setAccessibleDescription(Messages.getString("Wizard.multifirma.simple.contrafirma.ventana2.listaFirmantes.description")); // NOI18N //$NON-NLS-1$

        // Panel de la lista
        panelLista.setViewportView(this.listaFirmantes);
        panelCentral.add(panelLista, c);

        // Relacion entre etiqueta y arbol
        etiqueta.setLabelFor(this.arbolFirmas);
        // Asignacion de mnemonico
        etiqueta.setDisplayedMnemonic(KeyEvent.VK_B);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(panelLista, "multifirma.wizard.lista"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.comboFirmas, "multifirma.wizard.lista"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(panelArbol, "multifirma.wizard.arbolfirmantes"); //$NON-NLS-1$
    }

    /** Multifirma un fichero dado
     * @return true o false indicando si se ha multifirmado correctamente */
    boolean multifirmarFichero() {
        try {
            final String intText = ".countersign"; //$NON-NLS-1$
            byte[] signedData = null;
            final AOKeyStoreManager keyStoreManager = MultisignUtils.getAOKeyStoreManager(this.kssc, this);

            final byte[] signData1 = readFile(this.rutaFichero);

            // Recuperamos la clave del certificado
            final PrivateKeyEntry keyEntry = MultisignUtils.getPrivateKeyEntry(this.kssc, keyStoreManager, this);
            final AOSigner signer = AOSignerFactory.getSigner(signData1);

            if (signer == null) {
                throw new AOFormatFileException("El cliente no dispone de ningun manejador que soporte\r\nel fichero de firma indicado"); //$NON-NLS-1$
            }
            if (!signer.isSign(signData1)) {
                throw new AOInvalidFormatException("El fichero '" + this.rutaFichero //$NON-NLS-1$
                                                   + "' no es un fichero de firma soportado."); //$NON-NLS-1$
            }

            // Firmamos
            signedData = counterSignOperation(signer, signData1, keyEntry);

            if (signedData == null) {
                return false;
            }

            // Salvamos el fichero de datos
            final File savedFile =
                SelectionDialog.saveDataToFile(Messages.getString("Wizard.multifirma.simple.contrafirma.filechooser.save.title"), //$NON-NLS-1$
                                               signedData,
                                               signer.getSignedName(this.rutaFichero, intText),
                                               null,
                                               this);
            // Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
            if (savedFile == null) {
                return false;
            }
        }
        catch (final AOCancelledOperationException e) {
            LOGGER.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
            return false;
        }
        catch (final Exception e) {
            LOGGER.severe(e.toString());
            CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.simple.error"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }

        return true;
    }

    private byte[] readFile(final String filepath) {
        try ( final InputStream fileIn = AOUtil.loadFile(AOUtil.createURI(filepath)); ) {
            return AOUtil.getDataFromInputStream(fileIn);
        }
        catch (final FileNotFoundException e) {
            CustomDialog.showMessageDialog(
        		this,
        		true,
        		Messages.getString("Wizard.multifirma.simple.error.fichero.encontrar"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        }
        catch (final Exception e) {
        	Logger.getLogger("es.gob.afirma").severe("Error al leer el fichero " + filepath + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            CustomDialog.showMessageDialog(
        		this,
        		true,
        		Messages.getString("Wizard.multifirma.simple.error.fichero.leer"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        }
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        return null;
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 2));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
}
