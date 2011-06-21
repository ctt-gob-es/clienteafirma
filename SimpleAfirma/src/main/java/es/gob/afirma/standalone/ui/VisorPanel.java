/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileInputStream;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.signature.ValidateXMLSignature;
import es.gob.afirma.signers.aobinarysignhelper.ValidateCADES;
import es.gob.afirma.signers.aobinarysignhelper.ValidateCMS;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.ui.SignDetailPanel.SIGN_DETAIL_TYPE;

/** Panel para la espera y detecci&oacute;n autom&aacute;tica de insercci&oacute;n de DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Carlos Gamuci
 */
public final class VisorPanel extends JPanel {

    /** Version ID */
    private static final long serialVersionUID = 8309157734617505338L;

    private Frame parent = null;
    
    private final JButton openSign = new JButton();
    
    
    /** Construye un panel de espera a insercci&oacute;n de DNIe.
     * @param kl KeyListener para la detecci&oacute;n de la tecla ESC para el
     *        cierre del aplicativo y F1 para mostrar la ayuda
     * @param al ActionListener para el control de los botones
     * @param safirma SimpleAfirma para establecer el <code>Locale</code> seleccionado en el men&uacute; desplegable */
    public VisorPanel(final File signFile, final byte[] sign, final KeyListener kl, final ActionListener al, final Frame parent) {
        super(true);
        this.parent = parent;
        createUI(signFile, sign, kl, al);
    }
    
    private void createUI(final File signFile, final byte[] sign, final KeyListener kl, final ActionListener al) {
        this.setBackground(SimpleAfirma.WINDOW_COLOR);
        this.setLayout(new GridBagLayout());
        this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));
        
        openSign(signFile, sign);
    }

    private void openFile() {
        File signFile = FileUIManager.openFile(this.parent, null, null, "Abrir");
        if (signFile == null)
            return;
        
        this.openSign(signFile, null);
    }
    
    private void openSign(final File signFile, byte[] sign) {

        // Eliminamos el contenido previo
        removeAll();
        
        if (sign == null) {
            if (signFile != null) {
                try {
                    FileInputStream fis = new FileInputStream(signFile);
                    sign = AOUtil.getDataFromInputStream(fis);
                    try { fis.close(); } catch (Exception e) { }
                } catch (Exception e) {
                    Logger.getLogger("No se ha podido cargar el fichero de firma: " + e);
                }
            }
        }
        
        SIGN_DETAIL_TYPE panelType = SIGN_DETAIL_TYPE.KO;
        if (sign != null) {
            try {
                panelType = validateSign(sign) ? SIGN_DETAIL_TYPE.OK : SIGN_DETAIL_TYPE.KO;
            } catch (Exception e) {
                panelType = SIGN_DETAIL_TYPE.KO;
            }
        }
        
        final JPanel resultPanel = new SignResultPanel(panelType);
        final JPanel dataPanel = new SignDataPanel(signFile.getAbsolutePath(), sign, null, null);

        final JPanel returnPanel = new JPanel(true);
        returnPanel.setLayout(new BoxLayout(returnPanel, BoxLayout.Y_AXIS));
        returnPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
        this.openSign.setText("Abrir firma");
        this.openSign.setMnemonic('a');
        this.openSign.setAlignmentX(Component.CENTER_ALIGNMENT);
        returnPanel.add(this.openSign);
        this.openSign.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent ae) {
                openFile();
            }
        });

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.insets = new Insets(11, 11, 11, 11);
        add(resultPanel, c);
        c.weighty = 1.0;
        c.gridy = 1;
        c.insets = new Insets(0, 11, 11, 11);
        add(dataPanel, c);
        c.weighty = 0.0;
        c.gridy = 2;
        c.insets = new Insets(0, 11, 11, 11);
        add(returnPanel, c);
    }
    
    /**
     * Comprueba la validez de la firma.
     * @param sign Firma que se desea comprobar.
     * @return {@code true} si la firma es v&acute;lida, {@code false} en caso contrario.
     * @throws Exception Cuando los datos introducidos no se corresponden con una firma.
     */
    private boolean validateSign(byte[] sign) throws Exception {
        
        if (DataAnalizerUtil.isPDF(sign)) {
            return true;
        }
        else if (DataAnalizerUtil.isXML(sign)) {
            return ValidateXMLSignature.validate(sign);
        } else if (new ValidateCMS().isCMSSignedData(sign)) {
            return true;
        } else if (new ValidateCADES().isCADESSignedData(sign)) {
            return true;
        }
        return false;
    }
    
    public static void main(String[] args) {
        
        File signFile = new File("C:/Users/A122466/Desktop/Escritorio/Firma.csig");
        
        JPanel currentPanel = new VisorPanel(signFile, null, null, null, null);
        Container container = new MainScreen(null, currentPanel);
        Window window = (JFrame) container;
        
    }
}
