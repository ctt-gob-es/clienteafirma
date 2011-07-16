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

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;

import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.signature.SignValidity;
import es.gob.afirma.signature.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signature.ValidateBinarySignature;
import es.gob.afirma.signature.ValidateXMLSignature;
import es.gob.afirma.signers.AOCAdESSigner;
import es.gob.afirma.signers.AOCMSSigner;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.Messages;
import es.gob.afirma.standalone.VisorFirma;

/** Panel para la espera y detecci&oacute;n autom&aacute;tica de insercci&oacute;n de DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Carlos Gamuci
 */
public final class VisorPanel extends JPanel {

    /** Version ID */
    private static final long serialVersionUID = 8309157734617505338L;
    
    private final VisorFirma visorFirma;


    /** Construye un panel con la informaci&oacute;n extra&iacute;da de una firma. Si no se
     * indica la firma, esta se cargar&aacute; desde un fichero. Es obligatorio introducir
     * alguno de los dos par&aacute;metros. 
     * @param signFile Fichero de firma.
     * @param sign Firma.
     * @param vf VisorFirma para las acciones de los botones
     * @param allowReload <code>true</code> si se desea dar a usuario la opci&oacute;n de ver otras firmas en el
     *                    visor carg&aacute;ndolas mediante un bot&oacute;n, <code>false</code> en caso contrario
     */
    public VisorPanel(final File signFile, final byte[] sign, final VisorFirma vf, final boolean allowReload) {
        super(true);
        this.visorFirma = vf;
        createUI(signFile, sign, allowReload);
    }

    private void createUI(final File signFile, final byte[] sign, final boolean addReloadButton) {
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            this.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        this.setLayout(new GridBagLayout());
        this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

        openSign(signFile, sign, addReloadButton);
    }

    private void openSign(final File signFile, byte[] sign, final boolean addRealoadButton) {

        if (signFile == null && sign == null) {
            Logger.getLogger("es.gob.afirma").warning("Se ha intentado abrir una firma nula");  //$NON-NLS-1$ //$NON-NLS-2$
            return;
        }

        if (sign == null) {
            if (signFile != null) {
                try {
                    final FileInputStream fis = new FileInputStream(signFile);
                    sign = AOUtil.getDataFromInputStream(fis);
                    try { fis.close(); } catch (final Exception e) { }
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el fichero de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                }
            }
        }

        SignValidity validity = new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, null);
        if (sign != null) {
            try {
                validity = validateSign(sign);
            } catch (final Exception e) {
                validity = new SignValidity(SIGN_DETAIL_TYPE.KO, null);
            }
        }

        final JPanel resultPanel = new SignResultPanel(validity);
        final JPanel dataPanel = new SignDataPanel(signFile, sign, null, null);

        final JPanel bottonPanel = new JPanel(true);
        bottonPanel.setLayout(new FlowLayout(FlowLayout.TRAILING));
        
        if (addRealoadButton) {
            final JButton openSign = new JButton(Messages.getString("VisorPanel.1")); //$NON-NLS-1$
            openSign.setMnemonic('V');
            bottonPanel.add(openSign);
            openSign.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    if (VisorPanel.this.visorFirma != null) {
                        VisorPanel.this.visorFirma.loadNewSign();
                    }
                }
            });
        }
        
        final JButton closeVisor = new JButton(Messages.getString("VisorPanel.0")); //$NON-NLS-1$
        closeVisor.setMnemonic('C');
        closeVisor.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (VisorPanel.this.visorFirma != null) {
                    VisorPanel.this.visorFirma.closeApplication(0);
                }
                
            }
        });
        bottonPanel.add(closeVisor);

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            bottonPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        
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
        add(bottonPanel, c);

        repaint();
        
    }

    /**
     * Comprueba la validez de la firma.
     * @param sign Firma que se desea comprobar.
     * @return {@code true} si la firma es v&acute;lida, {@code false} en caso contrario.
     * @throws Exception Cuando los datos introducidos no se corresponden con una firma.
     */
    private SignValidity validateSign(final byte[] sign) throws Exception {
        if (DataAnalizerUtil.isPDF(sign)) {
            return new SignValidity(SIGN_DETAIL_TYPE.OK, null);
        } 
        else if (DataAnalizerUtil.isXML(sign)) {
            return ValidateXMLSignature.validate(sign);
        } 
        else if(new AOCMSSigner().isSign(sign) || new AOCAdESSigner().isSign(sign)) {
            return ValidateBinarySignature.validate(sign, null);
        }
        return new SignValidity(SIGN_DETAIL_TYPE.KO, null);
    }

//    public static void main(String[] args) {
//
//        File signFile = new File("C:/Users/A122466/Desktop/Escritorio/Firma.csig");
//
//        JPanel currentPanel = new VisorPanel(signFile, null, null);
//        Container container = new MainScreen(null, currentPanel);
//        Window window = (JFrame) container;
//
//    }
}
