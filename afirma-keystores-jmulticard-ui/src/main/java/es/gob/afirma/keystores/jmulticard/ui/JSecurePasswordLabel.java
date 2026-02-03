/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros
 * para la realizacion de procesos de autenticacion, firma electronica y validacion
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos
 * e Impulso de la Administracion Electronica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * European Union Public License publicada por la Comision Europea,
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.afirma.keystores.jmulticard.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.Timer;
import javax.swing.UIManager;

/**
 * Componente basado en <code>JLabel</code> para capturar contrase&ntilde;as usando
 * &uacute;nicamente arrays de <code>char</code> y restringiendo los caracteres aceptados.
 * 
 * @author Jose Luis Escanciano
 */
final class JSecurePasswordLabel extends JLabel {

    private static final long serialVersionUID = -4343328489072897605L;

    private transient final int delay = 500;

    transient ImageIcon eyeIcon;
    transient ImageIcon eyeOffIcon;
    transient int iconSize = 16;
    transient JPanel parentPanel;

    final char[] pass;
    char[] getPass() {
        return this.pass;
    }

    private final int maxChars;
    transient int passwordLength;

    private final Timer timer;
    Timer getTimer() {
        return this.timer;
    }

    private boolean showCursor;
    int getMaxChars() {
        return this.maxChars;
    }

    private boolean showPassword = false;

    void setShowPassword(final boolean show) {
        this.showPassword = show;
        updateText();
    }

    boolean isShowPassword() {
        return this.showPassword;
    }

    /**
     * Constructor.
     * 
     * @param maxLength     Longitud m&aacute;xima de la contrase&ntilde;a
     * @param passwordPanel Panel padre que contiene los elementos de la contrase&ntilde;a
     */
    JSecurePasswordLabel(final int maxLength, JPanel passwordPanel) {
        this.maxChars = maxLength;
        this.pass = new char[maxLength];
        this.parentPanel = passwordPanel;

        clearPassword();

        addKeyListener(new KeyAdapter() {
            @Override
            public void keyTyped(final KeyEvent ke) {

                if (ke.isControlDown() || ke.isAltDown() || ke.isAltGraphDown() || Character.isISOControl(ke.getKeyChar())) {
	            	ke.consume();
	            	return;
                }

                if (getPasswordLength() < getMaxChars()) {
                    JSecurePasswordLabel.this.pass[JSecurePasswordLabel.this.passwordLength++] = ke.getKeyChar();
                    ke.setKeyChar('\0');
                }
                updateText();
            }

            @Override
            public void keyPressed(final KeyEvent e) {
            	
                if (e.getKeyCode() == KeyEvent.VK_ENTER || e.getKeyCode() == KeyEvent.VK_SPACE) {
                    togglePasswordVisibility();
                    e.consume();
                    return;
                }

                // Pegar desde portapapeles
                if (((e.getModifiersEx() & InputEvent.CTRL_DOWN_MASK) != 0 && e.getKeyCode() == KeyEvent.VK_V)
                        || ((e.getModifiersEx() & InputEvent.SHIFT_DOWN_MASK) != 0
                                && e.getKeyCode() == KeyEvent.VK_INSERT)) {
                    pasteFromClipboard();
                    e.consume();
                    return;
                }

                if (e.getKeyCode() == KeyEvent.VK_BACK_SPACE && getPasswordLength() > 0) {
                    clearPassword(getPasswordLength() - 1);
                } else if (e.getKeyCode() == KeyEvent.VK_DELETE) {
                    clearPassword();
                }
                updateText();
            }
        });

        addFocusListener(new FocusListener() {
            @Override
            public void focusGained(final FocusEvent e) {
                JSecurePasswordLabel.this.setShowCursor(true);
                JSecurePasswordLabel.this.getTimer().start();
                if (AccesibilityUtils.isHighContrast()) {
                    setBackground(UIManager.getColor("TextField.background")); //$NON-NLS-1$
                    setForeground(UIManager.getColor("TextField.foreground")); //$NON-NLS-1$
                    JSecurePasswordLabel.this.parentPanel.setBackground(UIManager.getColor("Panel.background")); //$NON-NLS-1$
                } else {
                    setBackground(Color.WHITE);
                    JSecurePasswordLabel.this.parentPanel.setBackground(Color.WHITE);
                }
            }

            @Override
            public void focusLost(final FocusEvent e) {
                JSecurePasswordLabel.this.setShowCursor(false);
                JSecurePasswordLabel.this.getTimer().stop();
                JSecurePasswordLabel.this.updateText();
                if (AccesibilityUtils.isHighContrast()) {
                    setBackground(UIManager.getColor("TextField.background")); //$NON-NLS-1$
                    setForeground(UIManager.getColor("TextField.foreground")); //$NON-NLS-1$
                    JSecurePasswordLabel.this.parentPanel.setBackground(UIManager.getColor("Panel.background")); //$NON-NLS-1$
                } else {
                	setBackground(Color.LIGHT_GRAY);
                    JSecurePasswordLabel.this.parentPanel.setBackground(Color.LIGHT_GRAY);
                }                
            }
        });

        // Menu contextual para pegar desde portapapeles
        final JPopupMenu popup = new JPopupMenu();
        final JMenuItem pasteItem = new JMenuItem(
                Messages.getString("InputPasswordSmartcardDialog.pasteText")); //$NON-NLS-1$
        pasteItem.addActionListener(e -> pasteFromClipboard());
        popup.add(pasteItem);

        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    popup.show(e.getComponent(), e.getX(), e.getY());
                } else {
                    requestFocus();
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    popup.show(e.getComponent(), e.getX(), e.getY());
                } else {
                    requestFocus();
                }
            }
        });

        setFocusable(true);
        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLoweredBevelBorder(),
                BorderFactory.createEmptyBorder(0, 5, 0, 5)));
        setOpaque(true);
        setShowCursor(false);
        
        String imgEyePath;
        String imgEyeOffPath;
        if (!AccesibilityUtils.isHighContrast()) {
        	imgEyePath = "/images/eye.png"; //$NON-NLS-1$
        	imgEyeOffPath = "/images/eye-off.png"; //$NON-NLS-1$
        } else {
        	imgEyePath = "/images/eye-white.png"; //$NON-NLS-1$
        	imgEyeOffPath = "/images/eye-off-white.png"; //$NON-NLS-1$
        }

        // Iconos de mostrar/ocultar password
        this.eyeIcon = new ImageIcon(new ImageIcon(getClass().getResource(imgEyePath))
                .getImage().getScaledInstance(this.iconSize, this.iconSize, Image.SCALE_SMOOTH));
        this.eyeOffIcon = new ImageIcon(new ImageIcon(getClass().getResource(imgEyeOffPath))
                .getImage().getScaledInstance(this.iconSize, this.iconSize, Image.SCALE_SMOOTH));

        // Timer para cursor parpadeante
        this.timer = new Timer(this.delay, new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                if (arg0.getSource().equals(getTimer()) && hasFocus()) {
                    JSecurePasswordLabel.this.setShowCursor(!getShowCursor());
                    JSecurePasswordLabel.this.updateText();
                }
            }
        });
        this.timer.stop();

        AccesibilityUtils.remarcar(this);
        AccesibilityUtils.setContrastColor(this);
    }

    /** Muestra asterisco o caracteres seg&uacute;n visibilidad */
    synchronized void updateText() {
        final StringBuilder text = new StringBuilder();
        if (this.showPassword) {
            for (int i = 0; i < this.passwordLength; i++) {
                text.append(this.pass[i]);
            }
        } else {
            for (int i = 0; i < this.passwordLength; i++) {
                text.append('*');
            }
        }
        text.append(getShowCursor() ? "|" : " "); //$NON-NLS-1$ //$NON-NLS-2$
        setText(text.toString());
    }

    /** Limpia toda la contrase&ntilde;a */
    void clearPassword() {
        clearPassword(0);
    }

    /** Limpia la contrase&ntilde;a desde posici&oacute;n */
    void clearPassword(final int position) {
        for (int i = position; i < this.pass.length; i++) {
            this.pass[i] = '\0';
        }
        this.passwordLength = position;
        updateText();
    }

    /** Devuelve la contrase&ntilde;a y la limpia */
    char[] getPassword() {
        final char[] returned = new char[this.passwordLength];
        for (int i = 0; i < this.passwordLength; i++) {
            returned[i] = this.pass[i];
        }
        clearPassword();
        return returned;
    }

    /** Longitud de la contrase&ntilde;a introducida */
    int getPasswordLength() {
        return this.passwordLength;
    }

    /** Setter privado del cursor */
    synchronized void setShowCursor(final boolean show) {
        this.showCursor = show;
    }

    /** Getter privado del cursor */
    synchronized boolean getShowCursor() {
        return this.showCursor;
    }

    /** Copia caracteres desde portapapeles */
    private void pasteFromClipboard() {
        try {
            final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
            final Transferable t = clipboard.getContents(null);
            if (t != null && t.isDataFlavorSupported(DataFlavor.stringFlavor)) {
                final String text = (String) t.getTransferData(DataFlavor.stringFlavor);
                if (text == null || text.isEmpty()) {
                    return;
                }
                for (int i = 0; i < text.length() && this.passwordLength < this.maxChars; i++) {
                    final char c = text.charAt(i);
                    if (!Character.isISOControl(c)) {
                        this.pass[this.passwordLength++] = c;
                    }
                }
                updateText();
            }
        } catch (Exception e) {
            // Ignorar errores de portapapeles
        }
    }

    /** Alterna la visibilidad del password */
    void togglePasswordVisibility() {
        setShowPassword(!isShowPassword());
    }
    
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        // Solo dibujar borde amarillo si hay foco y estamos en alto contraste
        if (AccesibilityUtils.isHighContrast()) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setColor(Color.YELLOW); // Borde amarillo
            g2.setStroke(new BasicStroke(1));
            g2.drawRect(0, 0, getWidth() - 1, getHeight() - 1);
            g2.dispose();
            this.repaint();
        }
    }
   
}
