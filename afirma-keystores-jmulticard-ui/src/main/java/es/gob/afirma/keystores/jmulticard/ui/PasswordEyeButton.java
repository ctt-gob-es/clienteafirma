package es.gob.afirma.keystores.jmulticard.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;

/**
 * Boton para mostrar u ocultar contrase&ntilde;a.
 */
final class PasswordEyeButton extends JButton {

    private static final long serialVersionUID = 1L;

    private final JSecurePasswordLabel passwordLabel;

    private final Image eyeImage;
    private final Image eyeOffImage;

    private static final Border FOCUS_BORDER = new LineBorder(Color.BLACK, 1);
    private static final Border NO_BORDER = null;

    PasswordEyeButton(final JSecurePasswordLabel label,
                      final ImageIcon eyeIcon,
                      final ImageIcon eyeOffIcon) {
        this.passwordLabel = label;

        this.eyeImage = eyeIcon.getImage();
        this.eyeOffImage = eyeOffIcon.getImage();

        setFocusable(true);
        setBorder(NO_BORDER);
        setContentAreaFilled(false);
        setOpaque(false);

        setPreferredSize(new Dimension(eyeIcon.getIconWidth(), eyeIcon.getIconHeight()));

        addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent e) {
                setBorder(FOCUS_BORDER);
                repaint();
            }

            @Override
            public void focusLost(FocusEvent e) {
                setBorder(NO_BORDER);
                repaint();
            }
        });

        addActionListener(e -> togglePasswordVisibility());
    }

    /**
     * Marca la acci&oacute;n para mostrar u ocultar la contrase&ntilde;a.
     */
    private void togglePasswordVisibility() {
        this.passwordLabel.togglePasswordVisibility();
        repaint(); 
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g.create();

        Image img;
        if (this.passwordLabel.isShowPassword()) {
        	img = this.eyeOffImage;
        	setToolTipText(Messages.getString("InputPasswordSmartcardDialog.hidePassword")); //$NON-NLS-1$
        } else {
        	img = this.eyeImage;
        	setToolTipText(Messages.getString("InputPasswordSmartcardDialog.showPassword")); //$NON-NLS-1$
        }

        int x = (getWidth() - img.getWidth(null)) / 2;
        int y = (getHeight() - img.getHeight(null)) / 2;
        g2.drawImage(img, x, y, null);

        if (hasFocus() && AccesibilityUtils.isHighContrast()) {
            g2.setColor(Color.YELLOW);
            g2.setStroke(new BasicStroke(1));
            g2.drawRect(0, 0, getWidth() - 1, getHeight() - 1);
        }

        g2.dispose();
    }
}
