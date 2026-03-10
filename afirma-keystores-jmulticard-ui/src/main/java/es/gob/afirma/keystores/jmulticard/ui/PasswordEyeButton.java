package es.gob.afirma.keystores.jmulticard.ui;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.image.BufferedImage;

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

    private static final Insets FOCUS_MARGIN = new Insets(3, 3, 3, 3);

    PasswordEyeButton(final JSecurePasswordLabel label,
                      final ImageIcon eyeIcon,
                      final ImageIcon eyeOffIcon) {
        this.passwordLabel = label;

        this.eyeImage = eyeIcon.getImage();
        this.eyeOffImage = eyeOffIcon.getImage();

        setFocusable(true);
        setContentAreaFilled(false);
        setOpaque(false);
        setMargin(FOCUS_MARGIN);
        setBorderPainted(false);

        setPreferredSize(new Dimension(
                eyeIcon.getIconWidth() + FOCUS_MARGIN.left + FOCUS_MARGIN.right,
                eyeIcon.getIconHeight() + FOCUS_MARGIN.top + FOCUS_MARGIN.bottom
            ));

        addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent e) {
                setBorder(createFocusBorder());
                setBorderPainted(true);
                repaint();
            }

            @Override
            public void focusLost(FocusEvent e) {
                setBorderPainted(false);
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

    private Border createFocusBorder() {
        return new LineBorder(AccesibilityUtils.getAccessibleColor(this.passwordLabel), 1);
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g.create();

        Image baseImg;
        if (this.passwordLabel.isShowPassword()) {
            baseImg = this.eyeOffImage;
            setToolTipText(Messages.getString(
                "InputPasswordSmartcardDialog.hidePassword" //$NON-NLS-1$
            ));
        }
        else {
            baseImg = this.eyeImage;
            setToolTipText(Messages.getString(
                "InputPasswordSmartcardDialog.showPassword" //$NON-NLS-1$
            ));
        }

        Color iconColor;
        if (AccesibilityUtils.isHighContrast()) {
            iconColor = AccesibilityUtils.getAccessibleColor(this.passwordLabel);
        }
        else {
            iconColor = Color.BLACK;
        }

        Image img = colorize(baseImg, iconColor);

        int x = (getWidth() - img.getWidth(null)) / 2;
        int y = (getHeight() - img.getHeight(null)) / 2;

        g2.drawImage(img, x, y, null);

        if (hasFocus() && AccesibilityUtils.isHighContrast()) {
            g2.setColor(iconColor);
            g2.setStroke(new BasicStroke(1));
            g2.drawRect(0, 0, getWidth() - 1, getHeight() - 1);
        }

        g2.dispose();
    }

    /**
     * Metodo que da color al icono.
     * @param src Ruta de imagen.
     * @param color Color a aplicar.
     * @return Icono coloreado.
     */
    private Image colorize(Image src, Color color) {

        BufferedImage image = new BufferedImage(
            src.getWidth(null),
            src.getHeight(null),
            BufferedImage.TYPE_INT_ARGB
        );

        Graphics2D g = image.createGraphics();

        g.drawImage(src, 0, 0, null);

        g.setComposite(AlphaComposite.SrcAtop);
        g.setColor(color);
        g.fillRect(0, 0, image.getWidth(), image.getHeight());

        g.dispose();
        return image;
    }
}
