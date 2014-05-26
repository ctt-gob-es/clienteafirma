package es.gob.afirma.ui.principal;

import java.awt.Color;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.MouseEvent;

import javax.swing.Icon;
import javax.swing.JToggleButton;
import javax.swing.plaf.basic.BasicGraphicsUtils;

import es.gob.afirma.ui.utils.GeneralConfig;

/** ToggleButton cuyo aspecto viene dado por una imagen. La imagen puede cambiar seg&uacute;n
 * este seleccionado o no el bot&oacute;n. */
final class ToggleImageButton extends JToggleButton {

    /** SerialVersion UID */
    private static final long serialVersionUID = 6154127450477253680L;

    /** Texto que muestra el bot&oacute;n. */
    private String buttonText = null;

    /** Imagen que muestra el bot&oacute;n en estado desactivado. */
    private Image disabledImage = null;

    /** Icono que muestra el bot&oacute;n en estado desactivado. */
    private Icon disabledToggledIcon = null;

    /** Imagen que muestra el bot&oacute;n en estado normal. */
    private Image image = null;

    /** Indica cuando el cursor del rat&oacute;n est&aacute; sobre el bot&oacute;n. */
    private boolean mouseOver = false;

    /** Imagen que muestra el bot&oacute;n en estado seleccionado. */
    private Image selectedImage = null;

    /** Icono que muestra el bot&oacute;n en estado seleccionado. */
    private Icon selectedToggledIcon = null;

    /** Icono que muestra el bot&oacute;n en estado normal. */
    private Icon toggledIcon = null;

    /** Recupera el texto definido para el bot&oacute;n.
     * @return Texto del bot&oacute;n. */
    public String getButtonText() {
        return this.buttonText;
    }

    /** Recupera el icono del bot&oacute;n para el estado desactivado.
     * @return Icono del bot&oacute;n para el estado desactivado. */
    public Icon getDisabledToggledIcon() {
        return this.disabledToggledIcon;
    }

    /** Recupera el icono del bot&oacute;n para el estado seleccionado.
     * @return Icono del bot&oacute;n para el estado seleccionado. */
    public Icon getSelectedToggledIcon() {
        return this.selectedToggledIcon;
    }

    /** Devuelve el icono definido para el
     * @return Icono del bot&oacute;n. */
    public Icon getToggledIcon() {
        return this.toggledIcon;
    }

    @Override
    public void paint(final Graphics g) {

        super.paint(g);

        if (super.isSelected() && this.selectedImage != null) {
            g.drawImage(this.selectedImage, 2, 2, this.getWidth() - 4, this.getHeight() - 4, this.getParent());
        }
        else if (!super.isEnabled() && this.disabledImage != null) {
            g.drawImage(this.disabledImage, 2, 2, this.getWidth() - 4, this.getHeight() - 4, this.getParent());
        }
        else if (this.image != null) {
            g.drawImage(this.image, 2, 2, this.getWidth() - 4, this.getHeight() - 4, this.getParent());
        }

        Color color = null;
        if (this.isSelected()) {
            color = g.getColor();
            g.setColor(Color.white);
        }
        else if (!this.isEnabled()) {
            color = g.getColor();
            g.setColor(Color.gray);
        }

        if (this.getButtonText() != null) {
            // Se pinta el texto con el mnemonico correspondiente
            BasicGraphicsUtils.drawString(g,
                                          this.getButtonText(),
                                          this.getMnemonic(),
                                          48,
                                          (this.getHeight() + g.getFontMetrics().getAscent() - 4) / 2);
        }

        if (this.isSelected() || !this.isEnabled()) {
            g.setColor(color);
        }

        if (this.isSelected()) {
            if (this.getSelectedToggledIcon() != null) {
                this.getSelectedToggledIcon().paintIcon(this, g, 0, (this.getHeight() - this.getSelectedToggledIcon().getIconHeight()) / 2);
            }
        }
        else if (!this.isEnabled()) {
            if (this.getDisabledToggledIcon() != null) {
                this.getDisabledToggledIcon().paintIcon(this, g, 0, (this.getHeight() - this.getSelectedToggledIcon().getIconHeight()) / 2);
            }
        }
        else {
            if (this.getToggledIcon() != null) {
                this.getToggledIcon().paintIcon(this, g, 0, (this.getHeight() - this.getToggledIcon().getIconHeight()) / 2);
            }
        }

        // Repintado del contenedor de todos los botones
        final Container parent = this.getParent().getParent();
        if (parent != null) {
            parent.repaint();
        }
        else {
            this.getParent().repaint();
        }

    }

    @Override
    protected void paintBorder(final Graphics g) {
        /* No pintamos el borde. */
    }

    @Override
    protected void paintComponent(final Graphics g) {
        if ((this.isEnabled() && (this.mouseOver || this.isFocusOwner())) || GeneralConfig.isHighContrast()) {
            super.paintComponent(g);
        }
    }

    @Override
    protected void processMouseEvent(final MouseEvent e) {

        if (e.getID() == MouseEvent.MOUSE_ENTERED) {
            this.mouseOver = true;
            this.repaint();
        }
        else if (e.getID() == MouseEvent.MOUSE_EXITED) {
            this.mouseOver = false;
            this.repaint();
        }
        super.processMouseEvent(e);
    }

    /** Establece el texto del bot&oacute;n con imagen.
     * @param buttonText Texto del bot&oacute;n. */
    public void setButtonText(final String buttonText) {
        this.buttonText = buttonText;
        // Introduimos un texto compuesto por espacios en blancos para usarlo
        // para la medida del boton pero que no se sobrescriba sobre nuestro texto.
        // Como los espacios en blanco ocupan algo menos que los caracteres, agregamos
        // algunos adicionales

        // Texto que se va a mostrar
        final String text = new String("                                 ".substring(0, 8 + //$NON-NLS-1$
                                                                                     (this.buttonText.length() > 25 ? 25 : this.buttonText.length())));

        super.setText(text);
    }

    /** Establece la imagen que da aspecto al bot&oacute;n en su estado desactivado.
     * @param disabledImage Imagen que se mostrar&aacute;. */
    public void setDisabledImage(final Image disabledImage) {
        this.disabledImage = disabledImage;
    }

    /** Establece el icono del bot&oacute;n para el estado desactivado.
     * @param disabledToggledIcon Icono del bot&oacute;n para el estado desactivado.
     * @param baseIcon Icono transparente a partir del cual se toma la medida. */
    public void setDisabledToggledIcon(final Icon disabledToggledIcon, final Icon baseIcon) {
        this.disabledToggledIcon = disabledToggledIcon;
        super.setDisabledIcon(baseIcon);
    }

    /** {@inheritDoc} */
    @Override
    public void setIcon(final Icon defaultIcon) {
        /* No hacemos nada */
    }

    /** Establece la imagen que da aspecto al bot&oacute;n en su estado normal.
     * @param image Imagen que se mostrar&aacute;. */
    public void setImage(final Image image) {
        this.image = image;
    }

    /** {@inheritDoc} */
    @Override
    public void setSelectedIcon(final Icon selectedIcon) {
        /* No hacemos nada */
    }

    /** Establece la imagen que da aspecto al bot&oacute;n en su estado seleccionado.
     * @param selectedImage Imagen que se mostrar&aacute;. */
    public void setSelectedImage(final Image selectedImage) {
        this.selectedImage = selectedImage;
    }

    /** Establece el icono del bot&oacute;n para el estado seleccionado.
     * @param selectedToggledIcon Icono del bot&oacute;n para el estado seleccionado.
     * @param baseIcon Icono transparente a partir del cual se toma la medida. */
    public void setSelectedToggledIcon(final Icon selectedToggledIcon, final Icon baseIcon) {
        this.selectedToggledIcon = selectedToggledIcon;
        super.setSelectedIcon(baseIcon);
    }

    /** {@inheritDoc} */
    @Override
    public void setText(final String text) {
        /* No hacemos nada */
    }

    /** Establece el icono del bot&oacute;n.
     * @param toggledIcon Icono del bot&oacute;n.
     * @param baseIcon Icono transparente a partir del cual se toma la medida. */
    public void setToggledIcon(final Icon toggledIcon, final Icon baseIcon) {
        this.toggledIcon = toggledIcon;
        super.setIcon(baseIcon);
    }
}
