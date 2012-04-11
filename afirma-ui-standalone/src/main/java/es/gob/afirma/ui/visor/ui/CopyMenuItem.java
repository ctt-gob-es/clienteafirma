package es.gob.afirma.ui.visor.ui;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.text.JTextComponent;

final class CopyMenuItem extends JPopupMenu implements ClipboardOwner {

    private static final long serialVersionUID = 1750985678317829383L;

    private final JTextComponent textComponent;

    /** @return the textComponent */
    protected JTextComponent getTextComponent() {
        return this.textComponent;
    }

    private void createUI(final String text) {
        final JMenuItem copyItem = new JMenuItem(text);
        copyItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent ae) {
                if (getTextComponent() != null) {
                    Toolkit.getDefaultToolkit()
                           .getSystemClipboard()
                           .setContents(new StringSelection((getTextComponent().getSelectedText() != null)
                                                                                                          ? getTextComponent().getSelectedText()
                                                                                                          : getTextComponent().getText()),
                                        CopyMenuItem.this);
                }
            }
        });
        add(copyItem);
    }

    CopyMenuItem(final JTextComponent tc, final String text) {
        this.textComponent = tc;
        createUI(text);
    }

    @Override
    public void lostOwnership(final Clipboard clipboard, final Transferable contents) {
        // No implementado
    }
}