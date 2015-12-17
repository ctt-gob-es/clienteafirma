package es.gob.afirma.ui.visor.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.accessibility.AccessibleHyperlink;
import javax.accessibility.AccessibleHypertext;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.UIManager;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.html.HTMLDocument;

import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.GeneralConfig;

final class EditorFocusManager extends KeyAdapter implements FocusListener, HyperlinkListener, ComponentListener {

    private final JEditorPane displayPane;

    private final Style linkUnfocusedStyle;
    private final Style linkFocusedStyle;

    private final List<AccessibleHyperlink> hyperLinks;
    private final Map<AccessibleHyperlink, URL> hyperLinksTargets;

    private int selectedLink = 0;

    private final EditorFocusManagerAction hlAction;

    EditorFocusManager (final JEditorPane displayPane, final EditorFocusManagerAction efma) {

        super();
        this.displayPane = displayPane;
        this.hlAction = efma;

        final Font defaultFont = new Font(new JLabel().getFont().getAttributes());
        final String bodyRule = "body { font-family: " + defaultFont.getFamily() + "; font-size: " + defaultFont.getSize() + "pt; }";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
        ((HTMLDocument) this.displayPane.getDocument()).getStyleSheet().addRule(bodyRule);

        final StyleContext sc = new StyleContext();
        this.linkUnfocusedStyle = sc.addStyle("linkUnfocused", sc.getStyle(StyleContext.DEFAULT_STYLE)); //$NON-NLS-1$
        StyleConstants.setUnderline(this.linkUnfocusedStyle, true);
        if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
        	StyleConstants.setForeground(this.linkUnfocusedStyle, new Color(169, 226, 255, 255));
        }
        else {
        	StyleConstants.setForeground(this.linkUnfocusedStyle, Color.BLUE);
        }
        StyleConstants.setBackground(this.linkUnfocusedStyle, new Color(0, 0, 0, 0));

        this.linkFocusedStyle = sc.addStyle("linkFocused", sc.getStyle(StyleContext.DEFAULT_STYLE)); //$NON-NLS-1$
        if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
        	StyleConstants.setBackground(this.linkFocusedStyle, Color.WHITE);
            StyleConstants.setForeground(this.linkFocusedStyle, Color.BLACK);
        }
        else {
        	StyleConstants.setBackground(this.linkFocusedStyle, Color.BLUE);
            StyleConstants.setForeground(this.linkFocusedStyle, Color.WHITE);
        }

        final AccessibleHypertext accessibleHypertext = (AccessibleHypertext) this.displayPane.getAccessibleContext().getAccessibleText();
        this.hyperLinks = new ArrayList<>(accessibleHypertext.getLinkCount());
        this.hyperLinksTargets = new HashMap<>(accessibleHypertext.getLinkCount());
        for (int i=0; i<accessibleHypertext.getLinkCount(); i++) {
            final AccessibleHyperlink ahl = accessibleHypertext.getLink(i);
            this.hyperLinks.add(ahl);
            this.hyperLinksTargets.put(ahl, (URL) ahl.getAccessibleActionObject(0));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void keyPressed(final KeyEvent e) {

        switch (e.getKeyCode()) {
            case KeyEvent.VK_RIGHT:
            case KeyEvent.VK_DOWN:

                if (this.hyperLinks.size() > 1) {

                    int startIndex = this.hyperLinks.get(this.selectedLink).getStartIndex();
                    ((HTMLDocument) this.displayPane.getDocument()).setCharacterAttributes(
                         startIndex,
                         this.hyperLinks.get(this.selectedLink).getEndIndex() - startIndex,
                         this.linkUnfocusedStyle,
                         false
                    );

                    this.selectedLink = this.selectedLink + 1;
                    if (this.selectedLink == this.hyperLinks.size()) {
                        this.selectedLink = 0;
                    }

                    startIndex = this.hyperLinks.get(this.selectedLink).getStartIndex();
                    ((HTMLDocument) this.displayPane.getDocument()).setCharacterAttributes(
                         startIndex,
                         this.hyperLinks.get(this.selectedLink).getEndIndex() - startIndex,
                         this.linkFocusedStyle,
                         false
                    );

                }
                break;
            case KeyEvent.VK_LEFT:
            case KeyEvent.VK_UP:
                if (this.hyperLinks.size() > 1) {

                    int startIndex = this.hyperLinks.get(this.selectedLink).getStartIndex();
                    ((HTMLDocument) this.displayPane.getDocument()).setCharacterAttributes(
                         startIndex,
                         this.hyperLinks.get(this.selectedLink).getEndIndex() - startIndex,
                         this.linkUnfocusedStyle,
                         false
                    );

                    this.selectedLink = this.selectedLink - 1;
                    if (this.selectedLink == -1) {
                        this.selectedLink = this.hyperLinks.size()-1;
                    }

                    startIndex = this.hyperLinks.get(this.selectedLink).getStartIndex();
                    ((HTMLDocument) this.displayPane.getDocument()).setCharacterAttributes(
                         startIndex,
                         this.hyperLinks.get(this.selectedLink).getEndIndex() - startIndex,
                         this.linkFocusedStyle,
                         false
                    );

                }
                break;
            case KeyEvent.VK_SPACE:
            case KeyEvent.VK_ENTER:
                if (this.hlAction != null) {
                    this.hlAction.openHyperLink(
                       new HyperlinkEvent(
                           this, HyperlinkEvent.EventType.ACTIVATED, this.hyperLinksTargets.get(this.hyperLinks.get(this.selectedLink))
                       ),
                       this.selectedLink
                    );
                }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void focusGained(final FocusEvent e) {
        if (this.hyperLinks.size() > 0) {
            final int startIndex = this.hyperLinks.get(this.selectedLink).getStartIndex();
            ((HTMLDocument) this.displayPane.getDocument()).setCharacterAttributes(
                 startIndex,
                 this.hyperLinks.get(this.selectedLink).getEndIndex() - startIndex,
                 this.linkFocusedStyle,
                 false
            );
        }
    }

    /** {@inheritDoc} */
    @Override
    public void focusLost(final FocusEvent e) {
        if (this.hyperLinks.size() > 0) {
            final int startIndex = this.hyperLinks.get(this.selectedLink).getStartIndex();
            ((HTMLDocument) this.displayPane.getDocument()).setCharacterAttributes(
                 startIndex,
                 this.hyperLinks.get(this.selectedLink).getEndIndex() - startIndex,
                 this.linkUnfocusedStyle,
                 false
            );
        }
    }

    /** {@inheritDoc} */
    @Override
    public void hyperlinkUpdate(final HyperlinkEvent he) {
        if (HyperlinkEvent.EventType.ACTIVATED.equals(he.getEventType()) && (this.hlAction != null)) {
           this.hlAction.openHyperLink(he, this.selectedLink);
        }
    }

    private static int getBestFontSizeForJOptionPane(final int width, final int height, final String text, final String fontFamily, final int minSize) {

        final String bodyRule = "body { font-family: " + fontFamily + "; font-size: %f%pt; }";  //$NON-NLS-1$//$NON-NLS-2$

        for (int i = minSize; i < 100; i++) {

            final JEditorPane editorPane = new JEditorPane("text/html", text); //$NON-NLS-1$

            ((HTMLDocument) editorPane.getDocument()).getStyleSheet().addRule(bodyRule.replace("%f%", Integer.toString(i))); //$NON-NLS-1$

            editorPane.setSize(width, Integer.MAX_VALUE);
            editorPane.setEditable(false);

            final JPopupMenu popup = new JPopupMenu();
            popup.add(new JScrollPane(editorPane));
            final Dimension d = popup.getPreferredSize();
            popup.setPopupSize(Math.min(width, d.width), d.height);

            if (popup.getPreferredSize().height > height) {
                return i-1;
            }
        }
        return minSize;
    }

    private boolean editorFirstShow = true;

    /** {@inheritDoc} */
    @Override public void componentResized(final ComponentEvent e) {
        if (this.editorFirstShow) {
            final int bestFontSize = getBestFontSizeForJOptionPane(this.displayPane.getWidth(), this.displayPane.getHeight(), this.displayPane.getText(), UIManager.getFont("Label.font").getFamily(), UIManager.getFont("Label.font").getSize()); //$NON-NLS-1$ //$NON-NLS-2$
            final String bodyRule = "body { font-family: " + UIManager.getFont("Label.font").getFamily() + "; font-size: " + bestFontSize + "pt; }"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            ((HTMLDocument) this.displayPane.getDocument()).getStyleSheet().addRule(bodyRule);
            this.editorFirstShow = false;
        }
    }

    /** {@inheritDoc} */
    @Override public void componentMoved(final ComponentEvent e) { /* No implementado */ }

    /** {@inheritDoc} */
    @Override public void componentHidden(final ComponentEvent e) { /* No implementado */ }

    /** {@inheritDoc} */
    @Override public void componentShown(final ComponentEvent e) { /* No implementado */ }
}
