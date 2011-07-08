package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.net.URL;
import java.util.AbstractList;
import java.util.Vector;

import javax.accessibility.AccessibleHyperlink;
import javax.accessibility.AccessibleHypertext;
import javax.swing.JEditorPane;
import javax.swing.UIManager;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.html.HTMLDocument;

final class EditorFocusManager extends KeyAdapter implements FocusListener, HyperlinkListener {
    
    private final JEditorPane displayPane;
    
    private final Style linkUnfocusedStyle;
    private final Style linkFocusedStyle;
    
    private final AbstractList<AccessibleHyperlink> hyperLinks;
    
    private int selectedLink = 0;
    
    private final EditorFocusManagerAction hlAction;
    
    public EditorFocusManager (final JEditorPane displayPane, final EditorFocusManagerAction efma) {
        
        super();
        this.displayPane = displayPane;
        this.hlAction = efma;
        
        final Font font = UIManager.getFont("Label.font"); //$NON-NLS-1$
        final String bodyRule = "body { font-family: " + font.getFamily() + "; font-size: " + font.getSize() + "pt; }";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
        ((HTMLDocument) this.displayPane.getDocument()).getStyleSheet().addRule(bodyRule);
        
        final StyleContext sc = new StyleContext();
        this.linkUnfocusedStyle = sc.addStyle("linkUnfocused", sc.getStyle(StyleContext.DEFAULT_STYLE)); //$NON-NLS-1$
        StyleConstants.setUnderline(this.linkUnfocusedStyle, true);
        StyleConstants.setForeground(this.linkUnfocusedStyle, Color.BLUE);
        StyleConstants.setBackground(this.linkUnfocusedStyle, UIManager.getColor("Tree.background")); //$NON-NLS-1$
        
        this.linkFocusedStyle = sc.addStyle("linkFocused", sc.getStyle(StyleContext.DEFAULT_STYLE)); //$NON-NLS-1$
        StyleConstants.setBackground(this.linkFocusedStyle, UIManager.getColor("Tree.selectionBackground")); //$NON-NLS-1$
        StyleConstants.setForeground(this.linkFocusedStyle, UIManager.getColor("Tree.selectionForeground")); //$NON-NLS-1$
        
        final AccessibleHypertext accessibleHypertext = (AccessibleHypertext) this.displayPane.getAccessibleContext().getAccessibleText();        
        this.hyperLinks = new Vector<AccessibleHyperlink>(accessibleHypertext.getLinkCount());
        for (int i=0; i<accessibleHypertext.getLinkCount(); i++) {
            this.hyperLinks.add(accessibleHypertext.getLink(i));
        }

    }
    
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
                    this.hlAction.openHyperLink(new HyperlinkEvent(this, HyperlinkEvent.EventType.ACTIVATED, (URL) this.hyperLinks.get(this.selectedLink).getAccessibleActionObject(0)), this.selectedLink);
                }
        }
    }
        
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
    
    @Override
    public void focusLost(FocusEvent e) {
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

    @Override
    public void hyperlinkUpdate(final HyperlinkEvent he) {
        if (HyperlinkEvent.EventType.ACTIVATED.equals(he.getEventType())) {
            if (this.hlAction != null) {
                this.hlAction.openHyperLink(he, this.selectedLink);
            }
        }
    }
}
