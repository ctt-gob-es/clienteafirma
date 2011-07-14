import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.URI;

import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.JTextComponent;

import es.gob.afirma.standalone.Messages;

/** @author Tomas */
@SuppressWarnings("unused")
public class Test {

    /** @param argv
     * @throws Throwable */
    public final static void main(final String[] argv) throws Throwable {
    	System.out.println("Foco: " + UIManager.getColor("Focus.color"));   //$NON-NLS-1$//$NON-NLS-2$
    	System.out.println("Lista fondo de seleccion: " + UIManager.getColor("List.selectionBackground"));  //$NON-NLS-1$//$NON-NLS-2$
    	System.out.println("Texto fondo de seleccion: " + UIManager.getColor("TextField.selectionBackground"));  //$NON-NLS-1$//$NON-NLS-2$
    	
    	System.out.println("Desktop: " + Toolkit.getDefaultToolkit().getDesktopProperty("win.highContrast.on")); //$NON-NLS-1$ //$NON-NLS-2$
    	System.out.println("Desktop: " + Toolkit.getDefaultToolkit().getDesktopProperty("awt.highContrast.on"));  //$NON-NLS-1$//$NON-NLS-2$
    	
    	System.out.println("System Font: " + Toolkit.getDefaultToolkit().getDesktopProperty("win.system.font"));  //$NON-NLS-1$//$NON-NLS-2$

    	System.out.println();
    
        final String text = "<font size=\"-1\">Potentially looooooong text. " + 
            "Lorem ipsum dolor sit amet, consectetuer" +
            "adipiscing elit, sed diam nonummy nibh euismod " +
            "tincidunt ut laoreet dolore magna aliquam" + 
            "adipiscing elit, sed diam nonummy nibh euismod" + 
            "erat volutpat. Ut wisi enim ad minim veniam, " + 
            "quis nostrud exerci tation.</fonr>";

        final JEditorPane editorPane = new JEditorPane("text/html", text); //$NON-NLS-1$
        editorPane.setSize(300, Integer.MAX_VALUE);
        editorPane.setEditable(false);
        
        final JPopupMenu popup = new JPopupMenu();
        popup.add(new JScrollPane(editorPane));
        final Dimension d = popup.getPreferredSize();
        popup.setPopupSize(Math.min(300, d.width), d.height);

        System.out.println("Dimesion: " + popup.getPreferredSize());
        
    }
    
    
}
