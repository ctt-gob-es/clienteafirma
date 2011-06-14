import java.awt.BorderLayout;
import java.awt.Desktop;
import java.io.File;
import java.net.URI;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import es.gob.afirma.standalone.Messages;

/** @author Tomas */
@SuppressWarnings("unused")
public class Test {

    /** @param argv
     * @throws Throwable */
    public static void main(String[] argv) throws Throwable {
    	System.out.println("Foco: " + UIManager.getColor("Focus.color"));
    	System.out.println("Lista fondo de seleccion: " + UIManager.getColor("List.selectionBackground"));
    	System.out.println("Texto fondo de seleccion: " + UIManager.getColor("TextField.selectionBackground"));

    }
}
