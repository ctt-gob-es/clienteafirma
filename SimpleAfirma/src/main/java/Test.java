import java.awt.BorderLayout;
import java.awt.Desktop;
import java.io.File;
import java.net.URI;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import es.gob.afirma.standalone.Messages;


/**
 * @author Tomas
 *
 */
@SuppressWarnings("unused")
public class Test {

    /**
     * @param argv
     * @throws Throwable
     */
    public static void main(String[] argv) throws Throwable {  
    	    	
    	
//    	Desktop.getDesktop().open(new File("c:\\pruebas\\kaka.xml"));
    	
    	
    	
        JEditorPane jep = new JEditorPane("text/html", "La lluvia en <a href='http://foo.com/'>"  
        +"Sevilla</a> es una <a href='http://bar.com/'>maravilla</a>.");  
        jep.setEditable(false);  
        jep.setOpaque(false);  
        jep.addHyperlinkListener(new HyperlinkListener() {  
        	@Override
        	public void hyperlinkUpdate(final HyperlinkEvent hle) {  
        		if (HyperlinkEvent.EventType.ACTIVATED.equals(hle.getEventType())) {  
        			System.out.println(hle.getURL());  
        		}  
        	}  
    	});  
           
        JPanel p = new JPanel();  
        p.add(new JLabel("Foo."));  
        p.add(jep);  
        p.add(new JLabel("Bar."));  
          
        JFrame f = new JFrame("HyperlinkListener");  
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);  
        f.getContentPane().add(p, BorderLayout.CENTER);  
        f.setSize(400, 150);  
        f.setVisible(true);  
    }
}
