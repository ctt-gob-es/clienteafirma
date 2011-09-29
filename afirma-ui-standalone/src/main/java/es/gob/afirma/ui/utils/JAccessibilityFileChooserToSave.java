package es.gob.afirma.ui.utils;



import java.awt.Component;
import java.awt.Container;

import java.awt.event.KeyEvent;

import java.io.File;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JToggleButton;
import javax.swing.UIManager;

/**
 * Clase que extiende JFileChooser para hacerla accesible.
 * @author lmerayo
 *
 */
public class JAccessibilityFileChooserToSave extends JFileChooser{
	
	/**
	 * Serial version ID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor.
	 * @param file directorio.
	 */
	public JAccessibilityFileChooserToSave (File file)
	 {
		
		super(file);
		init(); //Se inicializa el componente

	 }//constructor
	
	/**
	 * Constructor.
	 */
	public JAccessibilityFileChooserToSave ()
	 {
		
		super();
		init(); //Se inicializa el componente
		

	 }//constructor

	
	/**
	 * Método que inicializa el componente JFileChooser para que sea accesible.
	 */
	private void init() {
		//Asignación de mnemonics
		
		
		//setLabelMnemonics((Container)this, "FileChooser.saveInLabelText", KeyEvent.VK_U);
		
		//Etiqueta Guardar en ...
		setLabelMnemonics((Container)this, "FileChooser.lookInLabelText", KeyEvent.VK_U);
		
		//Botón Cancelar
		setButtonMnemonics((Container)this, "FileChooser.cancelButtonText", KeyEvent.VK_C);
		
		//Botón Abrir
		setButtonMnemonics((Container)this, "FileChooser.openButtonText", KeyEvent.VK_A);
		
		//Toggle buttons
		//TODO: Revisar puesto que los botones que se hacen accesibles están predefinidos
		setToggleButtonMnemonics((Container)this);
	}

	/**
	 * Asigna el mnemónico indicado al botón identificado por la clave.
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnemónico.
	 * @param mnemonic mnemónico que se va a asignar al componente
	 */
	private void setButtonMnemonics( Container c, String key, int mnemonic ) {
	    int len = c.getComponentCount(); //Número de componentes del contenedor
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i); //Se obtiene un componente
	      //Se comprueba si es de tipo botón
	      if (comp instanceof JButton) {
		        JButton button = (JButton)comp;
		        //Se comprueba si su texto es el indicado por la clave
		        if (button.getText() ==  UIManager.get(key)) {
		        	//Se le asigna el mnemónico
		        	button.setMnemonic(mnemonic);
		        	break; //Salir del bucle
		        }
		    } else if (comp instanceof Container) {
		    	//Llamada recursiva
		    	setButtonMnemonics((Container)comp, key, mnemonic);
		    }
	    }//for
	  }
	
	/**
	 * Asigna el mnemónico indicado a la etiqueta identificada por la clave .
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnemónico.
	 * @param mnemonic mnemónico que se va a asignar al componente
	 */
	private void setLabelMnemonics( Container c, String key, int mnemonic ) {
		 //Número de componentes del contenedor
	    int len = c.getComponentCount();
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i);  //Se obtiene un componente
	      //Se comprueba si es de tipo etiqueta
	      if (comp instanceof JLabel) {
	        JLabel label = (JLabel)comp;
	        //Se comprueba si su texto es el indicado por la clave
	        if (label.getText() ==  UIManager.get(key)) {
	        	//Se le asigna el mnemónico
	        	label.setDisplayedMnemonic(mnemonic);
	        	break; //Salir del bucle
		    }
	      }else if (comp instanceof Container) {
	    	  	//Llamada recursiva
		    	setLabelMnemonics((Container)comp, key, mnemonic);
		    }
	    }//for
	  }
	
	/**
	 * Asigna un mnemónico predefinido a ciertos toggleButton contenidos en el componente.
	 * @param c contenedor global
	 */
	private void setToggleButtonMnemonics( Container c) {
		 //Número de componentes del contenedor
	    int len = c.getComponentCount();
	  //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i); //Se obtiene un componente
	      //Se comprueba si es de tipo JToggleButton
	      if (comp instanceof JToggleButton) {
		    	JToggleButton toggleButton = (JToggleButton) comp;
		    	 //Se almacena su texto asociado
		    	String text = toggleButton.getText();
		    	//Se comprueba que no esté vacío
		    	if (text!=null && !text.equalsIgnoreCase("")) {
		    		
		    		//Se tratan los botones según su texto
			    	if (text.equalsIgnoreCase("<html><center>Equipo</center></html>")) {
			    		//Se asigna un mnemónico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_E);
			    		//Como el texto del botón contiene código HTML se hace lo siguiente para que se muestre el mnemónico al usuario
			    		String newText = text.substring(0, 14) +"<u>"+text.charAt(14)+"</u>"+text.substring(15);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Elementos recientes</center></html>")) {
			    		//Se asigna un mnemónico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_L);
			    		//Como el texto del botón contiene código HTML se hace lo siguiente para que se muestre el mnemónico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Escritorio</center></html>")) {
			    		//Se asigna un mnemónico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_S);
			    		//Como el texto del botón contiene código HTML se hace lo siguiente para que se muestre el mnemónico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Mis documentos</center></html>")) {
			    		//Se asigna un mnemónico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_I);
			    		//Como el texto del botón contiene código HTML se hace lo siguiente para que se muestre el mnemónico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);

			    	} else if (text.equalsIgnoreCase("<html><center>Red</center></html>")) {
			    		//Se asigna un mnemónico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_R);
			    		//Como el texto del botón contiene código HTML se hace lo siguiente para que se muestre el mnemónico al usuario
			    		String newText = text.substring(0, 14) +"<u>"+text.charAt(14)+"</u>"+text.substring(15);
			    		toggleButton.setText(newText);
			    	}
		    	}
		    } else if (comp instanceof Container) {
		    	//Llamada recursiva
		    	setToggleButtonMnemonics((Container)comp);
		    }
	    }//for
	  }

}
