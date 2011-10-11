package es.gob.afirma.ui.utils;



import java.awt.Color;
import java.awt.Component;
import java.awt.Container;

import java.awt.event.KeyEvent;

import java.io.File;
import javax.swing.JButton;
import javax.swing.JComboBox;
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
	 * M�todo que inicializa el componente JFileChooser para que sea accesible.
	 */
	private void init() {
		

		//Se comprueba si se está en el modo Alto contraste
		if (GeneralConfig.isHighContrast()){
			setHighContrast((Container)this);
		}
		
		
		//Asignaci�n de mnemonics
		
		//setLabelMnemonics((Container)this, "FileChooser.saveInLabelText", KeyEvent.VK_U);
		
		//Etiqueta Guardar en ...
		setLabelMnemonics((Container)this, "FileChooser.lookInLabelText", KeyEvent.VK_U);
		
		//Bot�n Cancelar
		setButtonMnemonics((Container)this, "FileChooser.cancelButtonText", KeyEvent.VK_C);
		
		//Bot�n Abrir
		setButtonMnemonics((Container)this, "FileChooser.openButtonText", KeyEvent.VK_A);
		
		//Toggle buttons
		//TODO: Revisar puesto que los botones que se hacen accesibles est�n predefinidos
		setToggleButtonMnemonics((Container)this);
	}

	/**
	 * Asigna el mnem�nico indicado al bot�n identificado por la clave.
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnem�nico.
	 * @param mnemonic mnem�nico que se va a asignar al componente
	 */
	private void setButtonMnemonics( Container c, String key, int mnemonic ) {
	    int len = c.getComponentCount(); //N�mero de componentes del contenedor
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i); //Se obtiene un componente
	      //Se comprueba si es de tipo bot�n
	      if (comp instanceof JButton) {
		        JButton button = (JButton)comp;
		        //Se comprueba si su texto es el indicado por la clave
		        if (button.getText() ==  UIManager.get(key)) {
		        	//Se le asigna el mnem�nico
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
	 * Asigna el mnem�nico indicado a la etiqueta identificada por la clave .
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnem�nico.
	 * @param mnemonic mnem�nico que se va a asignar al componente
	 */
	private void setLabelMnemonics( Container c, String key, int mnemonic ) {
		 //N�mero de componentes del contenedor
	    int len = c.getComponentCount();
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i);  //Se obtiene un componente
	      //Se comprueba si es de tipo etiqueta
	      if (comp instanceof JLabel) {
	        JLabel label = (JLabel)comp;
	        //Se comprueba si su texto es el indicado por la clave
	        if (label.getText() ==  UIManager.get(key)) {
	        	//Se le asigna el mnem�nico
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
	 * Asigna un mnem�nico predefinido a ciertos toggleButton contenidos en el componente.
	 * @param c contenedor global
	 */
	private void setToggleButtonMnemonics( Container c) {
		 //N�mero de componentes del contenedor
	    int len = c.getComponentCount();
	  //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i); //Se obtiene un componente
	      //Se comprueba si es de tipo JToggleButton
	      if (comp instanceof JToggleButton) {
		    	JToggleButton toggleButton = (JToggleButton) comp;
		    	 //Se almacena su texto asociado
		    	String text = toggleButton.getText();
		    	//Se comprueba que no est� vac�o
		    	if (text!=null && !text.equalsIgnoreCase("")) {
		    		
		    		//Se tratan los botones seg�n su texto
			    	if (text.equalsIgnoreCase("<html><center>Equipo</center></html>")) {
			    		//Se asigna un mnem�nico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_E);
			    		//Como el texto del bot�n contiene c�digo HTML se hace lo siguiente para que se muestre el mnem�nico al usuario
			    		String newText = text.substring(0, 14) +"<u>"+text.charAt(14)+"</u>"+text.substring(15);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Elementos recientes</center></html>")) {
			    		//Se asigna un mnem�nico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_L);
			    		//Como el texto del bot�n contiene c�digo HTML se hace lo siguiente para que se muestre el mnem�nico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Escritorio</center></html>")) {
			    		//Se asigna un mnem�nico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_S);
			    		//Como el texto del bot�n contiene c�digo HTML se hace lo siguiente para que se muestre el mnem�nico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Mis documentos</center></html>")) {
			    		//Se asigna un mnem�nico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_I);
			    		//Como el texto del bot�n contiene c�digo HTML se hace lo siguiente para que se muestre el mnem�nico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);

			    	} else if (text.equalsIgnoreCase("<html><center>Red</center></html>")) {
			    		//Se asigna un mnem�nico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_R);
			    		//Como el texto del bot�n contiene c�digo HTML se hace lo siguiente para que se muestre el mnem�nico al usuario
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
	/**
	 * Define el modo alto contraste para los componentes de la ventana.
	 * @param c contenedor global
	 */
	public void setHighContrast(Container c) {
		 //N�mero de componentes del contenedor
	    int len = c.getComponentCount();
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i);  //Se obtiene un componente
	      //Se comprueba si es de tipo etiqueta
	      if (comp instanceof JLabel) {
	        JLabel label = (JLabel)comp;
	        if (GeneralConfig.isHighContrast()){
	        	label.setForeground(Color.WHITE);
	        } else {
	        	label.setForeground(Color.BLACK);
	        }
	      } else if(comp instanceof JToggleButton){
	    	  JToggleButton toggleButton = (JToggleButton)comp;
	    	  if (GeneralConfig.isHighContrast()){
	    		  toggleButton.setForeground(Color.WHITE);
	    	  } else {
	    		  toggleButton.setForeground(Color.BLACK);
	    	  }
	      } else if (comp instanceof JComboBox){
	    	  JComboBox comboBox = (JComboBox)comp;
	    	  if (GeneralConfig.isHighContrast()){
	    		  comboBox.setBackground(Color.WHITE);
	    	  } else {
	    		  comboBox.setBackground(Color.BLACK);  
	    	  }
	      } else if (comp instanceof Container) {
	    	  	//Llamada recursiva
	    	  setHighContrast((Container)comp);
		    }
	    }//for
	  }
}
