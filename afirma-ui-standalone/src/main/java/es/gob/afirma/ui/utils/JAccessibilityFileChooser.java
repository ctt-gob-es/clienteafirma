package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;

import java.awt.event.KeyEvent;

import java.io.File;
import java.util.Enumeration;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JToggleButton;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.filechooser.FileSystemView;

/**
 * Clase que extiende JFileChooser para hacerla accesible.
 * @author lmerayo
 *
 */
public class JAccessibilityFileChooser extends JFileChooser{
	
	/**
	 * Serial version ID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor.
	 * @param file directorio.
	 */
	public JAccessibilityFileChooser (File file)
	 {
		super(file);
//		UIDefaults d = UIManager.getDefaults();
//		Enumeration<Object> claves = d.keys();
//		while (claves.hasMoreElements())
//		   System.out.println(claves.nextElement());
		
		
		//Asignación de mnemonics
		
		//Etiqueta buscar en ...
		setLabelMnemonics((Container)this, "FileChooser.lookInLabelText", KeyEvent.VK_B);
		//setHighContrast((Container)this);
		
		//Botón Cancelar
		setButtonMnemonics((Container)this, "FileChooser.cancelButtonText", KeyEvent.VK_C);
		
		//Botón Abrir
		setButtonMnemonics((Container)this, "FileChooser.openButtonText", KeyEvent.VK_A);
		
		//Toggle buttons
		//TODO: Revisar puesto que los botones que se hacen accesibles están predefinidos
		setToggleButtonMnemonics((Container)this);

	 }//constructor


	/**
	 * Asigna el mnemónico indicado al botón identificado por la clave .
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnemónico.
	 * @param mnemonic mnemónico que se va a asignar al componente
	 */
	public void setButtonMnemonics( Container c, String key, int mnemonic ) {
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
	public void setLabelMnemonics( Container c, String key, int mnemonic ) {
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
		    }
	      } else if (comp instanceof Container) {
	    	  	//Llamada recursiva
		    	setLabelMnemonics((Container)comp, key, mnemonic);
		    }
	    }//for
	  }
	
	/**
	 * Asigna un mnemónico predefinido a ciertos toggleButton contenidos en el componente.
	 * @param c contenedor global
	 */
	public void setToggleButtonMnemonics( Container c) {
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
	/**
	 * Asigna el mnemónico indicado a la etiqueta identificada por la clave .
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnemónico.
	 * @param mnemonic mnemónico que se va a asignar al componente
	 */
	public void setHighContrast(Container c) {
		 //Número de componentes del contenedor
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
