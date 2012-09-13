/*
 * Controlador Java de la Secretaría de Estado de Administraciones Públicas
 * para el DNI electrónico.
 *
 * El Controlador Java para el DNI electrónico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electrónico en aplicaciones Java de terceros 
 * para la realización de procesos de autenticación, firma electrónica y validación 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electrónico, así como la realización 
 * de operaciones criptográficas de firma con el DNI electrónico. El Controlador ha 
 * sido diseñado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Dirección General de Modernización Administrativa, Procedimientos 
 * e Impulso de la Administración Electrónica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podrán elegir bajo cual de las
 * licencias desean utilizar el código fuente. Su elección deberá reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinará
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los términos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la versión 2.1 de la Licencia, o en una versión posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los términos de la 
 * European Union Public License publicada por la Comisión Europea, 
 * tanto en la versión 1.1 de la Licencia, o en una versión posterior.
 * 
 * Debería recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consúltelo en <http://www.gnu.org/licenses/>.
 * 
 * Debería recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consúltelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea útil, pero
 * SIN NINGUNA GARANTÍA; incluso sin la garantía implícita de comercialización
 * o idoneidad para un propósito particular.
 */
package es.gob.jmulticard.ui.passwordcallback.gui;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.Timer;

/**
 * Componente basado en JLabel para capturar Passwords usando &uacute;nicamente arrays de char y restringiendo los caracteres aceptados.
 * @author Jose Luis Escanciano
 *
 */
final class JSecurePasswordLabel extends JLabel {

	private static final long serialVersionUID = -4343328489072897605L;

	private final int delay = 500;
	private final char[] pass;
	private final int maxChars;
	private int passwordLength;
	private Timer timer;
	private boolean showCursor;

	int getMaxChars() {
		return this.maxChars;
	}

	/** Constructor.
	 * @param maxLength Longitud m&aacute;xima que puede tener la contrase&ntilde;a. */
	JSecurePasswordLabel(final int maxLength) {
		super();
		this.maxChars = maxLength;
		this.pass = new char[maxLength];
		clearPassword();
		this.addKeyListener(new KeyAdapter() {
			/** Indica si un caracter, encapsulado en el evento de pulsaci&oacute;n de tecla, es v&aacute;lido
			 * @param k Evento de pulsaci&oacute;n de tecla.
			 * @return Si el caracter es o no v&aacute;lido. */
			private boolean isValid(final char c){
				// Letras y numeros
				if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
					return true;
				}
				// Especiales permitidos
				if (c == '.' || c == '!' || c == '?' || c == '&' || c == '%' || c == '=' || c == '+' || c == '-' || c == '_' || c == '(' || c == ')' || c == '<' || c == '>') {
					return true;
				}
				return false;
			}

			@Override
			public void keyTyped(final KeyEvent ke) {
				//Caracteres validos password
				if (isValid(ke.getKeyChar()) && (getPasswordLength() < getMaxChars())) {
					JSecurePasswordLabel.this.pass[JSecurePasswordLabel.this.passwordLength++] = ke.getKeyChar();
					ke.setKeyChar('\0');
				}
				updateText();
			}

			@Override
			public void keyPressed(final KeyEvent arg0) {
				//Borrar
				if (arg0.getKeyCode() == KeyEvent.VK_BACK_SPACE && getPasswordLength() > 0) {
					clearPassword(getPasswordLength() - 1);
				}
				//Supr
				else if(arg0.getKeyCode() == KeyEvent.VK_DELETE) {
					clearPassword();
				}
				updateText();
			}
		});
		this.addFocusListener(new FocusListener() {
			@Override
			public void focusGained(final FocusEvent arg0) {
				JSecurePasswordLabel.this.setBackground(Color.WHITE);
				JSecurePasswordLabel.this.timer.start();
			}

			@Override
			public void focusLost(final FocusEvent arg0) {
				JSecurePasswordLabel.this.setShowCursor(false);
				JSecurePasswordLabel.this.setBackground(getParent().getBackground());
				JSecurePasswordLabel.this.timer.stop();
				JSecurePasswordLabel.this.updateText();
			}
		});
		this.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(final MouseEvent e) {
				JSecurePasswordLabel.this.requestFocus();
			}
		} );
		this.setFocusable(true);
		this.setOpaque(true);
		this.setBackground(Color.WHITE);
		this.setBorder(BorderFactory.createLoweredBevelBorder());
		this.setShowCursor(false);
		this.timer = new Timer(this.delay, new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if(arg0.getSource().equals(timer)){
					if(hasFocus()){
						JSecurePasswordLabel.this.setShowCursor(!getShowCursor());
						JSecurePasswordLabel.this.updateText();
					}
				}
			}
		});
		this.timer.stop();
	}

	/** Muestra un asterisco por cada caracter de la contrase&ntilde;a. */
	synchronized void updateText(){
		String text = " "; //$NON-NLS-1$
		for(int i = 0; i < this.passwordLength; i++){
			text += "*"; //$NON-NLS-1$
		}
		text += getShowCursor() ? "|" : " "; //$NON-NLS-1$ //$NON-NLS-2$
		setText(text);
	}

	/** Establece a ceros ('\0') toda la contrase&ntilde;a. */
	void clearPassword(){
		clearPassword(0);
	}

	/** Establece a ceros ('\0') toda la contrase&ntilde;a a partir de una posici&oacute;n inicial
	 * @param position Posici&oacute;n inicial */
	void clearPassword(final int position){
		for(int i = position; i < this.pass.length; i++){
			this.pass[i] = '\0';
		}
		this.passwordLength = position;
		updateText();
	}

	/** Retorna la contrase&ntilde;a introducida. Tras llamar a este m&eacute;todo, el Password del componente vac&iacute;a.
	 * @return Contrase&ntilde;a introducida en el componente. */
	char[] getPassword() {
		//Nos quedamos con el password valido, el resto lo descartamos
		final char[] returned = new char[this.passwordLength];
		for(int i = 0; i < this.passwordLength; i++){
			returned[i] = this.pass[i];
		}
		clearPassword();
		return returned;
	}

	/** Retorna la longitud de la contrase&ntilde;a introducida.
	 * @return Longitud de la contrase&ntilde;a introducida. */
	int getPasswordLength(){
		return this.passwordLength;
	}

	/**
	 * Setter privado para el campo showCursor que indica si ha de mostrarse el cursor o no
	 * @param show Si ha de mostrarse el cursor o no
	 */
	private synchronized void setShowCursor(boolean show){
		this.showCursor = show;
	}
	
	/**
	 * Getter privado para el campo showCursor que indica si ha de mostrarse el cursor o no
	 * @return Si ha de mostrarse el cursor o no
	 */
	private synchronized boolean getShowCursor(){
		return this.showCursor;
	}
}