/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.ui;

import java.awt.Dialog;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;

import es.gob.afirma.Messages;

/**
 * Di&aacute;logo para la creaci&oacute;n de una nueva contrase&ntilde;a.
 * More information about the original class is available from <a target="_top" href=
 * "http://ostermiller.org/utils/PasswordDialog.html">ostermiller.org</a>.
 * @author Stephen Ostermiller http://ostermiller.org/contact.pl?regarding=Java+Utilities
 */
public class PasswordDialog extends JDialog {

	private static final long serialVersionUID = -832548326686122133L;

	private JPasswordField pass1;
	private JPasswordField pass2;
	private JButton okButton;
	private JButton cancelButton;
	private JLabel descriptionLabel;
	
	/**
	 * Get the password that was entered into the dialog before
	 * the dialog was closed.
	 * @return the password from the password field.
	 */
	public char[] getPass(){
		return pass2.getPassword();
	}

	/**
	 * Finds out if user used the OK button or an equivalent action
	 * to close the dialog.
	 * Pressing enter in the password field may be the same as
	 * 'OK' but closing the dialog and pressing the cancel button
	 * are not.
	 *
	 * @return true if the the user hit OK, false if the user canceled.
	 */
	public boolean okPressed(){
		return pressed_OK;
	}

	/** Update this variable when the user makes an action. */
	private boolean pressed_OK = false;

	/**
	 * Create this dialog with the given parent and title.
	 * @param parent window from which this dialog is launched
	 * @param title the title for the dialog box window
	 */
	public PasswordDialog(Dialog parent, String title) {
		super(parent, title, true);
		setTitle(title);
		if (parent != null) setLocationRelativeTo(parent);
	}

	/**
	 * Create this dialog with the given parent and the default title.
	 * @param parent window from which this dialog is launched
	 */
	public PasswordDialog(Dialog parent) {
		this(parent, null);
	}
	
	/**
	 * Create this dialog with the given parent and title.
	 * @param parent window from which this dialog is launched
	 * @param title the title for the dialog box window
	 */
	public PasswordDialog(Frame parent, String title) {
		super(parent, title, true);
		setTitle(title);
		if (parent != null) setLocationRelativeTo(parent);
	}

	/**
	 * Create this dialog with the given parent and the default title.
	 * @param parent window from which this dialog is launched
	 */
	public PasswordDialog(Frame parent) {
		this(parent, null);
	}

	/** Create this dialog with the default title. */
	public PasswordDialog() {
		this((Frame)null, null);
	}

	/** Called by constructors to initialize the dialog. */
	@Override 
	protected void dialogInit(){

		pass1 = new JPasswordField("", 20);
		pass2 = new JPasswordField("", 20);
		okButton = new JButton(Messages.getString("PasswordDialog.0"));
		cancelButton = new JButton(Messages.getString("PasswordDialog.3"));
		descriptionLabel = new JLabel("");
		final JLabel nameLabel = new JLabel(Messages.getString("PasswordDialog.5"));
		final JLabel passLabel = new JLabel(Messages.getString("PasswordDialog.6"));
		
		super.dialogInit();

		KeyListener keyListener = new KeyAdapter() {
			@Override 
			public void keyPressed(KeyEvent e){
				if (e.getKeyCode() == KeyEvent.VK_ESCAPE || 
				   (e.getSource() == cancelButton && e.getKeyCode() == KeyEvent.VK_ENTER)) {
						pressed_OK = false;
						PasswordDialog.this.setVisible(false);
				}
				if (e.getSource() == okButton && e.getKeyCode() == KeyEvent.VK_ENTER) {
					pressed_OK = true;
					PasswordDialog.this.setVisible(false);
				}
			}
		};
		addKeyListener(keyListener);

		ActionListener actionListener = new ActionListener() {
			public void actionPerformed(ActionEvent e){
				Object source = e.getSource();
				if (source == pass1){
					// the user pressed enter in the pass1 field.
					pass1.transferFocus();
				} else {
					// other actions close the dialog.
					pressed_OK = (source == pass2 || source == okButton);
					if (pressed_OK) {
						String password = new String(pass2.getPassword());
						if (password.equals(new String(pass1.getPassword()))) {
							if (password.length() > 1) {
								PasswordDialog.this.setVisible(false);
							}
							else {
								JOptionPane.showMessageDialog(
									PasswordDialog.this,
									Messages.getString("PasswordDialog.7"),
									Messages.getString("PasswordDialog.8"),
									JOptionPane.ERROR_MESSAGE
								);
								pass1.setText("");
								pass1.grabFocus();
								pass2.setText("");
							}
						}
						else {
							JOptionPane.showMessageDialog(
								PasswordDialog.this,
								Messages.getString("PasswordDialog.11"),
								Messages.getString("PasswordDialog.8"),
								JOptionPane.ERROR_MESSAGE
							);
							pass1.setText("");
							pass1.grabFocus();
							pass2.setText("");
						}
					}
					else PasswordDialog.this.setVisible(false);
				}
			}
		};

		final GridBagConstraints c = new GridBagConstraints();
		final JPanel pane = new JPanel(new GridBagLayout());
		pane.setBorder(BorderFactory.createEmptyBorder(10, 20, 5, 20));
		c.fill = GridBagConstraints.BOTH;
		c.insets.top = 5;
		c.insets.bottom = 5;
		c.weightx = 1.0;
		c.gridwidth = 2;
		pane.add(descriptionLabel, c);
		
		c.gridy = 1;
		c.weightx = 0.0;
		c.gridwidth = 1;
		pane.add(nameLabel, c);

		c.gridx = 1;
		c.weightx = 1.0;
		pass1.addActionListener(actionListener);
		pass1.addKeyListener(keyListener);
		pane.add(pass1, c);

		c.gridy = 2;
		c.gridx = 0;
		c.weightx = 0.0;
		pane.add(passLabel, c);

		c.gridx = 1;
		c.weightx = 1.0;
		pass2.addActionListener(actionListener);
		pass2.addKeyListener(keyListener);
		pane.add(pass2, c);

		c.gridy = 3;
		c.gridx = 0;
		c.gridwidth = GridBagConstraints.REMAINDER;
		c.gridheight = GridBagConstraints.REMAINDER;
		c.anchor = GridBagConstraints.CENTER;
		final JPanel panel = new JPanel();
		okButton.addActionListener(actionListener);
		okButton.addKeyListener(keyListener);
		panel.add(okButton, c);
		cancelButton.addActionListener(actionListener);
		cancelButton.addKeyListener(keyListener);
		panel.add(cancelButton, c);
		pane.add(panel, c);

		getContentPane().add(pane);
		pack();
	}

	/**
	 * Establece el texto a mostrar para la solicitud de contrase&ntilde;a.
	 * @param descriptionText Texto para la solicitud de contrase&ntilde;a (Unicode)
	 */
	public void setDescriptionText(String descriptionText) {
		this.descriptionLabel.setText(descriptionText == null ? "" : descriptionText);
		pack();
	}
	
	/**
	 * Shows the dialog and returns true if the user pressed ok.
	 * @return true if the the user hit OK, false if the user canceled.
	 */
	public boolean showDialog(){
		setVisible(true);
		return okPressed();
	}

}
