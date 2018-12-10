package es.gob.afirma.standalone.ui.pdf;

import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

/**
 * Filtro de documento para restringir caracteres y longitud en los campos de posici&oacute;n
 * y tama&ntilde;o del &aacute;rea de firma.
 */
class NaturalNumbersDocFilter extends DocumentFilter {

	@Override
	public void insertString(FilterBypass fb, int offset, String text,
            AttributeSet attrs) throws BadLocationException {

		if (fb.getDocument().getLength() + text.length() > 4) {
			return;
		}

		boolean valid = true;
		for (int i = 0; valid && i < text.length(); i++) {
			if (!Character.isDigit(text.charAt(i))) {
				valid = false;
			}
		}
		if (valid) {
			super.insertString(fb, offset, text, attrs);
		}
	}

	@Override
	public void remove(FilterBypass fb, int offset, int length)
			throws BadLocationException {
		super.remove(fb, offset, length);
	}

	@Override
	public void replace(FilterBypass fb, int offset, int length, String text,
            AttributeSet attrs) throws BadLocationException {

		if (fb.getDocument().getLength() - length + text.length() > 4) {
			return;
		}

		boolean valid = true;
		for (int i = 0; valid && i < text.length(); i++) {
			if (!Character.isDigit(text.charAt(i))) {
				valid = false;
			}
		}
		if (valid) {
			super.replace(fb, offset, length, text, attrs);
		}
	}
}
