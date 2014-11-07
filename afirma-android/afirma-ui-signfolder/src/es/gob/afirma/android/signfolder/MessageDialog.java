package es.gob.afirma.android.signfolder;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;

/** Di&aacute;logo modal con el que mostrar al usuario un mensaje y un bot&oacute;n para ocultar el
 * di&aacute;logo y, opcionalmente, realizar una acci&oacute;n. */
@SuppressLint("ValidFragment")
final class MessageDialog extends DialogFragment {

	private String title = null;
	private String message = null;

	private DialogInterface.OnClickListener positiveListener = null;
	private DialogInterface.OnClickListener negativeListener = null;

	private final AlertDialog.Builder dialogBuilder;

	private final boolean needShowNegativeButton;

	/**
	 * Construye el di&aacute;logo.
	 * @param message Mensaje que mostrar al usuario.
	 * @param listener Manejador con la acci&oacute;n a realizar al cerrar el di&aacute;logo.
	 */
	@SuppressLint("ValidFragment")
	MessageDialog(final String message, final DialogInterface.OnClickListener positiveListener, final Activity activity) {
		this.message = message;
		this.positiveListener = positiveListener;
		this.needShowNegativeButton = false;

		if (activity == null) {
			this.dialogBuilder = new AlertDialog.Builder(getActivity());
		}
		else {
			this.dialogBuilder = new AlertDialog.Builder(activity);
		}
	}

	MessageDialog(final String message, final String title, final DialogInterface.OnClickListener positiveListener, final DialogInterface.OnClickListener negativeListener, final Activity activity) {
		this.message = message;
		this.title = title;
		this.positiveListener = positiveListener;
		this.negativeListener = negativeListener;
		this.needShowNegativeButton = true;

		if (activity == null) {
			this.dialogBuilder = new AlertDialog.Builder(getActivity());
		}
		else {
			this.dialogBuilder = new AlertDialog.Builder(activity);
		}
	}

	void setMessage(final String message) {
		this.message = message;
	}

	void setTitle(final String title) {
		this.title = title;
	}

	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {
		if (this.title != null) {
			this.dialogBuilder.setTitle(this.title);
		}
		this.dialogBuilder.setMessage(this.message);
		this.dialogBuilder.setPositiveButton(android.R.string.ok, this.positiveListener);

		if (this.needShowNegativeButton) {
			this.dialogBuilder.setNegativeButton(android.R.string.cancel, this.negativeListener);
		}
		return this.dialogBuilder.create();
	}
}
