package es.gob.afirma.android.signfolder;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.view.View;

/** Di&aacute;logo modal con el que mostrar al usuario un mensaje y un bot&oacute;n para ocultar el
 * di&aacute;logo y, opcionalmente, realizar una acci&oacute;n. */
@SuppressLint("ValidFragment")
class CustomAlertDialog extends DialogFragment {

	private static final String BUNDLE_ID_DIALOG = "dialog"; //$NON-NLS-1$
	private static final String BUNDLE_ID_TITLE = "title"; //$NON-NLS-1$
	private static final String BUNDLE_ID_MESSAGE = "message"; //$NON-NLS-1$
	private static final String BUNDLE_ID_POSITIVE_BUTTON = "pb"; //$NON-NLS-1$
	private static final String BUNDLE_ID_NEGATIVE_BUTTON = "nb"; //$NON-NLS-1$
	
	private static View vista = null;
	static DialogFragmentListener fragmentListener = null;

	public static CustomAlertDialog newInstance(int dialogId, String title, String message,
			String positiveButton, String negativeButton, DialogFragmentListener dFragmentListener) {
		
        if (dFragmentListener != null) {
        	fragmentListener = dFragmentListener;
        }
		
		CustomAlertDialog frag = new CustomAlertDialog();
        Bundle args = new Bundle();
        args.putInt(BUNDLE_ID_DIALOG, dialogId);
        if (title != null) {
        	args.putString(BUNDLE_ID_TITLE, title);
        }
        if (message != null) {
        	args.putString(BUNDLE_ID_MESSAGE, message);
        }
        if (positiveButton != null) {
        	args.putString(BUNDLE_ID_POSITIVE_BUTTON, positiveButton);
        }
        if (negativeButton != null) {
        	args.putString(BUNDLE_ID_NEGATIVE_BUTTON, negativeButton);
        }

        frag.setArguments(args);
        return frag;
    }

    public static CustomAlertDialog newInstance(int dialogId, String title, View message,
    		String positiveButton, String negativeButton, DialogFragmentListener dFragmentListener) {
		
        if (dFragmentListener != null) {
        	fragmentListener = dFragmentListener;
        }
    	
		CustomAlertDialog frag = new CustomAlertDialog();
        Bundle args = new Bundle();
        args.putInt(BUNDLE_ID_DIALOG, dialogId);
        if (title != null) {
        	args.putString(BUNDLE_ID_TITLE, title);
        }
        if (message != null) {
        	vista = message;
        }
        if (positiveButton != null) {
        	args.putString(BUNDLE_ID_POSITIVE_BUTTON, positiveButton);
        }
        if (negativeButton != null) {
        	args.putString(BUNDLE_ID_NEGATIVE_BUTTON, negativeButton);
        }

        frag.setArguments(args);
        return frag;
    }

	@Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
		
    	final int dialogId = getArguments().getInt(BUNDLE_ID_DIALOG);
        Builder dialogBuilder = new AlertDialog.Builder(getActivity());
        
        final String title = getArguments().getString(BUNDLE_ID_TITLE);
        if (title != null) {
        	dialogBuilder.setTitle(title);
        }
        
	    if (getArguments().getString(BUNDLE_ID_MESSAGE) != null) {
        	dialogBuilder.setMessage(getArguments().getString(BUNDLE_ID_MESSAGE));
        } else if (vista !=null){
        	dialogBuilder.setView(vista);
        }
        
        final String positiveButton = getArguments().getString(BUNDLE_ID_POSITIVE_BUTTON);
        if (positiveButton != null) {
        	dialogBuilder.setPositiveButton(positiveButton,
                    new DialogInterface.OnClickListener() {
                        @Override
						public void onClick(DialogInterface dialog, int whichButton) {
                        	fragmentListener.onDialogPositiveClick(dialogId);
                        }
                    }
                );
        }
        
        final String negativeButton = getArguments().getString(BUNDLE_ID_NEGATIVE_BUTTON);
        if (negativeButton != null) {
        	dialogBuilder.setNegativeButton(negativeButton,
                    new DialogInterface.OnClickListener() {
                        @Override
						public void onClick(DialogInterface dialog, int whichButton) {
                        	fragmentListener.onDialogNegativeClick(dialogId);
                        }
                    }
                );
        }

        return dialogBuilder.create();
    }
}
