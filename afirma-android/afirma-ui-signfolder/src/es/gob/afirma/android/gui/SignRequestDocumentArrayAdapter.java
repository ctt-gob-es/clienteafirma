package es.gob.afirma.android.gui;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;
import es.gob.afirma.android.signfolder.R;
import es.gob.afirma.android.signfolder.proxy.SignRequestDocument;

/** Asocia una lista de objetos de la clase <code>Option</code> a un layout donde mostrarlos de forma iterativa
 * @author Alberto Martinez */
public final class SignRequestDocumentArrayAdapter extends ArrayAdapter<SignRequestDocument> {

	private final Context c;
	private final int id;
	private final List<SignRequestDocument> items;

	/** Construye un objeto <code>FileArrayAdapter</code> con una lista de objetos de tipo <code>Option</code> y lo asocia a un contexto definido por
	 * el usuario y a un recurso donde se mostrar&aacute;n
	 * @param context Contexto de la aplicaci&oacute;n donde se mostrar&aacute; la lista
	 * @param objects Lista de objetos a pintar */
	public SignRequestDocumentArrayAdapter(final Context context, final List<SignRequestDocument> objects) {
		super(context, R.layout.array_adapter_file_chooser, objects);
		this.c = context;
		this.id = R.layout.array_adapter_file_chooser;
		this.items = objects;
	}

	@Override
	public SignRequestDocument getItem(final int i) {
		return this.items.get(i);
	}

	@Override
	public View getView(final int position, final View convertView, final ViewGroup parent) {
		
		View v = convertView;
		if (v == null) {
			final LayoutInflater vi = (LayoutInflater) this.c.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			v = vi.inflate(this.id, null);
		}
		final ImageView icon = (ImageView) v.findViewById(R.id.fileIcon);
		final TextView t1 = (TextView) v.findViewById(R.id.TextView01);

		t1.setText(this.items.get(position).getName());
		icon.setImageDrawable(this.c.getResources().getDrawable(R.drawable.icon_file));

		return v;
	}
}