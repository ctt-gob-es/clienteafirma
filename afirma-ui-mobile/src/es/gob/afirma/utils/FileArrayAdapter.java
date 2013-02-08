package es.gob.afirma.utils;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;
import es.gob.afirma.R;

/** Asocia una lista de objetos de la clase <code>Option</code> a un layout donde mostrarlos de forma iterativa
 * @author Alberto Mart&iacute;nez */
public class FileArrayAdapter extends ArrayAdapter<Option> {
    private final Context c;
    private final int id;
    private final List<Option> items;

    /** Construye un objeto <code>FileArrayAdapter</code> con una lista de objetos de tipo <code>Option</code> y lo asocia a un contexto definido por
     * el usuario y a un recurso donde se mostrarán
     * @param context Contexto de la aplicación donde se mostrará la lista
     * @param textViewResourceId Recurso en el que se pintará la lista
     * @param objects Lista de objetos a pintar */
    public FileArrayAdapter(final Context context, final int textViewResourceId, final List<Option> objects) {
        super(context, textViewResourceId, objects);
        this.c = context;
        this.id = textViewResourceId;
        this.items = objects;
    }

    @Override
    public Option getItem(final int i) {
        return this.items.get(i);
    }

    @Override
    public View getView(final int position, final View convertView, final ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            final LayoutInflater vi = (LayoutInflater) this.c.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            v = vi.inflate(this.id, null);
        }
        final Option o = this.items.get(position);
        if (o != null) {
            final TextView t1 = (TextView) v.findViewById(R.id.TextView01);
            final TextView t2 = (TextView) v.findViewById(R.id.TextView02);

            if (t1 != null) {
                t1.setText(o.getName());
            }
            if (t2 != null) {
                t2.setText(o.getData());
            }
        }
        return v;
    }
}