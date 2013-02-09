package es.gob.afirma;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;

/** Actividad que se muestra cuando se arranca la aplicaci&oaute;n pulsando su icono.
 * @author Alberto Mart&iacute;nez */
public final class MainActivity extends Activity {

    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }

    /** @param v Vista sobre la que se hace clic. */
    public void onClick(final View v) {
        final Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setClass(this, CertChooserActivity.class);
        startActivity(intent);
        finish();
    }
}