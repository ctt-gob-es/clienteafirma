package es.gob.afirma.android.signfolder;

import java.io.File;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Vector;

import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.DialogInterface.OnKeyListener;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.security.KeyChainException;
import android.support.v4.app.FragmentActivity;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TabHost;
import android.widget.TextView;
import android.widget.Toast;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.KeySelectedEvent;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.PrivateKeySelectionListener;
import es.gob.afirma.android.signfolder.DownloadFileTask.DownloadDocumentListener;
import es.gob.afirma.android.signfolder.LoadPetitionDetailsTask.LoadSignRequestDetailsListener;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestDetail;
import es.gob.afirma.android.signfolder.proxy.RequestResult;
import es.gob.afirma.android.signfolder.proxy.SignLine;
import es.gob.afirma.android.signfolder.proxy.SignRequest;
import es.gob.afirma.android.signfolder.proxy.SignRequest.RequestType;
import es.gob.afirma.android.signfolder.proxy.SignRequestDocument;

/** @author Carlos Gamuci */
public final class PetitionDetailsActivity extends FragmentActivity implements LoadSignRequestDetailsListener,
                                                                         DownloadDocumentListener,
                                                                         OperationRequestListener,
                                                                         PrivateKeySelectionListener,
                                                                         DialogFragmentListener {

	static final String EXTRA_RESOURCE_CERT_B64 = "es.gob.afirma.signfolder.cert"; //$NON-NLS-1$
	static final String EXTRA_RESOURCE_CERT_ALIAS = "es.gob.afirma.signfolder.alias"; //$NON-NLS-1$
	static final String EXTRA_RESOURCE_REQUEST_ID = "es.gob.afirma.signfolder.requestId"; //$NON-NLS-1$
	static final String EXTRA_RESOURCE_REQUEST_STATE = "es.gob.afirma.signfolder.requestState"; //$NON-NLS-1$

	static final int RESULT_SIGN_OK = 1;
	static final int RESULT_REJECT_OK = 2;
	static final int RESULT_SIGN_FAILED = 3;
	static final int RESULT_REJECT_FAILED = 4;

	private static final String PDF_MIMETYPE = "application/pdf"; //$NON-NLS-1$
	private static final String PDF_FILE_EXTENSION = ".pdf"; //$NON-NLS-1$
	private static final String DOC_FILE_EXTENSION = ".doc"; //$NON-NLS-1$
	private static final String DOCX_FILE_EXTENSION = ".docx"; //$NON-NLS-1$
	private static final String TXT_FILE_EXTENSION = ".txt"; //$NON-NLS-1$
	private static final String XML_FILE_EXTENSION = ".xml"; //$NON-NLS-1$

	static final int REQUEST_CODE = 3;

	/** Di&aacute;logo para confirmar la firma de peticiones. */
	private final static int DIALOG_CONFIRM_OPERATION = 15;

	/** Di&aacute;logo para informar de un error. */
	private final static int DIALOG_MSG_ERROR = 16;

	/** Di&aacute;logo para confirmar el rechazo de peticiones. */
	private final static int DIALOG_CONFIRM_REJECT = 13;

	/** Tag para la presentaci&oacute;n de di&aacute;logos */
	private final static String DIALOG_TAG = "dialog"; //$NON-NLS-1$

	/** Identificador de la firma PAdES. */
	public static final String SIGN_FORMAT_PADES = "PAdES"; //$NON-NLS-1$

	/** Identificador de la firma XAdES por defecto. */
	public static final String SIGN_FORMAT_XADES = "XAdES"; //$NON-NLS-1$

	private String certB64 = null;
	private String certAlias = null;

	private RequestDetail reqDetails = null;

	private String requestState = null;

	private List<File> tempDocuments = null;

	private ProgressDialog progressDialog = null;
	ProgressDialog getProgressDialog() {
		return this.progressDialog;
	}
	void setProgressDialog(final ProgressDialog pd) {
		this.progressDialog = pd;
	}

	private TabHost th;
	void setTabHost(final TabHost th){
		this.th = th;
	}
	TabHost getTabHost(){
		return this.th;
	}

	private CustomAlertDialog dialog;
	private void setCustomAlertDialog(final CustomAlertDialog dialog){
		this.dialog = dialog;
	}
	CustomAlertDialog getCustomAlertDialog(){
		return this.dialog;
	}

	@Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(
        		SignRequest.STATE_UNRESOLVED.equals(getIntent().getStringExtra(EXTRA_RESOURCE_REQUEST_STATE)) ?
        				R.layout.activity_unresolved_petition_details :
        					R.layout.activity_resolved_petition_details);

        setTabHost((TabHost) findViewById(android.R.id.tabhost));
        getTabHost().setup();
        getTabHost().getTabWidget().setDividerDrawable(R.drawable.tab_divider);

        setupTab(this.getResources().getString(R.string.details),1);
		setupTab(this.getResources().getString(R.string.sign_lines),2);
		setupTab(this.getResources().getString(R.string.docs),3);
     	getTabHost().setCurrentTab(0);



	}

	/**
	 * M&eacute;todo para cargar el contenido de cada pesta&ntilde;a del panel de detalle.
	 * @param view
	 * @param tag
	 * @param order
	 */
	private void setupTab(final String tag, final int order) {
		final View tabview = createTabView( getTabHost().getContext(), tag);
		if (order == 1) {
			getTabHost().addTab( getTabHost().newTabSpec(String.valueOf(R.id.tab1)).setIndicator(tabview).setContent(R.id.tab1));
		}
		else if(order == 2) {
			getTabHost().addTab( getTabHost().newTabSpec(String.valueOf(R.id.tab2)).setIndicator(tabview).setContent(R.id.tab2));
		}
		else if (order == 3) {
			getTabHost().addTab( getTabHost().newTabSpec(String.valueOf(R.id.tab3)).setIndicator(tabview).setContent(R.id.tab3));
		}
	}

	//metodo para crear la pesta�a del tab customizada
	private static View createTabView(final Context context, final String text) {
		final View view = LayoutInflater.from(context).inflate(R.layout.tabs_bg, null);
		final TextView tv = (TextView) view.findViewById(R.id.tabsText);
		tv.setText(text);
		return view;
	}
	@Override
	protected void onStart() {
		super.onStart();

		this.requestState = getIntent().getStringExtra(EXTRA_RESOURCE_REQUEST_STATE);
		if (getIntent() != null && getIntent().getStringExtra(EXTRA_RESOURCE_REQUEST_ID) != null &&
				getIntent().getStringExtra(EXTRA_RESOURCE_CERT_B64) != null &&
				getIntent().getStringExtra(EXTRA_RESOURCE_CERT_ALIAS) != null) {
			this.certB64 = getIntent().getStringExtra(EXTRA_RESOURCE_CERT_B64);
			this.certAlias = getIntent().getStringExtra(EXTRA_RESOURCE_CERT_ALIAS);

			if (this.reqDetails != null) {
				showRequestDetails(this.reqDetails);
			}
			else {
				final LoadPetitionDetailsTask lpdt = new LoadPetitionDetailsTask(
						getIntent().getStringExtra(EXTRA_RESOURCE_REQUEST_ID),
						this.certB64,
						CommManager.getInstance(),
						this);
				showProgressDialog(getString(R.string.dialog_msg_loading),lpdt);
				lpdt.execute();
			}
		}
	}

	/**
	 * M&eacute;todo para el acceso al certificado de firma desde las clases internas usadas por la Activity.
	 * @return Certificado en base 64.
	 */
	String getCertB64() {
		return this.certB64;
	}

    /**
     * Carga en las pesta&ntilde;as de los paneles de detalle de la solicitud la informaci&oacute;n
     * de la solicitud introducida.
     * @param details Solicitud de la que mostrar los datos.
     */
    @Override
    public void loadedSignRequestDetails(final RequestDetail details) {

    	dismissProgressDialog();

    	this.reqDetails = details;

    	showRequestDetails(details);
    }

    private void showRequestDetails(final RequestDetail details) {
    	// Pestana de detalle
    	((TextView) findViewById(R.id.subjectValue)).setText(details.getSubject());
    	((TextView) findViewById(R.id.referenceValue)).setText(details.getRef());
    	((TextView) findViewById(R.id.dateValue)).setText(details.getDate());
    	((TextView) findViewById(R.id.applicationValue)).setText(details.getApp());

    	//Dependiendo del tipo de peticion el boton tendra el texto de firmar o de visto bueno
    	if(SignRequest.STATE_UNRESOLVED.equals(this.requestState)){
	    	final Button typeButton = (Button) findViewById(R.id.btnSign);
	     	if(details.getType() == RequestType.SIGNATURE) {
	     		typeButton.setText(R.string.sign);
	     	} else {
	     		typeButton.setText(R.string.vb);
	     	}
    	}

    	// Listado de remitentes en la pestana de detalle
    	final ListView sendersList = (ListView) findViewById(R.id.listSenders);
    	sendersList.setEnabled(false);
    	sendersList.setAdapter(new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, details.getSenders()));

        // Listado de documentos en la pestana de documentos
    	final List<SignRequestDocument> documentsList = Arrays.asList(details.getDocs());
    	final ListView docsList = (ListView) findViewById(R.id.listDocs);
        docsList.setEnabled(false);
        docsList.setAdapter(new SignLineArrayAdapter(this, prepareDocsItems(documentsList)));


    	// Listado de lineas de firma en la pestana de lineas de firma
    	final ListView signLinesList = (ListView) findViewById(R.id.listSignLines);
    	signLinesList.setEnabled(false);
    	signLinesList.setAdapter(new SignLineArrayAdapter(this, prepareSignLineItems(details.getSignLines())));
    }

    private List<RequestDetailAdapterItem> prepareDocsItems(final List<SignRequestDocument> documentsList){
    	final List<RequestDetailAdapterItem> list = new ArrayList<PetitionDetailsActivity.RequestDetailAdapterItem>();
		list.add(new SignLineHeader(getString(R.string.docs)));
		//for (int i = 0; i < documentsList.size(); i++) {
		for (final SignRequestDocument doc : documentsList) {
			list.add(new DocItem(doc.getName(), doc.getSize(), doc.getMimeType(),
					doc.getId(), DownloadFileTask.DOCUMENT_TYPE_DATA));
		}

		if (SignRequest.STATE_SIGNED.equals(this.requestState) && this.reqDetails.getType() == RequestType.SIGNATURE) {

			// Firmas
			list.add(new SignLineHeader(getString(R.string.signs)));
			for (int i = 0; i<documentsList.size(); i++) {
				list.add(new DocItem(
						getString(R.string.sign_filename, documentsList.get(i).getName(),
								getSignatureExtension(documentsList.get(i).getSignFormat())),
								documentsList.get(i).getId(), DownloadFileTask.DOCUMENT_TYPE_SIGN));
			}

			// Informes de firma
			list.add(new SignLineHeader(getString(R.string.sign_reports)));
			for (int i = 0; i<documentsList.size(); i++) {
				list.add(new DocItem(
						getString(R.string.sign_report, documentsList.get(i).getName()),
								documentsList.get(i).getId(), DownloadFileTask.DOCUMENT_TYPE_REPORT));
			}
    	}
    	return list;
    }

    private final static String PADES_EXTENSION = "pdf";  //$NON-NLS-1$
    private final static String CADES_EXTENSION = "cades";  //$NON-NLS-1$
    private final static String XADES_EXTENSION = "xades";  //$NON-NLS-1$

    /**
     * Devuelve la extensi&oacute;n correspondiente a una firma a partir del formato de firma utilizado.
     * @param signFormat Formato de firma.
     */
    private static String getSignatureExtension(final String signFormat) {
    	String ext;
    	if (SIGN_FORMAT_PADES.equalsIgnoreCase(signFormat)) {
    		ext = PADES_EXTENSION;
    	}
    	else if (SIGN_FORMAT_XADES.equalsIgnoreCase(signFormat)) {
    		ext = XADES_EXTENSION;
    	}
    	else {
    		ext = CADES_EXTENSION;
    	}
    	return ext;
    }

    private List<RequestDetailAdapterItem> prepareSignLineItems(final Vector<SignLine>[] signLines) {

    	final List<RequestDetailAdapterItem> list = new ArrayList<PetitionDetailsActivity.RequestDetailAdapterItem>();
    	for (int i = 0; i < signLines.length; i++) {
    		final Vector<SignLine> signLine = signLines[i];
    		list.add(new SignLineHeader(getString(R.string.signline_header, Integer.valueOf(i + 1))));
    		for (final SignLine signer : signLine) {
    			list.add(new SignLineItem(signer));
    		}
    	}

		return list;
	}

	@Override
    public void errorLoadingSignRequestDetails() {
    	dismissProgressDialog();
    	Toast.makeText(this, getString(R.string.error_msg_loading_request_details), Toast.LENGTH_SHORT).show();
    	// Ha ocurrido un error al cargar el detalle de la aplicacion, cerramos la actividad
    	closeActivity();
    }

	/**
	 * Muestra un mensaje solicitando confirmacion al usuario para descargar y previsualizar
	 * el fichero seleccionado.
	 * @param docId Identificador del documento seleccionado.
	 */
	void showConfirmPreviewDialog(final String docId, final String filename, final String mimetype, final int docType) {

		final OnClickListener listener = new OnClickListener() {
			@Override
			public void onClick(final DialogInterface dlg, final int which) {
				final DownloadFileTask dlfTask =
						new DownloadFileTask(docId, docType,
								filename,
								mimetype,
								docType == DownloadFileTask.DOCUMENT_TYPE_SIGN,
								getCertB64(), CommManager.getInstance(), PetitionDetailsActivity.this, PetitionDetailsActivity.this);
				dlfTask.execute();
				showProgressDialogDownloadFile(getString(R.string.loading_doc), dlfTask);
			}
		};

		String dialogTitle;
		String dialogMessage;
		if (docType == DownloadFileTask.DOCUMENT_TYPE_SIGN) {
			dialogMessage = getString(R.string.dialog_msg_confirm_save_sign);
			dialogTitle = getString(R.string.dialog_msg_confirm_save_sign_title);
		} else {
			dialogMessage = getString(R.string.dialog_msg_confirm_preview);
			dialogTitle = getString(R.string.dialog_msg_confirm_preview_title);
		}

		final MessageDialog confirmPreviewDialog = new MessageDialog(dialogMessage,
				dialogTitle, listener, null, this);

		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				confirmPreviewDialog.show(getSupportFragmentManager(), "ConfirmDialog"); //$NON-NLS-1$;
			}
		});
	}

    /**
     * Interfaz que implementan los tipos de elemento que componen las l&iacute;neas de firma.
     */
    private interface RequestDetailAdapterItem {
    	public int getViewType();
    	public View getView(LayoutInflater inflater, View convertView);
    }

    /**
     * Tipo de elemento de la lista de lineas de firma (cabeceras y firmantes).
     */
    private enum SignLineItemType {
		SIGNLINE_ITEM, HEADER_ITEM, DOC_ITEM
	}

    class SignLineHeader implements RequestDetailAdapterItem {

    	private final String name;

        public SignLineHeader(final String name) {
            this.name = name;
        }

        @Override
        public int getViewType() {
            return SignLineItemType.HEADER_ITEM.ordinal();
        }

        @Override
        public View getView(final LayoutInflater inflater, final View convertView) {
            View view;
            if (convertView == null) {
                view = inflater.inflate(R.layout.array_adapter_header, null);
            } else {
                view = convertView;
            }
            final TextView text = (TextView) view.findViewById(R.id.header);
            text.setText(this.name);

            return view;
        }
    }

    final class SignLineItem implements RequestDetailAdapterItem {
        private final SignLine signer;

        SignLineItem(final SignLine signer) {
            this.signer = signer;

        }

        @Override
        public int getViewType() {
            return SignLineItemType.SIGNLINE_ITEM.ordinal();
        }

        @Override
        public View getView(final LayoutInflater inflater, final View convertView) {
            View view;
            if (convertView == null) {
                view = inflater.inflate(R.layout.array_adapter_signline_item, null);
                // Do some initialization
            } else {
                view = convertView;
            }

            ((TextView) view.findViewById(R.id.signer)).setText(this.signer.getSigner());
            if (this.signer.isDone()) {
            	((ImageView) view.findViewById(R.id.signerIcon)).setImageResource(R.drawable.icon_signline_done);
            }
            return view;
        }
    }

    final class DocItem implements RequestDetailAdapterItem, View.OnClickListener {
    	private final String docId;
    	private final String name;
    	private final String mimetype;
        private final int size;
        private final int docType;

        DocItem(final String name, final int size, final String mimetype, final String docId, final int docType) {
            this.name = name;
            this.size = size;
            this.mimetype = mimetype;
            this.docId = docId;
            this.docType = docType;
        }

        DocItem(final String name, final String docId, final int docType) {
            this.name = name;
            this.size = -1;
            this.mimetype = null;
            this.docId = docId;
            this.docType = docType;
        }

        @Override
        public int getViewType() {
            return SignLineItemType.DOC_ITEM.ordinal();
        }

        @Override
        public View getView(final LayoutInflater inflater, final View convertView) {
            View view;
            if (convertView == null) {
                view = inflater.inflate(R.layout.array_adapter_file_chooser, null);
            }
            else {
                view = convertView;
            }

            view.setBackgroundResource(R.drawable.array_adapter_selector_white);
            final ImageView icon = (ImageView) view.findViewById(R.id.fileIcon);
    		final TextView t1 = (TextView) view.findViewById(R.id.TextView01);
    		final TextView t2 = (TextView) view.findViewById(R.id.TextView02);

    		if (this.name.endsWith(PDF_FILE_EXTENSION)) {
    			icon.setImageResource(R.drawable.icon_pdf_file);
    		} else if (this.name.endsWith(DOC_FILE_EXTENSION) || this.name.endsWith(DOCX_FILE_EXTENSION)) {
    			icon.setImageResource(R.drawable.icon_doc_file);
    		} else if (this.name.endsWith(TXT_FILE_EXTENSION)) {
    			icon.setImageResource(R.drawable.icon_text_file);
    		} else if (this.name.endsWith(XML_FILE_EXTENSION)) {
    			icon.setImageResource(R.drawable.icon_xml_file);
    		} else {
    			icon.setImageResource(R.drawable.icon_file);
    		}

    		t1.setText(this.name);
    		if (this.size != -1) {
    			t2.setText(getString(R.string.file_chooser_tamano_del_fichero, formatFileSize(this.size)));
    		}
    		view.setOnClickListener(this);

            return view;
        }

        /** Devuelve una cadena con el tama&ntilde;o indicado  en bytes formateada.
         * @param fileSize Tama&ntilde;o que se quiere representar.
         * @return Cadena que representa el tama&ntilde;o. */
    	private String formatFileSize(final long fileSize) {

    		if (fileSize < 1024) {
    			return addDotMiles(fileSize) + " " + getString(R.string.bytes);  //$NON-NLS-1$
    		}
    		else if (fileSize/1024 < 1024) {
    			return addDotMiles(fileSize/1024) + " " + getString(R.string.kilobytes);  //$NON-NLS-1$
    		}
    		else {
    			final long kbs = fileSize/1024;
    			String fraction = Long.toString(kbs % 1024);
    			if (fraction.length() > 2) {
    				fraction = fraction.substring(0, 2);
    			}
    			return addDotMiles(kbs/1024) + "," + fraction + " " + getString(R.string.megabytes);  //$NON-NLS-1$ //$NON-NLS-2$
    		}
    	}

    	/**
    	 * Devuelve un n&uacute;mero en formato texto con los puntos de miles.
    	 * @param number N&uacute;mero que se quiere representar.
    	 * @return N&uacute;mero representado.
    	 */
    	private String addDotMiles(final long number) {

    		final String nString = Long.toString(number);
    		final StringBuffer buffer = new StringBuffer();
    		if (nString.length() > 3) {
    			int dotPos = nString.length() % 3;
    			if (dotPos > 0) {
    				buffer.append(nString.substring(0, dotPos));
    			}
    			while (dotPos < nString.length()) {
    				if (dotPos > 0) {
    					buffer.append('.');
    				}
    				buffer.append(nString.substring(dotPos, dotPos + 3));
    				dotPos += 3;
    			}

    		} else {
    			buffer.append(nString);
    		}
    		return buffer.toString();
    	}

        @Override
		public void onClick(final View v) {
			showConfirmPreviewDialog(this.docId, this.name, this.mimetype, this.docType);
		}
    }
    /**
     * Adaptador para la lista de l&iacute;neas de firma.
     */
    class SignLineArrayAdapter extends ArrayAdapter<RequestDetailAdapterItem> {

    	private final LayoutInflater mInflater;

    	public SignLineArrayAdapter(final Context context, final List<RequestDetailAdapterItem> objects) {
    		super(context, 0, objects);
    		this.mInflater = LayoutInflater.from(context);
		}

    	@Override
    	public int getViewTypeCount() {
    		return SignLineItemType.values().length;
    	}

    	@Override
    	public int getItemViewType(final int position) {
    		return getItem(position).getViewType();
    	}

    	@Override
    	public View getView(final int position, final View convertView, final ViewGroup parent) {
    		return getItem(position).getView(this.mInflater, convertView);
    	}
    }

	@Override
	public void downloadDocumentSuccess(final File documentFile, final String filename, final String mimetype, final int docType) {
		dismissProgressDialog();
		if (this.tempDocuments == null) {
			this.tempDocuments = new ArrayList<File>();
		}
		this.tempDocuments.add(documentFile);

		// Si el fichero no es de firma, lo abrimos
		if (docType != DownloadFileTask.DOCUMENT_TYPE_SIGN) {
			openFile(documentFile, mimetype);
		}
		else {
			try {
				runOnUiThread(new Runnable() {
					@Override
					public void run() {
						Toast.makeText(PetitionDetailsActivity.this, R.string.toast_msg_download_ok, Toast.LENGTH_SHORT).show();
					}
				});
			} catch (final Exception e) {
				Log.w(SFConstants.LOG_TAG, "No se pudo informar de que la firma se guardo correctamente: " + e); //$NON-NLS-1$
				e.printStackTrace();
			}
		}
	}

	@Override
	public void downloadDocumentError() {
		dismissProgressDialog();

		final CustomAlertDialog dlg = CustomAlertDialog.newInstance(
				DIALOG_MSG_ERROR,
				getString(R.string.error),
				getString(R.string.toast_error_previewing),
				getString(android.R.string.ok),
				null,
				this
				);

		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				dlg.show(getSupportFragmentManager(), DIALOG_TAG);
			}
		});
	}

	/**
	 * Abre y elimina un fichero
	 * @param documentFile Documento que se debe abrir.
	 * @param mimetype Tipo del documento.
	 */
	private void openFile(final File documentFile, final String mimetype) {

		if (mimetype != null && mimetype.equals(PDF_MIMETYPE) ||
				documentFile.getName().toLowerCase(Locale.US).endsWith(PDF_FILE_EXTENSION)) {
			viewPdf(documentFile);
		}
		else {
			final Intent intent = new Intent(Intent.ACTION_VIEW);
			intent.setDataAndType(Uri.fromFile(documentFile), mimetype);
			try {
				this.startActivity(intent);
			} catch (final ActivityNotFoundException e) {

				Log.w(SFConstants.LOG_TAG, "No se pudo abrir el fichero guardado: " + e); //$NON-NLS-1$
				e.printStackTrace();

				final MessageDialog md = new MessageDialog(
						getString(R.string.error_file_not_support), null, this);
				md.setTitle(getString(R.string.error_title_openning_file));
				runOnUiThread(new Runnable() {
					@Override
					public void run() {
						md.show(getSupportFragmentManager(), "ErrorDialog"); //$NON-NLS-1$
					}
				});
			}
		}
	}

	private void viewPdf (final File file) {
        final String adobePackage = "com.adobe.reader"; //$NON-NLS-1$
        final String gdrivePackage = "com.google.android.apps.viewer"; //$NON-NLS-1$
        boolean isGdriveInstalled = false;

        final Intent intent = new Intent(Intent.ACTION_VIEW);
        intent.setDataAndType(Uri.fromFile(file), PDF_MIMETYPE);

        final PackageManager pm = getApplicationContext().getPackageManager();
        final List<ResolveInfo> list = pm.queryIntentActivities(intent, 0);
        if (list.isEmpty()) {
            Log.w(SFConstants.LOG_TAG, "No hay visor pdf instalado"); //$NON-NLS-1$
            new AlertDialog.Builder(PetitionDetailsActivity.this)
                .setTitle(R.string.error)
                .setMessage(R.string.no_pdf_viewer_msg)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
					public void onClick(final DialogInterface dialog, final int which) {
                        return;
                    }
                })
                .create().show();
        }
        else {

            for (final ResolveInfo resolveInfo : list) {
                if (resolveInfo.activityInfo.name.startsWith(adobePackage)) {
                    intent.setPackage(resolveInfo.activityInfo.packageName);
                    startActivity(intent);
                    return;
                }
                else if (resolveInfo.activityInfo.name.startsWith(gdrivePackage)) {
                    intent.setPackage(resolveInfo.activityInfo.packageName);
                    isGdriveInstalled = true;
                }
            }

            if (isGdriveInstalled) {
            	startActivity(intent);
                return;
            }

            Log.i(SFConstants.LOG_TAG, "Ni Adobe ni Gdrive instalado"); //$NON-NLS-1$
            new AlertDialog.Builder(PetitionDetailsActivity.this)
                .setTitle(R.string.aviso)
                .setMessage(R.string.no_adobe_reader_msg)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
					public void onClick(final DialogInterface dialog, final int which) {
                        startActivity(intent);
                    }
                })
                .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
                    @Override
					public void onClick(final DialogInterface dialog, final int which) {
                        return;
                    }
                })
                .create().show();
        }
    }

	@Override
	protected void onDestroy() {
		super.onDestroy();

		if (this.tempDocuments != null) {
			for (final File doc : this.tempDocuments) {
				Log.i(SFConstants.LOG_TAG, "Se intenta borrar el fichero: " + doc.getAbsolutePath()); //$NON-NLS-1$
				doc.delete();
			}
		}
	}

    /** Metodo que define la accion a realizar al pulsar en el boton Sign
     * @param v Vista desde la que se invoco el metodo */
    public void onClickSign(final View v) {
    	if (this.reqDetails == null) {
    		return;
    	}

    	final CustomAlertDialog dlg = CustomAlertDialog.newInstance(
    			DIALOG_CONFIRM_OPERATION,
    			getString(R.string.aviso),
    			getConfirmDialogMessage(this.reqDetails.getType()),
    			getString(android.R.string.ok),
    			getString(R.string.cancel),
    			this
    			);

    	runOnUiThread(new Runnable() {
			@Override
			public void run() {
				dlg.show(getSupportFragmentManager(), DIALOG_TAG);
			}
		});
    }


    //Metodo que devuelve un string indicando el tipo de operacion a realizar: firmar, vb
    private String getConfirmDialogMessage(final RequestType type){
    	if(type == RequestType.APPROVE) {
    		return getString(R.string.dialog_msg_confirm_approve);
    	}
    	return getString(R.string.dialog_msg_confirm_sign);
    }

    /** Metodo que define la accion a realizar al pulsar en el boton Reject
     * @param v Vista desde la que se invoco el metodo */
    public void onClickReject(final View v) {
    	if (this.reqDetails == null) {
    		return;
    	}

    	// Mostramos el dialogo de rechazo
    	setCustomAlertDialog(CustomAlertDialog.newInstance(
    			DIALOG_CONFIRM_REJECT,
    			getString(R.string.dialog_title_confirm_reject),
    			getString(R.string.dialog_msg_reject_request),
    			getString(android.R.string.ok),
    			getString(android.R.string.cancel),
    			this)
    	);

    	runOnUiThread(new Runnable() {
			@Override
			public void run() {
				getCustomAlertDialog().show(getSupportFragmentManager(), DIALOG_TAG);
			}
		});
    }

    private void processRequest(final RequestType type) {

    	if (type == RequestType.SIGNATURE) {
    		showProgressDialog(getString(R.string.dialog_msg_signing_1),null);
    		new LoadSelectedPrivateKeyTask(this.certAlias, this, this).execute();
    	} else {
    		showProgressDialog(getString(R.string.dialog_msg_approving_1),null);
    		approveRequest();
    	}
    }

    @Override
	public synchronized void keySelected(final KeySelectedEvent kse) {

		final PrivateKeyEntry pke;
		try {
			pke = kse.getPrivateKeyEntry();
		}
		catch (final KeyChainException e) {
			Log.e(SFConstants.LOG_TAG, "Error al recuperar la clave privada seleccionada: " + e); //$NON-NLS-1$
			if ("4.1.1".equals(Build.VERSION.RELEASE) || "4.1.0".equals(Build.VERSION.RELEASE) || "4.1".equals(Build.VERSION.RELEASE)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				showToastMessage(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE_ANDROID_4_1));
				closeActivity();
			}
			else {
				showToastMessage(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE));
			}
			return;
		}
		catch (final KeyStoreException e) {
			Log.e(SFConstants.LOG_TAG, "El usuario no selecciono un certificado: " + e); //$NON-NLS-1$
			showToastMessage(ErrorManager.getErrorMessage(ErrorManager.ERROR_CANCELLED_OPERATION));
			return;
		}
		// Cuando se instala el certificado desde el dialogo de seleccion, Android da a elegir certificado
		// en 2 ocasiones y en la segunda se produce un "java.lang.AssertionError". Se ignorara este error.
		catch (final Throwable e) {
			Log.e(SFConstants.LOG_TAG, "Error desconocido en la seleccion del certificado: " + e.toString()); //$NON-NLS-1$
			showToastMessage(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE));
			return;
		}

		doSign(pke.getPrivateKey(), (X509Certificate[]) pke.getCertificateChain());
	}

	private void doSign(final PrivateKey pk, final X509Certificate[] certChain) {
		new SignRequestTask(
				this.reqDetails, pk, certChain, CommManager.getInstance(), this).execute();
	}

    private void rejectRequest(final String reason) {
    	new RejectRequestsTask(
    			this.reqDetails.getId(), this.certB64, CommManager.getInstance(), this, reason).execute();
    }

    private void approveRequest() {
    	new ApproveRequestsTask(
    			this.reqDetails.getId(), this.certB64, CommManager.getInstance(), this).execute();
    }

	@Override
	public void requestOperationFinished(final int operation, final RequestResult requestResult) {

		dismissProgressDialog();

		if (requestResult.isStatusOk()) {
			setResult(operation == OperationRequestListener.REJECT_OPERATION ?
					PetitionDetailsActivity.RESULT_REJECT_OK:
						PetitionDetailsActivity.RESULT_SIGN_OK);
			closeActivity();
		}
		else {
			Log.e(SFConstants.LOG_TAG, "Ha fallado la operacion"); //$NON-NLS-1$

			final int msgId = operation == OperationRequestListener.REJECT_OPERATION ?
					R.string.error_msg_rejecting_request :
						R.string.error_msg_procesing_request;
			showToastMessage(getString(msgId));
		}
	}

	@Override
	public void requestOperationFailed(final int operation, final RequestResult requests, final Throwable t) {

		dismissProgressDialog();

		Log.e(SFConstants.LOG_TAG, "Ha fallado la operacion con la excepcion: " + t); //$NON-NLS-1$
		if (t != null) {
			t.printStackTrace();
		}

		final int msgId = operation == OperationRequestListener.REJECT_OPERATION ?
				R.string.error_msg_rejecting_request :
					R.string.error_msg_procesing_request;
		showToastMessage(getString(msgId));
	}

	/**
	 * Muestra un mensaje en un toast.
	 * @param message
	 */
	void showToastMessage(final String message) {

		final Toast toast = Toast.makeText(this, message, Toast.LENGTH_LONG);
		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				toast.show();
			}
		});
	}

	/** Cierra el activity liberando recursos. */
	void closeActivity() {
		this.reqDetails = null;
		finish();
	}

	/** Cierra el di&aacute;logo de espera en caso de estar abierto. */
	void dismissProgressDialog() {
		if (getProgressDialog() != null) {
			runOnUiThread(new Runnable() {
				@Override
				public void run() {
					getProgressDialog().dismiss();
				}
			});
		}
	}
	/** Muestra un di&aacute;logo de espera con un mensaje. */
	private void showProgressDialog(final String message, final LoadPetitionDetailsTask lpdt) {
		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				try {
					setProgressDialog(ProgressDialog.show(PetitionDetailsActivity.this, null, message, true));
					if(lpdt != null){
						getProgressDialog().setOnKeyListener(new OnKeyListener() {
							@Override
							public boolean onKey(final DialogInterface dlg, final int keyCode, final KeyEvent event) {
								if (keyCode == KeyEvent.KEYCODE_BACK) {
									lpdt.cancel(true);
									dismissProgressDialog();
									closeActivity();
									return true;
								}
								return false;
							}
						});
					}
				} catch (final Exception e) {
					Log.e(SFConstants.LOG_TAG, "No se ha podido mostrar el dialogo de progreso: " + e); //$NON-NLS-1$
				}
			}
		});
	}

	/** Muestra un di&aacute;logo de espera con un mensaje. */
	void showProgressDialogDownloadFile(final String message, final DownloadFileTask dlfTask) {
		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				try {
					setProgressDialog(ProgressDialog.show(PetitionDetailsActivity.this, null, message, true));
					getProgressDialog().setOnKeyListener(new OnKeyListener() {

						@Override
						public boolean onKey(final DialogInterface dlg, final int keyCode, final KeyEvent event) {
							if (keyCode == KeyEvent.KEYCODE_BACK) {
								//Stop task, al presionar el boton volver del dialogo paramos la descarga del fichero
								dlfTask.cancel(true);
								dismissProgressDialog();
								return true;
							}
							return false;
						}
					});
				} catch (final Exception e) {
					Log.e(SFConstants.LOG_TAG, "No se ha podido mostrar el dialogo de progreso: " + e); //$NON-NLS-1$
				}
			}
		});
	}

	@Override
	public void onDialogPositiveClick(final int dialogId, final String reason) {
		if(dialogId == DIALOG_CONFIRM_OPERATION) {
			processRequest(this.reqDetails.getType());
		}
		// Dialogo de confirmacion de rechazo de peticiones
		else  if (dialogId == DIALOG_CONFIRM_REJECT) {
			getCustomAlertDialog().dismiss();
			showProgressDialog(getString(R.string.dialog_msg_rejecting_1), null);
			rejectRequest(reason);
		}
	}

	@Override
	public void onDialogNegativeClick(final int dialogId) {
		// No hacemos nada
	}

}