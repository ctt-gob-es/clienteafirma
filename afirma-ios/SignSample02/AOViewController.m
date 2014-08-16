//
//  AOViewController.m
//  SignSample02
//
//

#import <QuartzCore/QuartzCore.h>
#import "AOViewController.h"
#import "NSData+Base64.h"
#import "CADESSignUtils.h"
#import "CADESConstants.h"
#import "AlertProgressBar.h"

@interface AOViewController ()

@end

@implementation AOViewController

@synthesize signButton;
@synthesize pinButton;
@synthesize signDone;
@synthesize pinTextField;
@synthesize selectionButton;
@synthesize nombreCert = _nombreCert;
@synthesize tblView;
@synthesize nombreAlmacen = _nombreAlmacen;

// Fuente de datos para el PickerView
NSArray *pickerViewArray;

// Pin del llavero, para re-comprobarlo sin tener que re-cargar el PKCS#12
NSString *keychainPin = NULL;

// Certificado X.509
NSString *base64UrlSafeCertificateData = NULL;

//Datos globales que se usarán para la invocación a los distintos servlets
NSString *operation       = NULL;
NSString *datosInUse      = NULL;
NSString *signAlgoInUse   = NULL;
NSString *docId           = NULL;
NSString *cipherKey       = NULL;
NSString *urlServlet      = NULL;
NSString *signFormat      = NULL;
NSString *extraParams     = NULL;
NSString *triphasicServerURL = NULL;
NSString *fileId          = NULL;
NSString *rtServlet       =NULL;

// Clave privada RSA
SecKeyRef privateKey = NULL;
 
// URL de entrada a la aplicacion
NSString *startUrl = NULL;

NSString *certificateName = NULL;

AlertProgressBar *alertpb = NULL;

NSMutableArray *tableData = NULL;
bool tableLoaded = NO;

NSString *cellSelected = NULL;

NSMutableData *receivedData = NULL;

NSString *receivedString = NULL;

/************************************************/
/******** EVENTOS PRINCIPALES DE LA APP *********/
/************************************************/

//Primer método que se invoca cuando se carga la pantalla del segue
- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

//Este método se carga cada vez que se carga una pantalla en la app.
-(void)viewDidLoad {
    [super viewDidLoad];
    
    [[self navigationController] setNavigationBarHidden:YES animated:YES];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(onReadUrl:) name:@"urlReaded" object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(onGoingToBackGround:) name:UIApplicationDidEnterBackgroundNotification object:nil];
    
    //tabla de certificados
    if(tableLoaded){
        selectionButton.userInteractionEnabled=YES;
    }
    else{
        selectionButton.userInteractionEnabled=NO;
    }
    
    //definimos los bordes de la tabla.
    self.tblView.layer.borderWidth = 0.5;
    self.tblView.layer.borderColor = [[UIColor grayColor] CGColor];
    self.tblView.layer.cornerRadius = 6.0f;
    
    //definimos la seleccion de la tabla
    if([tableData count]>0){
        //ponemos el primer elemento seleccionado
        NSIndexPath *indexPath=[NSIndexPath indexPathForRow:0 inSection:0];
        [tblView selectRowAtIndexPath:indexPath animated:YES  scrollPosition:UITableViewScrollPositionBottom];
    }
    
    //etiqueta con el nombre del almacen seleccionado
    NSString *etiqueta = @"Por favor, introduzca la contraseña del almacén de claves ";
    if(cellSelected!=NULL){
        etiqueta = [etiqueta stringByAppendingString:@"'"];
        etiqueta = [etiqueta stringByAppendingString:cellSelected];
        etiqueta = [etiqueta stringByAppendingString:@"'"];
    }
    self.nombreAlmacen.text = etiqueta;
    
    //Establecemos el nombre del certificado del almacen
    self.nombreCert.text = certificateName;
}

-(BOOL) checkPin:(NSString*)pin {
    OSStatus status = [self openPkcs12Store:pin];
   
    if (status == 0) {
        [pinTextField setText:nil];
        return YES;
    }
    if (status == -25293) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"PIN incorrecto" message:@"La contraseña del almacén de claves es incorrecta" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
        [pinTextField setText:nil];
        
    }
    else {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"No se ha podido cargar el almacén de claves" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        NSLog(@"Error al cargar el almacen de claves: %ld", status);
        [alert show];
        [pinTextField setText:nil];
    }
    
    return NO;
}


-(OSStatus) openPkcs12Store:(NSString*)pin {
    
    // Cargamos el PKCS#12 desde como un recurso
    NSString *thePath = NULL;
#if TARGET_IPHONE_SIMULATOR

    // Cargamos el PKCS#12 desde como un recurso
    thePath = [[NSBundle mainBundle] pathForResource:@"ANF_PF_Activo" ofType:@"p12"];
    
#else

    // Cargamos el PKCS#12 con el nombre de la celda seleccionada.
    thePath = [self loadFileFromDocumentsFolder:cellSelected];
    
#endif
    
    NSData *PKCS12Data = [[NSData alloc] initWithContentsOfFile:thePath];
    CFDataRef inPKCS12Data = (CFDataRef) CFBridgingRetain(PKCS12Data);
    
    if (inPKCS12Data == NULL) {
        return 255;
    }
    
    OSStatus status = noErr;
	SecIdentityRef myIdentity;
	SecTrustRef myTrust;
    
	status = [CADESSignUtils extractIdentityAndTrust:inPKCS12Data :pin :&myIdentity :&myTrust];
	
	if (status != 0) {
        return status;
	}
    
    SecCertificateRef myReturnedCertificate = NULL;
    status = SecIdentityCopyCertificate (myIdentity, &myReturnedCertificate);
    
    if (status != 0){
        return status;
    }
    
    base64UrlSafeCertificateData = [CADESSignUtils encodeBase64:(NSData*) CFBridgingRelease(SecCertificateCopyData(myReturnedCertificate))];
    //NSLog(@"Certificado en Base64: %@", base64UrlSafeCertificateData);
    
    status = SecIdentityCopyPrivateKey(myIdentity, &privateKey);
    
    if (status != 0){
        return status;
    }
    
    CFStringRef certSummary = SecCertificateCopySubjectSummary(myReturnedCertificate);
    NSString* summaryString = [[NSString alloc] initWithString:(NSString*)certSummary];
    
    certificateName = summaryString;
    
    NSArray *arrayToLoadPicker = [[NSArray alloc] initWithObjects:summaryString, nil];
    pickerViewArray = arrayToLoadPicker;
    
    keychainPin = pin;
    
    return status;
    
}

/***************************************************/
/***** EVENTOS QUE LANZAN TRANSICIONES (SEGUES)*****/
/***************************************************/


//evento que se lanza 
-(BOOL)textFieldShouldReturn:(UITextField*)theTextField {
    [pinTextField resignFirstResponder];
    if ([self checkPin:pinTextField.text]) {
        [signButton setTitle:@"Firmar" forState:UIControlStateNormal];
        [self performSegueWithIdentifier:@"pinEntered" sender:self];
    }
    return YES;
}

-(void) onReadUrl:(NSNotification*) notification {
    NSString *url = [notification object];
    startUrl = url;
    signDone = false;
    signButton.userInteractionEnabled = NO;
    
    //Rellenamos la tabla de certificados con los facilitados por iTunes y si hay almacenes, habilitamos el botón de seleccion.
    [self listFilesFromDocumentsFolder];
    
    //Nos desplazamos a la pantalla de seleccion de pkcs12.
    @try {
        [self performSegueWithIdentifier:@"toSelectionScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
        NSLog(@"no se ha podido lanzar el segue %@",e);
    }
}

//cuando se pulsa el botón del centro del teléfono
-(void)onGoingToBackGround:(NSNotification*) notification {
    @try {
        cellSelected=NULL;
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
    }
}

//Se invoca cuando se realiza cualquier accion
- (BOOL)shouldPerformSegueWithIdentifier:(NSString *)identifier sender:(id)sender {
    
    //antes de pasar a la pantalla de firma, comprobamos que se introducido correctamente el pin.
    if([identifier isEqualToString:@"pinEntered"])
        return [self checkPin:pinTextField.text];
    
    //comprobamos que antes de pasar a la pantalla de introducir un pin, se ha seleccionado un almacen de certificados.
    else if([identifier isEqualToString:@"toPinScreen"]){
        if(cellSelected !=NULL){
            return YES;
        }
        else{
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"Por favor, seleccione un almacen de claves y certificados" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
            [alert show];
            return NO;
        }
        
    //en cualquier otra pantalla, continuamos con el flujo solicitado.
    }else
        return YES;
}


/***************************/
/******** ACCIONES *********/
/***************************/

// Evento de teclear el el campo de texto del PIN
-(IBAction)pinButtonPressed:(id)sender {
    pinTextField = sender;
}

-(IBAction)buttonPressed:(id)sender {
    if (signDone) {
        exit(EXIT_SUCCESS);
    }
    else {
        [self startSignatureProcess:startUrl];
    }
}


/*****************************************************************/
/******** METODOS DE LA PROPIA FIRMA Y ENVIO AL SERVIDOR *********/
/*****************************************************************/

-(void) startSignatureProcess:(NSString*) url {
    NSDictionary *urlParameters = [CADESSignUtils parseUrl:url];
    //NSLog(@"URL analizada: %@", url);
    
    //parámetro de operacion "op"
    if([urlParameters objectForKey:PARAMETER_NAME_OPERATION]!=NULL)
        operation  = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_OPERATION]];
    
    //parámetro de la clave de cifrado con el servidor "key"
    if([urlParameters objectForKey:PARAMETER_NAME_CIPHER_KEY]!=NULL)
        cipherKey  = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_CIPHER_KEY]];
    
    //parámetro de identificacion "Id" del usuario
    if([urlParameters objectForKey:PARAMETER_NAME_ID])
        docId      = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_ID]];
    
    //parámetro del servlet donde se almacena la firma "servlet"
    if([urlParameters objectForKey:PARAMETER_NAME_STSERVLET]!=NULL)
        urlServlet = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_STSERVLET]];
        
    //parámetro donde se recogen los datos originales. El documento llega dentro del parámetro "dat"
    if([urlParameters objectForKey:PARAMETER_NAME_DAT] !=NULL)
        datosInUse = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_DAT]];
    
    //parámetro "format" que indica el formato de firma.
    if([urlParameters objectForKey:PARAMETER_NAME_FORMAT]!=NULL)
        signFormat = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_FORMAT]];
    
    //parámetro "algorithm" que indica el algoritmo usado para la firma. 
    if([urlParameters objectForKey:PARAMETER_NAME_ALGORITHM2] != NULL)
        signAlgoInUse  = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_ALGORITHM2]];
    
    //parámetro properties
    if([urlParameters objectForKey:PARAMETER_NAME_PROPERTIES] != NULL){
        extraParams = [urlParameters objectForKey:PARAMETER_NAME_PROPERTIES];
        if(extraParams!=NULL){
            //URL DECODE
            extraParams = [extraParams stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
            extraParams = [CADESSignUtils urlSafeDecode:extraParams];
            NSData *dataReceibed = [CADESSignUtils base64DecodeString: extraParams];
            NSString* stringDataReceibed = [NSString stringWithUTF8String:[dataReceibed bytes]];
            
            //Los datos recibidos son un properties de java y se convierten por tanto a un NSDictionary
            NSDictionary *dict = [ CADESSignUtils javaProperties2Dictionary:stringDataReceibed];
            
            //Se recoge la prefirma y se vuelve a decodificar, ya que esta viene a su vez codificada en base64.
            triphasicServerURL  = [dict objectForKey:PARAMETER_NAME_TRIPHASIC_SERVER_URL];
        }
    }
    
    if([urlParameters objectForKey:PARAMETER_NAME_FILE_ID]!=NULL)
        fileId = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_FILE_ID]];
    
    if([urlParameters objectForKey:PARAMETER_NAME_RTSERVLET]!=NULL)
        rtServlet = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_RTSERVLET]];
    

    NSLog(@"Operacion: %@", operation);
    NSLog(@"Documento: %@", docId);
    NSLog(@"Servlet: %@", urlServlet);
    NSLog(@"Datos: %@", datosInUse);
    NSLog(@"Formato: %@", signFormat);
    NSLog(@"Algoritmo: %@", signAlgoInUse);
    NSLog(@"Clave de cifrado: %@", cipherKey);
    NSLog(@"Propiedades: %@", extraParams);
    
    if (!([operation isEqualToString:OPERATION_SIGN]
        || [operation isEqualToString:OPERATION_COSIGN]
        || [operation isEqualToString:OPERATION_COUNTERSIGN])) {

        //Notificamos del error al servidor si es posible
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_UNSUPPORTED_OPERATION_NAME];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_UNSUPPORTED_OPERATION_NAME];
        
        //hay que hacer esta llamada asincrona!!!
        [self errorReportAsync:errorToSend];
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"Código de operación desconocido. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
        signDone = true;
        signButton.userInteractionEnabled = NO;
        return;
    }
    
    if (datosInUse == nil) {
        
        if(fileId == nil){
        
            //Notificamos del error al servidor si es posible
            NSString *errorToSend = @"";
            errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA];
            errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
            errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA];
        
            [self errorReportAsync:errorToSend];
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"No se han indicado datos para firmar. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
            [alert show];
            signDone = true;
            signButton.userInteractionEnabled = NO;
            return;
        }
        else
        {
            if(cipherKey!=NULL && rtServlet!=NULL)
            {
                [self loadDataFromRtservlet:fileId rtServlet:rtServlet];
            }
            else{
                //Notificamos del error al servidor si es posible
                NSString *errorToSend = @"";
                errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA];
                errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
                errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA];
                
                [self errorReportAsync:errorToSend];
                UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"No se han indicado datos para firmar. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
                [alert show];
                signDone = true;
                signButton.userInteractionEnabled = NO;
                return;
 
            }
        }
    }

    if (docId == nil) {
        
        //Notificamos del error al servidor si es posible
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA_ID];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA_ID];
        
        [self errorReportAsync:errorToSend];
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"No se ha indicado el identificador de los datos firmados. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
        signDone = true;
        signButton.userInteractionEnabled = NO;
        return;
    }
    
    if (urlServlet == nil) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"No se ha indicado la URL del servidor. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
        signDone = true;
        signButton.userInteractionEnabled = NO;
        return;
    }
    
    if (signFormat == nil) {
        
        //Notificamos del error al servidor si es posible
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_NOT_SUPPORTED_FORMAT];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_NOT_SUPPORTED_FORMAT];
        
        [self errorReportAsync:errorToSend];
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"No se ha indicado un formato de firma. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
        signDone = true;
        signButton.userInteractionEnabled = NO;
        return;
    }
    else if (!([signFormat isEqualToString:CADES_FORMAT]
               || [signFormat isEqualToString:PADES_FORMAT]
               || [signFormat isEqualToString:XADES_FORMAT])){
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"Formato no soportado" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
        signDone = true;
        //[signButton setTitle:@"Cerrar aplicación" forState:UIControlStateNormal];
        signButton.userInteractionEnabled = NO;
        return;

    }
    
    if(![CADESSignUtils isValidAlgorithm:signAlgoInUse]){
        
        //Notificamos del error al servidor si es posible
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_NOT_SUPPORTED_ALGORITHM];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_NOT_SUPPORTED_ALGORITHM];
        
        [self errorReportAsync:errorToSend];
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"El Algoritmo no es váLido o no está soportado. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
        signDone = true;
        signButton.userInteractionEnabled = NO;
        return;
    }
    
    //Invocamos el método de petición al servidor.  
    [self processBackground];
    
    signDone = true;
    
    signButton.userInteractionEnabled = NO;
    signButton.enabled=NO;
}



/**
 Método donde se realiza la petición asíncrona al servidor de firma.
 Se recogen los datos introducidos por url y se monta la peticiíon de tipo POST.
 
 parámetros:
 -----------
 
 */
-(void)processBackground{
    
    /* LLAMADA ASINCRONA */
    
    NSString *post =@"";
    post = [post stringByAppendingString:PARAMETER_NAME_OPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:OPERATION_PRESIGN];
    post = [post stringByAppendingString:HTTP_AND];
    
    post = [post stringByAppendingString:PARAMETER_NAME_COPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:operation];
    post = [post stringByAppendingString:HTTP_AND];
    
    post = [post stringByAppendingString:PARAMETER_NAME_DOCID];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:datosInUse];
    post = [post stringByAppendingString:HTTP_AND];
    
    post = [post stringByAppendingString:PARAMETER_NAME_FORMAT];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:signFormat];
    post = [post stringByAppendingString:HTTP_AND];
    
    post = [post stringByAppendingString:PARAMETER_NAME_ALGORITHM];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:signAlgoInUse];
    post = [post stringByAppendingString:HTTP_AND];
    
    post = [post stringByAppendingString:PARAMETER_NAME_CERT];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:[CADESSignUtils urlSafeEncode: base64UrlSafeCertificateData]];
    post = [post stringByAppendingString:HTTP_AND];

    /*De momento no enviamos parámetros extra en el "pre", pero lo dejamos preparado.*/
    if(extraParams !=NULL){
        post = [post stringByAppendingString:PARAMETER_NAME_EXTRA_PARAM];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:extraParams];
    }
    
    NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
    NSString *postLength = [NSString stringWithFormat:@"%d", [postData length]];
    
    // Obtenemos la URL de las preferencias    
    NSURL* requestUrl = NULL;
    if(triphasicServerURL!=NULL)
        requestUrl = [[NSURL alloc] initWithString:triphasicServerURL];
    else
        requestUrl = [[NSURL alloc] initWithString:[[NSUserDefaults standardUserDefaults] stringForKey:@"server_url"]];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:requestUrl cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30.0];
    [request setHTTPMethod:@"POST"];
    [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
    [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [request setValue:@"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" forHTTPHeaderField:@"User-Agent"];
    [request setValue:@"text/plain,text/html,application/xhtml+xml,application/xml" forHTTPHeaderField:@"Accept"];
    [request setHTTPBody:postData];
    
    NSLog(@"\n\n");
    NSLog(@"Inicio de la llamada a PRE con la siguiente URL: %@", post);
    NSLog(@"\n\n");
    
    NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
    [connection start];
    
    //iniciamos la barra de progreso.
    alertpb = [[AlertProgressBar alloc]init];
    [alertpb createProgressBar:self.view];
         
}

/* METODOS DONDE SE RECIBE LA RESPUESTA DE LA CONEXION ASINCRONA */
/**
 Método donde se recibe la respuesta de la petición asíncrona.
 
 parámetros:
 -----------
 didReceiveData: Conexión establecida asíncrona.
 data:           Datos recibidos del servidor.
 */
//los datos van llegando por "rafagas". Lo que hay que ir haciendo es ir juntandolos todos.
-(void)connection:(NSURLConnection *)connection didReceiveData:
(NSData *)data
{
    // Append the new data to the receivedData.
    [receivedData appendData:data];
}

//Se confirma la respuesta. aprovechamos para inicializar los datos de respuesta
-(void)connection:(NSURLConnection *)connection didReceiveResponse:
(NSURLResponse *)response
{
    // Discard all previously received data.
    receivedData = [[NSMutableData alloc] init];
    
}

//cuando se ha terminado de leer los datos recibidos, terminamos ya la conexion y se pasa a la prefirma.
-(void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    // Connection succeeded in downloading the request.
    NSLog( @"Final de la recepción! se han recibido %d bytes", [receivedData length] );
    
    // Convert received data into string.
    receivedString = [[NSString alloc] initWithData:receivedData
                                           encoding:NSASCIIStringEncoding];
    NSLog( @"invocación desde connectionDidFinishLoading: %@", receivedString );

    NSString *dataReceibedb64 = [CADESSignUtils urlSafeDecode:receivedString];
    [self preSign:dataReceibedb64];
    
    // release the connection, and the data object
    [connection release];
    [receivedData release];
}

/**
 Método donde se procesan los errores de conexión con el servidor.
 
 parámetros:
 -----------
 connection:        Conexión establecida asíncrona.
 didFailWithError:  Error producido.
 */
- (void)connection:(NSURLConnection *)connection
  didFailWithError:(NSError *)error
{
    // Liberar la conexión
    [connection release];
    
    //Quitamos la notificación de la pila de notificaciones. Si se produce algun error, no quedará en la pila la notificación y por lo tanto, cuando se vuelva a ejecutar, no se ejecutará n-veces las llamadas que hay en la pila de notificacioens.
    NSNotificationCenter *notificationCenter = [NSNotificationCenter defaultCenter];
    [notificationCenter removeObserver:self name:@"eventType" object:nil];
    
    NSLog(@"Error - %@ %@",
          [error localizedDescription],
          [[error userInfo] objectForKey:NSURLErrorFailingURLStringErrorKey]);

    //destruimos la barra de progreso
    [alertpb destroy];
    
    //Notificamos del error al servidor
    NSString *errorToSend = @"";
    errorToSend = [errorToSend stringByAppendingString:ERROR_SIGNING];
    errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
    errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_SIGNING];

    [self errorReport:errorToSend];
    
    //mostramos un mensaje con el error producido.
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"No se ha podido establecer la conexión con el servidor. Por favor, vuelva de nuevo al navegador donde inició la operación e intentelo de nuevo." delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
    [alert show];
    signDone = true;
    //[signButton setTitle:@"Cerrar aplicación" forState:UIControlStateNormal];
    signButton.userInteractionEnabled = NO;
    signButton.enabled=NO;
    return;    
}

/**
 Método que trata los datos recibidos del servidor en base64.
 Una vez tratados los datos, se ejecuta la firma y se invoca la llamada para la ejecución del post.
 
 parámetros:
 -----------
 dataReceibedb64: Cadena recibida del servidor y que contiene un properties de java.
 
 */
-(void) preSign: (NSString*) dataReceibedb64{
   
    //Se reciben los datos en base64 y se decodifican
    NSData *dataReceibed = [CADESSignUtils base64DecodeString: dataReceibedb64];
    NSString* stringDataReceibed = [NSString stringWithUTF8String:[dataReceibed bytes]];
        
    //NSLog(@"Datos recibidos en respuesta a la prefirma (properties): %@", stringDataReceibed);
    
    //Los datos recibidos son un properties de java y se convierten por tanto a un NSDictionary
    NSDictionary *dict = [ CADESSignUtils javaProperties2Dictionary:stringDataReceibed];
        
    //Se recoge la prefirma y se vuelve a decodificar, ya que esta viene a su vez codificada en base64.
    NSString *operation  = [dict objectForKey:@"PRE"];
    NSString *session  = [dict objectForKey:@"SESSION"];
    bool isRecibedOk = false;
    //comprobamos que los datos del servidor han llegado correctamente
    if (operation!=NULL && session!=NULL){
        isRecibedOk = true;
    }
    
    if(isRecibedOk){
        NSString *preSignb64 = [CADESSignUtils urlSafeDecode:operation];
        preSignb64 = [preSignb64 stringByReplacingOccurrencesOfString:@"\\" withString:@""] ;
        
        NSData *dataPreSign = [CADESSignUtils base64DecodeString:preSignb64];
        
        if(dataPreSign.length>0){
            NSLog(@"Se pasa a realizar la firma PKCS1");
            //Con los datos de la prefirma decodificados, se procede a realizar la firma pkcs1.
            NSData *dataSigned = [CADESSignUtils signPkcs1:signAlgoInUse:&privateKey:dataPreSign];
            
            //Creamos el Diccionario con los datos necesarios para la postfirma y que los vamos a pasar mediante un evento.
            NSMutableDictionary *data = [NSMutableDictionary dictionaryWithCapacity:2];
            [data setObject:dataSigned forKey:@"data"];
            
            //NSLog(@"SESSION: %@", session);
            [data setObject:session forKey:@"session"];
            
            
            //Por último, se lanza la notificación del evento para que se realice la llamada al servidor de postfirma pasándole como parámetros el diccionario que contiene la firma pkcs1 y la prefirma.
            NSNotificationCenter *notificationCenter = [NSNotificationCenter defaultCenter];
            [notificationCenter postNotificationName:@"eventType"
                                              object:nil
                                            userInfo: data];
            
            [notificationCenter removeObserver:self name:@"eventType" object:nil];
        }
        else{
            isRecibedOk = false;
        }
    }
    
    if (!isRecibedOk){
        NSLog(@"No hay datos para realizar la firma!!!");
        //destruimos la barra de progreso
        [alertpb destroy];
        
        //Quitamos la notificación de la pila de notificaciones. Si se produce algun error, no quedará en la pila la notificación y por lo tanto, cuando se vuelva a ejecutar, no se ejecutará n-veces las llamadas que hay en la pila de notificacioens.
        NSNotificationCenter *notificationCenter = [NSNotificationCenter defaultCenter];
        [notificationCenter removeObserver:self name:@"eventType" object:nil];
        
        //Notificamos del error al servidor
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_SIGNING];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_SIGNING];
        
        [self errorReport:errorToSend];
        
        //mostramos un mensaje con el error producido.
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"Se ha producido un error al generar la firma. Por favor, vuelva a la página desde donde inició la operación." delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
    }
    
}

/**
 Método que realiza la operacion "post" de la firma. Invoca al servidor de nuevo haciendo una peticion POST.
 Después, se procesa el resultado dependiendo si la respuesta del servidor es correcta o errónea.
 
 parámetros:
 -----------
 notification: Notificación recibida por el manejador de eventos.
 
 */
-(void)eventHandler: (NSNotification *) notification
{
    //Quitamos la notificación de la pila de notificaciones. Si se produce algun error, no quedará en la pila la notificación y por lo tanto, cuando se vuelva a ejecutar, no se ejecutará n-veces las llamadas que hay en la pila de notificacioens.
    NSNotificationCenter *notificationCenter = [NSNotificationCenter defaultCenter];
    [notificationCenter removeObserver:self name:@"eventType" object:nil];
    
    //obtenemos del manejador de enventos los parametros de la firma pkcs1 y de la prefirma.
    NSDictionary *dict = [notification userInfo];
    NSData *dataSign = [dict objectForKey:@"data"];
    NSString *session  = [dict objectForKey:@"session"];
    //quitamos las "\", ya que al final del base 64 viene de la forma "....bGdl\=\=" (Corregido en servidor).
    //presign =[presign stringByReplacingOccurrencesOfString:@"\\" withString:@""];
    //Quitamos los espacios en blanco. el famoso .trim()
    session = [session stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    //codificamos los datos de la firma.
    NSString *dataSignString = [CADESSignUtils encodeBase64:dataSign];
    dataSignString = [dataSignString stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    
    //Creamos el NSMutableDictionary que equivaldrá al properties de java.
    NSMutableDictionary *dictParameters = [NSMutableDictionary dictionaryWithCapacity:2];
    [dictParameters setObject:dataSignString forKey:@"PK1"];
    [dictParameters setObject:session forKey:@"SESSION"];
    
    //NSLog(@"Valor PK1 enviado a la PostFirma: %@", dataSignString);
    //NSLog(@"Valor SESSION enviado a la PostFirma: %@", session);
    
    //Se pasa a formato properties codificado en b64
    NSString *paramsEncoded = [CADESSignUtils dictionary2JavaProperties:dictParameters];
    paramsEncoded = [paramsEncoded stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    
    //Creamos la cadena de envío al servidor POST
    NSString *post =@"";
    post = [post stringByAppendingString:PARAMETER_NAME_OPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:OPERATION_POSTSIGN];
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PARAMETER_NAME_COPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:operation];
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PARAMETER_NAME_DOCID];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:datosInUse];
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PARAMETER_NAME_FORMAT];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:signFormat];
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PARAMETER_NAME_ALGORITHM];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:signAlgoInUse];
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PARAMETER_NAME_CERT];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:[CADESSignUtils urlSafeEncode: base64UrlSafeCertificateData]];
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PARAMETER_NAME_EXTRA_PARAM];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:paramsEncoded];
    
    //Codificamos la url de post
    NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
    NSString *postLength = [NSString stringWithFormat:@"%d", [postData length]];
    
    // Obtenemos la URL del servidor de la pantalla de preferencias
    NSURL* requestUrl = NULL;
    if(triphasicServerURL!=NULL)
        requestUrl = [[NSURL alloc] initWithString:triphasicServerURL];
    else
        requestUrl = [[NSURL alloc] initWithString:[[NSUserDefaults standardUserDefaults] stringForKey:@"server_url"]];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:requestUrl cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30.0];
    [request setHTTPMethod:@"POST"];
    [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
    [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [request setValue:@"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" forHTTPHeaderField:@"User-Agent"];
    [request setValue:@"text/plain,text/html,application/xhtml+xml,application/xml" forHTTPHeaderField:@"Accept"];
    [request setHTTPBody:postData];
    
    NSLog(@"\n\n");
    NSLog(@"Se realiza la llamada a POST. La url es: %@", post);
    NSLog(@"\n\n");
    
    //realizamos la llamada al servidor.
    NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:nil error:nil];
    
    //Obtenemos la respuesta del servidor.
    NSString* responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];

    NSLog(@"La invocación a POST ha devuelto la siguiente respuesta: %@", responseString);

    //se valida si la respuesta es correcta
    if([responseString hasPrefix:@"OK"]){
        NSLog(@"se preparan los datos para realizar el storage.");
        NSRange range = [responseString rangeOfString:@"="];
        if(range.length>0){
            NSString *parte2 = [responseString substringFromIndex:range.location+1];//le sumamos 1 para que no coja el "="

            //invocamos al almacenamiento de la firma
            [self storeData:parte2];
        }
        
    }
    else{
        NSLog(@"La respuesta no es correcta. Se informa al usuario y se para el proceso");
        //destruimos la barra de progreso
        [alertpb destroy];
        
        //Quitamos la notificación de la pila de notificaciones. Si se produce algun error, no quedará en la pila la notificación y por lo tanto, cuando se vuelva a ejecutar, no se ejecutará n-veces las llamadas que hay en la pila de notificacioens.
        NSNotificationCenter *notificationCenter = [NSNotificationCenter defaultCenter];
        [notificationCenter removeObserver:self name:@"eventType" object:nil];
        
        //Notificamos del error al servidor
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_SIGNING];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_SIGNING];
        
        [self errorReport:errorToSend];
        
        //Se muestra el mensaje de respuesta al usuario.
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Proceso finalizado" message:@"Se ha producido un error en el proceso de firma. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde" delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
    }
    
}


/**
 Método que realiza la operacion "put" de la firma en el servidro de almacenamiento. La invocación al servidor es mediante una petición POST.
 
 parámetros:
 -----------
 dataSign: firma que será almacenada en el servidor.
 
 */
-(void)storeData:(NSString*) dataSign
{
    
    //Creamos la cadena de envío al servidor POST
    NSString *post =@"";
    post = [post stringByAppendingString:PARAMETER_NAME_OPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:OPERATION_PUT];
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PARAMETER_NAME_VERSION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:PARAMETER_NAME_VERSION_1_0];
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PARAMETER_NAME_ID];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:docId];
    post = [post stringByAppendingString:HTTP_AND];
    if(cipherKey!=NULL){

        //cifrado de la firma
        NSLog(@"FIRMA RESULTADO: %@", [CADESSignUtils urlSafeEncode:dataSign]);
        NSString *dataDecoded = [CADESSignUtils urlSafeDecode:dataSign];
        NSData *data = [CADESSignUtils base64DecodeString: dataDecoded];
        NSData *encryptedData = [CADESSignUtils DesEncrypt:cipherKey : data];
        NSString *datab64 = [CADESSignUtils encodeBase64:encryptedData];
        dataSign= [CADESSignUtils urlSafeEncode:datab64];
        post = [post stringByAppendingString:PARAMETER_NAME_CIPHER_KEY];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:cipherKey];
        post = [post stringByAppendingString:HTTP_AND];
    }
    
    //firma en base64
    post = [post stringByAppendingString:PARAMETER_NAME_DAT];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:dataSign];
    
    //Codificamos la url de post
    NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
    NSString *postLength = [NSString stringWithFormat:@"%d", [postData length]];
    
    // Obtenemos la URL del servidor de la pantalla de preferencias
    NSURL* requestUrl = [[NSURL alloc] initWithString:urlServlet];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:requestUrl cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30.0];
    [request setHTTPMethod:@"POST"];
    [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
    [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [request setValue:@"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" forHTTPHeaderField:@"User-Agent"];
    [request setValue:@"text/plain,text/html,application/xhtml+xml,application/xml" forHTTPHeaderField:@"Accept"];
    [request setHTTPBody:postData];
    
    //NSLog(@"\n\n");
    //NSLog(@"Realizamos el storage de la firma con los siguientes parámetros: %@", post);
    //NSLog(@"\n\n");
    
    
    //realizamos la llamada al servidor.
    NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:nil error:nil];
    
    //Obtenemos la respuesta del servidor.
    NSString* responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    
    NSLog(@"Respuesta del storage: %@", responseString);
    //quitamos el progressbar indefinido
    [alertpb destroy];
    
    //Respuesta que se mostrará al usuario. Se pone por omisión el mensaje de fallo.
    NSString *mensajeRespuesta = @"Se ha producido un error en el proceso de firma. Por favor, vuelva de nuevo al navegador donde inició la operación e inténtelo de nuevo más tarde";
    
    //se procesa la respuesta del servidor.
    if([responseString hasPrefix:@"OK"]){
        mensajeRespuesta=@"Se ha completado el proceso de firma. Por favor, vuelva al navegador desde el que inició la operación para continuar";
    }
    
    //Se muestra el mensaje de respuesta al usuario.
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Proceso finalizado" message:mensajeRespuesta delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
    [alert show];
    
}

/**
 Método que obtiene los datos de trabajo desde el servidor intermedio.
 
 parámetros:
 -----------
 fileId: Identificador del fichero de datos.
 rtServlet: Dirección del servidor intermedio.
 
 */
-(NSString *) loadDataFromRtservlet:(NSString*) fileId rtServlet:(NSString *)rtServlet
{
    
    NSString* responseString = NULL;
    
    if(urlServlet!=NULL && docId != NULL){
        //Creamos la cadena de envío al servidor POST
        NSString *post =@"";
        post = [post stringByAppendingString:PARAMETER_NAME_OPERATION];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:OPERATION_GET];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_VERSION];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:PARAMETER_NAME_VERSION_1_0];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_ID];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:fileId];
        
        //Codificamos la url de post
        NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
        NSString *postLength = [NSString stringWithFormat:@"%d", [postData length]];
        
        // Obtenemos la URL del servidor de la pantalla de preferencias
        NSURL* requestUrl = [[NSURL alloc] initWithString:rtServlet];
        NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:requestUrl cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30.0];
        [request setHTTPMethod:@"POST"];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
        [request setValue:@"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" forHTTPHeaderField:@"User-Agent"];
        [request setValue:@"text/plain,text/html,application/xhtml+xml,application/xml" forHTTPHeaderField:@"Accept"];
        [request setHTTPBody:postData];
        
        NSLog(@"Se recogen los datos del fichero del rtServlet con los siguientes datos: %@", post);
        
        //realizamos la llamada al servidor.
        NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:nil error:nil];
        
        //Obtenemos la respuesta del servidor.
        NSString* responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        
        NSLog(@"Respuesta del rtserver: %@", responseString);

        @try {
            NSString *base64 = [responseString substringToIndex:3];
            NSData *encoded = [CADESSignUtils base64DecodeString: base64];
            NSData *decoded = NULL;
            
            decoded = [CADESSignUtils DesDecrypt: cipherKey:encoded];
            
            //deshacemos el padding especial
            NSData *finalDecoded = [[NSData alloc] init];
            NSRange range = NSMakeRange(0,(decoded.length - 8) - [[responseString substringToIndex:1] intValue]);
            
            [decoded getBytes:finalDecoded range:range];
            
            datosInUse = [CADESSignUtils encodeBase64:finalDecoded];
        }
        @catch (NSException *exception) {
            NSLog(@"Se ha producido un error al obtener el fichero: %@", exception.description );
        }
        @finally {
            
        }
    }
    
    return responseString;
    
    
}



/**
 Método que notifica de un error en la aplicación al servidor de guardado de firmas "storage" de forma síncrona.
 
 parámetros:
 -----------
 dataSign: error producido.
 
 */
-(NSString*)errorReport:(NSString*) error
{
    NSString* responseString = NULL;
    
    if(urlServlet!=NULL && docId != NULL){
        //Creamos la cadena de envío al servidor POST
        NSString *post =@"";
        post = [post stringByAppendingString:PARAMETER_NAME_OPERATION];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:OPERATION_PUT];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_VERSION];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:PARAMETER_NAME_VERSION_1_0];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_ID];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:docId];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_DAT];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:error];
        
        //Codificamos la url de post
        NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
        NSString *postLength = [NSString stringWithFormat:@"%d", [postData length]];
        
        // Obtenemos la URL del servidor de la pantalla de preferencias
        NSURL* requestUrl = [[NSURL alloc] initWithString:urlServlet];
        NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:requestUrl cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30.0];
        [request setHTTPMethod:@"POST"];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
        [request setValue:@"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" forHTTPHeaderField:@"User-Agent"];
        [request setValue:@"text/plain,text/html,application/xhtml+xml,application/xml" forHTTPHeaderField:@"Accept"];
        [request setHTTPBody:postData];
        
        NSLog(@"Se ha producido un error. informamos al servidor de storage con los siguietnes parámetros: %@", post);
        
        //realizamos la llamada al servidor.
        NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:nil error:nil];
        
        //Obtenemos la respuesta del servidor.
        responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        
    }
    [alertpb destroy];
    
    return responseString;
}


/**
 Método que notifica de un error en la aplicación al servidor de guardado de firmas "storage" de forma síncrona.
 
 parámetros:
 -----------
 dataSign: error producido.
 
 */
-(NSString *) errorReportAsync:(NSString*) error
{
    
    NSString* responseString = NULL;
    
    if(urlServlet!=NULL && docId != NULL){
        //Creamos la cadena de envío al servidor POST
        NSString *post =@"";
        post = [post stringByAppendingString:PARAMETER_NAME_OPERATION];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:OPERATION_PUT];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_VERSION];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:PARAMETER_NAME_VERSION_1_0];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_ID];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:docId];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_DAT];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:error];
        
        //Codificamos la url de post
        NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
        NSString *postLength = [NSString stringWithFormat:@"%d", [postData length]];
        
        // Obtenemos la URL del servidor de la pantalla de preferencias
        NSURL* requestUrl = [[NSURL alloc] initWithString:urlServlet];
        NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:requestUrl cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30.0];
        [request setHTTPMethod:@"POST"];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
        [request setValue:@"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" forHTTPHeaderField:@"User-Agent"];
        [request setValue:@"text/plain,text/html,application/xhtml+xml,application/xml" forHTTPHeaderField:@"Accept"];
        [request setHTTPBody:postData];
        
        NSLog(@"Se ha producido un error. informamos al servidor de storage con los siguietnes parámetros: %@", post);
        
        //realizamos la llamada al servidor.
        //NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:nil error:nil];
        [NSURLConnection
         sendAsynchronousRequest:request
         queue:[[NSOperationQueue alloc] init]
         completionHandler:^(NSURLResponse *response,
                             NSData *data,
                             NSError *error)
         {
             
             if ([data length] >0 && error == nil)
             {
                 
                 // DO YOUR WORK HERE
                 //Obtenemos la respuesta del servidor.
                 //responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
                 NSLog(@"Respuesta del servidor: %@",[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]);
                 
             }
             else if ([data length] == 0 && error == nil)
             {
                 NSLog(@"No hay respuesta del servidor.");
             }
             else if (error != nil){
                 NSLog(@"Error = %@", error);
             }
             
         }];
                
    }
    [alertpb destroy];
    
    return responseString;
}

/****************************************/
/***** GESTION DE LA CARPETA ITUNES *****/
/****************************************/

//Carga carga el almacen de certificados del directorio iTunes.
-(NSString*)loadFileFromDocumentsFolder:(NSString *) filename {
    // Se obtiene el almacen de la carpeta de documentos.
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    return [documentsDirectory stringByAppendingPathComponent:filename];
    
}

//Carga en la lista de almacenes los almacenes encontrados en Itunes.
-(void)listFilesFromDocumentsFolder {
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    
    NSFileManager *manager = [NSFileManager defaultManager];
    NSArray *fileList = [manager contentsOfDirectoryAtPath:documentsDirectory error:nil];
    
#if TARGET_IPHONE_SIMULATOR
    //habilitamos el boton de seleccion de certificado.
    tableLoaded = YES;
    tableData = [[NSMutableArray alloc] init];
    
    [tableData addObject:@"ANF USUARIO ACTIVO"];
    [tableData addObject:@"ANF USUARIO ACTIVO"];
    [tableData addObject:@"ANF USUARIO ACTIVO"];
    [tableData addObject:@"ANF USUARIO ACTIVO"];
     
    
#else
    if([fileList count]>0){
        //habilitamos el boton de seleccion de certificado.
        tableLoaded = YES;
        tableData = [[NSMutableArray alloc] init];
        for (NSString *s in fileList){
            [tableData addObject:s];
        }
    }
    else{
        //Si no hay documentos, mostramos el mensaje.
        //Se muestra el mensaje de respuesta al usuario.
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error en la carga de almacenes." message:@"No dispone de almacenes de claves y certificados en la aplicación. Para añadir almacenes de claves y certificados a la aplicación, por favor, añádalos desde la configuración de la aplicación en iTunes." delegate:self cancelButtonTitle:@"Cerrar" otherButtonTitles:nil];
        [alert show];
    }
#endif
    //establecemos el primer elemento como seleccionado
    if([tableData count] >0){
        cellSelected = [tableData objectAtIndex:0];
    }
}

- (IBAction) btnDisplayFiles {
    [self listFilesFromDocumentsFolder];    
}

/******************************************************************/
/******** METODOS IMPLEMENTADOS DE LA TABLA DE CERTIFICADOS *******/
/******************************************************************/

#pragma mark -
#pragma mark Table view data source
// Detalla el numbre de secciones en la tabla.
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

// Detalla el número de filas en la tabla.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return [tableData count];
}

// Detalla la apariencia de las celdas.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    static NSString *CellIdentifier = @"Cell";
    
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:CellIdentifier] autorelease];
    }
   
    //Cell View
    UIView *cellView = [[UIView alloc]initWithFrame:CGRectMake(0, 0, 320, 65)];
    
    //ImageView
    UIImageView *imageView = [[UIImageView alloc]initWithFrame:CGRectMake(4, 4, 42, 36)];
    imageView.userInteractionEnabled = NO;
    imageView.image = [UIImage imageNamed:@"certificados.png"];
    
    //Label
    UILabel *lblFor = [[UILabel alloc]initWithFrame:CGRectMake(60, 15, 360, 21)];
    lblFor.text = [tableData objectAtIndex:indexPath.row];
    lblFor.backgroundColor = [UIColor clearColor];
    lblFor.font = [UIFont fontWithName:@"ArialMT" size:12];
    lblFor.tag =1;
    //Adding Views to Cell View
    [cellView addSubview:imageView];
    [cellView addSubview:lblFor];
    
    for(UIView *view in cell.contentView.subviews){
        if ([view isKindOfClass:[UIView class]]) {
            [view removeFromSuperview];
        }
    }
    
    [cell.contentView addSubview:cellView];
            
    return cell;
}

//Nos devuelve la fila seleccionada.
-(void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{       
    UITableViewCell *selectedCell = [tableView cellForRowAtIndexPath:indexPath];
    UILabel *myTextLabel     = [selectedCell  viewWithTag:1]; 
    cellSelected = myTextLabel.text;
}

/*********************************/
/***** GESTION DE LA MEMORIA *****/
/*********************************/

- (void)dealloc {
    [_nombreCert release];
    [_nombreAlmacen release];
    [super dealloc];
}
@end
