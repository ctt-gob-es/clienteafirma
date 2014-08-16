//
//  AOCertificateSelectionViewController.m
//  SignSample02
//
//

#import "AOCertificateSelectionViewController.h"
#import <QuartzCore/QuartzCore.h>
#import "AOPinViewController.h"
#import "CADESSignUtils.h"
#import "CADESConstants.h"
#import "AOEntity.h"
#import "AOXMLReader.h"
#import "DesCypher.h"
#import "Base64.h"

@interface AOCertificateSelectionViewController ()

@end


@implementation AOCertificateSelectionViewController

@synthesize tblView;
@synthesize startUrl = _startUrl;
@synthesize selectionButton;
@synthesize parameters;

NSMutableArray *tableData2 = NULL;
bool tableLoaded2 = NO;
NSString *cellSelected2 = NULL;
bool isTableLoaded = NO;

bool *retrievingDataFromServletCert = false;
bool *reportErrorCert = false;

NSString *stServletCert = NULL;
NSString *idDocCert = NULL;
NSString *rtServletCert = NULL;
NSString *cipherKeyCert = NULL;
NSString *fileIdCert = NULL;
NSMutableDictionary *dataLoadedCert = NULL;

// URL de entrada a la aplicacion


- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    //Parseamos los parámetros.
     NSLog(@"Parámetro url : %@", self.startUrl);
   
    
    //Rellenamos la tabla de certificados con los facilitados por iTunes y si hay almacenes, habilitamos el botón de seleccion.
    [self listFilesFromDocumentsFolder];
    [[self navigationController] setNavigationBarHidden:YES animated:YES];   
    
    //tabla de certificados
    if(isTableLoaded){
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
    if([tableData2 count]>0){
        //ponemos el primer elemento seleccionado
        NSIndexPath *indexPath=[NSIndexPath indexPathForRow:0 inSection:0];
        [tblView selectRowAtIndexPath:indexPath animated:YES  scrollPosition:UITableViewScrollPositionBottom];
    }

    //Lo ponemos aqui para que de tiempo a cargar los datos.
    self.parameters = [self parseUrl:self.startUrl];
    
    self.screenName = @"IOS AOCertificateSelectionViewController - Certificate selection window";
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

//Se invoca cuando se realiza cualquier accion
- (BOOL)shouldPerformSegueWithIdentifier:(NSString *)identifier sender:(id)sender {
    
    //comprobamos que antes de pasar a la pantalla de introducir un pin, se ha seleccionado un almacen de certificados.
    if([identifier isEqualToString:@"toPinScreen"]){
        if(cellSelected2 !=NULL){
            return YES;
        }
        else{
                       
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"atencion",nil) message:NSLocalizedString(@"seleccion_almacen",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles: nil];
            
            UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(50, 6, 40, 40)];
            
            NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"info_mini.png"]];
            UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
            [imageView setImage:bkgImg];
            [bkgImg release];
            [path release];
            
            [alert addSubview:imageView];
            [imageView release];
            
            [alert show];
            [alert release];
            
            return NO;
        }
    }
    return YES;

}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([[segue identifier] isEqualToString:@"toPinScreen"]) {
        
        // Get destination view
        AOPinViewController *pvc = [segue destinationViewController];
        
        // Set the selected button in the new view
        [pvc setNombreCertInUse:cellSelected2];
        //asignamos para obtener todos los datos de la conexion asíncrona. Si aqui no llegan todos los dato, habría que poner un flag entre el viewDidLoad y el fin de la conexión.
        self.parameters = dataLoadedCert;
        [pvc setParameters:self.parameters];
    }
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
    isTableLoaded = YES;
    tableData2 = [[NSMutableArray alloc] init];
    
    [tableData2 addObject:@"ANF USUARIO ACTIVO"];
    
#else
    if([fileList count]>0){
        //habilitamos el boton de seleccion de certificado.
        isTableLoaded = YES;
        tableData2 = [[NSMutableArray alloc] init];
        for (NSString *s in fileList){
            [tableData2 addObject:s];
        }
    }
    else{
        //Notificamos del error al servidor
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_NOT_CERTIFICATE];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_NOT_CERTIFICATE];
        [self errorReportAsync:errorToSend];
        
        //Si no hay documentos, mostramos el mensaje.
        //Se muestra el mensaje de respuesta al usuario.
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"atencion",nil) message: NSLocalizedString(@"no_almacenes",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil)otherButtonTitles:nil];
        
        UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(50, 6, 40, 40)];
        
        NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"info_mini.png"]];
        UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
        [imageView setImage:bkgImg];
        [bkgImg release];
        [path release];
        
        [alert addSubview:imageView];
        [imageView release];
        
        [alert show];
        [alert release];
    }
#endif
    //establecemos el primer elemento como seleccionado
    if([tableData2 count] >0){
        cellSelected2 = [tableData2 objectAtIndex:0];
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
    return [tableData2 count];
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
    lblFor.text = [tableData2 objectAtIndex:indexPath.row];
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
    UILabel *myTextLabel     = (UILabel *)[selectedCell  viewWithTag:1];
    cellSelected2 = myTextLabel.text;
}


/******************************************************************/
/********                  LECTURA DE LA URL                *******/
/******************************************************************/
-(NSDictionary*) parseUrl:(NSString*) urlString {
    
    NSDictionary *urlParameters = [CADESSignUtils parseUrl:urlString];
    dataLoadedCert = [urlParameters mutableCopy];    
    NSString *datosInUseCert      = NULL;
    //NSString *idDoc           = NULL;
    //NSString *stServlet       = NULL;
    
    /***** PRUEBAS 
    [dataLoadedCert removeObjectForKey:PARAMETER_NAME_DAT];
    [dataLoadedCert setObject:@"http://172.24.36.241:8080/SignatureStorageServer/RetrieveXML" forKey:PARAMETER_NAME_RTSERVLET];
    [dataLoadedCert setObject:@"12345678" forKey:PARAMETER_NAME_CIPHER_KEY];
    [dataLoadedCert setObject:@"12345678" forKey:PARAMETER_NAME_FILE_ID];
        
    FIN PRUEBAS *****/
            
    //Leemos si existen datos en la url
    if([dataLoadedCert objectForKey:PARAMETER_NAME_DAT] !=NULL)
        datosInUseCert = [[NSString alloc] initWithString:[dataLoadedCert objectForKey:PARAMETER_NAME_DAT]];
    
    //leemos la url del servlet de almacenamiento
    if([dataLoadedCert objectForKey:PARAMETER_NAME_STSERVLET] !=NULL)
        stServletCert = [[NSString alloc] initWithString:[dataLoadedCert objectForKey:PARAMETER_NAME_STSERVLET]];
    
    //leemos el identificador del usuario
    if([dataLoadedCert objectForKey:PARAMETER_NAME_ID] !=NULL)
        idDocCert = [[NSString alloc] initWithString:[dataLoadedCert objectForKey:PARAMETER_NAME_ID]];
    
    if (datosInUseCert == nil) {
        
        if([dataLoadedCert objectForKey:PARAMETER_NAME_FILE_ID]!=NULL)
            fileIdCert = [[NSString alloc] initWithString:[dataLoadedCert objectForKey:PARAMETER_NAME_FILE_ID]];
        
        if(fileIdCert == nil){
            //Notificamos del error al servidor si es posible
            NSString *errorToSend = @"";
            errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA];
            errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
            errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA];
            
            if(stServletCert!=NULL & idDocCert!=NULL)
                [self errorReportAsync:errorToSend];
            
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message:NSLocalizedString(@"no_datos_firmar",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
            
            UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(75, 6, 40, 40)];
            
            NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"warning_mini.png"]];
            UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
            [imageView setImage:bkgImg];
            [bkgImg release];
            [path release];
            
            [alert addSubview:imageView];
            [imageView release];
            
            [alert show];
            [alert release];
                      
            self.selectionButton.userInteractionEnabled = NO;
            self.selectionButton.enabled=NO;
            //return;
        }
        else{
            
            
            if([dataLoadedCert objectForKey:PARAMETER_NAME_RTSERVLET]!=NULL)
                rtServletCert = [[NSString alloc] initWithString:[dataLoadedCert objectForKey:PARAMETER_NAME_RTSERVLET]];
            
            if([dataLoadedCert objectForKey:PARAMETER_NAME_CIPHER_KEY]!=NULL)
                cipherKeyCert  = [[NSString alloc] initWithString:[dataLoadedCert objectForKey:PARAMETER_NAME_CIPHER_KEY]];
            
            
            if(cipherKeyCert!=NULL && rtServletCert!=NULL){
                
                [self loadDataFromRtservlet];                                
            }
            else{
                //Notificamos del error al servidor si es posible
                NSString *errorToSend = @"";
                errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA];
                errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
                errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA];
                
                if(stServletCert!=NULL & idDocCert!=NULL)
                    //[self errorReportAsync:errorToSend urlServlet:stServlet docId:idDoc];
                    [self errorReportAsync:errorToSend];
                
                UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"no_datos_firmar",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
                
                UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(75, 6, 40, 40)];
                
                NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"warning_mini.png"]];
                UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
                [imageView setImage:bkgImg];
                [bkgImg release];
                [path release];
                
                [alert addSubview:imageView];
                [imageView release];
                
                [alert show];
                [alert release];
                
                self.selectionButton.userInteractionEnabled = NO;                
                self.selectionButton.enabled=NO;
            }
        }
    }
        
    return dataLoadedCert;
}


/**
 Método que obtiene los datos de trabajo desde el servidor intermedio.
 
 */
-(void) loadDataFromRtservlet
{
    
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
    post = [post stringByAppendingString:fileIdCert];
    
    //Codificamos la url de post
    NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
    NSString *postLength = [NSString stringWithFormat:@"%d", [postData length]];
    
    // Obtenemos la URL del servidor de la pantalla de preferencias
    NSURL* requestUrl = [[NSURL alloc] initWithString:rtServletCert];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:requestUrl cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30.0];
    [request setHTTPMethod:@"POST"];
    [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
    [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [request setValue:@"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" forHTTPHeaderField:@"User-Agent"];
    [request setValue:@"text/plain,text/html,application/xhtml+xml,application/xml" forHTTPHeaderField:@"Accept"];
    [request setHTTPBody:postData];
        
    NSLog(@"Se recogen los datos del fichero del rtServlet con los siguientes datos: %@", post);
        
    retrievingDataFromServletCert = true;
    
    NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
    [connection start];
    
}

/**
 Método que notifica de un error en la aplicación al servidor de guardado de firmas "storage" de forma síncrona.
 
 parámetros:
 -----------
 dataSign: error producido.
 
 */
-(void) errorReportAsync:(NSString*) error
{
        
    if(stServletCert!=NULL && idDocCert != NULL){
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
        post = [post stringByAppendingString:idDocCert];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_DAT];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:error];
        
        //Codificamos la url de post
        NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
        NSString *postLength = [NSString stringWithFormat:@"%d", [postData length]];
        
        // Obtenemos la URL del servidor de la pantalla de preferencias
        NSURL* requestUrl = [[NSURL alloc] initWithString:stServletCert];
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
        
        reportErrorCert = true;
        NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
        [connection start];
    }
    
}


/* METODOS DONDE SE RECIBE LA RESPUESTA DE LA CONEXION ASINCRONA */
/**
 Método donde se recibe la respuesta de la petición asíncrona.
 
 parámetros:
 -----------
 didReceiveData: Conexión establecida asíncrona.
 data:           Datos recibidos del servidor.
 */

NSMutableData *receivedDataCert = NULL;
NSString *receivedStringCert = NULL;

//los datos van llegando por "rafagas". Lo que hay que ir haciendo es ir juntandolos todos.
-(void)connection:(NSURLConnection *)connection didReceiveData:
(NSData *)data
{
    // Append the new data to the receivedData.
    [receivedDataCert appendData:data];
}

//Se confirma la respuesta. aprovechamos para inicializar los datos de respuesta
-(void)connection:(NSURLConnection *)connection didReceiveResponse:
(NSURLResponse *)response
{
    // Discard all previously received data.
    receivedDataCert = [[NSMutableData alloc] init];
    
}

//cuando se ha terminado de leer los datos recibidos, terminamos ya la conexion y se pasa a la prefirma.
-(void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    // Connection succeeded in downloading the request.
    NSLog( @"Final de la recepción! se han recibido %d bytes", [receivedDataCert length] );
    
    // Convert received data into string.
    receivedStringCert = [[NSString alloc] initWithData:receivedDataCert
                                           encoding:NSASCIIStringEncoding];
    NSLog( @"invocación desde connectionDidFinishLoading: %@", receivedStringCert );
    
    if (retrievingDataFromServletCert){
        
        retrievingDataFromServletCert=false;
        NSString* datosInUse = NULL;
        
        //Obtenemos la respuesta del servidor.
        NSString* responseString = [[NSString alloc] initWithData:receivedDataCert encoding:NSUTF8StringEncoding];
        
        NSLog(@"Respuesta del rtserver: %@", responseString);
        
        @try {
            
            NSData *decoded = [DesCypher decypherData:responseString sk:[cipherKeyCert dataUsingEncoding:NSUTF8StringEncoding]];
                        
            datosInUse = [[NSString alloc] initWithData:decoded encoding:NSASCIIStringEncoding];
            
            AOEntity *entidad = [[AOEntity alloc] init];
            AOXMLReader *xmlReader = [[AOXMLReader alloc] init];
            entidad = [xmlReader loadXMLByString:datosInUse ];
            
            if(entidad.datField!=NULL)
                [dataLoadedCert setObject:entidad.datField forKey:PARAMETER_NAME_DAT];
            
            if(entidad.formatField!=NULL)
                [dataLoadedCert setObject:entidad.formatField forKey:PARAMETER_NAME_FORMAT];
            
            if(entidad.algorithmField!=NULL)
                [dataLoadedCert setObject:entidad.algorithmField forKey:PARAMETER_NAME_ALGORITHM2];
            
            if(entidad.propertiesField!=NULL)
                [dataLoadedCert setObject:entidad.propertiesField forKey:PARAMETER_NAME_PROPERTIES];
            
            if(entidad.idField!=NULL)
                [dataLoadedCert setObject:entidad.idField forKey:PARAMETER_NAME_ID];
            
            if(entidad.stServletField!=NULL)
                [dataLoadedCert setObject:entidad.stServletField forKey:PARAMETER_NAME_STSERVLET];
        }
        @catch (NSException *exception) {
            NSLog(@"Se ha producido un error al obtener el fichero: %@", exception.description );
        }
        
    }
    // la respuesta a un reporte de error
    else if(reportErrorCert){
        reportErrorCert = false;
        //responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSLog(@"Respuesta del servidor: %@",[[NSString alloc] initWithData:receivedDataCert encoding:NSUTF8StringEncoding]);
    }
    
    // release the connection, and the data object
    [connection release];
    [receivedDataCert release];
}

/**************************/
/*** PREOTECCIONES SSL ****/
/**************************/

//para las protecciones ssl

- (BOOL)connection:(NSURLConnection *)connection canAuthenticateAgainstProtectionSpace:(NSURLProtectionSpace *)protectionSpace {
    return [protectionSpace.authenticationMethod isEqualToString:NSURLAuthenticationMethodServerTrust];
}

//Acepta todos las conexiones ssl
//Deprecado a partir de ios 5.
- (void)connection:(NSURLConnection *)connection didReceiveAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge {
    [challenge.sender useCredential:[NSURLCredential credentialForTrust:challenge.protectionSpace.serverTrust] forAuthenticationChallenge:challenge];
}
//Acepta todas las conexiones ssl
//Nuevo método no deprecado
-(void)connection:(NSURLConnection *)connection willSendRequestForAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge{
    [challenge.sender useCredential:[NSURLCredential credentialForTrust:challenge.protectionSpace.serverTrust] forAuthenticationChallenge:challenge];
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
    
    NSLog(@"Error - %@ %@",
          [error localizedDescription],
          [[error userInfo] objectForKey:NSURLErrorFailingURLStringErrorKey]);
    
    //Notificamos del error al servidor
    NSString *errorToSend = @"";
    errorToSend = [errorToSend stringByAppendingString:ERROR_SIGNING];
    errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
    errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_SIGNING];
    
    [self errorReportAsync:errorToSend];
    
    return;
}

 
@end
