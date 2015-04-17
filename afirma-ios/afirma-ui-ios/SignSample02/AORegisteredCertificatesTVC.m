//
//  AORegisteredCertificatesTVC.m
//  SignSample02
//
//  Created by Rocio Tovar on 24/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "AORegisteredCertificatesTVC.h"
#import "AOCertificateCell.h"
#import "OpenSSLCertificateHelper.h"
#import "CertificateUtils.h"
#import "AOUtils.h"
#import "ColorChart.h"
#import "AOSignViewController.h"
#import "CADESSignUtils.h"
#import "CADESConstants.h"
#import "AOEntity.h"
#import "DesCypher.h"
#import "AOXMLReader.h"

@interface AORegisteredCertificatesTVC ()
{
    AOCertificateInfo *_selectedCertificate;
    NSMutableArray *_certificatesArray;
    NSMutableDictionary *_opParameters;
    NSString *_idDocCert;
    NSString *_stServletCert;
    NSString *_fileIdCert;
    NSString *_rtServletCert;
    NSString *_cipherKeyCert;
    bool _reportErrorCert;
    bool _retrievingDataFromServletCert;
}

@end

@implementation AORegisteredCertificatesTVC

#pragma mark - Life Cycle

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    [self.navigationItem setRightBarButtonItems:@[self.navigationItem.rightBarButtonItem, self.editButtonItem] animated:YES];
    [self.tableView setTableFooterView:[[UIView alloc] initWithFrame:CGRectZero]];
    
    if (_mode == AORegisteredCertificatesTVCModeSign) {
        [self parseUrl:_startURL];
    }
}

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
    
    [self.navigationController setNavigationBarHidden:NO animated:YES];
    [self.navigationController.navigationBar setTintColor:THEME_COLOR];
    [self reloadCertificates];
    [self.tableView reloadData];
    [self.tableView setAllowsSelection:_mode == AORegisteredCertificatesTVCModeSign];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    
    _certificatesArray = nil;
}

#pragma mark - User Interaction

- (IBAction)didClickEditButton:(id)sender
{
    if ([_certificatesArray count] > 0) {
        [self editTable:!self.editing];
    }
}

- (void)editTable:(BOOL)edit
{
    [self setEditing:edit animated:NO];
    [self.tableView reloadData];
    [self.editButtonItem setTitle:edit ? @"Hecho":@"Editar"];
}

#pragma mark - UITableViewDataSource

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    [self.editButtonItem setEnabled:_certificatesArray && _certificatesArray.count > 0];
    return _certificatesArray ? _certificatesArray.count : 0;
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    AOCertificateCell *cell = [tableView dequeueReusableCellWithIdentifier:@"CertificateCell"];
    [cell setCertificateInfo:_certificatesArray[indexPath.row] forEditingCell:self.isEditing];
    [cell setSelectionStyle:_mode == AORegisteredCertificatesTVCModeManagement ? UITableViewCellSelectionStyleNone : UITableViewCellSelectionStyleDefault];
    [cell setAccessoryType:_mode == AORegisteredCertificatesTVCModeManagement ? UITableViewCellAccessoryNone : UITableViewCellAccessoryDisclosureIndicator];
    
    return cell;
}

- (UITableViewCellEditingStyle)tableView:(UITableView *)aTableView editingStyleForRowAtIndexPath:(NSIndexPath *)indexPath

{
    if (self.editing && _certificatesArray && _certificatesArray.count > 0) {
        return UITableViewCellEditingStyleDelete;
        
    }
    
    return UITableViewCellEditingStyleNone;
    
}

- (NSString *)tableView:(UITableView *)tableView titleForDeleteConfirmationButtonForRowAtIndexPath:(NSIndexPath *)indexPath
{
    return @"Eliminar";
}

- (void)tableView:(UITableView *)aTableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
{
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        
        OSStatus status = [self deleteCertificate:_certificatesArray[indexPath.row]];
        NSString *errorMessage = nil;
        
        switch (status) {
            case noErr :
            case errSecItemNotFound:
                errorMessage = @"Se ha eliminado el certificado correctamente";
                break;
            default:
                errorMessage = @"Se ha producido un error";
                break;
        }
        
        [[[UIAlertView alloc] initWithTitle:errorMessage message:@"" delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil] show];
        
        [self reloadCertificates];
        [self.tableView reloadData];
    }
}

#pragma mark - UITableViewDelegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    NSIndexPath *selectedIndexPath = [self.tableView indexPathForSelectedRow];
    _selectedCertificate = _certificatesArray[selectedIndexPath.row];
    
    if ([[CertificateUtils sharedWrapper] searchIdentityByName:_selectedCertificate.subject] == YES) {
        NSLog(@"LoginViewController::prepareForSegue::selected certificate....");
        [[NSUserDefaults standardUserDefaults] setObject:@{kAOUserDefaultsKeyAlias:_selectedCertificate.subject} forKey:kAOUserDefaultsKeyCurrentCertificate];
        [[NSUserDefaults standardUserDefaults] synchronize];
        [[CertificateUtils sharedWrapper] setSelectedCertificateName:_selectedCertificate.subject];
    } else {
        [[[UIAlertView alloc] initWithTitle:@"Se ha producido un error al cargar el certificado"
                                    message:@""
                                   delegate:nil
                          cancelButtonTitle:@"OK"
                          otherButtonTitles:nil] show];
    }
}

#pragma mark - Certificates

- (void)reloadCertificates
{
    _certificatesArray = nil;
    _certificatesArray = [[OpenSSLCertificateHelper getAddedCertificatesInfo] mutableCopy];
    
    if (!_certificatesArray) {
        _certificatesArray = [[NSMutableArray alloc] init];
    }
    
    if (_certificatesArray.count == 0) {
        [self editTable:NO];
    }
}

- (OSStatus)deleteCertificate:(AOCertificateInfo *)certificateInfo
{
    OSStatus status = noErr;
    status = [OpenSSLCertificateHelper deleteCertificate:certificateInfo];
    
    if (status == noErr) {
        NSLog(@"deleterWithCertificateName::Certificate %@ is deleted from keychain:", certificateInfo.subject);
    } else {
        NSLog(@"deleterWithCertificateName::Certificate %@ is deleted from keychain:", certificateInfo.subject);
        NSLog(@"No Se ha eliminado el certificado correctamente.Error: %i", (int)status);
    }
    
    return status;
}

#pragma mark - Navigation

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([segue.identifier isEqualToString:@"showSignVC"]) {
        AOSignViewController *signVC = segue.destinationViewController;
        
        [signVC setParameters:_opParameters];
        [signVC setCertificateName:_selectedCertificate.subject];
        [signVC setBase64UrlSafeCertificateData:[CertificateUtils sharedWrapper].base64UrlSafeCertificateData];
        [signVC setPrivateKey:[CertificateUtils sharedWrapper].privateKey];
    }
}

#pragma mark - 
#pragma mark - Inherited methods...
#pragma mark -

/******************************************************************/
/********                  LECTURA DE LA URL                *******/
/******************************************************************/
-(void) parseUrl:(NSString*) urlString
{
    NSDictionary *urlParameters = [CADESSignUtils parseUrl:urlString];
    _opParameters = [urlParameters mutableCopy];
    NSString *datosInUseCert = NULL;
    
    //Leemos si existen datos en la url
    if([_opParameters objectForKey:PARAMETER_NAME_DAT] !=NULL)
        datosInUseCert = [[NSString alloc] initWithString:[_opParameters objectForKey:PARAMETER_NAME_DAT]];
    
    //leemos la url del servlet de almacenamiento
    if([_opParameters objectForKey:PARAMETER_NAME_STSERVLET] !=NULL)
        _stServletCert = [[NSString alloc] initWithString:[_opParameters objectForKey:PARAMETER_NAME_STSERVLET]];
    
    //leemos el identificador del usuario
    if([_opParameters objectForKey:PARAMETER_NAME_ID] !=NULL)
        _idDocCert = [[NSString alloc] initWithString:[_opParameters objectForKey:PARAMETER_NAME_ID]];
    
    if (datosInUseCert == nil) {
        
        if([_opParameters objectForKey:PARAMETER_NAME_FILE_ID]!=NULL)
            _fileIdCert = [[NSString alloc] initWithString:[_opParameters objectForKey:PARAMETER_NAME_FILE_ID]];
        
        if(_fileIdCert == nil){
            //Notificamos del error al servidor si es posible
            NSString *errorToSend = @"";
            errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA];
            errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
            errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA];
            
            if(_stServletCert!=NULL & _idDocCert!=NULL)
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
            
            [self.tableView setAllowsSelection:NO];
        } else{
            if([_opParameters objectForKey:PARAMETER_NAME_RTSERVLET]!=NULL)
                _rtServletCert = [[NSString alloc] initWithString:[_opParameters objectForKey:PARAMETER_NAME_RTSERVLET]];
            
            if([_opParameters objectForKey:PARAMETER_NAME_CIPHER_KEY]!=NULL)
                _cipherKeyCert  = [[NSString alloc] initWithString:[_opParameters objectForKey:PARAMETER_NAME_CIPHER_KEY]];
            
            NSLog(@"Clave de cifrado: %@", _cipherKeyCert);
            
            if(_cipherKeyCert!=NULL && _rtServletCert!=NULL){
                [self loadDataFromRtservlet];
            }
            else{
                //Notificamos del error al servidor si es posible
                NSString *errorToSend = @"";
                errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA];
                errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
                errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA];
                
                if(_stServletCert!=NULL & _idDocCert!=NULL)
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
                
                [self.tableView setAllowsSelection:NO];
            }
        }
    }
}

/**
 Método que notifica de un error en la aplicación al servidor de guardado de firmas "storage" de forma síncrona.
 
 parámetros:
 -----------
 dataSign: error producido.
 
 */
-(void) errorReportAsync:(NSString*) error
{
    if(_stServletCert!=NULL && _idDocCert != NULL){
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
        post = [post stringByAppendingString:_idDocCert];
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_DAT];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:error];
        
        //Codificamos la url de post
        //Changed NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
        NSData *postData = [post dataUsingEncoding:NSUTF8StringEncoding allowLossyConversion:YES];
        NSString *postLength = [NSString stringWithFormat:@"%d", (int) [postData length]];
        
        // Obtenemos la URL del servidor de la pantalla de preferencias
        NSURL* requestUrl = [[NSURL alloc] initWithString:_stServletCert];
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
        
        _reportErrorCert = true;
        NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
        [connection start];
    }
    
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
    post = [post stringByAppendingString:_fileIdCert];
    
    //Codificamos la url de post
    //CHANGED NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
    
    NSData *postData = [post dataUsingEncoding:NSUTF8StringEncoding allowLossyConversion:YES];
    NSString *postLength = [NSString stringWithFormat:@"%d", (int) [postData length]];
    
    // Obtenemos la URL del servidor de la pantalla de preferencias
    NSURL* requestUrl = [[NSURL alloc] initWithString:_rtServletCert];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:requestUrl cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30.0];
    [request setHTTPMethod:@"POST"];
    [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
    [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [request setValue:@"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" forHTTPHeaderField:@"User-Agent"];
    [request setValue:@"text/plain,text/html,application/xhtml+xml,application/xml" forHTTPHeaderField:@"Accept"];
    [request setHTTPBody:postData];
    
    NSLog(@"AOCertificateSelectionViewController: Se recogen los datos del fichero del rtServlet con los siguientes datos: %@", post);
    
    _retrievingDataFromServletCert = true;
    
    NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
    [connection start];
    
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
    NSLog( @"AOCertificateSelectionViewController: Final de la recepción! se han recibido %d bytes", (int) [receivedDataCert length]);
    
    // Convert received data into string.
    //Changed receivedStringCert = [[NSString alloc] initWithData:receivedDataCert encoding:NSASCIIStringEncoding];
    receivedStringCert = [[NSString alloc] initWithData:receivedDataCert
                                               encoding:NSUTF8StringEncoding];
    
    NSLog( @"AOCertificateSelectionViewController: Descarga finalizada");
    
    if (_retrievingDataFromServletCert){
        
        _retrievingDataFromServletCert=false;
        NSString* datosInUse = NULL;
        
        //Obtenemos la respuesta del servidor.
        NSString* responseString = [[NSString alloc] initWithData:receivedDataCert encoding:NSUTF8StringEncoding];
        
        NSLog(@"AOCertificateSelectionViewController: Hemos obtenido los datos del rtserver");
        
        @try {
            
            //NSLog(@"XML Cifrado: %@", responseString);
            NSData *decoded = [DesCypher decypherData:responseString sk:[_cipherKeyCert dataUsingEncoding:NSUTF8StringEncoding]];
            
            //Changed datosInUse = [[NSString alloc] initWithData:decoded encoding:NSASCIIStringEncoding];
            datosInUse = [[NSString alloc] initWithData:decoded encoding:NSUTF8StringEncoding];
            
            //NSLog(@"XML Descifrado: %@", decoded);
            
            AOEntity *entidad = [[AOEntity alloc] init];
            AOXMLReader *xmlReader = [[AOXMLReader alloc] init];
            entidad = [xmlReader loadXMLByString:datosInUse ];
            
            //NSLog(@"Campo de datos: %@", entidad.datField);
            
            if(entidad.datField!=NULL)
                [_opParameters setObject:entidad.datField forKey:PARAMETER_NAME_DAT];
            
            if(entidad.formatField!=NULL)
                [_opParameters setObject:entidad.formatField forKey:PARAMETER_NAME_FORMAT];
            
            if(entidad.algorithmField!=NULL)
                [_opParameters setObject:entidad.algorithmField forKey:PARAMETER_NAME_ALGORITHM2];
            
            if(entidad.propertiesField!=NULL)
                [_opParameters setObject:entidad.propertiesField forKey:PARAMETER_NAME_PROPERTIES];
            
            if(entidad.idField!=NULL)
                [_opParameters setObject:entidad.idField forKey:PARAMETER_NAME_ID];
            
            if(entidad.stServletField!=NULL)
                [_opParameters setObject:entidad.stServletField forKey:PARAMETER_NAME_STSERVLET];
        }
        @catch (NSException *exception) {
            NSLog(@"Se ha producido un error al obtener el fichero: %@", exception.description );
        }
        
    }
    // la respuesta a un reporte de error
    else if(_reportErrorCert){
        _reportErrorCert = false;
        //responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSLog(@"AOCertificateSelectionViewController: Respuesta del servidor: %@",[[NSString alloc] initWithData:receivedDataCert encoding:NSUTF8StringEncoding]);
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
