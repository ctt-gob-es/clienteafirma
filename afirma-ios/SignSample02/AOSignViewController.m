//
//  AOSignViewController.m
//  SignSample02
//
//

#import "AOSignViewController.h"
#import "NSData+Base64.h"
#import "CADESSignUtils.h"
#import "CADESConstants.h"
#import "AlertProgressBar.h"
#import "CADESMonoPhase.h"
#include "CADESOID.h"
#include "AODropBoxOperations.h"
#import "GAI.h"
#import "GAIDictionaryBuilder.h"

@interface AOSignViewController ()

@end

@implementation AOSignViewController

@synthesize parameters = _parameters;
@synthesize nombreCert = _nombreCert;
@synthesize certificateName = _certificateName;
@synthesize signButton = _signButton;
@synthesize base64UrlSafeCertificateData = _base64UrlSafeCertificateData;

//Datos globales que se usarán para la invocación a los distintos servlets
NSString *operation       = NULL;
NSString *datosInUse      = NULL;
NSString *signAlgoInUse   = NULL;
NSString *docId           = NULL;
NSString *cipherKey       = NULL;
NSString *urlServlet      = NULL;
NSString *signFormat      = NULL;
NSString *extraParams     = NULL;
NSDictionary *dictExtraParams = NULL;
NSString *triphasicServerURL = NULL;
NSString *fileId          = NULL;
NSString *rtServlet       =NULL;
NSString *cloudName       =NULL;

bool *storingData = false;
bool *retrievingDataFromServlet = false;
bool *postSign = false;
bool *reportError = false;
bool *localSignature = false;

AlertProgressBar *alertpb = NULL;

NSMutableData *receivedData = NULL;

NSString *receivedString = NULL;

// Clave privada RSA
SecKeyRef privateKey = NULL;
-(void) setPrivateKey:(SecKeyRef *) privatKey{
    privateKey = *privatKey;
}

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
    [[self navigationController] setNavigationBarHidden:YES animated:YES];
    //Establecemos el nombre del certificado del almacen
    self.nombreCert.text = self.certificateName;
    
    [self preloadData];
    
    self.screenName = @"IOS AOSignViewController - Start signature process window";
}

-(IBAction)buttonPressed:(id)sender {
    [self startSignatureProcess];
    
    id<GAITracker> tracker= [[GAI sharedInstance] defaultTracker];
    NSDictionary *urlParameters = self.parameters;
    
    NSString *format = @"";
    if([urlParameters objectForKey:PARAMETER_NAME_FORMAT]!=NULL)
        format = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_FORMAT]];
    NSString *algorithm = @"";
    if([urlParameters objectForKey:PARAMETER_NAME_ALGORITHM2] != NULL)
        algorithm  = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_ALGORITHM2]];
    
    NSString *label=@"Operacion='1', formato='";
    label=[label stringByAppendingString:format];
    label=[label stringByAppendingString:@"', algoritmo='"];
    label=[label stringByAppendingString:algorithm];
    label=[label stringByAppendingString:@"'"];
    
    [tracker send:[[GAIDictionaryBuilder createEventWithCategory:@"IOS Signature"     // Event category (required)
        action:@"Signature"  // Event action (required)
        label:label          // Event label
        value:nil] build]];    // Event value
}

//cuando se pulsa el botón del centro del teléfono
-(void)onGoingToBackGround:(NSNotification*) notification {
    @try {
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
    }
}


/*****************************************************************/
/******** METODOS DE LA PROPIA FIRMA Y ENVIO AL SERVIDOR *********/
/*****************************************************************/

-(void) preloadData{
    
    NSDictionary *urlParameters = self.parameters;
    
    //parámetro donde se recogen los datos originales. El documento llega dentro del parámetro "dat"
    if([urlParameters objectForKey:PARAMETER_NAME_DAT] !=NULL){
        NSString *data =[urlParameters objectForKey:PARAMETER_NAME_DAT];
        data = [data stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
        datosInUse = [[NSString alloc] initWithString:data];
    }
    
    if([urlParameters objectForKey:PARAMETER_NAME_FILE_ID]!=NULL)
        fileId = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_FILE_ID]];
    
    if([urlParameters objectForKey:PARAMETER_NAME_RTSERVLET]!=NULL)
        rtServlet = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_RTSERVLET]];
    
    //parámetro de la clave de cifrado con el servidor "key"
    if([urlParameters objectForKey:PARAMETER_NAME_CIPHER_KEY]!=NULL)
        cipherKey  = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_CIPHER_KEY]];
    
    if([urlParameters objectForKey:PARAMETER_NAME_ID])
        docId      = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_ID]];
    
    if (datosInUse == nil) {
        
        if(fileId == nil){
            
            //Notificamos del error al servidor si es posible
            NSString *errorToSend = @"";
            errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA];
            errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
            errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA];
            
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_datos_firmar",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
            
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
            
            self.signButton.userInteractionEnabled = NO;
            return;
        }
        else{
            if(cipherKey!=NULL && rtServlet!=NULL){
                [self loadDataFromRtservlet:fileId rtServlet:rtServlet];
            }
            else{
                //Notificamos del error al servidor si es posible
                NSString *errorToSend = @"";
                errorToSend = [errorToSend stringByAppendingString:ERROR_MISSING_DATA];
                errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
                errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_MISSING_DATA];
                
                [self errorReportAsync:errorToSend];
                
                UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_datos_firmar",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
                
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
                
                self.signButton.userInteractionEnabled = NO;
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
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"error_datos_firmar",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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
        
        self.signButton.userInteractionEnabled = NO;
        return;
    }
    
}

-(void) startSignatureProcess {
    NSDictionary *urlParameters = self.parameters;
    //NSLog(@"URL analizada: %@", url);
    
    //parámetro de operacion "op"
    if([urlParameters objectForKey:PARAMETER_NAME_OPERATION]!=NULL)
        operation  = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_OPERATION]];
    
    //parámetro del servlet donde se almacena la firma "servlet"
    if([urlParameters objectForKey:PARAMETER_NAME_STSERVLET]!=NULL)
        urlServlet = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_NAME_STSERVLET]];
    
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
            dictExtraParams = dict;
            
            //Se recoge la prefirma y se vuelve a decodificar, ya que esta viene a su vez codificada en base64.
            triphasicServerURL  = [dict objectForKey:PARAMETER_NAME_TRIPHASIC_SERVER_URL];
        }
    }
    
    //parámetro "local" que indica que la firma se inició desde el propio dispositivo
    if([urlParameters objectForKey:PARAMETER_LOCAL_SIGNATURE] != NULL){
        NSString *local= [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_LOCAL_SIGNATURE]];
        if([local isEqualToString:@"true"])
            localSignature = true;
        
        cloudName = [[NSString alloc] initWithString:[urlParameters objectForKey:PARAMETER_LOCAL_CLOUD_NAME]];
    }
    
    
    if([urlParameters objectForKey:PARAMETER_NAME_TARGET] != NULL){
        NSString *extraParams2 = [urlParameters objectForKey:PARAMETER_NAME_TARGET];
        if(extraParams2!=NULL){
            //URL DECODE
            extraParams2 = [extraParams2 stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
            extraParams2 = [CADESSignUtils urlSafeDecode:extraParams2];
            
            NSMutableDictionary *aux = [NSMutableDictionary dictionaryWithDictionary:dictExtraParams];
            
            if(![extraParams2 isEqualToString:PARAMETER_NAME_TARGET_TREE] || ![extraParams2 isEqualToString:PARAMETER_NAME_TARGET_LEAFS]){
                //Notificamos del error al servidor si es posible
                NSString *errorToSend = @"";
                errorToSend = [errorToSend stringByAppendingString:ERROR_NOT_TARGET];
                errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
                errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_NOT_TARGET];
                
                [self errorReportAsync:errorToSend];
                
                UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"error_objetivo_contrafirma",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
                
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
                
                self.signButton.userInteractionEnabled = NO;
                return;
                
            }
            else{
                if(aux == NULL){
                    aux = [[NSMutableDictionary alloc] init];
                }
                [aux setObject:extraParams2 forKey:PARAMETER_NAME_TARGET];
                
                dictExtraParams = aux;
            }
            
        }
    }
    
    
    NSLog(@"Operacion: %@", operation);
    NSLog(@"Documento: %@", docId);
    NSLog(@"Servlet: %@", urlServlet);
    NSLog(@"Datos: %@", datosInUse);
    NSLog(@"Formato: %@", signFormat);
    NSLog(@"Algoritmo: %@", signAlgoInUse);
    NSLog(@"Clave de cifrado: %@", cipherKey);
    if(extraParams!=NULL)
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
        
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"error_codigo_desconocido",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil)  otherButtonTitles:nil];
        
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
        self.signButton.userInteractionEnabled = NO;
        return;
    }
    
    if (urlServlet == nil) {
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message:  NSLocalizedString(@"error_url_servidor",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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
        self.signButton.userInteractionEnabled = NO;
        return;
    }
    
    if (signFormat == nil) {
        
        //Notificamos del error al servidor si es posible
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_NOT_SUPPORTED_FORMAT];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_NOT_SUPPORTED_FORMAT];
        
        [self errorReportAsync:errorToSend];
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_formato_firma",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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

        self.signButton.userInteractionEnabled = NO;
        return;
    }
    else if (!([signFormat isEqualToString:CADES_FORMAT]
               || [signFormat isEqualToString:CADES_TRI_FORMAT]
               || [signFormat isEqualToString:PADES_TRI_FORMAT]
               || [signFormat isEqualToString:XADES_TRI_FORMAT]
               || [signFormat isEqualToString:PADES_FORMAT]
               || [signFormat isEqualToString:XADES_FORMAT])){
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message:  NSLocalizedString(@"error_formato_no_soportado",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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
        
        //[signButton setTitle:@"Cerrar aplicación" forState:UIControlStateNormal];
        self.signButton.userInteractionEnabled = NO;
        return;
        
    }
    
    if(![CADESSignUtils isValidAlgorithm:signAlgoInUse]){
        
        //Notificamos del error al servidor si es posible
        NSString *errorToSend = @"";
        errorToSend = [errorToSend stringByAppendingString:ERROR_NOT_SUPPORTED_ALGORITHM];
        errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
        errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_NOT_SUPPORTED_ALGORITHM];
        
        [self errorReportAsync:errorToSend];
                
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"error_algoritmo_no_soportado",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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
        //[signButton setTitle:@"Cerrar aplicación" forState:UIControlStateNormal];
        self.signButton.userInteractionEnabled = NO;
        return;
    }
    
    if([signFormat isEqualToString:CADES_FORMAT]){
        
        //Invocamos la firma monofasica
        [self cadesMonoPhasic];
    }
    else if ([signFormat isEqualToString:CADES_TRI_FORMAT] ||
             [signFormat isEqualToString:PADES_FORMAT] ||
             [signFormat isEqualToString:PADES_TRI_FORMAT] ||
             [signFormat isEqualToString:XADES_FORMAT] ||
             [signFormat isEqualToString:XADES_TRI_FORMAT]){
        //Invocamos la firma trifásica
        [self cadesTriPhasic];
    }
       
    //[signButton setTitle:@"Cerrar aplicación" forState:UIControlStateNormal];
    self.signButton.userInteractionEnabled = NO;
    self.signButton.enabled=NO;
    
    
}

/**
 Método donde se realiza la firma monofasica.
 
 parámetros:
 -----------
 
 */
-(void)cadesMonoPhasic{
    
    NSString *aux= [CADESSignUtils urlSafeDecode:datosInUse];
    //NSData *contentData = [NSData dataFromBase64String:aux];
    NSData *contentData = [CADESSignUtils base64DecodeString:aux];
    NSLog(@"%@",[NSString stringWithUTF8String:[contentData bytes]]);
    
    NSString *contentDescription = [[NSString alloc]init];
    contentDescription=@"binary";
    NSString *policyOID = NULL;
    NSString *policyHash = NULL;
    NSString *policyUri = NULL;
    char *policyHashAlg = NULL;
    NSString *mode = PROPERTIES_PARAMETER_MODE_IMPLICIT;
    char *precalculatedHashAlgorithm = NULL;
    NSData *precalculatedHash = NULL;
    
    char *signAlgorithm = RSA_OID; //siempre se usa RSA, por ahora.
    int signingCertificateV2 = 0; //Por defecto es SigningCertificateV1
    
    /* INCIIO PRUEBAS
     
     NSMutableDictionary *dictAux = [dictExtraParams mutableCopy];
     [dictAux setObject:@"urn:oid:2.16.724.1.3.1.1.2.1.8" forKey:PROPERTIES_PARAMETER_POLICYIDENTIFIER];
     [dictAux setObject:@"7SxX3erFuH31TvAw9LZ70N7p1vA=" forKey:PROPERTIES_PARAMETER_POLICYIDENTIFIERHASH];
     [dictAux setObject:@"http://www.w3.org/2000/09/xmldsig#sha256" forKey:PROPERTIES_PARAMETER_POLICYIDENTIFIERHASHALGORITHM];
     [dictAux setObject:@"http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf" forKey:PROPERTIES_PARAMETER_POLICYQUALIFIER];
     [dictAux setObject:PROPERTIES_PARAMETER_MODE_EXPLICIT forKey:PROPERTIES_PARAMETER_MODE];
     [dictAux setObject:@"http://www.w3.org/2000/09/xmldsig#sha256" forKey:PROPERTIES_PARAMETER_PRECALCULATEDHASHALGORITHM];
     [dictAux setObject:@"true" forKey:PROPERTIES_PARAMETER_SIGNINGCERTIFICATEV2];
    
     
     dictExtraParams = dictAux;
     FIN PRUEBAS */
    
    
    if(dictExtraParams!=NULL){
        
        //Parámetro de modo: explícito o implícito
        if([dictExtraParams objectForKey:PROPERTIES_PARAMETER_MODE]!=NULL){
            if([[dictExtraParams objectForKey:PROPERTIES_PARAMETER_MODE] isEqualToString:PROPERTIES_PARAMETER_MODE_EXPLICIT])
                mode = PROPERTIES_PARAMETER_MODE_EXPLICIT;
        }
        
        //parámetro de algoritmo precalculado
        if([dictExtraParams objectForKey:PROPERTIES_PARAMETER_PRECALCULATEDHASHALGORITHM]!=NULL){
            
            //tenemos que deshacer el formato "http://www.w3.org/2000/09/xmldsig#sha256"
            NSString *auxPreCalculatedHashAlg = [dictExtraParams objectForKey:PROPERTIES_PARAMETER_PRECALCULATEDHASHALGORITHM];
            NSRange range = [auxPreCalculatedHashAlg rangeOfString:@"#" options:NSBackwardsSearch];
            auxPreCalculatedHashAlg = [auxPreCalculatedHashAlg substringFromIndex:range.location+1];
            
            precalculatedHashAlgorithm = [CADESSignUtils getHashAlgorithmOID:auxPreCalculatedHashAlg];
            
            //Si el HASH viene precalculado, entonces viene el hash en si en los datos!!!!
            precalculatedHash = contentData;
            
            mode = PROPERTIES_PARAMETER_MODE_EXPLICIT;
            
            //Modificamos a SigningCertificateV2 en caso de que venga otro algoritmo de hash que no sea el sha1
            if(strcmp(precalculatedHashAlgorithm, SHA1_OID)!=0){
                signingCertificateV2 = 1; // Usamos SigningCertificateV2
            }
        }
        
        //Parámetro de la política de firma
        if([dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYIDENTIFIER]!=NULL
           && [dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYIDENTIFIERHASH]!=NULL
           && [dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYIDENTIFIERHASHALGORITHM]!=NULL
           && [dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYQUALIFIER]!=NULL){
            
            policyOID = [dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYIDENTIFIER];
            
            //quitamos el "urn:oid:" en caso de que lo tuviera
            policyOID = [policyOID stringByReplacingOccurrencesOfString:@"urn:oid:" withString:@""];
            
            //decodificamos el hash que viene codificado en b64?¿?¿¿?¿?¿
            /*
             NSData *auxPolicyHash = [NSData dataFromBase64String:[dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYIDENTIFIERHASH]];
             policyHash = [NSString stringWithUTF8String:[auxPolicyHash bytes]];
             */
            policyHash = [dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYIDENTIFIERHASH];
            
            //obtenemos el algoritmo de la política
            NSString *auxPolicyHashAlg = NULL;
            auxPolicyHashAlg = [dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYIDENTIFIERHASHALGORITHM];
            NSRange range = [auxPolicyHashAlg rangeOfString:@"#" options:NSBackwardsSearch];
            auxPolicyHashAlg = [auxPolicyHashAlg substringFromIndex:range.location+1];
            policyHashAlg= [CADESSignUtils getHashAlgorithmOID:auxPolicyHashAlg];
            
            //URL de la política
            policyUri = [dictExtraParams objectForKey:PROPERTIES_PARAMETER_POLICYQUALIFIER];
            
            //Modificamos a SigningCertificateV2 en caso de que venga otro algoritmo de hash que no sea el sha1
            if(strcmp(policyHashAlg, SHA1_OID)!=0){
                signingCertificateV2 = 1; // Usamos SigningCertificateV2
            }
        }
        
        //Parámetro de SigningCertificateV2
        if([dictExtraParams objectForKey:PROPERTIES_PARAMETER_SIGNINGCERTIFICATEV2]!=NULL){
            bool v2 = [[dictExtraParams objectForKey:PROPERTIES_PARAMETER_SIGNINGCERTIFICATEV2] boolValue];
            if (v2){
                signingCertificateV2 = 1;
            }
        }
        else{
            if(strcmp([CADESSignUtils getAlgorithmOID:signAlgoInUse],SHA1_OID)!=0){
                signingCertificateV2 = 1; // Usamos SigningCertificateV2
            }
        }
        
    }
    else{
        if(strcmp([CADESSignUtils getAlgorithmOID:signAlgoInUse],SHA1_OID)!=0){
            signingCertificateV2 = 1; // Usamos SigningCertificateV2
        }
    }
    
    NSData *signature = [CADESMonoPhase
                         getCadesMonoPhase: self.base64UrlSafeCertificateData
                         mode: mode
                         contentData: contentData
                         privateKey: &privateKey
                         signAlgoInUse: signAlgoInUse
                         contentDescription: contentDescription
                         policyOID: policyOID
                         policyHash: policyHash
                         policiHashAlg: policyHashAlg                               policyUri: policyUri
                         signAlgorithm: signAlgorithm
                         signingCertificateV2: signingCertificateV2
                         precalculatedHashAlgorithm: precalculatedHashAlgorithm
                         precalculatedHash: precalculatedHash];
    
    
    //[signature writeToFile:@"/Users/tomas/David/pruebas/signature.csig" atomically:TRUE];
    NSLog(@"%@",signature);
    
    
    //iniciamos la barra de progreso.
    alertpb = [[AlertProgressBar alloc]init];
    [alertpb createProgressBar:self.view];
    
    //invocamos al almacenamiento de la firma
    NSString *finalSignature = [CADESSignUtils encodeBase64:signature];
    finalSignature = [CADESSignUtils urlSafeEncode:finalSignature];
    [self storeData:finalSignature];
    
}

/**
 Método donde se realiza la petición asíncrona al servidor de firma.
 Se recogen los datos introducidos por url y se monta la peticiíon de tipo POST.
 
 parámetros:
 -----------
 
 */
-(void)cadesTriPhasic{
    
    /* LLAMADA ASINCRONA */
    
    NSString *post =@"";
    //OPERATION OP : PRE
    post = [post stringByAppendingString:PARAMETER_NAME_OPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:OPERATION_PRESIGN];
    post = [post stringByAppendingString:HTTP_AND];
    
    //COPERATION = SIGN / COSIGN /COUNTERSIGN
    post = [post stringByAppendingString:PARAMETER_NAME_COPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:operation];
    post = [post stringByAppendingString:HTTP_AND];
    
    //DOCID: DATOS A FIRMAR
    post = [post stringByAppendingString:PARAMETER_NAME_DOCID];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:datosInUse];
    post = [post stringByAppendingString:HTTP_AND];
    
    //FORMAT: FORMATO DE FIRMA
    post = [post stringByAppendingString:PARAMETER_NAME_FORMAT];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:signFormat];
    post = [post stringByAppendingString:HTTP_AND];
    
    //ALGORITHM: ALGORITMO DE FIRMA
    post = [post stringByAppendingString:PARAMETER_NAME_ALGORITHM];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:signAlgoInUse];
    post = [post stringByAppendingString:HTTP_AND];
    
    //CERT: CERTIFICADO DE FITMA
    post = [post stringByAppendingString:PARAMETER_NAME_CERT];
    post = [post stringByAppendingString:HTTP_EQUALS];
    NSString *certificate = [CADESSignUtils urlSafeEncode: self.base64UrlSafeCertificateData];
    //la siguiente línea quita el ultimo caracter que debe ser de nueva línea "\n" esto estaba
    //fastidiando los demás parámetros.
    certificate = [certificate substringToIndex:[certificate length] - 1];
    post = [post stringByAppendingString:certificate];
    
    //PARAMETERS: PARAMETROS EXTRA
    if(extraParams !=NULL){
        post = [post stringByAppendingString:HTTP_AND];
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
    
    //se procesa la respuesta del servidor.
    
    //Se recoge el resultado de la petición de almacenamiento
    if(storingData){
        storingData= false;
        
        //Obtenemos la respuesta del servidor.
        NSString* responseString = [[NSString alloc] initWithData:receivedData encoding:NSUTF8StringEncoding];
        
        NSLog(@"Respuesta del storage: %@", responseString);
        //quitamos el progressbar indefinido
        [alertpb destroy];
        
        
        //se procesa la respuesta del servidor.
        if([responseString hasPrefix:@"OK"]){
            
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"ok",nil) message: NSLocalizedString(@"proceso_finalizado_trifasico",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles: nil];
            
            UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(75, 6, 40, 40)];
            
            NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"ok_mini.png"]];
            UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
            [imageView setImage:bkgImg];
            [bkgImg release];
            [path release];
            
            [alert addSubview:imageView];
            [imageView release];
            
            [alert show];
            [alert release];
        }
        else{
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil)  message: NSLocalizedString(@"error_proceso_firma",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
            
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

        }
        
       
        
    }
    // Se recogen los datos del servidor
    else if (retrievingDataFromServlet){
        retrievingDataFromServlet=false;
        //Obtenemos la respuesta del servidor.
        NSString* responseString = [[NSString alloc] initWithData:receivedData encoding:NSUTF8StringEncoding];
        
        NSLog(@"Respuesta del rtserver: %@", responseString);
        
        @try {
            
            NSString *base64 = [responseString substringFromIndex:2];
            base64 = [CADESSignUtils urlSafeDecode:base64];
            NSData *encoded = [CADESSignUtils base64DecodeString: base64];
            NSData *decoded = NULL;
            
            decoded = [CADESSignUtils DesDecrypt: cipherKey:encoded];
            
            //deshacemos el pading especial
            NSData *finalDecoded = [NSData dataWithBytes: encoded length:decoded.length - [[responseString substringToIndex:1] intValue] ];
            
            
            datosInUse = [[NSString alloc] initWithString:[CADESSignUtils urlSafeEncode:[CADESSignUtils encodeBase64:finalDecoded]]];
        }
        @catch (NSException *exception) {
            NSLog(@"Se ha producido un error al obtener el fichero: %@", exception.description );
        }
    }
    //Obtenemos la postfirma
    else if(postSign){
        postSign = false;
        
        //Obtenemos la respuesta del servidor.
        NSString* responseString = [[NSString alloc] initWithData:receivedData encoding:NSUTF8StringEncoding];
        
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
            
            [self errorReportAsync:errorToSend];
            
            //Se muestra el mensaje de respuesta al usuario.
            
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_proceso_firma",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
            
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
            
        }
        
    }
    // la respuesta a un reporte de error
    else if(reportError){
        reportError = false;
        //responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSLog(@"Respuesta del servidor: %@",[[NSString alloc] initWithData:receivedData encoding:NSUTF8StringEncoding]);
        [alertpb destroy];
    }
    //en cualquier otro caso, se trataría la prefirma.
    else{
        NSString *dataReceibedb64 = [CADESSignUtils urlSafeDecode:receivedString];
        [self Sign:dataReceibedb64];
    }
    
    // release the connection, and the data object
    [connection release];
    [receivedData release];
}

/**************************/
/*** PREOTECCIONES SSL ****/
/**************************/


//para las protecciones ssl

- (BOOL)connection:(NSURLConnection *)connection canAuthenticateAgainstProtectionSpace:(NSURLProtectionSpace *)protectionSpace {
    return [protectionSpace.authenticationMethod isEqualToString:NSURLAuthenticationMethodServerTrust];
}

/*
//Acepta todos las conexiones ssl
//Deprecado a partir de ios 5.
- (void)connection:(NSURLConnection *)connection didReceiveAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge {
    [challenge.sender useCredential:[NSURLCredential credentialForTrust:challenge.protectionSpace.serverTrust] forAuthenticationChallenge:challenge];
}
*/
//Acepta todas las conexiones ssl
//Nuevo método no deprecado
-(void)connection:(NSURLConnection *)connection willSendRequestForAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge{
    [challenge.sender useCredential:[NSURLCredential credentialForTrust:challenge.protectionSpace.serverTrust] forAuthenticationChallenge:challenge];
}

/*
 
 - (BOOL)connection:(NSURLConnection *)connection canAuthenticateAgainstProtectionSpace:(NSURLProtectionSpace *)protectionSpace {
 return true;
 }
 
 - (void)connection:(NSURLConnection *)connection didReceiveAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge {
 NSLog(@"Challenge Accepted");
 if ([challenge previousFailureCount] == 0) {
 if ([[challenge protectionSpace] authenticationMethod] == NSURLAuthenticationMethodServerTrust) {
 NSLog(@"Challenge Trust");
 [challenge.sender useCredential:[NSURLCredential credentialForTrust:challenge.protectionSpace.serverTrust] forAuthenticationChallenge:challenge];
 } else if ([[challenge protectionSpace] authenticationMethod] == NSURLAuthenticationMethodClientCertificate) {
 NSLog(@"Challenge Certificate");
 SecIdentityRef identity = [self getClientCertificate];
 
 NSURLCredential *identityCredential = [NSURLCredential credentialWithIdentity:identity certificates:nil persistence:NSURLCredentialPersistenceForSession];
 [challenge.sender useCredential:identityCredential forAuthenticationChallenge:challenge];
 }
 } else {
 NSLog(@"Cancel Challenges");
 [challenge.sender cancelAuthenticationChallenge:challenge];
 }
 }
 
 */

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
    
    //destruimos la barra de progreso
    [alertpb destroy];
    
    //Notificamos del error al servidor
    NSString *errorToSend = @"";
    errorToSend = [errorToSend stringByAppendingString:ERROR_SIGNING];
    errorToSend = [errorToSend stringByAppendingString:ERROR_SEPARATOR];
    errorToSend = [errorToSend stringByAppendingString:DESC_ERROR_SIGNING];
    
    //[self errorReportAsync:errorToSend];
    
    //mostramos un mensaje con el error producido.
    
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"error_conexion_servidor",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
    
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
    
    //[signButton setTitle:@"Cerrar aplicación" forState:UIControlStateNormal];
    self.signButton.userInteractionEnabled = NO;
    self.signButton.enabled=NO;
    return;
}

/**
 Método que trata los datos recibidos del servidor en base64.
 Una vez tratados los datos, se ejecuta la firma y se invoca la llamada para la ejecución del post.
 
 parámetros:
 -----------
 dataReceibedb64: Cadena recibida del servidor y que contiene un properties de java.
 
 */
-(void) Sign: (NSString*) dataReceibedb64{
    
    //Se reciben los datos en base64 y se decodifican
    NSData *dataReceibed = [CADESSignUtils base64DecodeString: dataReceibedb64];
    NSString* stringDataReceibed = [NSString stringWithUTF8String:[dataReceibed bytes]];
    
    NSLog(@"Datos recibidos en respuesta a la prefirma (properties): %@", stringDataReceibed);
    
    //Los datos recibidos son un properties de java y se convierten por tanto a un NSDictionary
    NSDictionary *dict = [ CADESSignUtils javaProperties2Dictionary:stringDataReceibed];
    
     //Contamos los datos del tipo PRE.X
    int count = 0;
    NSInteger count2 = [dict count];
    id objects[count2];
    id keys[count2];
    [dict getObjects:objects andKeys:keys];
    
    for (int i = 0; i < count2; i++) {
        if ([keys[i] rangeOfString:@"PRE."].location != NSNotFound) {
            count++;
        } 
    }
   
    
    if(count==0){
        NSLog(@"El servidor no ha informado del numero de firmas que envía.");
        //mostramos un mensaje con el error producido.
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_proceso_firma",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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
        
        return;
    }
    
    NSMutableDictionary *dataProcessed = [NSMutableDictionary dictionaryWithDictionary:dict];
    
    for (int i = 0; i< count; i++){
        
        //Obtenemos la prefirma PRE.X
        NSString *element = PROPERTY_NAME_PRESIGN_PREFIX;
        element = [ element stringByAppendingString:[NSString stringWithFormat:@"%d",i]];
        
        NSString *elementx = [dict objectForKey:element];
        if(elementx== NULL){
            NSLog(@"El servidor no ha devuelto la prefirma %d : %@", i, stringDataReceibed);
                        
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_proceso_firma",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
            
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
            
            return;
        }
        NSData *data = [CADESSignUtils base64DecodeString:elementx];
        if(data.length>0){
            NSLog(@"Se pasa a realizar la firma PKCS1 de la prefirma %d",i);
            //Con los datos de la prefirma decodificados, se procede a realizar la firma pkcs1.
            NSData *dataSigned = [CADESSignUtils signPkcs1:signAlgoInUse privateKey:&privateKey data:data];
            NSString *elementp = [PROPERTY_NAME_PKCS1_SIGN_PREFIX stringByAppendingString:[NSString stringWithFormat:@"%d",i]];
            [dataProcessed setObject:[CADESSignUtils encodeBase64:dataSigned] forKey:elementp];
                       
        }
        NSString *needPre = [dict objectForKey:PROPERTY_NAME_NEED_PRE];
        if([needPre isEqualToString:@"false"]){
            [dataProcessed removeObjectForKey:element];
        }
        
    }
    
    
    
    NSString *paramsEncoded = [CADESSignUtils dictionary2JavaProperties:dataProcessed];
    paramsEncoded = [paramsEncoded stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    
    //Creamos la cadena de envío al servidor POST
    NSString *post =[[NSMutableString alloc] init];
    post = [post stringByAppendingString:PARAMETER_NAME_OPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:OPERATION_POSTSIGN];
    post = [post stringByAppendingString:HTTP_AND];
    
    post = [post stringByAppendingString:PARAMETER_NAME_COPERATION];
    post = [post stringByAppendingString:HTTP_EQUALS];
    post = [post stringByAppendingString:operation];
    post = [post stringByAppendingString:HTTP_AND];
    
    //Se pasa a formato properties codificado en b64
    NSString *needData = [dict objectForKey:PROPERTY_NAME_NEED_DATA];
    if([needData isEqualToString:@"true"]){        
        post = [post stringByAppendingString:PARAMETER_NAME_DOCID];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:datosInUse];
        post = [post stringByAppendingString:HTTP_AND];
    }
    
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
    post = [post stringByAppendingString:[CADESSignUtils urlSafeEncode: self.base64UrlSafeCertificateData]];
    
    //la siguiente línea quita el ultimo caracter que debe ser de nueva línea "\n" esto estaba
    //fastidiando los demás parámetros.
    post = [post substringToIndex:[post length] - 1];
    
    //PARAMETERS: PARAMETROS EXTRA
    if(extraParams !=NULL){
        post = [post stringByAppendingString:HTTP_AND];
        post = [post stringByAppendingString:PARAMETER_NAME_EXTRA_PARAM];
        post = [post stringByAppendingString:HTTP_EQUALS];
        post = [post stringByAppendingString:extraParams];
    }
    
    post = [post stringByAppendingString:HTTP_AND];
    post = [post stringByAppendingString:PROPERTY_NAME_SESSION_DATA_PREFIX];
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
    //NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:nil error:nil];
    
    postSign= true;
    
    NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
    [connection start];
    
}

/**
 Método que realiza la operacion "put" de la firma en el servidro de almacenamiento. La invocación al servidor es mediante una petición POST.
 
 parámetros:
 -----------
 dataSign: firma que será almacenada en el servidor.
 
 */
-(void)storeData:(NSString*) dataSign
{
    
    if(localSignature){
        //quitamos el progressbar indefinido
        [alertpb destroy];
        
        NSLog(@"Se pasa a guardar la firma en DROPBOX");
        AODropBoxOperations *dbOperation = [[AODropBoxOperations alloc] init];
        [dbOperation saveFile:dataSign filename:[cloudName stringByAppendingString:@".csig"] controller:self ];
        
    }
    else{
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
        NSString *dataDecoded = [CADESSignUtils urlSafeDecode:dataSign];
        NSData *data = [CADESSignUtils base64DecodeString: dataDecoded];
        
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
        
        NSLog(@"\n\n");
        NSLog(@"Realizamos el storage de la firma con los siguientes parámetros: %@", post);
        NSLog(@"\n\n");
        
        
        //realizamos la llamada al servidor.
        //NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:nil error:nil];
        
        storingData= true;
        
        NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
        [connection start];
    }
    
}

/**
 Método que notifica de un error en la aplicación al servidor de guardado de firmas "storage" de forma síncrona.
 
 parámetros:
 -----------
 dataSign: error producido.
 
 */
-(NSString *) loadDataFromRtservlet:(NSString*) fileId rtServlet:(NSString *)rtServlet
{
    
    NSString* responseString = NULL;
    
    if(rtServlet!=NULL && docId != NULL){
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
        //NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:nil error:nil];
        
        retrievingDataFromServlet = true;
        
        NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
        [connection start];
        
    }
    
    return responseString;
    
    
}


/**
 Método que notifica de un error en la aplicación al servidor de guardado de firmas "storage" de forma síncrona.
 
 parámetros:
 -----------
 dataSign: error producido.
 
 */
-(void) errorReportAsync:(NSString*) error
{
    
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
        
        reportError = true;
        NSURLConnection* connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
        [connection start];
        
    }
}

#pragma mark AlertView Delegate
-(void) alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex
{
    [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)dealloc {
    [_nombreCert release];
    [_signButton release];
    [super dealloc];
}
- (void)viewDidUnload {
    [self setNombreCert:nil];
    [self setSignButton:nil];
    [super viewDidUnload];
}
@end
