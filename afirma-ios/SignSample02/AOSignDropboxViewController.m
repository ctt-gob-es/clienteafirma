//
//  AOSignDropboxViewController.m
//  SignSample02
//
//

#import "AOSignDropboxViewController.h"
#import <DropboxSDK/DropboxSDK.h>
#import <QuartzCore/QuartzCore.h>
#import "AlertProgressBar.h"
#import "AOUrlDropbox.h"
#import "AOCertificateSelectionViewController.h"
#import "CADESConstants.h"
#import "CADESSignUtils.h"

@interface AOSignDropboxViewController ()

- (NSString*)storePath;

@property (nonatomic, readonly) DBRestClient* restClient;

@end

@implementation AOSignDropboxViewController

@synthesize dbxTblView;
AlertProgressBar *alertpbdbs = NULL;
NSString *cellSelectedDropBoxSign = NULL;
NSString *storeRootSign = nil;
NSString *paramDirectorySign = NULL;
NSString *filePath = NULL;
bool started = true;

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
	// Do any additional setup after loading the view.
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(onGoingToBackGround:) name:UIApplicationDidEnterBackgroundNotification object:nil];
    
    if (![[DBSession sharedSession] isLinked]) {
        
        AOUrlDropbox *obj=[AOUrlDropbox getInstance];
        
        NSURL *nsurl = [NSURL URLWithString:[obj.str stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding]];
        [[DBSession sharedSession] handleOpenURL:nsurl];
        [[DBSession sharedSession] linkFromController:self];
    }
       
    
	// Do any additional setup after loading the view.
    
    if ([DBSession sharedSession].root == kDBRootDropbox) {
        storeRootSign = @"/Almacenes@firma";
    } else {
        storeRootSign = @"/";
    }
    
    [self.restClient loadMetadata:storeRootSign withHash:storeHash];
    
    //definimos los bordes de la tabla.
    self.dbxTblView.layer.borderWidth = 0.5;
    self.dbxTblView.layer.borderColor = [[UIColor grayColor] CGColor];
    self.dbxTblView.layer.cornerRadius = 6.0f;
    
    //definimos la seleccion de la tabla
    /*
     if([tableData2 count]>0){
     //ponemos el primer elemento seleccionado
     NSIndexPath *indexPath=[NSIndexPath indexPathForRow:0 inSection:0];
     [#import <QuartzCore/QuartzCore.h> selectRowAtIndexPath:indexPath animated:YES  scrollPosition:UITableViewScrollPositionBottom];
     }
     */
    
    if(started)
    {
        started=false;
        //iniciamos la barra de progreso.
        alertpbdbs = [[AlertProgressBar alloc]init];
        [alertpbdbs createProgressBar:self.view];
    }
    
    self.screenName = @"IOS AOSignDropBoxViewController - DropBox cloud signature process start";

}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (NSString*)storePath {
    return [NSTemporaryDirectory() stringByAppendingPathComponent:@"signature.csig"];
}

//cuando se pulsa el botón del centro
-(void)onGoingToBackGround:(NSNotification*) notification {
    //Destruimos la sesión
    [[DBSession sharedSession] unlinkAll];
    //quitamos el progressbar indefinido
    [alertpbdbs destroy];
    @try {
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
    }
}

-(IBAction)backButtonPressed:(id)sender{
    @try {
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
        NSLog(@"Error segue : %@",e);
    }
}

-(IBAction)logOutButtonPressed:(id)sender{
    //Destruimos la sesión
    [[DBSession sharedSession] unlinkAll];
    @try {
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
        NSLog(@"Error segue : %@",e);
    }
}

-(IBAction)signButtonPressed:(id)sender{
    if(cellSelectedDropBoxSign != NULL){
        
        //iniciamos la barra de progreso.
        alertpbdbs = [[AlertProgressBar alloc]init];
        [alertpbdbs createProgressBar:self.view];
        
        NSURL *documentDir = [[[NSFileManager defaultManager] URLsForDirectory:NSDocumentDirectory inDomains:NSUserDomainMask] objectAtIndex:0];
        NSURL *tmpDir = [[documentDir URLByDeletingLastPathComponent] URLByAppendingPathComponent:@"tmp" isDirectory:YES];
        
        NSString *origDirectory = [@"/" stringByAppendingString:cellSelectedDropBoxSign];
        NSString *destDirectory = [tmpDir path];
        destDirectory = [destDirectory stringByAppendingString:@"/"];
        destDirectory = [destDirectory stringByAppendingString:cellSelectedDropBoxSign];
        
        [[self restClient] loadFile:origDirectory intoPath:destDirectory];
        
        paramDirectorySign = destDirectory;
        
        NSLog(@"Fichero temporal copiado desde origen : %@",origDirectory);
        NSLog(@"Fichero temporal copiado hasta destino : %@",destDirectory);
               
        
    }
    else{
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:
                              NSLocalizedString(@"atencion",nil) message:
                              NSLocalizedString(@"seleccion_fichero_firma",nil) delegate:self cancelButtonTitle:
                              NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
                
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

}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    //quitamos el progressbar indefinido
    [alertpbdbs destroy];
    if ([[segue identifier] isEqualToString:@"toSelectionScreen"]) {
        NSLog(@"Pasamos a la pantalla de seleccion de Certificado5");
        
        NSString *generatedURL = [self generateURL];
        // Get destination view
        AOCertificateSelectionViewController *vc = [segue destinationViewController];
        
        // Set the selected button in the new view
        // Set the selected button in the new view
        [vc setStartUrl:generatedURL];
    }
}

/***********************************************************************/
/******** METODOS IMPLEMENTADOS DE LA TABLA DE CERTIFICADOS DRBX *******/
/***********************************************************************/

#pragma mark -
#pragma mark Table view data source
// Detalla el numbre de secciones en la tabla.
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

// Detalla el número de filas en la tabla.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return [storePaths count];
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
    imageView.image = [UIImage imageNamed:@"imagen-fichero_pequeño.png"];
    
    //Label
    UILabel *lblFor = [[UILabel alloc]initWithFrame:CGRectMake(60, 15, 360, 21)];
    lblFor.text = [storePaths objectAtIndex:indexPath.row];
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
    cellSelectedDropBoxSign = myTextLabel.text;
}

/***********************************************************************/
/******** METODOS IMPLEMENTADOS DE LA API DE DROPBOX  ******************/
/***********************************************************************/

#pragma mark DBRestClientDelegate methods

- (void)restClient:(DBRestClient*)client loadedMetadata:(DBMetadata*)metadata {
    [storeHash release];
    storeHash = [metadata.hash retain];
   
    NSMutableArray* newStorePaths = [NSMutableArray new];
    for (DBMetadata* child in metadata.contents) {
        if (!child.isDirectory ) {
            [newStorePaths addObject:child.filename];
        }
    }
    [storePaths release];
    storePaths = newStorePaths;
    
    [dbxTblView reloadData];
    
    //quitamos el progressbar indefinido
    [alertpbdbs destroy];
    started=true;
    //[self loadRandomPhoto];
}

- (void)restClient:(DBRestClient*)client metadataUnchangedAtPath:(NSString*)path {
    //[self loadRandomPhoto];
    //quitamos el progressbar indefinido
    [alertpbdbs destroy];
    
}

- (void)restClient:(DBRestClient*)client loadMetadataFailedWithError:(NSError*)error {
    NSLog(@"restClient:loadMetadataFailedWithError: %@", [error localizedDescription]);
    //[self displayError];
    //[self setWorking:NO];
    //quitamos el progressbar indefinido
    [alertpbdbs destroy];
}

- (void)restClient:(DBRestClient*)client loadedThumbnail:(NSString*)destPath {
    //[self setWorking:NO];
    //imageView.image = [UIImage imageWithContentsOfFile:destPath];
    //quitamos el progressbar indefinido
    [alertpbdbs destroy];
}

- (void)restClient:(DBRestClient*)client loadThumbnailFailedWithError:(NSError*)error {
    //[self setWorking:NO];
    //[self displayError];
    //quitamos el progressbar indefinido
    [alertpbdbs destroy];
}

- (DBRestClient*)restClient {
    if (restClient == nil) {
        restClient = [[DBRestClient alloc] initWithSession:[DBSession sharedSession]];
        restClient.delegate = self;
    }
    return restClient;
}


- (void)restClient:(DBRestClient*)client loadedFile:(NSString*)localPath
       contentType:(NSString*)contentType metadata:(DBMetadata*)metadata {
    [alertpbdbs destroy];
    NSLog(@"File loaded into path: %@", localPath);
    filePath = localPath;
    //Se ha descargado el fichero y ya podemos pasar a la siguiente transicion
    [self performSegueWithIdentifier:@"toSelectionScreen" sender:self];
}

- (void)restClient:(DBRestClient*)client loadFileFailedWithError:(NSError*)error {
    [alertpbdbs destroy];
    
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_descarga_fichero",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
    
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
    
    NSLog(@"There was an error loading the file - %@", error);
}

-(NSString *) generateURL{
    
    /*
     URL TOMADA DE REFERENCIA
    afirma://sign/?
     op=sign
     &key=21289893
     &id=001215677026
     &metro=true
     &stservlet=http://172.24.22.106:8080/SignatureStorageServer/storage
     &rtservlet=http://172.24.22.106:8080/SignatureStorageServer/retrieve
     &format=CAdES
     &algorithm=SHA1withRSA
     &dat=VGV4dG8gZGUgcHJ1ZWJh
    */
    
    NSString *URL = @"afirma://";
    URL = [URL stringByAppendingString:OPERATION_SIGN];
    URL = [URL stringByAppendingString:@"/"];
    URL = [URL stringByAppendingString:HTTP_CGI];
    
    //NSString *URL =PARAMETER_NAME_OPERATION;
    URL = [URL stringByAppendingString:PARAMETER_NAME_OPERATION];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:OPERATION_SIGN];
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_CIPHER_KEY];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:@"00000000"];
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_ID];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:@"000000000000"];
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_METRO];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:@"false"];
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_STSERVLET];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:@"http://"];
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_RTSERVLET];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:@"http://"];
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_FORMAT];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:CADES_FORMAT];
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_ALGORITHM2];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:@"SHA512withRSA"];
    
    //Esto es para identificar que la firma es local y que el resultado debe guardarse en dropbox.
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_LOCAL_SIGNATURE];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:@"true"];
    
    //Nombre del fichero
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_LOCAL_CLOUD_NAME];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:cellSelectedDropBoxSign];
    
    NSArray *keys = [NSArray arrayWithObjects:
                     //PROPERTIES_PARAMETER_POLICYIDENTIFIER,
                     //PROPERTIES_PARAMETER_POLICYIDENTIFIERHASH,
                     //PROPERTIES_PARAMETER_POLICYIDENTIFIERHASHALGORITHM,
                     //PROPERTIES_PARAMETER_POLICYQUALIFIER,
                     PROPERTIES_PARAMETER_MODE,
                     //PROPERTIES_PARAMETER_PRECALCULATEDHASHALGORITHM,
                     PROPERTIES_PARAMETER_SIGNINGCERTIFICATEV2,
                     nil];
    NSArray *objs = [NSArray arrayWithObjects:
                     //@"urn:oid:2.16.724.1.3.1.1.2.1.8",
                     //@"7SxX3erFuH31TvAw9LZ70N7p1vA=",
                     //@"http://www.w3.org/2000/09/xmldsig#sha256",
                     //@"http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf",
                     PROPERTIES_PARAMETER_MODE_IMPLICIT,
                     //@"http://www.w3.org/2000/09/xmldsig#sha256",
                     @"true",
                     nil];
   NSDictionary *dataProcessed = [NSDictionary dictionaryWithObjects:objs forKeys:keys];
        
    NSString *paramsEncoded = [CADESSignUtils dictionary2JavaProperties:dataProcessed];
    paramsEncoded = [paramsEncoded stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_PROPERTIES];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    URL = [URL stringByAppendingString:paramsEncoded];
    
    URL = [URL stringByAppendingString:HTTP_AND];
    URL = [URL stringByAppendingString:PARAMETER_NAME_DAT];
    URL = [URL stringByAppendingString:HTTP_EQUALS];
    
    //Cargamos el fichero descargado de dropbox en un string b64
    NSData *data = [[NSFileManager defaultManager] contentsAtPath:filePath];
    //NSString *datBase64 = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    NSString *datBase64 = [CADESSignUtils encodeBase64:data];
    
    URL = [URL stringByAppendingString:datBase64];
    
    return URL;
}


- (void)dealloc {
    [logout release];
    [super dealloc];
}
- (void)viewDidUnload {
    [logout release];
    logout = nil;
    [super viewDidUnload];
}
@end
