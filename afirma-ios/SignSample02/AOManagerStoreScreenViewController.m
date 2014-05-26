//
//  AOManagerStoreScreenViewController.m
//  SignSample02
//
//

#import "AOManagerStoreScreenViewController.h"
#import <DropboxSDK/DropboxSDK.h>
#import "CADESConstants.h"
#import <QuartzCore/QuartzCore.h>

@interface AOManagerStoreScreenViewController () <DBSessionDelegate, DBNetworkRequestDelegate>

@end


@implementation AOManagerStoreScreenViewController

@synthesize tblViewManager;
@synthesize tableDataManager;
NSString *cellSelectedManager = NULL;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    return self;
}
 

- (void)viewDidLoad
{
    [super viewDidLoad];
    [[self navigationController] setNavigationBarHidden:YES animated:YES];
			
    cellSelectedManager = NULL;
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(onGoingToBackGround:) name:UIApplicationDidEnterBackgroundNotification object:nil];
    
    //Rellenamos la tabla de certificados con los facilitados por iTunes y si hay almacenes, habilitamos el botón de seleccion.
    [self listFilesFromDocumentsFolder];
    
    //definimos los bordes de la tabla.
    self.tblViewManager.layer.borderWidth = 0.5;
    self.tblViewManager.layer.borderColor = [[UIColor grayColor] CGColor];
    self.tblViewManager.layer.cornerRadius = 6.0f;
    
    self.screenName = @"IOS AOManagerStoreScreenViewController - Certificate manager window";
}

-(IBAction)buttonPressed:(id)sender {
    if ([[DBSession sharedSession] isLinked]) {
        [[DBSession sharedSession] unlinkAll];
    }
    [self connectDropbox];
}


-(IBAction)deleteButtonPressed:(id)sender{
    NSLog(@"Borrando almacen %@",cellSelectedManager);
    
    if(cellSelectedManager!=NULL){
        NSString *mensaje = NSLocalizedString(@"preg_borrar_almacen",nil);
        mensaje = [mensaje stringByAppendingString:cellSelectedManager];
        mensaje = [mensaje stringByAppendingString:@"?"];
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"atencion",nil) message:mensaje delegate:self cancelButtonTitle:NSLocalizedString(@"cancelar",nil) otherButtonTitles:NSLocalizedString(@"borrar",nil),nil];
        
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
    else{
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"atencion",nil) message: NSLocalizedString(@"seleccion_almacen_borrar",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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


- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Overriden to allow any orientation.
    return YES;
}

//cuando se pulsa el botón del centro
/*
-(void)onGoingToBackGround:(NSNotification*) notification {
    @try {
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
    }
}
 */

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (void)viewDidUnload {
   
    [dropBoxButton release];
    dropBoxButton = nil;
    [deleteButton release];
    deleteButton = nil;
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}


- (void)dealloc {
	
    [dropBoxButton release];
    [deleteButton release];
    [super dealloc];
}



#pragma mark -
#pragma mark Application lifecycle

- (BOOL) connectDropbox {
    
    NSDictionary *launchOptions=nil;
    
	// Set these variables before launching the app
    NSString* appKey = URL_STORE;
	NSString* appSecret = PASS_STORE;
	NSString *root = kDBRootAppFolder; // Should be set to either kDBRootAppFolder or kDBRootDropbox
	// You can determine if you have App folder access or Full Dropbox along with your consumer key/secret
	// from https://dropbox.com/developers/apps
	
	// Look below where the DBSession is created to understand how to use DBSession in your app
			
	DBSession* session =
    [[DBSession alloc] initWithAppKey:appKey appSecret:appSecret root:root];
	session.delegate = self; // DBSessionDelegate methods allow you to handle re-authenticating
	[DBSession setSharedSession:session];
    [session release];
	
	[DBRequest setNetworkRequestDelegate:self];
    	
    
    //rootViewController.photoViewController = [[PhotoViewController new] autorelease];
    if ([[DBSession sharedSession] isLinked]) {
        //navigationController.viewControllers = [NSArray arrayWithObjects:rootViewController, rootViewController.photoViewController, nil];
    }
    
    // Add the navigation controller's view to the window and display.
    //[window addSubview:navigationController.view];
    //[window makeKeyAndVisible];
	
	NSURL *launchURL = [launchOptions objectForKey:UIApplicationLaunchOptionsURLKey];
	NSInteger majorVersion =
    [[[[[UIDevice currentDevice] systemVersion] componentsSeparatedByString:@"."] objectAtIndex:0] integerValue];
	if (launchURL && majorVersion < 4) {
		// Pre-iOS 4.0 won't call application:handleOpenURL; this code is only needed if you support
		// iOS versions 3.2 or below
		//[self application:application handleOpenURL:launchURL];
		return NO;
	}
    
    if (![[DBSession sharedSession] isLinked]) {
		[[DBSession sharedSession] linkFromController:self];
    }
    //else {
    //    [[DBSession sharedSession] unlinkAll];
    //    [[[[UIAlertView alloc]
    //       initWithTitle:@"Account Unlinked!" message:@"Your dropbox account has been unlinked"
    //       delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil]
    //      autorelease]
    //     show];
    //}
    
    return YES;
}

#pragma mark -
#pragma mark DBSessionDelegate methods

- (void)sessionDidReceiveAuthorizationFailure:(DBSession*)session userId:(NSString *)userId {
	relinkUserId = [userId retain];
	    
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"atencion",nil) message:NSLocalizedString(@"sesion_finalizada",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cancelar",nil) otherButtonTitles:NSLocalizedString(@"conectar",nil), nil];
    
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


#pragma mark -
#pragma mark UIAlertViewDelegate methods

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)index {
    
    if(index != alertView.cancelButtonIndex){
        [self deleteFileFromDocumentsFolder];
    }
    
    /*
     Si se da al boton cancelar, no se hace nada.
	if (index != alertView.cancelButtonIndex) {
		[[DBSession sharedSession] linkUserId:relinkUserId fromController:self];
	}
	[relinkUserId release];
	relinkUserId = nil;
     */
}


#pragma mark -
#pragma mark DBNetworkRequestDelegate methods

static int outstandingRequests;

- (void)networkRequestStarted {
	outstandingRequests++;
	if (outstandingRequests == 1) {
		[[UIApplication sharedApplication] setNetworkActivityIndicatorVisible:YES];
	}
}

- (void)networkRequestStopped {
	outstandingRequests--;
	if (outstandingRequests == 0) {
		[[UIApplication sharedApplication] setNetworkActivityIndicatorVisible:NO];
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
    tableDataManager = [[NSMutableArray alloc] init];
    
    [tableDataManager addObject:@"ANF USUARIO ACTIVO"];
    
#else
    if([fileList count]>0){
        //habilitamos el boton de seleccion de certificado.
        
        tableDataManager = [[NSMutableArray alloc] init];
        for (NSString *s in fileList){
            [tableDataManager addObject:s];
        }
        [tblViewManager reloadData];
    }
    else{
        [tableDataManager removeAllObjects];
        [tblViewManager reloadData];
        //Si no hay documentos, mostramos el mensaje.
        //Se muestra el mensaje de respuesta al usuario.
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"bienvenida",nil) message:NSLocalizedString(@"bienvenida_msg",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
        /*
        UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(75, 6, 40, 40)];
        
        
        NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"warning_mini.png"]];
        UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
        [imageView setImage:bkgImg];
        [bkgImg release];
        [path release];
         
        
        [alert addSubview:imageView];
        [imageView release];
        */
        [alert show];
        [alert release];
    }
#endif
    //establecemos el primer elemento como seleccionado
    //if([tableDataManager count] >0){
    //    tableDataManager = [tableDataManager objectAtIndex:0];
    //}
}

//Carga en la lista de almacenes los almacenes encontrados en Itunes.
-(void)deleteFileFromDocumentsFolder {
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    
    
    NSFileManager *manager = [NSFileManager defaultManager];
    NSArray *fileList = [manager contentsOfDirectoryAtPath:documentsDirectory error:nil];
    NSString *nameWithExtension = NULL;
    
    NSLog(@"file: %@", fileList);
    
    for (NSString* fileName in [manager contentsOfDirectoryAtPath: documentsDirectory error:nil]) {
        NSLog(@"file: %@", fileName);
        if ([fileName rangeOfString:cellSelectedManager].location != NSNotFound) {
            nameWithExtension = [[NSString alloc]initWithString:fileName];
        } 
    }
    
    if(nameWithExtension!=NULL){
        NSString *filePath = [documentsDirectory stringByAppendingString:@"/"];
        filePath = [filePath stringByAppendingString:nameWithExtension];
        
        NSError *error;
        [[NSFileManager defaultManager] removeItemAtPath: filePath error: &error];
        //if(error!=NULL){
        //    NSLog(@"No se ha podido eliminar el fichero %@. se ha producido un error %@",filePath,error.description);
        //}
        [self listFilesFromDocumentsFolder];
    }
    else{
        //Se muestra el mensaje de respuesta al usuario.
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"error_borrar_almacen",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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
        
        NSLog(@"No se ha podido eliminar el fichero %@",cellSelectedManager);
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
    return [tableDataManager count];
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
    lblFor.text = [tableDataManager objectAtIndex:indexPath.row];
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
    UILabel *myTextLabel     = (UILabel *) [selectedCell  viewWithTag:1];
    cellSelectedManager = myTextLabel.text;
}

#pragma mark -
#pragma mark UIAlertViewDelegate methods


@end




