//
//  AODropBoxCloudViewController.m
//  SignSample02
//
//

#import "AODropBoxCloudViewController.h"
#import <DropboxSDK/DropboxSDK.h>
#import <QuartzCore/QuartzCore.h>
#import "AlertProgressBar.h"
#import "AOUrlDropbox.h"
#import "AOPinDropBoxCloudViewController.h"

@interface AODropBoxCloudViewController () <DBRestClientDelegate>

- (NSString*)storePath;

@property (nonatomic, readonly) DBRestClient* restClient;

@end

@implementation AODropBoxCloudViewController

@synthesize dbxTblView;
AlertProgressBar *alertpbdb = NULL;
NSString *cellSelectedDropBox = NULL;
NSString *storeRoot = nil;
NSString *paramDirectory = NULL;
bool started2 = true;

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
    
     [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(onGoingToBackGround:) name:UIApplicationDidEnterBackgroundNotification object:nil];
    
    if (![[DBSession sharedSession] isLinked]) {
        
        AOUrlDropbox *obj=[AOUrlDropbox getInstance];
        
        NSURL *nsurl = [NSURL URLWithString:[obj.str stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding]];
        [[DBSession sharedSession] handleOpenURL:nsurl];
        [[DBSession sharedSession] linkFromController:self];
    }
    
	// Do any additional setup after loading the view.
    
    if ([DBSession sharedSession].root == kDBRootDropbox) {
        storeRoot = @"/Almacenes@firma";
    } else {
        storeRoot = @"/";
    }
    
    [self.restClient loadMetadata:storeRoot withHash:storeHash];
    
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
    
    if(started2)
    {
        started2=false;
        //iniciamos la barra de progreso.
        alertpbdb = [[AlertProgressBar alloc]init];
        [alertpbdb createProgressBar:self.view];
    }
    
    self.screenName = @"IOS AODropBoxCloudViewController - Dropbox cloud cert list";
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (NSString*)storePath {
    return [NSTemporaryDirectory() stringByAppendingPathComponent:@"store.pfx"];
}

//cuando se pulsa el botón del centro
-(void)onGoingToBackGround:(NSNotification*) notification {
    //Destruimos la sesión
    [[DBSession sharedSession] unlinkAll];
    //quitamos el progressbar indefinido
    [alertpbdb destroy];
    @try {
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
    }
}

-(IBAction)buttonPressed:(id)sender {
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

-(IBAction)saveStorebuttonPressed:(id)sender{
    if(cellSelectedDropBox != NULL){
        
        //iniciamos la barra de progreso.
        alertpbdb = [[AlertProgressBar alloc]init];
        [alertpbdb createProgressBar:self.view];
        
        NSURL *documentDir = [[[NSFileManager defaultManager] URLsForDirectory:NSDocumentDirectory inDomains:NSUserDomainMask] objectAtIndex:0];
        NSURL *tmpDir = [[documentDir URLByDeletingLastPathComponent] URLByAppendingPathComponent:@"tmp" isDirectory:YES];
        
        NSString *origDirectory = [@"/" stringByAppendingString:cellSelectedDropBox];
        NSString *destDirectory = [tmpDir path];
        destDirectory = [destDirectory stringByAppendingString:@"/"];
        destDirectory = [destDirectory stringByAppendingString:cellSelectedDropBox];
        
        [[self restClient] loadFile:origDirectory intoPath:destDirectory];
        
        paramDirectory = destDirectory;
        
       NSLog(@"Fichero temporal copiado desde origen : %@",origDirectory);
       NSLog(@"Fichero temporal copiado hasta destino : %@",destDirectory);
        
         [self performSegueWithIdentifier:@"toPinDropBox" sender:self];
       
    }
    else{
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"atencion",nil) message: NSLocalizedString(@"seleccion_almacen_guradar",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles: nil];
        
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
    [alertpbdb destroy];
    if ([[segue identifier] isEqualToString:@"toPinDropBox"]) {
        
        // Get destination view
        AOPinDropBoxCloudViewController *vc = [segue destinationViewController];
        
        // Set the selected button in the new view
        [vc setParamNameCertDropBox:cellSelectedDropBox];
        [vc setParamUrlCertDropBox:paramDirectory];
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
    imageView.image = [UIImage imageNamed:@"certificados.png"];
    
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
    cellSelectedDropBox = myTextLabel.text;
}

/***********************************************************************/
/******** METODOS IMPLEMENTADOS DE LA API DE DROPBOX  ******************/
/***********************************************************************/

#pragma mark DBRestClientDelegate methods

- (void)restClient:(DBRestClient*)client loadedMetadata:(DBMetadata*)metadata {
    [storeHash release];
    storeHash = [metadata.hash retain];
    
    NSArray* validExtensions = [NSArray arrayWithObjects:@"pfx", @"p12", nil];
    NSMutableArray* newStorePaths = [NSMutableArray new];
    for (DBMetadata* child in metadata.contents) {
        NSString* extension = [[child.path pathExtension] lowercaseString];
        if (!child.isDirectory && [validExtensions indexOfObject:extension] != NSNotFound) {
            [newStorePaths addObject:child.filename];
        }
    }
    [storePaths release];
    storePaths = newStorePaths;
    
    [dbxTblView reloadData];
    
    //quitamos el progressbar indefinido
    [alertpbdb destroy];
    started2=true;
}

- (void)restClient:(DBRestClient*)client metadataUnchangedAtPath:(NSString*)path {
    //[self loadRandomPhoto];
    //quitamos el progressbar indefinido
    [alertpbdb destroy];
}

- (void)restClient:(DBRestClient*)client loadMetadataFailedWithError:(NSError*)error {
    NSLog(@"restClient:loadMetadataFailedWithError: %@", [error localizedDescription]);
    //[self displayError];
    //[self setWorking:NO];
    //quitamos el progressbar indefinido
    [alertpbdb destroy];
}

- (void)restClient:(DBRestClient*)client loadedThumbnail:(NSString*)destPath {
    //[self setWorking:NO];
    //imageView.image = [UIImage imageWithContentsOfFile:destPath];
    //quitamos el progressbar indefinido
    [alertpbdb destroy];
}

- (void)restClient:(DBRestClient*)client loadThumbnailFailedWithError:(NSError*)error {
    //[self setWorking:NO];
    //[self displayError];
    //quitamos el progressbar indefinido
    [alertpbdb destroy];
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
    [alertpbdb destroy];
    NSLog(@"File loaded into path: %@", localPath);
}

- (void)restClient:(DBRestClient*)client loadFileFailedWithError:(NSError*)error {
    [alertpbdb destroy];
    
    
    
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"error_descarga_fichero",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
    
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


- (void)dealloc {
    [LogOffDropbox release];
    [saveStore release];
    [super dealloc];
}
- (void)viewDidUnload {
    [LogOffDropbox release];
    LogOffDropbox = nil;
    [saveStore release];
    saveStore = nil;
    [super viewDidUnload];
}
@end
