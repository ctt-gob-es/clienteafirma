//
//  AODropBoxOperations.m
//  SignSample02
//
//

#import "AODropBoxOperations.h"
#import <DropboxSDK/DropboxSDK.h>
#import "CADESSignUtils.h"
#import "NSData+Base64.h"

@implementation AODropBoxOperations

NSString *cloudNameDropbox;
UIViewController *cloudController;

@synthesize restClient;

- (DBRestClient *)restClient {
    if (!restClient) {
        restClient =
        [[DBRestClient alloc] initWithSession:[DBSession sharedSession]];
        restClient.delegate = self;
    }
    return restClient;
}

-(BOOL *) saveFile:(NSString*)signature filename:(NSString *)filename controller:(UIViewController *) controller{

    cloudController = controller;
    
    NSURL *documentDir = [[[NSFileManager defaultManager] URLsForDirectory:NSDocumentDirectory inDomains:NSUserDomainMask] objectAtIndex:0];
    NSURL *tmpDir = [[documentDir URLByDeletingLastPathComponent] URLByAppendingPathComponent:@"tmp" isDirectory:YES];
    
    
    NSString *directory = [tmpDir path];
    directory= [directory stringByAppendingString:@"/"];
    //ponemos la fecha
    //NSDate *currentTime = [NSDate date];
    //NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
    //[dateFormatter setDateFormat:@"dd-MM-yyyy (hh mm ss) "];
    //NSString *resultString = [dateFormatter stringFromDate: currentTime];
    //directory= [directory stringByAppendingString:resultString];
    directory= [directory stringByAppendingString:filename];
    
    NSString *dataDecoded = [CADESSignUtils urlSafeDecode:signature];
    NSData *data = [CADESSignUtils base64DecodeString: dataDecoded];
    [data writeToFile:directory atomically:YES];
    
    //filename = [resultString stringByAppendingString:filename];
    filename = [filename stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
    
    cloudNameDropbox = [[NSString alloc] initWithString:filename];
    
    NSString *destDir = @"/";
    [[self restClient] uploadFile:filename toPath:destDir withParentRev:nil fromPath:directory];

    return true;
}

- (void)restClient:(DBRestClient*)client uploadedFile:(NSString*)destPath
from:(NSString*)srcPath metadata:(DBMetadata*)metadata {
    
    NSLog(@"File uploaded successfully to path: %@", metadata.path);
    
    //Se muestra el mensaje de respuesta al usuario.
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"ok",nil) message:[ NSLocalizedString(@"finalizado",nil) stringByAppendingString:cloudNameDropbox] delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles: nil];
    
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

- (void)restClient:(DBRestClient*)client uploadFileFailedWithError:(NSError*)error {
    NSLog(@"File upload failed with error - %@", error);
    //Se muestra el mensaje de respuesta al usuario.
    
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message:[ NSLocalizedString(@"error_guardar_firma",nil) stringByAppendingString:[error description]] delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
    
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

#pragma mark AlertView Delegate
-(void) alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex
{
    [cloudController performSegueWithIdentifier:@"toFirstScreen" sender:cloudController];
    
}

@end
