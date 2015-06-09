//
//  DocumentCertificatesViewController.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 19/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "DocumentCertificatesViewController.h"
#import "CertificateUtils.h"

@interface DocumentCertificatesViewController ()

@end

@implementation DocumentCertificatesViewController

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];

    return self;
}

// Custom initialization using story board
- (id)initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];

    if (self) {
        waitingForDelete = NO;
        watingForRegister = NO;
    }

    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];

    _selectedCertificate = nil;
    files = [self findFiles:[NSArray arrayWithObjects:@"p12", @"pfx", nil]];

    if ([files count ] == 0) {
        _messageView.text = @"La aplicación esta solicitando acceso a su almacen de certificados y no dispone de ninguno registrado.\n\n  Para instalar su certificado :\n 1. Conecte su dispositivo a su PC o Mac.\n 2. Localice el certificado que desea instalar ....(debe conocer el pin del certificado)\n3. En iTunes seleccione su certificado y arrástrelo a la ventana de documentos...\n4. Vuelva a esta pantalla y registrelo en el almacen del dispositivo.\n";
        [_messageView sizeToFit];
    }

    // Tabulacion de la tabla
    self.tableView.contentInset = UIEdgeInsetsMake(20, 0, 10, -30);
    [self.tableView setTableFooterView:[[UIView alloc] initWithFrame:CGRectZero]];
}

// Dispose of any resources that can be recreated.
- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    T21LogDebug(@"didReceiveMemoryWarning");

}

- (IBAction)didTapOnBackButton:(id)sender
{
    [self dismissViewControllerAnimated:YES completion:nil];
}

#pragma mark - Table view data source
// Return the number of sections.
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{

    return 1;
}

// Return the number of rows in the section.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{

    return [files count];
}

// Configure the tableview cell
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"CertificateFileCell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier ];

    cell.textLabel.text = [files objectAtIndex:[indexPath row]];

    return cell;
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    T21LogDebug(@"didSelectRowAtIndexPath row=%ld", (long)[indexPath row]);

    _selectedCertificate = [files objectAtIndex:[indexPath row]];
}

// Find files in Document directory
- (NSArray *)findFiles:(NSArray *)extensions
{

#if TARGET_IPHONE_SIMULATOR

    NSMutableArray *arrayCertsMut = [[NSMutableArray alloc] init];

    [arrayCertsMut addObject:@"ANF_PF_Activo"];
    [arrayCertsMut addObject:@"PFActivoFirSHA1"];

    return arrayCertsMut;

#else

    NSMutableArray *matches = [@[] mutableCopy];
    NSFileManager *fManager = [NSFileManager defaultManager];
    NSString *item;
    NSString *ext;
    NSArray *contents = [fManager contentsOfDirectoryAtPath:[NSHomeDirectory() stringByAppendingPathComponent:@"Documents"] error:nil];

    // >>> this section here adds all files with the chosen extension to an array
    for (item in contents) {
        for (ext in extensions) {
            if ([[item pathExtension] isEqualToString:ext]) {
                [matches addObject:item];
            }
        }
    }

    return matches;

#endif /* if TARGET_IPHONE_SIMULATOR */
}

- (void)viewDidUnload
{
    [SVProgressHUD dismiss];
    [super viewDidUnload];
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    T21LogDebug(@"DocumentCertificatesViewController::prepareForSegue identifier=%@", [segue identifier]);

    if ([segue.identifier isEqualToString:@"segueModalCertificates"]) {
        NSIndexPath *selectedRowIndexPath = [self.tableView indexPathForSelectedRow];
        T21LogDebug(@"DocumentCertificatesViewController::prepareForSegue selected index=%ld", (long)[selectedRowIndexPath row]);

        // Sets data in Aplication delegate objet to be shared for the application's tab
        _selectedCertificate = [files objectAtIndex:[selectedRowIndexPath row]];
        ModalCertificatesController *modalController  = [segue destinationViewController];
        modalController.selectedCertificate = _selectedCertificate;
        modalController.modalPresentationStyle = 17;
        [modalController setDelegate:self];
    }
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    UIInterfaceOrientation des = self.interfaceOrientation;

    if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad) { // iPad
        if (des == UIInterfaceOrientationPortrait || des == UIInterfaceOrientationPortraitUpsideDown) { // ipad-portairait

        } else { // ipad -landscape

        }
    } else { // iphone
        UIInterfaceOrientation des = self.interfaceOrientation;

        if (des == UIInterfaceOrientationPortrait || des == UIInterfaceOrientationPortraitUpsideDown) { // iphone portrait

        } else { // iphone -landscape

        }
    }

    return YES;
}

#pragma mark - ModalCertificatesControllerDelegate

- (void)certificateAdded
{
    [self didTapOnBackButton:nil];
}

@end
