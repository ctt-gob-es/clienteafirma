//
//  AttachmentViewController.m
//  PortaFirmas_@Firma
//
//  Created by Antonio Fiñana Sánchez on 19/10/12.
//  Copyright (c) 2012 Luis Lopez. All rights reserved.
//

#import "AttachmentViewController.h"
#import "PreviewViewController.h"
#import "Document.h"

#define SECTIONS_TITLES @[@"Documentos", @"Firmas", @"Informes de firmas"]

typedef NS_ENUM (NSInteger, PFAttachmentVCSection)
{
    PFAttachmentVCSectionDocuments,
    PFAttachmentVCSectionSignatures,
    PFAttachmentVCSectionSignaturesReport
};

@interface AttachmentViewController ()

@end

@implementation AttachmentViewController
@synthesize dataSource = _dataSource;

- (void)viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
    // Custom initialization
    self.navigationController.toolbarHidden = YES;

}

- (void)viewDidLoad
{
    [super viewDidLoad];
    [self.tableView setTableFooterView:[[UIView alloc] initWithFrame:CGRectZero]];

    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;

    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    NSInteger numberOfSections = 1;

    if (_detail && _detail.type == PFRequestTypeSign && _requestStatus == PFRequestStatusSigned) {
        numberOfSections = 3;
    }

    return numberOfSections;
}

- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section
{
    return SECTIONS_TITLES[section];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    T21LogDebug(@"AttachmentViewController::numberOfRowsInSection=%ld. rows=%ld", (long)section, (unsigned long)[_dataSource count]);

    return [_dataSource count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    T21LogDebug(@"AttachmentViewController::cellForRowAtIndexPath row=%ld", (long)[indexPath row]);

    static NSString *CellIdentifier = @"AttachmentsCell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];

    if (cell == nil) {
        T21LogError(@"AttachmentViewController::cell is nill");
    }

    [self configureCell:cell forDocument:_dataSource[indexPath.row] inSection:indexPath.section];

    return cell;
}

- (void)configureCell:(UITableViewCell *)cell forDocument:(Document *)document inSection:(NSInteger)section
{
    switch (section) {
        case PFAttachmentVCSectionDocuments:
            [cell.textLabel setText:document.nm];
            break;
        case PFAttachmentVCSectionSignatures:
            [cell.textLabel setText:[NSString stringWithFormat:@"%@_firmado.%@", document.nm, [document getSignatureExtension]]];
            break;
        case PFAttachmentVCSectionSignaturesReport:
            [cell.textLabel setText:[NSString stringWithFormat:@"report_%@.pdf", document.nm]];
            break;
    }
}

#pragma mark - Navigation

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    T21LogDebug(@"AttachmentViewController::prepareForSegue identifier=%@", [segue identifier]);
    NSIndexPath *selectedIndexPath = [self.tableView indexPathForSelectedRow];

    if ([segue.identifier isEqualToString:@"segueShowPreview"]) {

        PreviewViewController *previewViewController = [segue destinationViewController];
        // Configure the cell...
        Document *selectedDoc = _dataSource[selectedIndexPath.row];
        T21LogDebug(@"AttachmentViewController::prepareForSegue document Id:%@", [selectedDoc docid]);

        PFRequestCode requestCode = [PFHelper getPFRequestCodeForSection:selectedIndexPath.section];
        [selectedDoc prepareForRequestWithCode:requestCode];
        [previewViewController setRequestCode:requestCode];
        [previewViewController setDocId:selectedDoc.docid];
        [previewViewController setDataSource:selectedDoc];
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

@end
