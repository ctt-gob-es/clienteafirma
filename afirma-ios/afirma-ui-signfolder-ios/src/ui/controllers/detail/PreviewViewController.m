//
//  PreviewViewController.m
//  PortaFirmas_@Firma
//
//  Created by Antonio Fiñana Sánchez on 19/10/12.
//  Copyright (c) 2012 Luis Lopez. All rights reserved.
//

#import "PreviewViewController.h"
#import "PreviewXMLController.h"
// #import "NSData+Base64.h"
#import "WSDataController.h"
#import "AppDelegate.h"
#import "Base64Utils.h"
#import "XMLController.h"

@interface PreviewViewController ()
{
    BOOL _isShowingAlertView;
}

@end

@implementation PreviewViewController
@synthesize webView = _webView;
@synthesize  docId = _docId, dataSource = _dataSource;

- (id)initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];

    if (self) {
        // Custom initialization

        dataController = [[WSDataController alloc] init];
        dataController.delegate = self;
        _isShowingAlertView = NO;
    }

    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    [self loadWebService];
}

- (void)viewDidAppear:(BOOL)animated
{
    [self.parentViewController setHidesBottomBarWhenPushed:TRUE];
    [self.navigationController setToolbarHidden:YES];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)viewDidUnload
{

    [super viewDidUnload];
}

- (void)viewDidDisappear:(BOOL)animated
{
    [super viewDidDisappear:animated];

}

- (void)loadWebService
{
    [SVProgressHUD showWithMaskType:SVProgressHUDMaskTypeBlack];
    NSString *data = [PreviewXMLController buildRequestWithId:_docId];

    T21LogDebug(@"PreviewXMLController::loadWebService.message data=%@", data);

    // loadRequest
    [dataController loadPostRequestWithData:data code:_requestCode];
    [dataController startConnection];
}

- (void)didReceiveParserWithError:(NSString *)errorString
{
    [SVProgressHUD dismiss];
    T21LogDebug(@"UnassignedRequestTableViewController::didReceiveParserWithError: %@", errorString);
    [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"Error", @"")
                                message:errorString
                               delegate:nil
                      cancelButtonTitle:NSLocalizedString(@"OK", @"")
                      otherButtonTitles:nil] show];
}

- (void)doParse:(NSData *)data
{
    [SVProgressHUD dismiss];

    NSString *dataString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];

    if (dataString) {
        NSXMLParser *nsXmlParser = [[NSXMLParser alloc] initWithData:data];
        XMLController *parser = [[XMLController alloc] init];
        [nsXmlParser setDelegate:parser];

        if ([nsXmlParser parse] && [parser finishWithError]) {
            [self showAlertWithDelegateAndMessage:[NSString stringWithFormat:@"%@\n(%@)", parser.err, parser.errorCode]];

            return;
        }
    }

    [_webView loadData:data MIMEType:_dataSource.mmtp textEncodingName:@"UTF-8" baseURL:nil];
    [_webView setScalesPageToFit:YES];
    [_webView setAutoresizingMask:UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight];

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

#pragma mark - UIWebViewDelegate

- (void)webView:(UIWebView *)webView didFailLoadWithError:(NSError *)error
{
    [self showAlertWithDelegateAndMessage:@"Lo sentimos pero la previsualización de este tipo de documentos no está disponible."];
}

- (void)showAlertWithDelegateAndMessage:(NSString *)message
{
    if (!_isShowingAlertView) {
        _isShowingAlertView = YES;
        [[[UIAlertView alloc] initWithTitle:@"Previsualización no disponible"
                                    message:message
                                   delegate:self
                          cancelButtonTitle:@"OK"
                          otherButtonTitles:nil, nil] show];
    }
}

#pragma mark - UIAlertViewDelegate

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex
{
    _isShowingAlertView = NO;
    [self.navigationController popViewControllerAnimated:YES];
}

@end
