//
//  AppDelegate.m
//  Cliente @firma para OS X
//
//  Created by Tomas Garcia-Meras on 27/6/14.
//  Copyright (c) 2014 Gobierno de España. All rights reserved.
//

#import "AppDelegate.h"

@interface AppDelegate ()
            
@property (weak) IBOutlet NSWindow *window;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    @try
    {
        launch(nil);
    }
    @catch (NSException *exception)
    {
        NSAlert *alert = [[NSAlert alloc] init];
        [alert setAlertStyle:NSAlertStyleCritical];
        [alert setMessageText:[exception reason]];
        [alert runModal];
    }

}

- (void)applicationWillTerminate:(NSNotification *)aNotification
{
    // Insert code here to tear down your application
}

- (void) awakeFromNib
{
    
    // Obtenemos el gestor de eventos
    NSAppleEventManager *appleEventManager = [NSAppleEventManager sharedAppleEventManager];
    
    // Nos registramos para recibir el evento de invocacion por protocolo
    [appleEventManager setEventHandler:self andSelector:@selector(handleGetURLEvent:withReplyEvent:) forEventClass:kInternetEventClass andEventID:kAEGetURL];
    
}

- (void)handleGetURLEvent:(NSAppleEventDescriptor *)event withReplyEvent:(NSAppleEventDescriptor *)replyEvent
{
    
    NSURL *url = [NSURL URLWithString:[[event paramDescriptorForKeyword:keyDirectObject] stringValue]];
    
    @try {
        launch([[url absoluteString] UTF8String]);
    }
    @catch (NSException *exception)
    {
        NSAlert *alert = [[NSAlert alloc] init];
        [alert setAlertStyle:NSAlertStyleCritical];
        [alert setMessageText:[exception reason]];
        [alert runModal];
        
    }

}

int launch(const char *);

int launch(const char *commandName)
{
    
    // Establecemos el diretorio actual al home del usuario
    chdir([NSHomeDirectory() UTF8String]);
    
    // Obtenemos el Bundle...
    NSBundle *mainBundle = [NSBundle mainBundle];
    
    // ...Y su ruta
    NSString *mainBundlePath = [mainBundle bundlePath];
    
    // Creamos la linea de comandos
    NSTask *task;
    task = [[NSTask alloc] init];
    [task setLaunchPath:[NSString stringWithFormat:@"%@/Contents/Resources/Home/bin/java", mainBundlePath]];
    
    NSString *protocolUrl;
    @try	
    {
        protocolUrl = [[NSString alloc] initWithUTF8String:commandName];
    }
    @catch(NSException *exception)
    {
        protocolUrl = nil;
    }
    	
    NSArray *arguments;
    arguments = [NSArray arrayWithObjects: @"-Xms1024M",
                                           @"-Xmx2048M",
                                           @"-Djdk.tls.maxHandshakeMessageSize=65536",
                                           @"--add-exports",
                                           @"java.desktop/com.apple.eawt=ALL-UNNAMED",
                                           @"-Dcom.apple.macos.useScreenMenuBar=true",
                                           @"-Xdock:name=Autofirma",
                                           [NSString stringWithFormat:@"-Xdock:icon=%@/Contents/Resources/JAR/icon.icns", mainBundlePath],
                                           [NSString stringWithFormat:@"-Djava.library.path=%@/Contents/Resources/JAR", mainBundlePath],
                                           @"-jar",
                                           [NSString stringWithFormat:@"%@/Contents/Resources/JAR/autofirma.jar", mainBundlePath],
                                           protocolUrl,
                                           nil];
    [task setArguments: arguments];
    
    [task setStandardError:[NSFileHandle fileHandleWithNullDevice]];
    [task setStandardOutput:[NSFileHandle fileHandleWithNullDevice]];
    
    [task launch];
    
    [NSThread sleepForTimeInterval:5.0];
    [NSApp terminate:nil];
    
    return 0;
    
}


@end
