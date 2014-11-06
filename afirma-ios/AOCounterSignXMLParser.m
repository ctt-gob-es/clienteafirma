//
//  AOXMLParser.m
//  SignSample02
//
//  Created by Javier on 20/10/14.
//  Copyright (c) 2014 Atos. All rights reserved.
//

#import "AOCounterSignXMLParser.h"
#import "AOCounterSignPreItems.h"

@implementation AOCounterSignXMLParser

// Punto de partida del parseo, se comprueban que los datos son validos y se prepara el contenedor de los datos.
-(AOCounterSignPreItems *)parseXML:(NSData *) data{
        if (data != nil) {
            self.counterSignPreItems = [[AOCounterSignPreItems alloc] init];
            self.xmlParser = [[NSXMLParser alloc] initWithData:data];
            self.xmlParser.delegate = self;
            // Start parsing.
            [self.xmlParser parse];
        }
    // Si se ha producido un error, enviamos el contenedor sin contenido.
    if (self.error) {
        self.counterSignPreItems = [[AOCounterSignPreItems alloc] init];
    }
    return self.counterSignPreItems;
}

// Se ejecuta antes de iniciar el parseo, se inician las variables que hacen falta para el proceso.
-(void)parserDidStartDocument:(NSXMLParser *)parser{
    self.error = false;
    self.dValue = [[NSString alloc] init];
    self.ddValue = [[NSString alloc] init];
    self.foundValue = [[NSString alloc] init];
}

-(void)parserDidEndDocument:(NSXMLParser *)parser{
}

// Cuando se produce un error durante el proceso de parseo, invalidamos el resultado.
-(void)parser:(NSXMLParser *)parser parseErrorOccurred:(NSError *)parseError{
    NSLog(@"Error durante la realizacion del parseo del XML: %@", [parseError localizedDescription]);
    self.error = true;
}

// Se ejecuta al detectar el comienzo de una nueva etiqueta XML.
-(void)parser:(NSXMLParser *)parser didStartElement:(NSString *)elementName namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qName attributes:(NSDictionary *)attributeDict{
    self.currentElement = elementName;
}

// Se ejecuta al detectar el final de una etiqueta XML,
// si es una etiqueta de interes para nosotros, usamos los datos que hemos ido almacenando para
// guardarlos en el contenedor que devolveremos.
-(void)parser:(NSXMLParser *)parser didEndElement:(NSString *)elementName namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qName{
    
    // Si encontramos la fecha, la almacenamos sin mas
    if ([elementName isEqualToString:@"cs"]) {
        
        NSString *aux = [[self.foundValue stringByReplacingOccurrencesOfString:@"\n" withString:@""]stringByReplacingOccurrencesOfString:@" " withString:@""];
        [self.counterSignPreItems setDate:aux];
    }
    // Si encontramos d o dd, al ser un par de valores, deberemos esperar a tener ambos para poder introducir los datos.
    // Por eso las variables solo se limpian tras haber hecho la insercion.
    else if ([elementName isEqualToString:@"d"] && ![self.ddValue  isEqual: @""]){
        
        NSString *auxDd = [[[[self.ddValue stringByReplacingOccurrencesOfString:@"\n" withString:@""] stringByReplacingOccurrencesOfString:@"\r" withString:@""] stringByReplacingOccurrencesOfString:@"\t" withString:@""] stringByReplacingOccurrencesOfString:@" " withString:@""];
        NSString *auxD = [[[[self.dValue stringByReplacingOccurrencesOfString:@"\n" withString:@""] stringByReplacingOccurrencesOfString:@"\r" withString:@""] stringByReplacingOccurrencesOfString:@"\t" withString:@""] stringByReplacingOccurrencesOfString:@" " withString:@""];
        [[self.counterSignPreItems elements] setObject:auxD forKey:auxDd ];
        
        self.dValue = @"";
        self.ddValue = @"";
    }
    else if ([elementName isEqualToString:@"dd"] && ![self.dValue  isEqual: @""]){
        
        NSString *auxDd = [[[[self.ddValue stringByReplacingOccurrencesOfString:@"\n" withString:@""] stringByReplacingOccurrencesOfString:@"\r" withString:@""] stringByReplacingOccurrencesOfString:@"\t" withString:@""] stringByReplacingOccurrencesOfString:@" " withString:@""];
        NSString *auxD = [[[[self.dValue stringByReplacingOccurrencesOfString:@"\n" withString:@""] stringByReplacingOccurrencesOfString:@"\r" withString:@""] stringByReplacingOccurrencesOfString:@"\t" withString:@""] stringByReplacingOccurrencesOfString:@" " withString:@""];
        [[self.counterSignPreItems elements] setObject:auxD forKey:auxDd ];
        
        self.dValue = @"";
        self.ddValue = @"";
    }
    
    self.foundValue = @"";
    
}

// Se optiene el contenido que se encuentra dentro de la etiqueta XML, el contenido que nos interesa.
-(void)parser:(NSXMLParser *)parser foundCharacters:(NSString *)string{
    
    if ([self.currentElement isEqualToString:@"cs"]) {
        // Evitamos seleccionar datos que no son de utilidad. 
        if (![[string stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] isEqualToString:@"\n"]) {
            self.foundValue = string;
        }
    }
    if([self.currentElement isEqualToString:@"d"]){
        if (![[string stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] isEqualToString:@"\n"]) {
            self.dValue = string;
        }
    }
    if([self.currentElement isEqualToString:@"dd"]){
        if (![[string stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] isEqualToString:@"\n"]) {
            self.ddValue = string;
        }
    }
}


@end
