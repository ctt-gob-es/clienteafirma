// Registro del esquema "afirma:" en Firefox.
//
// network.protocol-handler.expose.afirma es imprescindible en Firefox moderno:
// sin ella, network.protocol-handler.expose-all vale true por defecto, Firefox
// considera que el esquema le corresponde a el, no sabe que hacer con "afirma:"
// y no hace nada. Ni dialogo, ni error, ni entrada en la consola.
//
// network.protocol-handler.app.afirma ya no existe en el motor: comprobado con
// `strings` sobre libxul.so de Firefox 153, cero apariciones de
// "network.protocol-handler.app" frente a expose., external. y warn-external.,
// que si estan. Se conserva por compatibilidad con versiones antiguas que
// todavia la entiendan; no estorba en las que no.
pref("network.protocol-handler.expose.afirma",false);
pref("network.protocol-handler.external.afirma",true);
pref("network.protocol-handler.warn-external.afirma",false);
pref("network.protocol-handler.app.afirma","/usr/bin/autofirma");
