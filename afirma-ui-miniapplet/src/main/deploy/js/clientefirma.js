
function sign(dataB64, algorithm, format, params) {
	return clienteFirma.sign(dataB64, algorithm, format, params);
}

function coSign(signB64, dataB64, algorithm, format, params) {
	return clienteFirma.coSign(signB64, dataB64, algorithm, format, params);
}

function counterSign(signB64, algorithm, format, params) {
	return clienteFirma.counterSign(signB64, algorithm, format, params);
}

function getSignersStructure(signB64) {
	return clienteFirma.getSignersStructure(signB64);
}

function getEcoJava() {
	return clienteFirma.getEcoJava();
}

function verifyPlatform() {
	return clienteFirma.verifyPlatform();
}

function getBase64FromText(plainText, charset) {
	return clienteFirma.getBase64FromText(plainText, charset);
}

function getTextFromBase64(dataB64, charset) {
	return clienteFirma.getTextFromBase64(dataB64, charset);
}

function saveDataToFile(dataB64, title, fileName, extension, description) {
	return clienteFirma.saveDataToFile(dataB64, title, fileName, extension, description);
}

function loadFilePath(title, extensions, description) {
	return clienteFirma.loadFilePath(title, extensions, description);
}

function getFileContent(title, extensions, description) {
	return clienteFirma.getFileContent(title, extensions, description);
}

function getFileNameContentBase64(title, extensions, description) {
	return clienteFirma.getFileNameContentBase64(title, extensions, description);
}

function getFileNameContentText(title, extensions, description) {
	return clienteFirma.getFileNameContentText(title, extensions, description);
}

function getErrorMessage() {
	return clienteFirma.getErrorMessage();
}

