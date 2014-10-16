package es.gob.afirma.crypto.handwritten.wacom;

import com.WacomGSS.STU.Protocol.PenData;

final class Iso197947Helper {

	static String createIsoXml(final PenData[] pda) {


		final StringBuilder sb = new StringBuilder();
		sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n") //$NON-NLS-1$
		.append("<sig:SignatureSignTimeSeries SchemaVersion=\"0.1\"\n") //$NON-NLS-1$
		.append("xmlns:cmn=\"http://standards.iso.org/iso-iec/19794/-1/ed-2/amd/2\"\n") //$NON-NLS-1$
		.append("xmlns:sig=\"http://standards.iso.org/iso-iec/19794/-7\"\n") //$NON-NLS-1$
		.append("xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n") //$NON-NLS-1$
		.append("xsi:schemaLocation=\"http://standards.iso.org/iso-iec/19794/-7 19794-\n") //$NON-NLS-1$
		.append("7_Amd1_DAM.xsd \">\n") //$NON-NLS-1$
		.append("   <sig:Version>\n") //$NON-NLS-1$
		.append("     <cmn:Major>1</cmn:Major>\n") //$NON-NLS-1$
		.append("     <cmn:Minor>0</cmn:Minor>\n") //$NON-NLS-1$
		.append("   </sig:Version>\n") //$NON-NLS-1$
		.append("   <sig:RepresentationList>\n") //$NON-NLS-1$
		.append("      <sig:Representation>\n") //$NON-NLS-1$
		.append("         <sig:CaptureDateAndTime>2014-01-13T12:00:00</sig:CaptureDateAndTime>\n") //$NON-NLS-1$
		.append("         <sig:CaptureDevice>\n") //$NON-NLS-1$
		.append("            <sig:DeviceID>\n") //$NON-NLS-1$
		.append("               <cmn:Organization>1</cmn:Organization>\n") //$NON-NLS-1$
		.append("               <cmn:Identifier>1234</cmn:Identifier>\n") //$NON-NLS-1$
		.append("            </sig:DeviceID>\n") //$NON-NLS-1$
		.append("         <sig:DeviceTechnology>Electromagnetic</sig:DeviceTechnology>\n") //$NON-NLS-1$
		.append("      </sig:CaptureDevice>\n") //$NON-NLS-1$
		.append("      <sig:ChannelDescription>\n") //$NON-NLS-1$
		.append("         <sig:InclusionField>0F00</sig:InclusionField>\n") //$NON-NLS-1$
		.append("         <sig:Description>\n") //$NON-NLS-1$
		.append("            <sig:ScalingValue>1</sig:ScalingValue>\n") //$NON-NLS-1$
		.append("            <sig:MinChannelValue>0</sig:MinChannelValue>\n") //$NON-NLS-1$
		.append("            <sig:MaxChannelValue>0</sig:MaxChannelValue>\n") //$NON-NLS-1$
		.append("            <sig:AverageChannelValue>0</sig:AverageChannelValue>\n") //$NON-NLS-1$
		.append("            <sig:StandardDeviationValue>0</sig:StandardDeviationValue>\n") //$NON-NLS-1$
		.append("            <sig:ConstantValue>true</sig:ConstantValue>\n") //$NON-NLS-1$
		.append("            <sig:RemovedLinearComponent>true</sig:RemovedLinearComponent>\n") //$NON-NLS-1$
		.append("         </sig:Description>\n") //$NON-NLS-1$
		.append("      </sig:ChannelDescription>\n") //$NON-NLS-1$
		.append("      <sig:SamplePointList>\n"); //$NON-NLS-1$

		for (PenData pd : pda) {
			sb.append("         <sig:SamplePoint>\n") //$NON-NLS-1$
			.append("               <sig:PenTipCoord>\n") //$NON-NLS-1$
			.append("                  <sig:X>" + pd.getX() + "</sig:X>\n") //$NON-NLS-1$ //$NON-NLS-2$
			.append("                  <sig:Y>" + pd.getY() + "</sig:Y>\n") //$NON-NLS-1$ //$NON-NLS-2$
			.append("                  <sig:Z>" + pd.getPressure() + "</sig:Z>\n") //$NON-NLS-1$ //$NON-NLS-2$
			.append("               </sig:PenTipCoord>\n") //$NON-NLS-1$
			.append("         </sig:SamplePoint>\n"); //$NON-NLS-1$
		}

		sb.append("</sig:SamplePointList>\n") //$NON-NLS-1$
		.append("</sig:Representation>\n") //$NON-NLS-1$
		.append("</sig:RepresentationList>\n") //$NON-NLS-1$
		.append("<sig:VendorSpecificData>\n") //$NON-NLS-1$
		.append("<cmn:TypeCode>0</cmn:TypeCode>\n") //$NON-NLS-1$
		.append("<cmn:Data>MA==</cmn:Data>\n") //$NON-NLS-1$
		.append("</sig:VendorSpecificData>\n") //$NON-NLS-1$
		.append("</sig:SignatureSignTimeSeries>\n"); //$NON-NLS-1$
		return sb.toString();

	}

}
