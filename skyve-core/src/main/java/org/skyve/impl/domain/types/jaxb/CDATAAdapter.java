package org.skyve.impl.domain.types.jaxb;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

/**
 * Handle CDATA sections.
 */
public class CDATAAdapter extends XmlAdapter<String, String> {
	@Override
	public String marshal(String value) throws Exception {
		return new StringBuilder(value.length() + XMLMetaData.CDATA_MIN_LENGTH)
						.append(XMLMetaData.CDATA_START_TAG)
						.append(value)
						.append(XMLMetaData.CDATA_END_TAG).toString();
	}

	@Override
	public String unmarshal(String value) throws Exception {
		return value;
	}
}
