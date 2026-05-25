package org.skyve.impl.domain.types.jaxb;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

/**
 * Handle CDATA sections.
 */
/**
 * JAXB {@link XmlAdapter} that wraps {@code String} element content in a CDATA
 * section during marshalling and unwraps it during unmarshalling.
 *
 * <p>Applied to metadata string fields (e.g., documentation blocks, SQL text) that
 * may contain characters requiring XML escaping.  The CDATA wrapper preserves the
 * raw content verbatim in the serialised XML.
 *
 * <p>Threading: stateless; instances are safe for concurrent use.
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
