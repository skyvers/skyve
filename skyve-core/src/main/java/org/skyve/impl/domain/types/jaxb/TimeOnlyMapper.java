package org.skyve.impl.domain.types.jaxb;

import java.util.Date;

import org.skyve.domain.types.TimeOnly;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlSchemaType(name = "time")
//@XmlJavaTypeAdapter(TimeOnlyMapper.class)
/**
 * JAXB {@link XmlAdapter} that converts between the XML {@code xs:time} type
 * ({@link java.util.Date}) and Skyve's {@link org.skyve.domain.types.TimeOnly} domain type.
 *
 * <p>On unmarshalling, wraps the JAXB-supplied {@code Date} in a new
 * {@link org.skyve.domain.types.TimeOnly} preserving the epoch millisecond value.
 * A {@code null} input produces a {@code null} output in both directions.
 *
 * <p>Threading: stateless; instances are safe for concurrent use.
 *
 * @see org.skyve.domain.types.TimeOnly
 */
public class TimeOnlyMapper extends XmlAdapter<Date, TimeOnly> {
	@Override
	public TimeOnly unmarshal(Date time) throws Exception {
		return (time == null) ? null : new TimeOnly(time.getTime());
	}

	@Override
	public Date marshal(TimeOnly timeOnly) throws Exception {
		return timeOnly;
	}
}
